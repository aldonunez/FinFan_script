#include "stdafx.h"
#include "Compiler.h"
#include "OpCodes.h"
#include <ctype.h>
#include <cstdarg>
#include "BinderVisitor.h"
#include "FolderVisitor.h"


Compiler::Compiler( U8* codeBin, int codeBinLen, ICompilerEnv* env, ICompilerLog* log, int modIndex ) :
    mCodeBin( codeBin ),
    mCodeBinPtr( codeBin ),
    mCodeBinEnd( codeBin + codeBinLen ),
    mEnv( env ),
    mRep( log ),
    mModIndex( modIndex ),
    mInFunc( false ),
    mCurFunc(),
    mCurExprDepth(),
    mMaxExprDepth(),
    mCompiled(),
    mCalculatedStats(),
    mStats()
{
}

CompilerErr Compiler::Compile( Unit* progTree )
{
    try
    {
        BindAttributes( progTree );
        FoldConstants( progTree );
        GenerateCode( progTree );

        GenerateLambdas();
        GenerateSentinel();
    }
    catch ( CompilerException& ex )
    {
        return ex.GetError();
    }

    mCompiled = true;

    return CERR_OK;
}

void Compiler::GetStats( CompilerStats& stats )
{
    if ( mCompiled && !mCalculatedStats )
    {
        mStats.CodeBytesWritten = mCodeBinPtr - mCodeBin;

        CalculateStackDepth();

        mCalculatedStats = true;
    }

    stats = mStats;
}

I32* Compiler::GetData()
{
    return &mGlobals.front();
}

size_t Compiler::GetDataSize()
{
    return mGlobals.size();
}

void Compiler::BindAttributes( Unit* progTree )
{
    BinderVisitor binder( mGlobalTable, mEnv, mRep.GetLog() );

    binder.Bind( progTree );

    mGlobals.resize( binder.GetDataSize() );
}

void Compiler::FoldConstants( Unit* progTree )
{
    FolderVisitor folder( mRep.GetLog() );

    folder.Fold( progTree );
}

void Compiler::GenerateCode( Unit* progTree )
{
    progTree->Accept( this );
}

void Compiler::VisitUnit( Unit* unit )
{
    for ( auto& varNode : unit->DataDeclarations )
        Generate( varNode.get() );

    for ( auto& funcNode : unit->FuncDeclarations )
        funcNode->Accept( this );
}

void Compiler::VisitArrayTypeRef( ArrayTypeRef* typeRef )
{
}

void Compiler::VisitInitList( InitList* initList )
{
}

void Compiler::VisitParamDecl( ParamDecl* paramDecl )
{
}

void Compiler::Generate( Syntax* elem )
{
    GenStatus status = { ExprKind::Other };
    Generate( elem, GenConfig::Statement(), status );
}

void Compiler::Generate( Syntax* elem, const GenConfig& config )
{
    GenStatus status = { ExprKind::Other };
    Generate( elem, config, status );
}

void Compiler::Generate( Syntax* node, const GenConfig& config, GenStatus& status )
{
    mGenStack.push_back( { config, status } );

    node->Accept( this );

    mGenStack.pop_back();

    if ( status.kind != ExprKind::Logical )
    {
        if ( config.trueChain != nullptr )
        {
            PushPatch( config.trueChain );

            mCodeBinPtr[0] = (config.invert && status.kind != ExprKind::Comparison) ? OP_BFALSE : OP_BTRUE;
            mCodeBinPtr += BranchInst::Size;
            DecreaseExprDepth();

            PushPatch( config.falseChain );

            mCodeBinPtr[0] = OP_B;
            mCodeBinPtr += BranchInst::Size;
        }
        else if ( config.invert && status.kind != ExprKind::Comparison )
        {
            if ( !config.discard )
            {
                mCodeBinPtr[0] = OP_NOT;
                mCodeBinPtr += 1;
            }
        }
    }
}

void Compiler::GenerateDiscard( Syntax* elem )
{
    GenConfig config{};

    GenerateDiscard( elem, config );
}

void Compiler::GenerateDiscard( Syntax* elem, const GenConfig& config )
{
    GenConfig configDiscard = config.WithDiscard();
    GenStatus status = { ExprKind::Other };

    mGenStack.push_back( { configDiscard, status } );

    elem->Accept( this );

    mGenStack.pop_back();

    if ( !status.discarded )
    {
        assert( status.discarded );
        mRep.LogWarning( elem->Line, elem->Column, "Deprecated: POP was emitted." );

        *mCodeBinPtr = OP_POP;
        mCodeBinPtr++;

        DecreaseExprDepth();
    }
}

void Compiler::VisitNumberExpr( NumberExpr* numberExpr )
{
    GenerateNumber( numberExpr, Config(), Status() );
}

void Compiler::GenerateNumber( NumberExpr* number, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    EmitLoadConstant( number->Value );
}

void Compiler::EmitLoadConstant( int32_t value )
{
    if ( (value >= INT8_MIN) && (value <= INT8_MAX) )
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = (U8) value;
        mCodeBinPtr += 2;
    }
    else
    {
        *mCodeBinPtr = OP_LDC;
        mCodeBinPtr++;
        WriteI32( mCodeBinPtr, value );
    }

    IncreaseExprDepth();
}

void Compiler::VisitNameExpr( NameExpr* nameExpr )
{
    GenerateSymbol( nameExpr, Config(), Status() );
}

void Compiler::GenerateSymbol( NameExpr* symbol, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    auto decl = symbol->Decl.get();

    switch ( decl->Kind )
    {
    case DeclKind::Global:
        mCodeBinPtr[0] = OP_LDMOD;
        mCodeBinPtr[1] = mModIndex;
        mCodeBinPtr += 2;
        WriteU16( mCodeBinPtr, ((Storage*) decl)->Offset );
        IncreaseExprDepth();
        break;

    case DeclKind::Local:
        mCodeBinPtr[0] = OP_LDLOC;
        mCodeBinPtr[1] = ((Storage*) decl)->Offset;
        mCodeBinPtr += 2;
        IncreaseExprDepth();
        break;

    case DeclKind::Arg:
        mCodeBinPtr[0] = OP_LDARG;
        mCodeBinPtr[1] = ((Storage*) decl)->Offset;
        mCodeBinPtr += 2;
        IncreaseExprDepth();
        break;

    case DeclKind::Func:
    case DeclKind::Forward:
        mRep.ThrowError( CERR_SEMANTICS, symbol, "functions don't have values" );
        break;

    case DeclKind::Const:
        {
            std::unique_ptr<NumberExpr> number( new NumberExpr() );
            number->Line = symbol->Line;
            number->Column = symbol->Column;
            number->Value = ((Constant*) decl)->Value;
            VisitNumberExpr( number.get() );
        }
        break;

    default:
        assert( false );
        mRep.ThrowInternalError();
    }
}

void Compiler::GenerateEvalStar( CallOrSymbolExpr* callOrSymbol, const GenConfig& config, GenStatus& status )
{
    auto& symbol = callOrSymbol->Symbol;
    auto decl = symbol->Decl.get();

    if ( decl->Kind == DeclKind::Func || decl->Kind == DeclKind::Forward )
    {
        std::unique_ptr<CallExpr> call( new CallExpr() );
        std::unique_ptr<NameExpr> nameExpr( new NameExpr() );

        nameExpr->String = symbol->String;
        nameExpr->Decl = symbol->Decl;
        call->Head = std::move( nameExpr );

        GenerateCall( call.get(), config, status );
    }
    else
    {
        GenerateSymbol( symbol.get(), config, status );
    }
}

void Compiler::VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol )
{
    GenerateEvalStar( callOrSymbol, Config(), Status() );
}

void Compiler::GenerateArithmetic( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    auto& op = binary->Op;
    int primitive;

    if ( op == "+" )
        primitive = PRIM_ADD;
    else if ( op == "-" )
        primitive = PRIM_SUB;
    else if ( op == "*" )
        primitive = PRIM_MUL;
    else if ( op == "/" )
        primitive = PRIM_DIV;
    else if ( op == "%" )
        primitive = PRIM_MOD;
    else
    {
        assert( false );
        mRep.ThrowInternalError();
    }

    GenerateBinaryPrimitive( binary, primitive, config, status );
}

void Compiler::VisitBinaryExpr( BinaryExpr* binary )
{
    if ( binary->Op[0] == '='
        || binary->Op[0] == '<'
        || binary->Op[0] == '>' )
    {
        GenerateComparison( binary, Config(), Status() );
    }
    else if ( binary->Op == "and" )
    {
        GenerateAnd( binary, Config(), Status() );
    }
    else if ( binary->Op == "or" )
    {
        GenerateOr( binary, Config(), Status() );
    }
    else
    {
        GenerateArithmetic( binary, Config(), Status() );
    }
}

void Compiler::GenerateReturn( ReturnStatement* retStmt, const GenConfig& config, GenStatus& status )
{
    if ( retStmt->Inner != nullptr )
    {
        Generate( retStmt->Inner.get() );
    }
    else
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = 0;
        mCodeBinPtr += 2;
        IncreaseExprDepth();
    }

    mCodeBinPtr[0] = OP_RET;
    mCodeBinPtr += 1;
    DecreaseExprDepth();

    status.discarded = true;
    status.tailRet = true;
}

void Compiler::VisitReturnStatement( ReturnStatement* retStmt )
{
    GenerateReturn( retStmt, Config(), Status() );
}

void Compiler::GenerateCond( CondExpr* condExpr, const GenConfig& config, GenStatus& status )
{
    PatchChain  leaveChain;
    bool        foundCatchAll = false;
    int         exprDepth = mCurExprDepth;

    GenConfig statementConfig = GenConfig::Statement( config.discard )
        .WithLoop( config.breakChain, config.nextChain );

    // TODO: check all the clauses for tail-return. If they all do, then set status.tailRet.

    for ( int i = 0; i < (int) condExpr->Clauses.size(); i++ )
    {
        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        auto clause = condExpr->Clauses[i].get();

        // TODO: make the check more general.

        bool isConstantTrue = false;

        if ( clause->Condition->Kind == SyntaxKind::Number )
        {
            if ( ((NumberExpr*) clause->Condition.get())->Value != 0 )
                isConstantTrue = true;
        }
        else if ( clause->Condition->Kind == SyntaxKind::Name )
        {
            auto decl = ((NameExpr*) clause->Condition.get())->Decl.get();

            if ( decl != nullptr && decl->Kind == DeclKind::Const && ((Constant*) decl)->Value != 0 )
                isConstantTrue = true;
        }

        if ( isConstantTrue )
        {
            if ( clause->Body.Statements.size() == 0 )
            {
                if ( !config.discard )
                {
                    mCodeBinPtr[0] = OP_LDC_S;
                    mCodeBinPtr[1] = 1;
                    mCodeBinPtr += 2;
                    IncreaseExprDepth();
                }
            }
            else
            {
                GenStatus clauseStatus = { ExprKind::Other };
                GenerateImplicitProgn( &clause->Body, statementConfig, clauseStatus );
            }
            foundCatchAll = true;
            break;
        }

        if ( clause->Body.Statements.size() == 0 )
        {
            Generate( clause->Condition.get() );

            if ( !config.discard )
            {
                mCodeBinPtr[0] = OP_DUP;
                mCodeBinPtr++;
                IncreaseExprDepth();
            }

            PushPatch( &leaveChain );

            mCodeBinPtr[0] = OP_BTRUE;
            mCodeBinPtr += BranchInst::Size;
            DecreaseExprDepth();
        }
        else
        {
            PatchChain  trueChain;
            PatchChain  falseChain;

            Generate( clause->Condition.get(), GenConfig::Expr( &trueChain, &falseChain, false ) );
            ElideTrue( &trueChain, &falseChain );
            Patch( &trueChain );

            // True
            GenStatus clauseStatus = { ExprKind::Other };
            GenerateImplicitProgn( &clause->Body, statementConfig, clauseStatus );

            if ( i < (int) condExpr->Clauses.size() - 1 || !config.discard )
            {
                PushPatch( &leaveChain );
                mCodeBinPtr[0] = OP_B;
                mCodeBinPtr += BranchInst::Size;
            }

            ElideFalse( &trueChain, &falseChain );
            Patch( &falseChain );
        }
    }

    if ( !foundCatchAll )
    {
        if ( !config.discard )
        {
            mCodeBinPtr[0] = OP_LDC_S;
            mCodeBinPtr[1] = 0;
            mCodeBinPtr += 2;
            IncreaseExprDepth();
        }
    }

    Patch( &leaveChain );

    if ( config.discard )
        status.discarded = true;
}

void Compiler::VisitCondExpr( CondExpr* condExpr )
{
    GenerateCond( condExpr, Config(), Status() );
}

void Compiler::GenerateSet( AssignmentExpr* assignment, const GenConfig& config, GenStatus& status )
{
    // Value
    Generate( assignment->Right.get() );

    if ( config.discard )
    {
        status.discarded = true;
    }
    else
    {
        *mCodeBinPtr++ = OP_DUP;
        IncreaseExprDepth();
    }

    if ( assignment->Left->Kind == SyntaxKind::Index )
    {
        auto indexExpr = (IndexExpr*) assignment->Left.get();

        GenerateArrayElementRef( indexExpr );

        mCodeBinPtr[0] = OP_STOREI;
        mCodeBinPtr++;

        DecreaseExprDepth( 2 );
        return;
    }

    auto targetSym = (NameExpr*) assignment->Left.get();
    auto decl = targetSym->Decl.get();

    switch ( decl->Kind )
    {
    case DeclKind::Global:
        mCodeBinPtr[0] = OP_STMOD;
        mCodeBinPtr[1] = mModIndex;
        mCodeBinPtr += 2;
        WriteU16( mCodeBinPtr, ((Storage*) decl)->Offset );
        break;

    case DeclKind::Local:
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = ((Storage*) decl)->Offset;
        mCodeBinPtr += 2;
        break;

    case DeclKind::Arg:
        mCodeBinPtr[0] = OP_STARG;
        mCodeBinPtr[1] = ((Storage*) decl)->Offset;
        mCodeBinPtr += 2;
        break;

    case DeclKind::Func:
    case DeclKind::Forward:
    case DeclKind::ExternalFunc:
    case DeclKind::NativeFunc:
        mRep.ThrowError( CERR_SEMANTICS, targetSym, "functions can't be assigned a value" );
        break;

    case DeclKind::Const:
        mRep.ThrowError( CERR_SEMANTICS, targetSym, "Constants can't be changed" );
        break;

    default:
        assert( false );
        mRep.ThrowInternalError();
    }

    DecreaseExprDepth();
}

void Compiler::VisitAssignmentExpr( AssignmentExpr* assignment )
{
    GenerateSet( assignment, Config(), Status() );
}

void Compiler::VisitProcDecl( ProcDecl* procDecl )
{
    U32 addr = (mCodeBinPtr - mCodeBin);

    auto func = (Function*) procDecl->Decl.get();

    func->Address = addr;

    auto patchIt = mPatchMap.find( func->Name );
    if ( patchIt != mPatchMap.end() )
        PatchCalls( &patchIt->second, addr );

    mEnv->AddExternal( procDecl->Name, External_Bytecode, func->Address );

    GenerateProc( procDecl, func );
}

void Compiler::GenerateLambda( LambdaExpr* lambdaExpr, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    *mCodeBinPtr = OP_LDC;
    mCodeBinPtr++;

    DeferredLambda lambda = { 0 };
    lambda.Definition = lambdaExpr->Proc.get();
    lambda.Patch = mCodeBinPtr;
    mLambdas.push_back( lambda );

    // Add the reference to the deferred lambda just linked
    mLocalAddrRefs.push_back( { AddrRefKind::Lambda, mLambdas.size() - 1 } );

    WriteU32( mCodeBinPtr, 0 );
    IncreaseExprDepth();
}

void Compiler::VisitLambdaExpr( LambdaExpr* lambdaExpr )
{
    GenerateLambda( lambdaExpr, Config(), Status() );
}

void Compiler::GenerateFunction( AddrOfExpr* addrOf, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    U32 addr = 0;

    auto func = (Function*) addrOf->Inner->Decl.get();

    if ( func->Address != INT32_MAX )
    {
        addr = func->Address;
    }
    else
    {
        PatchChain* chain = PushFuncPatch( func->Name );

        AddrRef ref = { AddrRefKind::Inst };
        ref.InstPtr = &chain->First->Inst;
        mLocalAddrRefs.push_back( ref );
    }

    mCodeBinPtr[0] = OP_LDC;
    mCodeBinPtr++;
    WriteU24( mCodeBinPtr, addr );
    mCodeBinPtr[0] = mModIndex;
    mCodeBinPtr++;

    IncreaseExprDepth();
}

void Compiler::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
    GenerateFunction( addrOf, Config(), Status() );
}

void Compiler::GenerateFuncall( CallExpr* call, const GenConfig& config, GenStatus& status )
{
    for ( int i = call->Arguments.size() - 1; i >= 0; i-- )
    {
        Generate( call->Arguments[i].get() );
    }

    Generate( call->Head.get() );

    int argCount = call->Arguments.size();

    mCodeBinPtr[0] = OP_CALLI;
    mCodeBinPtr[1] = CallFlags::Build( argCount, config.discard );
    mCodeBinPtr += 2;

    DecreaseExprDepth();

    if ( config.discard )
        status.discarded = true;
    else
        IncreaseExprDepth();

    DecreaseExprDepth( argCount );

    if ( mInFunc )
        mCurFunc->CallsIndirectly = true;
}

void Compiler::GenerateLet( LetStatement* letStmt, const GenConfig& config, GenStatus& status )
{
    for ( auto& binding : letStmt->Variables )
    {
        GenerateLetBinding( binding.get() );
    }

    GenerateImplicitProgn( &letStmt->Body, config, status );
}

void Compiler::GenerateLetBinding( DataDecl* binding )
{
    auto local = (Storage*) binding->GetDecl();

    if ( binding->TypeRef == nullptr )
    {
        if ( binding->Initializer != nullptr )
        {
            Generate( binding->Initializer.get() );
            mCodeBinPtr[0] = OP_STLOC;
            mCodeBinPtr[1] = local->Offset;
            mCodeBinPtr += 2;
            DecreaseExprDepth();
        }
    }
    else if ( binding->TypeRef->Kind == SyntaxKind::Other )
    {
        auto type = (ArrayTypeRef*) binding->TypeRef.get();

        if ( binding->Initializer != nullptr )
        {
            AddLocalDataArray( local, binding->Initializer.get(), type->Size );
        }
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, binding, "'let' binding takes a name or name and type" );
    }
}

void Compiler::VisitLetStatement( LetStatement* letStmt )
{
    GenerateLet( letStmt, Config(), Status() );
}

// TODO: try to merge this with AddGlobalDataArray. Separate the array processing from the code generation

void Compiler::AddLocalDataArray( Storage* local, Syntax* valueElem, size_t size )
{
    if ( valueElem->Kind != SyntaxKind::Other )
        mRep.ThrowError( CERR_SEMANTICS, valueElem, "Arrays must be initialized with array initializer" );

    Syntax* lastTwoElems[2] = {};
    size_t locIndex = local->Offset;
    size_t i = 0;

    auto initList = (InitList*) valueElem;

    for ( auto& entry : initList->Values )
    {
        if ( i == size )
            mRep.ThrowError( CERR_SEMANTICS, valueElem, "Array has too many initializers" );

        lastTwoElems[0] = lastTwoElems[1];
        lastTwoElems[1] = entry.get();

        Generate( entry.get() );
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = (U8) locIndex;
        mCodeBinPtr += 2;
        i++;
        locIndex--;
        DecreaseExprDepth();
    }

    I32 prevValue = 0;
    I32 step = 0;

    if ( initList->HasExtra && lastTwoElems[1] != nullptr )
    {
        prevValue = GetElementValue( lastTwoElems[1], "Array initializer extrapolation requires a constant" );

        if ( lastTwoElems[0] != nullptr )
        {
            auto prevValue2 = GetOptionalElementValue( lastTwoElems[0] );
            if ( prevValue2.has_value() )
                step = prevValue - prevValue2.value();
        }
    }

    for ( ; i < size; i++ )
    {
        I32 newValue = prevValue + step;

        EmitLoadConstant( newValue );
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = (U8) locIndex;
        mCodeBinPtr += 2;
        locIndex--;
        DecreaseExprDepth();

        prevValue = newValue;
    }
}

void Compiler::GenerateCall( CallExpr* call, const GenConfig& config, GenStatus& status )
{
    if ( call->Head->Kind != SyntaxKind::Name )
        mRep.ThrowError( CERR_SEMANTICS, call, "Direct call requires a named function" );

    auto op = (NameExpr*) call->Head.get();

    for ( int i = call->Arguments.size() - 1; i >= 0; i-- )
    {
        Generate( call->Arguments[i].get() );
    }

    int argCount = call->Arguments.size();
    U8 callFlags = CallFlags::Build( argCount, config.discard );

    auto decl = call->Head->GetDecl();

    if ( decl == nullptr )
    {
        mRep.ThrowInternalError( "Call head has no declaration" );
    }
    else if ( decl->Kind == DeclKind::Func )
    {
        Function* func = (Function*) decl;
        U32 addr = 0;

        if ( func->Address != INT32_MAX )
        {
            addr = func->Address;
        }
        else
        {
            PatchChain* chain = PushFuncPatch( func->Name );

            AddrRef ref = { AddrRefKind::Inst };
            ref.InstPtr = &chain->First->Inst;
            mLocalAddrRefs.push_back( ref );
        }

        mCodeBinPtr[0] = OP_CALL;
        mCodeBinPtr[1] = callFlags;
        mCodeBinPtr += 2;
        WriteU24( mCodeBinPtr, addr );

        if ( mInFunc )
            mCurFunc->CalledFunctions.push_back( op->String );
    }
    else
    {
        int opCode = 0;
        I32 id = 0;

        if ( decl->Kind == DeclKind::ExternalFunc )
        {
            opCode = OP_CALLM;
            id = ((ExternalFunction*) decl)->Id;

            // TODO: Add this external call to the list of called functions
        }
        else if ( decl->Kind == DeclKind::NativeFunc )
        {
            id = ((NativeFunction*) decl)->Id;

            if ( id >= 0x100 )
                opCode = OP_CALLNATIVE;
            else
                opCode = OP_CALLNATIVE_S;
        }
        else
        {
            assert( false );
            mRep.ThrowInternalError();
        }

        mCodeBinPtr[0] = opCode;
        mCodeBinPtr[1] = callFlags;
        mCodeBinPtr += 2;

        if ( opCode == OP_CALLNATIVE_S )
        {
            *(U8*) mCodeBinPtr = (U8) id;
            mCodeBinPtr += 1;
        }
        else
        {
            WriteU32( mCodeBinPtr, id );
        }
    }

    IncreaseExprDepth();

    if ( config.discard )
    {
        status.discarded = true;
        DecreaseExprDepth();
    }

    DecreaseExprDepth( argCount );
}

void Compiler::VisitCallExpr( CallExpr* call )
{
    if ( call->IsIndirect )
        GenerateFuncall( call, Config(), Status() );
    else
        GenerateCall( call, Config(), Status() );
}

void Compiler::GenerateFor( ForStatement* forStmt, const GenConfig& config, GenStatus& status )
{
    // Variable name

    auto local = (Storage*) forStmt->IndexDecl.get();

    int primitive;
    int step;

    if ( 0 == strcmp( forStmt->Comparison.c_str(), "below" ) )
    {
        primitive = PRIM_LT;
        step = 1;
    }
    else if ( 0 == strcmp( forStmt->Comparison.c_str(), "to" ) )
    {
        primitive = PRIM_LE;
        step = 1;
    }
    else if ( 0 == strcmp( forStmt->Comparison.c_str(), "downto" ) )
    {
        primitive = PRIM_GE;
        step = -1;
    }
    else if ( 0 == strcmp( forStmt->Comparison.c_str(), "above" ) )
    {
        primitive = PRIM_GT;
        step = -1;
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, forStmt, "Expected symbol: to, downto, above, below" );
    }

    PatchChain  bodyChain;
    PatchChain  testChain;
    PatchChain  breakChain;
    PatchChain  nextChain;

    // Beginning expression
    Generate( forStmt->First.get() );
    mCodeBinPtr[0] = OP_DUP;
    mCodeBinPtr[1] = OP_STLOC;
    mCodeBinPtr[2] = local->Offset;
    mCodeBinPtr += 3;
    IncreaseExprDepth();
    DecreaseExprDepth();

    // The unconditional jump below leaves one value on the expression stack.
    // Decrease the depth here, because this value overlaps the first one pushed
    // for a usual test.
    DecreaseExprDepth();

    PushPatch( &testChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    U8* bodyPtr = mCodeBinPtr;

    // Body
    GenerateStatements( &forStmt->Body, config.WithLoop( &breakChain, &nextChain ), status );

    Patch( &nextChain );

    if ( forStmt->Step != nullptr )
        Generate( forStmt->Step.get() );
    else
        EmitLoadConstant( step );

    mCodeBinPtr[0] = OP_LDLOC;
    mCodeBinPtr[1] = local->Offset;
    mCodeBinPtr[2] = OP_PRIM;
    mCodeBinPtr[3] = PRIM_ADD;
    mCodeBinPtr[4] = OP_DUP;
    mCodeBinPtr[5] = OP_STLOC;
    mCodeBinPtr[6] = local->Offset;
    mCodeBinPtr += 7;
    IncreaseExprDepth();
    DecreaseExprDepth();
    IncreaseExprDepth();
    DecreaseExprDepth();

    Patch( &testChain );

    // Ending expression
    Generate( forStmt->Last.get() );

    mCodeBinPtr[0] = OP_PRIM;
    mCodeBinPtr[1] = primitive;
    mCodeBinPtr += 2;
    DecreaseExprDepth();

    PushPatch( &bodyChain );
    mCodeBinPtr[0] = OP_BTRUE;
    mCodeBinPtr += BranchInst::Size;
    DecreaseExprDepth();

    Patch( &bodyChain, bodyPtr );
    Patch( &breakChain );

    GenerateNilIfNeeded( config, status );
}

void Compiler::VisitForStatement( ForStatement* forStmt )
{
    GenerateFor( forStmt, Config(), Status() );
}

void Compiler::GenerateSimpleLoop( LoopStatement* loopStmt, const GenConfig& config, GenStatus& status )
{
    PatchChain  breakChain;
    PatchChain  nextChain;

    U8* bodyPtr = mCodeBinPtr;

    // Body
    GenerateStatements( &loopStmt->Body, config.WithLoop( &breakChain, &nextChain ), status );

    if ( loopStmt->Condition == nullptr )
    {
        PushPatch( &nextChain );
        mCodeBinPtr[0] = OP_B;
        mCodeBinPtr += BranchInst::Size;

        Patch( &nextChain, bodyPtr );
    }
    else
    {
        GenStatus exprStatus;
        PatchChain bodyChain;

        Patch( &nextChain );

        Generate( loopStmt->Condition.get(), GenConfig::Expr( &bodyChain, &breakChain, false ), exprStatus );
        ElideFalse( &bodyChain, &breakChain );

        Patch( &bodyChain, bodyPtr );
    }

    Patch( &breakChain );

    GenerateNilIfNeeded( config, status );
}

void Compiler::VisitLoopStatement( LoopStatement* loopStmt )
{
    GenerateSimpleLoop( loopStmt, Config(), Status() );
}

void Compiler::GenerateDo( WhileStatement* whileStmt, const GenConfig& config, GenStatus& status )
{
    PatchChain  breakChain;
    PatchChain  nextChain;
    PatchChain  trueChain;

    U8* testPtr = mCodeBinPtr;

    // Test expression
    Generate( whileStmt->Condition.get(), GenConfig::Expr( &trueChain, &breakChain, false ) );

    ElideTrue( &trueChain, &breakChain );
    Patch( &trueChain );

    // Body
    GenerateStatements( &whileStmt->Body, config.WithLoop( &breakChain, &nextChain ), status );

    PushPatch( &nextChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    Patch( &breakChain );
    Patch( &nextChain, testPtr );

    GenerateNilIfNeeded( config, status );
}

void Compiler::VisitWhileStatement( WhileStatement* whileStmt )
{
    GenerateDo( whileStmt, Config(), Status() );
}

void Compiler::GenerateBreak( BreakStatement* breakStmt, const GenConfig& config, GenStatus& status )
{
    if ( config.breakChain == nullptr )
        mRep.ThrowError( CERR_SEMANTICS, breakStmt, "Cannot use break outside of a loop" );

    PushPatch( config.breakChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    status.discarded = true;
}

void Compiler::VisitBreakStatement( BreakStatement* breakStmt )
{
    GenerateBreak( breakStmt, Config(), Status() );
}

void Compiler::GenerateNext( NextStatement* nextStmt, const GenConfig& config, GenStatus& status )
{
    if ( config.nextChain == nullptr )
        mRep.ThrowError( CERR_SEMANTICS, nextStmt, "Cannot use next outside of a loop" );

    PushPatch( config.nextChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    status.discarded = true;
}

void Compiler::VisitNextStatement( NextStatement* nextStmt )
{
    GenerateNext( nextStmt, Config(), Status() );
}

void Compiler::GenerateCase( CaseExpr* caseExpr, const GenConfig& config, GenStatus& status )
{
    GenerateGeneralCase( caseExpr, config, status );
}

void Compiler::GenerateGeneralCase( CaseExpr* caseExpr, const GenConfig& config, GenStatus& status )
{
    PatchChain exitChain;

    const GenConfig& statementConfig = config;

    if ( caseExpr->TestKeyDecl != nullptr )
    {
        auto local = (Storage*) caseExpr->TestKeyDecl.get();

        Generate( caseExpr->TestKey.get() );
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = local->Offset;
        mCodeBinPtr += 2;
        DecreaseExprDepth();

        // Replace the keyform expression with the temporary variable
        std::unique_ptr<NameExpr> localSym( new NameExpr() );
        localSym->String = "$testKey";
        localSym->Decl = caseExpr->TestKeyDecl;
        caseExpr->TestKey = std::move( localSym );
    }

    for ( auto& clause : caseExpr->Clauses )
    {
        PatchChain falseChain;
        PatchChain trueChain;

        int i = 0;

        for ( auto& key : clause->Keys )
        {
            i++;

            Generate( caseExpr->TestKey.get() );
            Generate( key.get() );

            mCodeBinPtr[0] = OP_PRIM;
            mCodeBinPtr[1] = PRIM_EQ;
            mCodeBinPtr += 2;
            DecreaseExprDepth();

            if ( i == clause->Keys.size() )
            {
                PushPatch( &falseChain );
                mCodeBinPtr[0] = OP_BFALSE;
                mCodeBinPtr += BranchInst::Size;
                DecreaseExprDepth();
            }
            else
            {
                PushPatch( &trueChain );
                mCodeBinPtr[0] = OP_BTRUE;
                mCodeBinPtr += BranchInst::Size;
                DecreaseExprDepth();
            }
        }

        Patch( &trueChain );

        GenerateImplicitProgn( &clause->Body, statementConfig, status );

        PushPatch( &exitChain );
        mCodeBinPtr[0] = OP_B;
        mCodeBinPtr += BranchInst::Size;

        Patch( &falseChain );
    }

    if ( caseExpr->Fallback != nullptr )
    {
        GenerateImplicitProgn( &caseExpr->Fallback->Body, statementConfig, status );
    }

    Patch( &exitChain );
}

void Compiler::VisitCaseExpr( CaseExpr* caseExpr )
{
    GenerateCase( caseExpr, Config(), Status() );
}

void Compiler::VisitUnaryExpr( UnaryExpr* unary )
{
    if ( unary->Op == "not" )
    {
        Generate( unary->Inner.get(), Config().Invert(), Status() );
        Status().kind = ExprKind::Logical;
    }
    else if ( unary->Op == "-" )
    {
        GenerateUnaryPrimitive( unary->Inner.get(), Config(), Status() );
    }
}

void Compiler::GenerateComparison( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    auto& op = binary->Op;
    int positivePrimitive;
    int negativePrimitive;

    if ( op == "=" )
    {
        positivePrimitive = PRIM_EQ;
        negativePrimitive = PRIM_NE;
    }
    else if ( op == "<>" )
    {
        positivePrimitive = PRIM_NE;
        negativePrimitive = PRIM_EQ;
    }
    else if ( op == "<" )
    {
        positivePrimitive = PRIM_LT;
        negativePrimitive = PRIM_GE;
    }
    else if ( op == "<=" )
    {
        positivePrimitive = PRIM_LE;
        negativePrimitive = PRIM_GT;
    }
    else if ( op == ">" )
    {
        positivePrimitive = PRIM_GT;
        negativePrimitive = PRIM_LE;
    }
    else if ( op == ">=" )
    {
        positivePrimitive = PRIM_GE;
        negativePrimitive = PRIM_LT;
    }
    else
    {
        assert( false );
        mRep.ThrowInternalError();
    }

    GenerateBinaryPrimitive( binary, config.invert ? negativePrimitive : positivePrimitive, config, status );
    status.kind = ExprKind::Comparison;
}

void Compiler::GenerateAnd( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    ConjSpec spec = { &Compiler::GenerateAndClause, &Compiler::GenerateOrClause };
    GenerateConj( &spec, binary, config );
    status.kind = ExprKind::Logical;

    if ( config.discard )
        status.discarded = true;
}

void Compiler::GenerateOr( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    ConjSpec spec = { &Compiler::GenerateOrClause, &Compiler::GenerateAndClause };
    GenerateConj( &spec, binary, config );
    status.kind = ExprKind::Logical;

    if ( config.discard )
        status.discarded = true;
}

void Compiler::GenerateConj( ConjSpec* spec, BinaryExpr* binary, const GenConfig& config )
{
    if ( config.trueChain == nullptr )
    {
        Atomize( spec, binary, config.invert, config.discard );
        return;
    }

    if ( config.invert )
    {
        (this->*(spec->NegativeGenerator))( binary->Left.get(), config );
    }
    else
    {
        (this->*(spec->PositiveGenerator))( binary->Left.get(), config );
    }

    Generate( binary->Right.get(), config );
}

void Compiler::GenerateAndClause( Syntax* elem, const GenConfig& config )
{
    PatchChain  newTrueChain;
    Generate( elem, config.WithTrue( &newTrueChain ) );
    ElideTrue( &newTrueChain, config.falseChain );
    Patch( &newTrueChain );
}

void Compiler::GenerateOrClause( Syntax* elem, const GenConfig& config )
{
    PatchChain  newFalseChain;
    Generate( elem, config.WithFalse( &newFalseChain ) );
    ElideFalse( config.trueChain, &newFalseChain );
    Patch( &newFalseChain );
}

void Compiler::Atomize( ConjSpec* spec, BinaryExpr* binary, bool invert, bool discard )
{
    PatchChain  trueChain;
    PatchChain  falseChain;

    GenerateConj( spec, binary, GenConfig::Expr( &trueChain, &falseChain, invert ) );

    ElideTrue( &trueChain, &falseChain );

    Patch( &trueChain );

    if ( !discard )
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = 1;
        mCodeBinPtr[2] = OP_B;
        mCodeBinPtr += 3;

        // Offset of 2 to jump over LDC.S below.
        BranchInst::WriteOffset( mCodeBinPtr, 2 );
    }

    Patch( &falseChain );

    if ( !discard )
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = 0;
        mCodeBinPtr += 2;

        IncreaseExprDepth();
    }
}

U8 Compiler::InvertJump( U8 opCode )
{
    switch ( opCode )
    {
    case OP_BTRUE:  return OP_BFALSE;
    case OP_BFALSE: return OP_BTRUE;
    default:
        assert( false );
        mRep.ThrowInternalError();
    }
}

void Compiler::PushPatch( PatchChain* chain )
{
    InstPatch* link = new InstPatch;
    link->Inst = mCodeBinPtr;
    link->Next = chain->First;
    chain->First = link;
}

void Compiler::PopPatch( PatchChain* chain )
{
    assert( chain->First != nullptr );

    auto link = chain->First;
    chain->First = chain->First->Next;
    delete link;
}

Compiler::PatchChain* Compiler::PushFuncPatch( const std::string& name )
{
    auto patchIt = mPatchMap.find( name );
    if ( patchIt == mPatchMap.end() )
    {
        auto result = mPatchMap.insert( PatchMap::value_type( { name, PatchChain() } ) );
        patchIt = std::move( result.first );
    }

    PushPatch( &patchIt->second );

    return &patchIt->second;
}

void Compiler::ElideTrue( PatchChain* trueChain, PatchChain* falseChain )
{
    if (   trueChain->First  == nullptr
        || falseChain->First == nullptr )
        return;

    U8* target = mCodeBinPtr;
    size_t diff = target - (trueChain->First->Inst + BranchInst::Size);

    if ( diff == BranchInst::Size
        && mCodeBinPtr[-BranchInst::Size] == OP_B
        && &mCodeBinPtr[-BranchInst::Size] == falseChain->First->Inst
        )
    {
        falseChain->First->Inst = trueChain->First->Inst;
        trueChain->First->Inst[0] = InvertJump( trueChain->First->Inst[0] );

        // Remove the branch instruction.
        PopPatch( trueChain );
        mCodeBinPtr -= BranchInst::Size;
    }
}

void Compiler::ElideFalse( PatchChain* trueChain, PatchChain* falseChain )
{
    if ( falseChain->First == nullptr )
        return;

    U8* target = mCodeBinPtr;
    size_t diff = target - (falseChain->First->Inst + BranchInst::Size);

    if ( diff == 0 )
    {
        // Remove the branch instruction.
        PopPatch( falseChain );
        mCodeBinPtr -= BranchInst::Size;
    }
}

void Compiler::Patch( PatchChain* chain, U8* targetPtr )
{
    U8* target = (targetPtr != nullptr) ? targetPtr : mCodeBinPtr;

    for ( InstPatch* link = chain->First; link != nullptr; link = link->Next )
    {
        ptrdiff_t diff = target - (link->Inst + BranchInst::Size);

        if ( diff < BranchInst::OffsetMin || diff > BranchInst::OffsetMax )
            mRep.ThrowError( CERR_UNSUPPORTED, nullptr, "Branch target is too far." );

        BranchInst::StoreOffset( &link->Inst[1], diff );
    }
}

void Compiler::PatchCalls( PatchChain* chain, U32 addr )
{
    for ( InstPatch* link = chain->First; link != nullptr; link = link->Next )
    {
        int offset = 0;

        switch ( link->Inst[0] )
        {
        case OP_CALL:   offset = 2; break;
        case OP_LDC:    offset = 1; break;
        default:        assert( false ); break;
        }

        StoreU24( &link->Inst[offset], addr );
    }
}

void Compiler::GenerateUnaryPrimitive( Syntax* elem, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        Generate( elem, config, status );
    }
    else
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = 0;
        mCodeBinPtr += 2;

        Generate( elem );

        mCodeBinPtr[0] = OP_PRIM;
        mCodeBinPtr[1] = PRIM_SUB;
        mCodeBinPtr += 2;

        IncreaseExprDepth();
        DecreaseExprDepth();
    }
}

void Compiler::GenerateBinaryPrimitive( BinaryExpr* binary, int primitive, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        GenerateDiscard( binary->Left.get() );
        GenerateDiscard( binary->Right.get() );
        status.discarded = true;
    }
    else
    {
        Generate( binary->Left.get() );
        Generate( binary->Right.get() );

        mCodeBinPtr[0] = OP_PRIM;
        mCodeBinPtr[1] = primitive;
        mCodeBinPtr += 2;

        DecreaseExprDepth();
    }
}

void Compiler::GenerateArrayElementRef( IndexExpr* indexExpr )
{
    if ( indexExpr->Head->Kind != SyntaxKind::Name )
        mRep.ThrowError( CERR_SEMANTICS, indexExpr, "Only named arrays can be indexed" );

    Generate( indexExpr->Index.get() );

    auto symbol = (NameExpr*) indexExpr->Head.get();
    uint32_t addrWord;

    auto decl = symbol->Decl.get();

    if ( decl == nullptr )
    {
        mRep.ThrowError( CERR_SEMANTICS, symbol, "symbol not found '%s'", symbol->String.c_str() );
    }
    else
    {
        switch ( decl->Kind )
        {
        case DeclKind::Global:
            addrWord = CodeAddr::Build( ((Storage*) decl)->Offset, mModIndex );
            mCodeBinPtr[0] = OP_LDC;
            mCodeBinPtr++;
            WriteU32( mCodeBinPtr, addrWord );
            break;

        case DeclKind::Local:
            mCodeBinPtr[0] = OP_LDLOCA;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
            break;

        default:
            mRep.ThrowError( CERR_SEMANTICS, symbol, "'aref' supports only globals and locals" );
        }
    }

    IncreaseExprDepth();

    mCodeBinPtr[0] = OP_PRIM;
    mCodeBinPtr[1] = PRIM_ADD;
    mCodeBinPtr += 2;

    DecreaseExprDepth();
}

void Compiler::GenerateAref( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    GenerateArrayElementRef( indexExpr );

    mCodeBinPtr[0] = OP_LOADI;
    mCodeBinPtr++;

    DecreaseExprDepth();
    IncreaseExprDepth();
}

void Compiler::VisitIndexExpr( IndexExpr* indexExpr )
{
    GenerateAref( indexExpr, Config(), Status() );
}

void Compiler::GenerateDefvar( VarDecl* varDecl, const GenConfig& config, GenStatus& status )
{
    auto global = (Storage*) varDecl->GetDecl();

    if ( varDecl->TypeRef == nullptr )
    {
        if ( varDecl->Initializer != nullptr )
        {
            AddGlobalData( global->Offset, varDecl->Initializer.get() );
        }
    }
    else if ( varDecl->TypeRef->Kind == SyntaxKind::Other )
    {
        auto type = (ArrayTypeRef*) varDecl->TypeRef.get();

        if ( varDecl->Initializer != nullptr )
        {
            AddGlobalDataArray( global, varDecl->Initializer.get(), type->Size );
        }
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, varDecl, "'defvar' takes a name or name and type" );
    }
}

void Compiler::VisitVarDecl( VarDecl* varDecl )
{
    GenerateDefvar( varDecl, Config(), Status() );
}

void Compiler::AddGlobalData( U32 offset, Syntax* valueElem )
{
    mGlobals[offset] = GetElementValue( valueElem, "Globals must be initialized with constant data" );
}

void Compiler::AddGlobalDataArray( Storage* global, Syntax* valueElem, size_t size )
{
    if ( valueElem->Kind != SyntaxKind::Other )
        mRep.ThrowError( CERR_SEMANTICS, valueElem, "Arrays must be initialized with array initializer" );

    size_t i = 0;

    auto initList = (InitList*) valueElem;

    for ( auto& entry : initList->Values )
    {
        if ( i == size )
            mRep.ThrowError( CERR_SEMANTICS, valueElem, "Array has too many initializers" );

        AddGlobalData( global->Offset + i, entry.get() );
        i++;
    }

    I32 prevValue = 0;
    I32 step = 0;

    if ( initList->HasExtra )
    {
        if ( i >= 1 )
            prevValue = mGlobals[global->Offset + i - 1];

        if ( i >= 2 )
            step = prevValue - mGlobals[global->Offset + i - 2];
    }

    for ( ; i < size; i++ )
    {
        I32 newValue = prevValue + step;

        mGlobals[global->Offset + i] = newValue;
        prevValue = newValue;
    }
}

void Compiler::VisitConstDecl( ConstDecl* constDecl )
{
    // Nothing
}

void Compiler::GenerateLambdas()
{
    long i = 0;

    for ( auto it = mLambdas.begin(); it != mLambdas.end(); it++, i++ )
    {
        int address = mCodeBinPtr - mCodeBin;
        int addrWord = CodeAddr::Build( address, mModIndex );
        StoreU32( it->Patch, addrWord );

        auto func = (Function*) it->Definition->GetDecl();

        func->Address = address;

        GenerateProc( it->Definition, func );
    }
}

void Compiler::GenerateProc( ProcDecl* procDecl, Function* func )
{
    mInFunc = true;
    mCurFunc = func;

    constexpr uint8_t PushInstSize = 2;

    U8* bodyPtr = mCodeBinPtr;
    U8* pushCountPatch = nullptr;

    // Assume that there are local variables
    *mCodeBinPtr = OP_PUSH;
    mCodeBinPtr++;
    pushCountPatch = mCodeBinPtr;
    mCodeBinPtr++;

    mCurExprDepth = 0;
    mMaxExprDepth = 0;
    mLocalAddrRefs.clear();

    GenConfig config = GenConfig::Statement();
    GenStatus status = { ExprKind::Other };

    mGenStack.push_back( { config, status } );

    procDecl->Body.Accept( this );

    mGenStack.pop_back();

    if ( !status.tailRet )
    {
        mCodeBinPtr[0] = OP_RET;
        mCodeBinPtr += 1;
    }

    if ( func->LocalCount > 0 )
    {
        *pushCountPatch = (uint8_t) func->LocalCount;
    }
    else
    {
        // No locals. So, delete the PUSH instruction
        memmove( bodyPtr, bodyPtr + PushInstSize, (mCodeBinPtr - bodyPtr) - PushInstSize );
        mCodeBinPtr -= PushInstSize;

        // If local lambda references were generated, then shift them
        // This also includes references to any function
        for ( auto ref : mLocalAddrRefs )
        {
            U8** ppInst = nullptr;

            switch ( ref.Kind )
            {
            case AddrRefKind::Lambda:
                ppInst = &mLambdas[ref.LambdaIndex].Patch;
                break;

            case AddrRefKind::Inst:
                ppInst = ref.InstPtr;
                break;

            default:
                mRep.ThrowInternalError();
            }

            *ppInst -= PushInstSize;
        }
    }

    func->ExprDepth = mMaxExprDepth;

    mCurFunc = nullptr;
    mInFunc = false;
}

void Compiler::GenerateImplicitProgn( StatementList* stmtList, const GenConfig& config, GenStatus& status )
{
    for ( int i = 0; i < (int) stmtList->Statements.size() - 1; i++ )
    {
        GenerateDiscard( stmtList->Statements[i].get() );
    }

    if ( stmtList->Statements.size() >= 1 )
    {
        Generate( stmtList->Statements.back().get(), config, status );
    }
    else    // There are no expressions
    {
        GenerateNilIfNeeded( config, status );
    }
}

void Compiler::VisitStatementList( StatementList* stmtList )
{
    GenerateImplicitProgn( stmtList, Config(), Status() );
}

void Compiler::GenerateStatements( StatementList* list, const GenConfig& config, GenStatus& status )
{
    for ( auto& node : list->Statements )
    {
        GenerateDiscard( node.get(), config );
    }
}

void Compiler::GenerateNilIfNeeded( const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
    }
    else
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = 0;
        mCodeBinPtr += 2;

        IncreaseExprDepth();
    }
}

const Compiler::GenConfig& Compiler::Config() const
{
    assert( mGenStack.size() >= 1 );
    return mGenStack.back().config;
}

Compiler::GenStatus& Compiler::Status()
{
    assert( mGenStack.size() >= 1 );
    return mGenStack.back().status;
}

void Compiler::GenerateSentinel()
{
    for ( int i = 0; i < SENTINEL_SIZE; i++ )
    {
        mCodeBinPtr[i] = OP_SENTINEL;
    }

    mCodeBinPtr += SENTINEL_SIZE;
}

std::optional<I32> Compiler::GetOptionalElementValue( Syntax* elem )
{
    if ( elem->Kind == SyntaxKind::Number )
    {
        auto number = (NumberExpr*) elem;
        return number->Value;
    }
    else if ( elem->Kind == SyntaxKind::Name )
    {
        auto decl = ((NameExpr*) elem)->Decl.get();

        if ( decl != nullptr && decl->Kind == DeclKind::Const )
        {
            auto constant = (Constant*) decl;
            return constant->Value;
        }
    }

    return std::optional<I32>();
}

I32 Compiler::GetElementValue( Syntax* elem, const char* message )
{
    auto optValue = GetOptionalElementValue( elem );

    if ( optValue.has_value() )
        return optValue.value();

    if ( message != nullptr )
        mRep.ThrowError( CERR_SEMANTICS, elem, message );
    else
        mRep.ThrowError( CERR_SEMANTICS, elem, "Expected a constant value" );
}

void Compiler::IncreaseExprDepth()
{
    mCurExprDepth++;

    if ( mMaxExprDepth < mCurExprDepth )
        mMaxExprDepth = mCurExprDepth;
}

void Compiler::DecreaseExprDepth( int amount )
{
    assert( amount >= 0 );
    assert( amount <= mCurExprDepth );

    mCurExprDepth -= amount;
}

void Compiler::CalculateStackDepth()
{
    mStats.Lambda = {};
    mStats.Static = {};
    mStats.CallsIndirectly = false;

    for ( const auto& [name, decl] : mGlobalTable )
    {
        if ( decl->Kind == DeclKind::Func )
        {
            Function*  func = (Function*) decl.get();
            CallStats* callStats;

            CalculateStackDepth( func );

            if ( name._Starts_with( "$Lambda$" ) )
            {
                callStats = &mStats.Lambda;
            }
            else
            {
                callStats = &mStats.Static;

                if ( func->CallsIndirectly )
                    mStats.CallsIndirectly = true;
            }

            // Only count parameters of publics, if that's ever a distinction

            int16_t stackUsage = func->TreeStackUsage + func->ArgCount;

            callStats->MaxCallDepth  = std::max( callStats->MaxCallDepth,  func->CallDepth );
            callStats->MaxStackUsage = std::max( callStats->MaxStackUsage, stackUsage );

            if ( func->IsRecursive )
                callStats->Recurses = true;
        }
    }
}

void Compiler::CalculateStackDepth( Function* func )
{
    if ( func->IsDepthKnown )
        return;

    if ( func->IsCalculating )
    {
        func->IsRecursive = true;
        return;
    }

    // TODO: Put Machine::FRAME_WORDS somewhere common, and use it here.

    func->IsCalculating = true;
    func->IndividualStackUsage = 2 + func->LocalCount + func->ExprDepth;

    int16_t maxChildDepth = 0;
    int16_t maxChildStackUsage = 0;

    for ( const auto& name : func->CalledFunctions )
    {
        if ( auto it = mGlobalTable.find( name );
            it != mGlobalTable.end() )
        {
            if ( it->second->Kind != DeclKind::Func )
                continue;

            auto childFunc = (Function*) it->second.get();

            CalculateStackDepth( childFunc );

            maxChildDepth = std::max( maxChildDepth, childFunc->CallDepth );
            maxChildStackUsage = std::max( maxChildStackUsage, childFunc->TreeStackUsage );

            if ( childFunc->CallsIndirectly )
                func->CallsIndirectly = true;

            if ( childFunc->IsRecursive )
            {
                func->IsRecursive = true;
                break;
            }
        }
    }

    func->IsCalculating = false;
    func->IsDepthKnown = true;
    func->CallDepth = 1 + maxChildDepth;
    func->TreeStackUsage = func->IndividualStackUsage + maxChildStackUsage;
}


//----------------------------------------------------------------------------

void Log( ICompilerLog* log, LogCategory category, int line, int col, const char* format, va_list args );


Reporter::Reporter( ICompilerLog* log ) :
    mLog( log )
{
    assert( log != nullptr );
}

ICompilerLog* Reporter::GetLog()
{
    return mLog;
}

void Reporter::ThrowError( CompilerErr exceptionCode, Syntax* elem, const char* format, ... )
{
    va_list args;
    va_start( args, format );
    int line = 0;
    int column = 0;
    if ( elem != nullptr )
    {
        line = elem->Line;
        column = elem->Column;
    }
    ThrowError( exceptionCode, line, column, format, args );
    va_end( args );
}

void Reporter::ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args )
{
    Log( LOG_ERROR, line, col, format, args );
    throw CompilerException( exceptionCode );
}

void Reporter::ThrowInternalError()
{
    ThrowInternalError( "Internal error" );
}

void Reporter::ThrowInternalError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CERR_INTERNAL, 0, 0, format, args );
    va_end( args );
}

void Reporter::Log( LogCategory category, int line, int col, const char* format, va_list args )
{
    ::Log( mLog, category, line, col, format, args );
}

void Reporter::LogWarning( int line, int col, const char* format, ... )
{
    va_list args;
    va_start( args, format );
    Log( LOG_WARNING, line, col, format, args );
    va_end( args );
}


void Log( ICompilerLog* log, LogCategory category, int line, int col, const char* format, va_list args )
{
    if ( log != nullptr )
    {
        char msg[256] = "";
        vsprintf_s( msg, format, args );
        log->Add( category, line, col, msg );
    }
}
