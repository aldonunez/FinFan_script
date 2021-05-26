// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

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
    mModIndex( modIndex )
{
    mLoadedAddrDecl.reset( new LoadedAddressDeclaration() );
    mLoadedAddrDecl->Kind = DeclKind::LoadedAddress;
}

void Compiler::AddUnit( Unique<Unit>&& unit )
{
    mUnits.push_back( std::move( unit ) );
}

void Compiler::AddModule( std::shared_ptr<ModuleDeclaration> moduleDecl )
{
    mModuleTable.insert( SymTable::value_type( moduleDecl->Name, moduleDecl ) );
}

CompilerErr Compiler::Compile()
{
    try
    {
        BindAttributes();
        FoldConstants();
        GenerateCode();

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

std::shared_ptr<ModuleDeclaration> Compiler::GetMetadata( const char* modName )
{
    std::shared_ptr<ModuleDeclaration> modDecl( new ModuleDeclaration() );

    modDecl->Name = modName;
    modDecl->Kind = DeclKind::Module;
    modDecl->Table = std::move( mPublicTable );
    modDecl->Type = std::shared_ptr<ModuleType>( new ModuleType() );

    return modDecl;
}

void Compiler::BindAttributes()
{
    BinderVisitor binder( mModIndex, mGlobalTable, mModuleTable, mPublicTable, mEnv, mRep.GetLog() );

    for ( auto& unit : mUnits )
        binder.Declare( unit.get() );

    for ( auto& unit : mUnits )
        binder.Bind( unit.get() );

    mGlobals.resize( binder.GetDataSize() );
}

void Compiler::FoldConstants()
{
    FolderVisitor folder( mRep.GetLog() );

    for ( auto& unit : mUnits )
        folder.Fold( unit.get() );
}

void Compiler::GenerateCode()
{
    for ( auto& unit : mUnits )
        unit->Accept( this );
}

void Compiler::VisitUnit( Unit* unit )
{
    for ( auto& varNode : unit->DataDeclarations )
        Generate( varNode.get() );

    for ( auto& funcNode : unit->FuncDeclarations )
        funcNode->Accept( this );
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
        mRep.LogWarning( elem->FileName, elem->Line, elem->Column, "Deprecated: POP was emitted." );

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

    EmitLoadConstant( (int32_t) number->Value );
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
    GenerateValue( symbol, symbol->GetDecl(), config, status );
}

void Compiler::GenerateValue( Syntax* node, Declaration* decl, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        status.baseDecl = decl;
        return;
    }

    EmitLoadScalar( node, decl, 0 );
}

void Compiler::EmitLoadScalar( Syntax* node, Declaration* decl, int32_t offset )
{
    switch ( decl->Kind )
    {
    case DeclKind::Global:
        mCodeBinPtr[0] = OP_LDMOD;
        mCodeBinPtr[1] = ((GlobalStorage*) decl)->ModIndex;
        mCodeBinPtr += 2;
        WriteU16( mCodeBinPtr, ((GlobalStorage*) decl)->Offset + offset );
        IncreaseExprDepth();
        break;

    case DeclKind::Local:
        mCodeBinPtr[0] = OP_LDLOC;
        mCodeBinPtr[1] = ((LocalStorage*) decl)->Offset - offset;
        mCodeBinPtr += 2;
        IncreaseExprDepth();
        break;

    case DeclKind::Param:
        mCodeBinPtr[0] = OP_LDARG;
        mCodeBinPtr[1] = ((ParamStorage*) decl)->Offset + offset;
        mCodeBinPtr += 2;
        IncreaseExprDepth();
        break;

    case DeclKind::Func:
    case DeclKind::Forward:
        mRep.ThrowError( CERR_SEMANTICS, node, "functions don't have values" );
        break;

    case DeclKind::Const:
        assert( offset == 0 );
        EmitLoadConstant( ((Constant*) decl)->Value );
        break;

    case DeclKind::LoadedAddress:
        if ( offset > 0 )
            EmitSpilledAddrOffset( offset );

        mCodeBinPtr[0] = OP_LOADI;
        mCodeBinPtr++;
        DecreaseExprDepth();
        IncreaseExprDepth();
        break;

    default:
        assert( false );
        mRep.ThrowInternalError();
    }
}

void Compiler::EmitSpilledAddrOffset( int32_t offset )
{
    EmitLoadConstant( offset );

    mCodeBinPtr[0] = OP_PRIM;
    mCodeBinPtr[1] = PRIM_ADD;
    mCodeBinPtr += 2;

    DecreaseExprDepth();
}

void Compiler::GenerateEvalStar( CallOrSymbolExpr* callOrSymbol, const GenConfig& config, GenStatus& status )
{
    auto& symbol = callOrSymbol->Symbol;
    auto decl = symbol->GetDecl();

    if ( decl->Kind == DeclKind::Func
        || decl->Kind == DeclKind::Forward
        || decl->Kind == DeclKind::NativeFunc )
    {
        std::vector<Unique<Syntax>> args;

        GenerateCall( decl, args, config, status );
    }
    else
    {
        Generate( symbol.get() );
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

// TODO: move
void Compiler::VisitCountofExpr( CountofExpr* countofExpr )
{
    if ( Config().discard )
    {
        Status().discarded = true;
        return;
    }

    auto& arrayType = (ArrayType&) *countofExpr->Expr->Type;

    if ( arrayType.Count != 0 )
    {
        EmitLoadConstant( arrayType.Count );
    }
    else
    {
        assert( false );
        mRep.ThrowInternalError();
    }
}

void Compiler::GenerateCond( CondExpr* condExpr, const GenConfig& config, GenStatus& status )
{
    PatchChain  falseChain;
    PatchChain  leaveChain;
    bool        foundCatchAll = false;
    int         exprDepth = mCurExprDepth;
    U8*         startPtr = mCodeBinPtr;

    GenConfig statementConfig = GenConfig::Statement( config.discard )
        .WithLoop( config.breakChain, config.nextChain );

    // TODO: check all the clauses for tail-return. If they all do, then set status.tailRet.

    for ( int i = 0; i < (int) condExpr->Clauses.size(); i++ )
    {
        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        auto clause = condExpr->Clauses[i].get();

        auto optVal = GetOptionalSyntaxValue( clause->Condition.get() );

        bool isConstantTrue = optVal.has_value() && optVal.value() != 0;

        if ( isConstantTrue )
        {
            if ( clause->Body.Statements.size() == 0 && !condExpr->IsIf )
            {
                GenStatus clauseStatus = { ExprKind::Other };
                Generate( clause->Condition.get(), statementConfig, clauseStatus );
            }
            else
            {
                GenStatus clauseStatus = { ExprKind::Other };
                GenerateImplicitProgn( &clause->Body, statementConfig, clauseStatus );
            }
            foundCatchAll = true;
            break;
        }

        if ( clause->Body.Statements.size() == 0 && !condExpr->IsIf )
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

            falseChain = PatchChain();

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
        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        if ( !config.discard )
        {
            mCodeBinPtr[0] = OP_LDC_S;
            mCodeBinPtr[1] = 0;
            mCodeBinPtr += 2;
            IncreaseExprDepth();
        }
    }

    if ( (mCodeBinPtr - startPtr) >= BranchInst::Size
        && mCodeBinPtr[-BranchInst::Size] == OP_B
        && leaveChain.First != nullptr
        && leaveChain.First->Inst == &mCodeBinPtr[-BranchInst::Size]
        && falseChain.PatchedPtr == mCodeBinPtr )
    {
        mCodeBinPtr -= BranchInst::Size;
        Patch( &falseChain );
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

    int32_t         offset = 0;
    Declaration*    decl = nullptr;

    CalcAddress( assignment->Left.get(), decl, offset );

    EmitStoreScalar( assignment->Left.get(), decl, offset );
}

void Compiler::EmitStoreScalar( Syntax* node, Declaration* decl, int32_t offset )
{
    switch ( decl->Kind )
    {
    case DeclKind::Global:
        mCodeBinPtr[0] = OP_STMOD;
        mCodeBinPtr[1] = ((GlobalStorage*) decl)->ModIndex;
        mCodeBinPtr += 2;
        WriteU16( mCodeBinPtr, ((GlobalStorage*) decl)->Offset + offset );
        break;

    case DeclKind::Local:
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = ((LocalStorage*) decl)->Offset - offset;
        mCodeBinPtr += 2;
        break;

    case DeclKind::Param:
        mCodeBinPtr[0] = OP_STARG;
        mCodeBinPtr[1] = ((ParamStorage*) decl)->Offset + offset;
        mCodeBinPtr += 2;
        break;

    case DeclKind::Func:
    case DeclKind::Forward:
    case DeclKind::NativeFunc:
        mRep.ThrowError( CERR_SEMANTICS, node, "functions can't be assigned a value" );
        break;

    case DeclKind::Const:
        mRep.ThrowError( CERR_SEMANTICS, node, "Constants can't be changed" );
        break;

    case DeclKind::LoadedAddress:
        if ( offset > 0 )
            EmitSpilledAddrOffset( offset );

        mCodeBinPtr[0] = OP_STOREI;
        mCodeBinPtr++;
        DecreaseExprDepth();
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

    auto func = (Function*) addrOf->Inner->GetDecl();

    mCodeBinPtr[0] = OP_LDC;
    mCodeBinPtr++;

    EmitFuncAddress( (Function*) addrOf->Inner->GetDecl(), mCodeBinPtr );

    IncreaseExprDepth();
}

void Compiler::EmitFuncAddress( Function* func, uint8_t*& dstPtr )
{
    U32 addr = 0, modIndex = 0;

    if ( func->Address != INT32_MAX )
    {
        addr = func->Address;
        modIndex = func->ModIndex;
    }
    else
    {
        PatchChain* chain = PushFuncPatch( func->Name, dstPtr );

        if ( mInFunc )
        {
            AddrRef ref = { AddrRefKind::Inst };
            ref.InstPtr = &chain->First->Inst;
            mLocalAddrRefs.push_back( ref );
        }

        modIndex = mModIndex;
    }

    WriteU24( dstPtr, addr );
    dstPtr[0] = modIndex;
    dstPtr++;
}

void Compiler::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
    GenerateFunction( addrOf, Config(), Status() );
}

void Compiler::GenerateFuncall( CallExpr* call, const GenConfig& config, GenStatus& status )
{
    GenerateCallArgs( call->Arguments );

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
        if ( binding->Kind == SyntaxKind::VarDecl )
            GenerateLetBinding( binding.get() );
    }

    GenerateImplicitProgn( &letStmt->Body, config, status );
}

void Compiler::GenerateLetBinding( DataDecl* binding )
{
    auto local = (LocalStorage*) binding->GetDecl();

    GenerateLocalInit( local->Offset, binding->Initializer.get() );
}

void Compiler::GenerateLocalInit( int32_t offset, Syntax* initializer )
{
    if ( initializer == nullptr )
        return;

    auto type = initializer->Type.get();

    if ( IsScalarType( type->GetKind() ) )
    {
        Generate( initializer );
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = offset;
        mCodeBinPtr += 2;
        DecreaseExprDepth();
    }
    else if ( type->GetKind() == TypeKind::Array )
    {
        auto arrayType = (ArrayType*) type;

        AddLocalDataArray( offset, initializer, arrayType->Count );
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, initializer, "'let' binding takes a name or name and type" );
    }
}

void Compiler::VisitLetStatement( LetStatement* letStmt )
{
    GenerateLet( letStmt, Config(), Status() );
}

// TODO: try to merge this with AddGlobalDataArray. Separate the array processing from the code generation

void Compiler::AddLocalDataArray( int32_t offset, Syntax* valueElem, size_t size )
{
    if ( valueElem->Kind != SyntaxKind::ArrayInitializer )
        mRep.ThrowError( CERR_SEMANTICS, valueElem, "Arrays must be initialized with array initializer" );

    auto    initList = (InitList*) valueElem;
    size_t  locIndex = offset;
    size_t  i = 0;

    for ( auto& entry : initList->Values )
    {
        if ( i == size )
            mRep.ThrowError( CERR_SEMANTICS, valueElem, "Array has too many initializers" );

        GenerateLocalInit( locIndex, entry.get() );
        i++;
        locIndex -= entry->Type->GetSize();
    }

    if ( initList->Fill == ArrayFill::Extrapolate && initList->Values.size() > 0 )
    {
        size_t count = initList->Values.size();
        I32 prevValue = 0;
        I32 step = 0;

        prevValue = GetSyntaxValue( initList->Values.back().get(), "Array initializer extrapolation requires a constant" );

        if ( initList->Values.size() > 1 )
        {
            auto prevValue2 = GetOptionalSyntaxValue( initList->Values[count - 2].get() );
            if ( prevValue2.has_value() )
                step = prevValue - prevValue2.value();
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
    else if ( initList->Fill == ArrayFill::Repeat && initList->Values.size() > 0 )
    {
        Syntax* lastNode = initList->Values.back().get();

        for ( ; i < size; i++ )
        {
            GenerateLocalInit( locIndex, lastNode );
            locIndex -= lastNode->Type->GetSize();
        }
    }
}

void Compiler::GenerateCall( CallExpr* call, const GenConfig& config, GenStatus& status )
{
    GenerateCall( call->Head->GetDecl(), call->Arguments, config, status );
}

void Compiler::GenerateCallArgs( std::vector<Unique<Syntax>>& arguments )
{
    for ( int i = arguments.size() - 1; i >= 0; i-- )
    {
        Generate( arguments[i].get() );
    }
}

void Compiler::GenerateCall( Declaration* decl, std::vector<Unique<Syntax>>& arguments, const GenConfig& config, GenStatus& status )
{
    GenerateCallArgs( arguments );

    int argCount = arguments.size();
    U8 callFlags = CallFlags::Build( argCount, config.discard );

    if ( decl == nullptr )
    {
        mRep.ThrowInternalError( "Call head has no declaration" );
    }
    else if ( decl->Kind == DeclKind::Func
        && ((Function*) decl)->ModIndex == mModIndex )
    {
        Function* func = (Function*) decl;
        U32 addr = 0;

        mCodeBinPtr[0] = OP_CALL;
        mCodeBinPtr[1] = callFlags;
        mCodeBinPtr += 2;

        if ( func->Address != INT32_MAX )
        {
            addr = func->Address;
        }
        else
        {
            PatchChain* chain = PushFuncPatch( func->Name, mCodeBinPtr );

            AddrRef ref = { AddrRefKind::Inst };
            ref.InstPtr = &chain->First->Inst;
            mLocalAddrRefs.push_back( ref );
        }

        WriteU24( mCodeBinPtr, addr );

        if ( mInFunc )
            mCurFunc->CalledFunctions.push_back( { mCurExprDepth, func->Name } );
    }
    else
    {
        int opCode = 0;
        I32 id = 0;

        if ( decl->Kind == DeclKind::Func )
        {
            auto func = (Function*) decl;

            opCode = OP_CALLM;
            id = CodeAddr::Build( func->Address, func->ModIndex );

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

    auto local = (LocalStorage*) forStmt->IndexDecl.get();

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
    PatchChain  exitChain;
    int         exprDepth = mCurExprDepth;

    const GenConfig& statementConfig = config;

    if ( caseExpr->TestKeyDecl != nullptr )
    {
        auto local = (LocalStorage*) caseExpr->TestKeyDecl.get();

        Generate( caseExpr->TestKey.get() );
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = local->Offset;
        mCodeBinPtr += 2;
        DecreaseExprDepth();

        // Replace the keyform expression with the temporary variable
        Unique<NameExpr> localSym( new NameExpr() );
        localSym->String = "$testKey";
        localSym->Decl = caseExpr->TestKeyDecl;
        localSym->Type = localSym->Decl->Type;
        caseExpr->TestKey = std::move( localSym );
    }

    for ( auto& clause : caseExpr->Clauses )
    {
        PatchChain falseChain;
        PatchChain trueChain;

        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

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

    // Restore the expression depth, so that it doesn't accumulate
    mCurExprDepth = exprDepth;

    if ( caseExpr->Fallback != nullptr )
    {
        GenerateImplicitProgn( &caseExpr->Fallback->Body, statementConfig, status );
    }
    else
    {
        GenerateNilIfNeeded( config, status );
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
    PushPatch( chain, mCodeBinPtr );
}

void Compiler::PushPatch( PatchChain* chain, U8* patchPtr )
{
    InstPatch* link = new InstPatch;
    link->Inst = patchPtr;
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

Compiler::PatchChain* Compiler::PushFuncPatch( const std::string& name, U8* patchPtr )
{
    auto patchIt = mPatchMap.find( name );
    if ( patchIt == mPatchMap.end() )
    {
        auto result = mPatchMap.insert( PatchMap::value_type( { name, PatchChain() } ) );
        patchIt = std::move( result.first );
    }

    PushPatch( &patchIt->second, patchPtr );

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

    chain->PatchedPtr = target;
}

void Compiler::PatchCalls( PatchChain* chain, U32 addr )
{
    for ( InstPatch* link = chain->First; link != nullptr; link = link->Next )
    {
        StoreU24( link->Inst, addr );
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

void Compiler::EmitLoadAddress( Syntax* node, Declaration* baseDecl, I32 offset )
{
    if ( baseDecl == nullptr )
    {
        mRep.ThrowInternalError( "Missing declaration" );
    }
    else
    {
        uint32_t addrWord;

        switch ( baseDecl->Kind )
        {
        case DeclKind::Global:
            addrWord = CodeAddr::Build(
                ((GlobalStorage*) baseDecl)->Offset + offset,
                ((GlobalStorage*) baseDecl)->ModIndex );
            mCodeBinPtr[0] = OP_LDC;
            mCodeBinPtr++;
            WriteU32( mCodeBinPtr, addrWord );
            break;

        case DeclKind::Local:
            mCodeBinPtr[0] = OP_LDLOCA;
            mCodeBinPtr[1] = ((LocalStorage*) baseDecl)->Offset - offset;
            mCodeBinPtr += 2;
            break;

        default:
            mRep.ThrowError( CERR_SEMANTICS, node, "'aref' supports only globals and locals" );
        }
    }

    IncreaseExprDepth();
}

void Compiler::GenerateArefAddr( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status )
{
    assert( config.calcAddr );

    auto arrayType = (ArrayType&) *indexExpr->Head->Type;

    Generate( indexExpr->Head.get(), config, status );

    std::optional<int32_t> optIndexVal;

    optIndexVal = GetOptionalSyntaxValue( indexExpr->Index.get() );

    if ( optIndexVal.has_value() )
    {
        status.offset += optIndexVal.value() * arrayType.ElemType->GetSize();
        return;
    }

    if ( !status.spilledAddr )
    {
        EmitLoadAddress( indexExpr, status.baseDecl, status.offset );

        // Set this after emitting the original decl's address above
        status.baseDecl = mLoadedAddrDecl.get();
        status.offset = 0;
        status.spilledAddr = true;
    }

    if ( status.offset > 0 )
    {
        EmitSpilledAddrOffset( status.offset );

        status.offset = 0;
    }

    Generate( indexExpr->Index.get() );

    if ( arrayType.ElemType->GetSize() > 255 )
    {
        mCodeBinPtr[0] = OP_INDEX;
        mCodeBinPtr += 1;
        WriteU32( mCodeBinPtr, arrayType.ElemType->GetSize() );
    }
    else if ( arrayType.ElemType->GetSize() > 1 )
    {
        mCodeBinPtr[0] = OP_INDEX_S;
        mCodeBinPtr[1] = arrayType.ElemType->GetSize();
        mCodeBinPtr += 2;
    }
    else
    {
        // TODO: Do we need an add-address primitive that doesn't overwrite the module byte?
        mCodeBinPtr[0] = OP_PRIM;
        mCodeBinPtr[1] = PRIM_ADD;
        mCodeBinPtr += 2;
    }

    DecreaseExprDepth();
}

void Compiler::GenerateAref( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        GenerateArefAddr( indexExpr, config, status );
        return;
    }

    Declaration* baseDecl = nullptr;
    int32_t offset = 0;

    CalcAddress( indexExpr, baseDecl, offset );

    EmitLoadScalar( indexExpr, baseDecl, offset );
}

void Compiler::VisitIndexExpr( IndexExpr* indexExpr )
{
    GenerateAref( indexExpr, Config(), Status() );
}

void Compiler::CalcAddress( Syntax* expr, Declaration*& baseDecl, int32_t& offset )
{
    GenConfig config{};
    GenStatus status{};

    config.calcAddr = true;

    Generate( expr, config, status );

    baseDecl = status.baseDecl;
    offset = status.offset;
}

void Compiler::VisitSliceExpr( SliceExpr* sliceExpr )
{
    const GenConfig& config = Config();
    GenStatus& status = Status();

    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        Generate( sliceExpr->Head.get(), config, status );

        auto arrayType = (ArrayType&) *sliceExpr->Head->Type;

        int32_t indexVal = GetSyntaxValue( sliceExpr->FirstIndex.get(), "" );

        status.offset += indexVal * arrayType.ElemType->GetSize();
        return;
    }

    assert( false );
}

void Compiler::VisitDotExpr( DotExpr* dotExpr )
{
    GenerateValue( dotExpr, dotExpr->GetDecl(), Config(), Status() );
}

void Compiler::GenerateDefvar( VarDecl* varDecl, const GenConfig& config, GenStatus& status )
{
    auto global = (GlobalStorage*) varDecl->GetDecl();

    GenerateGlobalInit( global->Offset, varDecl->Initializer.get() );
}

void Compiler::GenerateGlobalInit( int32_t offset, Syntax* initializer )
{
    if ( initializer == nullptr )
        return;

    auto type = initializer->Type.get();

    if ( IsScalarType( type->GetKind() ) )
    {
        AddGlobalData( offset, initializer );
    }
    else if ( type->GetKind() == TypeKind::Array )
    {
        auto arrayType = (ArrayType*) type;

        AddGlobalDataArray( offset, initializer, arrayType->Count );
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, initializer, "'defvar' takes a name or name and type" );
    }
}

void Compiler::VisitVarDecl( VarDecl* varDecl )
{
    GenerateDefvar( varDecl, Config(), Status() );
}

void Compiler::AddGlobalData( U32 offset, Syntax* valueElem )
{
    if ( valueElem->Type->GetKind() == TypeKind::Pointer )
    {
        if ( valueElem->Kind == SyntaxKind::AddrOfExpr )
        {
            auto addrOf = (AddrOfExpr*) valueElem;

            uint8_t* dstPtr = (uint8_t*) &mGlobals[offset];

            EmitFuncAddress( (Function*) addrOf->Inner->GetDecl(), dstPtr );
        }
        else
        {
            mRep.ThrowInternalError();
        }
    }
    else
    {
        mGlobals[offset] = GetSyntaxValue( valueElem, "Globals must be initialized with constant data" );
    }
}

void Compiler::AddGlobalDataArray( int32_t offset, Syntax* valueElem, size_t size )
{
    if ( valueElem->Kind != SyntaxKind::ArrayInitializer )
        mRep.ThrowError( CERR_SEMANTICS, valueElem, "Arrays must be initialized with array initializer" );

    auto    initList = (InitList*) valueElem;
    size_t  i = 0;
    size_t  globalIndex = offset;

    for ( auto& entry : initList->Values )
    {
        if ( i == size )
            mRep.ThrowError( CERR_SEMANTICS, valueElem, "Array has too many initializers" );

        GenerateGlobalInit( globalIndex, entry.get() );
        i++;
        globalIndex += entry->Type->GetSize();
    }

    if ( initList->Fill == ArrayFill::Extrapolate && i >= 1 )
    {
        I32 prevValue = mGlobals[offset + i - 1];
        I32 step = 0;

        if ( i >= 2 )
        {
            step = prevValue - mGlobals[offset + i - 2];
        }

        for ( ; i < size; i++ )
        {
            I32 newValue = prevValue + step;

            mGlobals[offset + i] = newValue;
            prevValue = newValue;
        }
    }
    else if ( initList->Fill == ArrayFill::Repeat && i >= 1 )
    {
        Syntax* lastNode = initList->Values.back().get();

        for ( ; i < size; i++ )
        {
            GenerateGlobalInit( globalIndex, lastNode );
            globalIndex += lastNode->Type->GetSize();
        }
    }
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

I32 Compiler::GetSyntaxValue( Syntax* node, const char* message )
{
    auto optValue = GetOptionalSyntaxValue( node );

    if ( optValue.has_value() )
        return optValue.value();

    if ( message != nullptr )
        mRep.ThrowError( CERR_SEMANTICS, node, message );
    else
        mRep.ThrowError( CERR_SEMANTICS, node, "Expected a constant value" );
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

            int16_t stackUsage = func->TreeStackUsage + func->ParamCount;

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
    func->IndividualStackUsage = 2 + func->LocalCount;

    int16_t maxChildDepth = 0;
    int16_t maxChildStackUsage = func->ExprDepth;

    for ( const auto& site : func->CalledFunctions )
    {
        if ( auto it = mGlobalTable.find( site.FunctionName );
            it != mGlobalTable.end() )
        {
            if ( it->second->Kind != DeclKind::Func )
                continue;

            auto childFunc = (Function*) it->second.get();

            CalculateStackDepth( childFunc );

            int16_t childStackUsage = childFunc->TreeStackUsage + site.ExprDepth;

            maxChildDepth = std::max( maxChildDepth, childFunc->CallDepth );
            maxChildStackUsage = std::max( maxChildStackUsage, childStackUsage );

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

void Log( ICompilerLog* log, LogCategory category, const char* fileName, int line, int col, const char* format, va_list args );


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
    const char* fileName = nullptr;
    int line = 0;
    int column = 0;
    if ( elem != nullptr )
    {
        fileName = elem->FileName;
        line = elem->Line;
        column = elem->Column;
    }
    ThrowError( exceptionCode, fileName, line, column, format, args );
    va_end( args );
}

void Reporter::ThrowError( CompilerErr exceptionCode, const char* fileName, int line, int col, const char* format, va_list args )
{
    Log( LOG_ERROR, fileName, line, col, format, args );
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
    ThrowError( CERR_INTERNAL, "", 0, 0, format, args );
    va_end( args );
}

void Reporter::Log( LogCategory category, const char* fileName, int line, int col, const char* format, va_list args )
{
    ::Log( mLog, category, fileName, line, col, format, args );
}

void Reporter::LogWarning( const char* fileName, int line, int col, const char* format, ... )
{
    va_list args;
    va_start( args, format );
    Log( LOG_WARNING, fileName, line, col, format, args );
    va_end( args );
}


void Log( ICompilerLog* log, LogCategory category, const char* fileName, int line, int col, const char* format, va_list args )
{
    if ( log != nullptr )
    {
        char msg[256] = "";
        vsprintf_s( msg, format, args );
        log->Add( category, fileName, line, col, msg );
    }
}
