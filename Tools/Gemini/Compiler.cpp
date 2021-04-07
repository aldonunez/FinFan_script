#include "stdafx.h"
#include "Compiler.h"
#include "OpCodes.h"
#include <ctype.h>
#include <cstdarg>


Compiler::Compiler( U8* codeBin, int codeBinLen, ICompilerEnv* env, ICompilerLog* log, int modIndex ) :
    mCodeBin( codeBin ),
    mCodeBinPtr( codeBin ),
    mCodeBinEnd( codeBin + codeBinLen ),
    mForwards( 0 ),
    mEnv( env ),
    mLog( log ),
    mModIndex( modIndex ),
    mCurLocalCount(),
    mMaxLocalCount(),
    mInFunc( false ),
    mCurFunc(),
    mCurExprDepth(),
    mMaxExprDepth(),
    mCompiled(),
    mCalculatedStats(),
    mStats()
{
    mGeneratorMap.insert( GeneratorMap::value_type( "+", &Compiler::GenerateArithmetic ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "*", &Compiler::GenerateArithmetic ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "/", &Compiler::GenerateArithmetic ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "%", &Compiler::GenerateArithmetic ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "-", &Compiler::GenerateNegate ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "and", &Compiler::GenerateAnd ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "or", &Compiler::GenerateOr ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "not", &Compiler::GenerateNot ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "=", &Compiler::GenerateComparison ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "<>", &Compiler::GenerateComparison ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "<", &Compiler::GenerateComparison ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "<=", &Compiler::GenerateComparison ) );
    mGeneratorMap.insert( GeneratorMap::value_type( ">", &Compiler::GenerateComparison ) );
    mGeneratorMap.insert( GeneratorMap::value_type( ">=", &Compiler::GenerateComparison ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "return", &Compiler::GenerateReturn ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "if", &Compiler::GenerateIf ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "cond", &Compiler::GenerateCond ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "progn", &Compiler::GenerateProgn ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "set", &Compiler::GenerateSet ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "defun", &Compiler::GenerateDefun ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "lambda", &Compiler::GenerateLambda ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "funcall", &Compiler::GenerateFuncall ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "let", &Compiler::GenerateLet ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "loop", &Compiler::GenerateLoop ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "do", &Compiler::GenerateDo ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "break", &Compiler::GenerateBreak ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "next", &Compiler::GenerateNext ) );
    mGeneratorMap.insert( GeneratorMap::value_type( "case", &Compiler::GenerateCase ) );
}

CompilerErr Compiler::Compile( Slist* progTree )
{
    try
    {
        MakeStdEnv();

        mSymStack.push_back( &mConstTable );
        mSymStack.push_back( &mGlobalTable );

        for ( auto it = progTree->Elements.begin(); it != progTree->Elements.end(); it++ )
            Generate( it->get() );

        GenerateLambdas();

        mSymStack.pop_back();
        mSymStack.pop_back();

        if ( mForwards != 0 )
            ThrowUnresolvedFuncsError();

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

void Compiler::Generate( Element* elem )
{
    GenStatus status = { Expr_Other };
    Generate( elem, GenConfig::Statement(), status );
}

void Compiler::Generate( Element* elem, const GenConfig& config )
{
    GenStatus status = { Expr_Other };
    Generate( elem, config, status );
}

void Compiler::Generate( Element* elem, const GenConfig& config, GenStatus& status )
{
    if ( elem->Code == Elem_Number )
    {
        GenerateNumber( (Number*) elem, config, status );
    }
    else if ( elem->Code == Elem_Symbol )
    {
        GenerateSymbol( (Symbol*) elem, config, status );
    }
    else if ( elem->Code == Elem_Slist )
    {
        GenerateSlist( (Slist*) elem, config, status );
    }
    else
    {
        assert( false );
        ThrowInternalError();
    }

    if ( status.kind != Expr_Logical )
    {
        if ( config.trueChain != nullptr )
        {
            PushPatch( config.trueChain );

            mCodeBinPtr[0] = (config.invert && status.kind != Expr_Comparison) ? OP_BFALSE : OP_BTRUE;
            mCodeBinPtr += BranchInst::Size;
            DecreaseExprDepth();

            PushPatch( config.falseChain );

            mCodeBinPtr[0] = OP_B;
            mCodeBinPtr += BranchInst::Size;
        }
        else if ( config.invert && status.kind != Expr_Comparison )
        {
            if ( !config.discard )
            {
                mCodeBinPtr[0] = OP_NOT;
                mCodeBinPtr += 1;
            }
        }
    }
}

void Compiler::GenerateDiscard( Element* elem )
{
    GenConfig config{};

    GenerateDiscard( elem, config );
}

void Compiler::GenerateDiscard( Element* elem, const GenConfig& config )
{
    if ( elem->Code != Elem_Slist )
        return;

    GenStatus status = { Expr_Other };
    GenerateSlist( (Slist*) elem, config.WithDiscard(), status );

    if ( !status.discarded )
    {
        assert( status.discarded );
        LogWarning( elem->Line, elem->Column, "Deprecated: POP was emitted." );

        *mCodeBinPtr = OP_POP;
        mCodeBinPtr++;

        DecreaseExprDepth();
    }
}

void Compiler::GenerateNumber( Number* number, const GenConfig& config, GenStatus& status )
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

void Compiler::GenerateSymbol( Symbol* symbol, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    auto decl = FindSymbol( symbol->String );
    if ( decl != nullptr )
    {
        switch ( decl->Kind )
        {
        case Decl_Global:
            mCodeBinPtr[0] = OP_LDMOD;
            mCodeBinPtr[1] = mModIndex;
            mCodeBinPtr += 2;
            WriteU16( mCodeBinPtr, ((Storage*) decl)->Offset );
            IncreaseExprDepth();
            break;

        case Decl_Local:
            mCodeBinPtr[0] = OP_LDLOC;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
            IncreaseExprDepth();
            break;

        case Decl_Arg:
            mCodeBinPtr[0] = OP_LDARG;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
            IncreaseExprDepth();
            break;

        case Decl_Func:
        case Decl_Forward:
            ThrowError( CERR_SEMANTICS, symbol, "functions don't have values" );
            break;

        case Decl_Const:
            {
                Number number;
                number.Code = Elem_Number;
                number.Column = symbol->Column;
                number.Line = symbol->Line;
                number.Value = ((ConstDecl*) decl)->Value;
                GenerateNumber( &number, config, status );
            }
            break;

        default:
            assert( false );
            ThrowInternalError();
        }
    }
    else
    {
        ThrowError( CERR_SEMANTICS, symbol, "symbol not found '%s'", symbol->String.c_str() );
    }
}

void Compiler::GenerateSlist( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() == 0 )
        ThrowError( CERR_UNSUPPORTED, list, "empty lists are unsupported" );

    auto head = list->Elements[0].get();
    if ( head->Code == Elem_Symbol )
    {
        Symbol* op = (Symbol*) head;
        auto it = mGeneratorMap.find( op->String );
        if ( it != mGeneratorMap.end() )
        {
            (this->*(it->second))( list, config, status );
        }
        else
        {
            GenerateCall( list, config, status );
        }
    }
    else
    {
        ThrowError( CERR_UNSUPPORTED, head, "only symbols are supported at the head of a list" );
    }
}

void Compiler::GenerateArithmetic( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 3 )
        ThrowError( CERR_SEMANTICS, list, "arithmetic functions take 2 or more operands" );
    if ( list->Elements.size() > 3 )
        ThrowError( CERR_UNSUPPORTED, list, "arithmetic functions with more than 2 operands are unsupported" );

    auto op = (Symbol*) list->Elements[0].get();
    int primitive;

    if ( op->String == "+" )
        primitive = PRIM_ADD;
    else if ( op->String == "*" )
        primitive = PRIM_MUL;
    else if ( op->String == "/" )
        primitive = PRIM_DIV;
    else if ( op->String == "%" )
        primitive = PRIM_MOD;
    else
    {
        assert( false );
        ThrowInternalError();
    }

    GenerateBinaryPrimitive( list, primitive, config, status );
}

void Compiler::GenerateNegate( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() == 3 )
    {
        GenerateBinaryPrimitive( list, PRIM_SUB, config, status );
    }
    else if ( list->Elements.size() == 2 )
    {
        GenerateUnaryPrimitive( list->Elements[1].get(), config, status );
    }
    else if ( list->Elements.size() == 1 )
    {
        ThrowError( CERR_SEMANTICS, list, "too few arguments for negation or subtraction function" );
    }
    else
    {
        ThrowError( CERR_UNSUPPORTED, list, "arithmetic functions with more than 2 operands are unsupported" );
    }
}

void Compiler::GenerateReturn( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() > 2 )
        ThrowError( CERR_UNSUPPORTED, list, "returning more than 1 value is unsupported" );

    if ( list->Elements.size() == 2 )
    {
        Generate( list->Elements[1].get() );
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

void Compiler::GenerateIf( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 3 || list->Elements.size() > 4 )
        ThrowError( CERR_SEMANTICS, list, "'if' takes 2 or 3 arguments" );

    U8* leaveOffset;

    PatchChain  newTrueChain;
    PatchChain  newFalseChain;

    Generate( list->Elements[1].get(), GenConfig::Expr( &newTrueChain, &newFalseChain, false ) );

    ElideTrue( &newTrueChain, &newFalseChain );
    Patch( &newTrueChain );

    GenConfig statementConfig = GenConfig::Statement( config.discard );
    int exprDepth = mCurExprDepth;

    // True
    Generate( list->Elements[2].get(), statementConfig );
    mCodeBinPtr[0] = OP_B;
    leaveOffset = &mCodeBinPtr[1];
    mCodeBinPtr += BranchInst::Size;

    ElideFalse( &newTrueChain, &newFalseChain );

    U8* falsePtr = mCodeBinPtr;

    // False

    // Restore the expression depth, so that it doesn't accumulate
    mCurExprDepth = exprDepth;

    if ( list->Elements.size() == 4 )
    {
        Generate( list->Elements[3].get(), statementConfig );
    }
    else
    {
        if ( !config.discard )
        {
            mCodeBinPtr[0] = OP_LDC_S;
            mCodeBinPtr[1] = 0;
            mCodeBinPtr += 2;
            IncreaseExprDepth();
        }
    }

    ptrdiff_t ptrOffset = mCodeBinPtr - leaveOffset - (BranchInst::Size - 1);

    if ( ptrOffset < BranchInst::OffsetMin || ptrOffset > BranchInst::OffsetMax )
        ThrowError( CERR_UNSUPPORTED, list, "Branch target is too far." );

    if ( ptrOffset != 0 )
    {
        BranchInst::StoreOffset( leaveOffset, ptrOffset );
    }
    else
    {
        // Remove the uncoditional branch out of the True clause.
        mCodeBinPtr -= BranchInst::Size;
        falsePtr = mCodeBinPtr;
    }

    Patch( &newFalseChain, falsePtr );

    if ( config.discard )
        status.discarded = true;

    // TODO: if both branches tail-return, then set status.tailRet.
    //       This doesn't apply, if there's only one branch.
}

void Compiler::GenerateCond( Slist* list, const GenConfig& config, GenStatus& status )
{
    PatchChain  leaveChain;
    bool        foundCatchAll = false;
    int         exprDepth = mCurExprDepth;

    GenConfig statementConfig = GenConfig::Statement( config.discard );

    // TODO: check all the clauses for tail-return. If they all do, then set status.tailRet.

    for ( size_t i = 1; i < list->Elements.size(); i++ )
    {
        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        auto clause = list->Elements[i].get();
        if ( clause->Code != Elem_Slist )
            ThrowError( CERR_SEMANTICS, clause, "each clause of COND must be a list" );

        auto clauseList = (Slist*) clause;
        if ( clauseList->Elements.size() < 1 )
            ThrowError( CERR_SEMANTICS, clause, "each clause of COND must have at least one argument" );

        // TODO: make the check more general.

        bool isConstantTrue = false;

        if ( clauseList->Elements[0].get()->Code == Elem_Number )
        {
            if ( ((Number*) clauseList->Elements[0].get())->Value != 0 )
                isConstantTrue = true;
        }
        else if ( clauseList->Elements[0].get()->Code == Elem_Symbol )
        {
            auto decl = FindSymbol( ((Symbol*) clauseList->Elements[0].get())->String );
            if ( decl != nullptr && decl->Kind == Decl_Const && ((ConstDecl*) decl)->Value != 0 )
                isConstantTrue = true;
        }

        if ( isConstantTrue )
        {
            if ( clauseList->Elements.size() == 1 )
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
                GenStatus clauseStatus = { Expr_Other };
                GenerateImplicitProgn( clauseList, 1, statementConfig, clauseStatus );
            }
            foundCatchAll = true;
            break;
        }

        if ( clauseList->Elements.size() == 1 )
        {
            Generate( clauseList->Elements[0].get() );

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

            Generate( clauseList->Elements[0].get(), GenConfig::Expr( &trueChain, &falseChain, false ) );
            ElideTrue( &trueChain, &falseChain );
            Patch( &trueChain );

            // True
            GenStatus clauseStatus = { Expr_Other };
            GenerateImplicitProgn( clauseList, 1, statementConfig, clauseStatus );

            if ( i < list->Elements.size() - 1 || !config.discard )
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

void Compiler::GenerateProgn( Slist* list, const GenConfig& config, GenStatus& status )
{
    GenerateImplicitProgn( list, 1, config, status );
}

void Compiler::GenerateSet( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() != 3 )
        ThrowError( CERR_SEMANTICS, list, "'set' takes 2 arguments" );

    // Value
    Generate( list->Elements[2].get() );

    if ( config.discard )
    {
        status.discarded = true;
    }
    else
    {
        *mCodeBinPtr++ = OP_DUP;
        IncreaseExprDepth();
    }

    if ( list->Elements[1]->Code != Elem_Symbol )
        ThrowError( CERR_SEMANTICS, list->Elements[1].get(), "'set' : first argument must be a symbol" );

    auto targetSym = (Symbol*) list->Elements[1].get();
    auto decl = FindSymbol( targetSym->String );
    if ( decl != nullptr )
    {
        switch ( decl->Kind )
        {
        case Decl_Global:
            mCodeBinPtr[0] = OP_STMOD;
            mCodeBinPtr[1] = mModIndex;
            mCodeBinPtr += 2;
            WriteU16( mCodeBinPtr, ((Storage*) decl)->Offset );
            break;

        case Decl_Local:
            mCodeBinPtr[0] = OP_STLOC;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
            break;

        case Decl_Arg:
            mCodeBinPtr[0] = OP_STARG;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
            break;

        case Decl_Func:
        case Decl_Forward:
            ThrowError( CERR_SEMANTICS, targetSym, "functions can't be assigned a value" );
            break;

        default:
            assert( false );
            ThrowInternalError();
        }

        DecreaseExprDepth();
    }
    else
    {
        ThrowError( CERR_SEMANTICS, targetSym, "symbol not found '%s'", targetSym->String.c_str() );
    }
}

void Compiler::GenerateDefun( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( mInFunc )
        ThrowError( CERR_SEMANTICS, list, "a function can't be defined inside another" );

    if ( list->Elements.size() < 3 )
        ThrowError( CERR_SEMANTICS, list, "'defun' takes 2 or more arguments" );

    if ( list->Elements[1]->Code != Elem_Symbol )
        ThrowError( CERR_SEMANTICS, list->Elements[1].get(), "'defun' first argument must be a name" );

    Symbol* funcSym = (Symbol*) list->Elements[1].get();
    U32 addr = (mCodeBinPtr - mCodeBin);

    SymTable::iterator it = mGlobalTable.find( funcSym->String );
    Function* func = nullptr;

    if ( it != mGlobalTable.end() )
    {
        if ( it->second->Kind == Decl_Forward )
        {
            func = (Function*) it->second.get();
            func->Kind = Decl_Func;
            func->Address = addr;
            PatchCalls( &func->Patches, addr );
            mForwards--;
        }
        else if ( it->second->Kind == Decl_Func )
        {
            ThrowError( CERR_SEMANTICS, funcSym, "the function '%s' is already defined", funcSym->String.c_str() );
        }
        else
        {
            ThrowError( CERR_SEMANTICS, funcSym, "the symbol '%s' is already defined", funcSym->String.c_str() );
        }
    }
    else
    {
        func = AddFunc( funcSym->String, addr );
    }

    mEnv->AddExternal( funcSym->String, External_Bytecode, func->Address );

    GenerateProc( list, 2, func );
}

void Compiler::GenerateLambda( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 2 )
        ThrowError( CERR_SEMANTICS, list, "'lambda' takes 1 or more arguments" );

    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    *mCodeBinPtr = OP_LDC;
    mCodeBinPtr++;

    DeferredLambda lambda = { 0 };
    lambda.Definition = list;
    lambda.Patch = mCodeBinPtr;
    mLambdas.push_back( lambda );

    // Add the index of the deferred lambda just linked
    mLocalLambdas.push_back( mLambdas.size() - 1 );

    WriteU32( mCodeBinPtr, 0 );
    IncreaseExprDepth();
}

void Compiler::GenerateFuncall( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 2 )
        ThrowError( CERR_SEMANTICS, list, "'funcall' takes 1 or more arguments" );

    for ( size_t i = list->Elements.size() - 1; i > 0; i-- )
    {
        Generate( list->Elements[i].get() );
    }

    int argCount = list->Elements.size() - 2;

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

void Compiler::GenerateLet( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 2 )
        ThrowError( CERR_SEMANTICS, list, "'let' takes 1 or more arguments" );
    if ( list->Elements[1]->Code != Elem_Slist )
        ThrowError( CERR_SEMANTICS, list->Elements[1].get(), "'let' : first argument must be a list" );

    Slist* allList = (Slist*) list->Elements[1].get();
    SymTable localTable;

    for ( size_t i = 0; i < allList->Elements.size(); i++ )
    {
        if ( allList->Elements[i]->Code != Elem_Slist )
            ThrowError( CERR_SEMANTICS, allList->Elements[i].get(), "'let' : element %d of binding list must be a list", i+1 );

        Slist* localList = (Slist*) allList->Elements[i].get();

        if ( localList->Elements.size() != 2 )
            ThrowError( CERR_SEMANTICS, localList, "'let' : binding pair must have 2 elements" );
        if ( localList->Elements[0]->Code != Elem_Symbol )
            ThrowError( CERR_SEMANTICS, localList->Elements[0].get(), "'let' : first element of binding pair must be a symbol" );

        Symbol* localSym = (Symbol*) localList->Elements[0].get();
        Storage* local = AddLocal( localTable, localSym->String, mCurLocalCount );

        mCurLocalCount++;
        if ( mCurLocalCount > mMaxLocalCount )
            mMaxLocalCount = mCurLocalCount;

        if ( localList->Elements.size() > 1 )
        {
            Generate( localList->Elements[1].get() );
            mCodeBinPtr[0] = OP_STLOC;
            mCodeBinPtr[1] = local->Offset;
            mCodeBinPtr += 2;
            DecreaseExprDepth();
        }
    }

    mSymStack.push_back( &localTable );

    GenerateImplicitProgn( list, 2, config, status );

    mSymStack.pop_back();

    mCurLocalCount -= localTable.size();
}

void Compiler::GenerateCall( Slist* list, const GenConfig& config, GenStatus& status )
{
    auto op = (Symbol*) list->Elements[0].get();

    for ( size_t i = list->Elements.size() - 1; i > 0; i-- )
    {
        Generate( list->Elements[i].get() );
    }

    int argCount = list->Elements.size() - 1;
    U8 callFlags = CallFlags::Build( argCount, config.discard );

    SymTable::iterator it = mGlobalTable.find( op->String );
    if ( it != mGlobalTable.end() )
    {
        Function* func = (Function*) it->second.get();
        U32 addr = 0;

        if ( it->second->Kind == Decl_Func )
        {
            addr = func->Address;
        }
        else if ( it->second->Kind == Decl_Forward )
        {
            PushPatch( &func->Patches );
        }
        else
        {
            ThrowError( CERR_SEMANTICS, op, "'%s' is not a function", op->String.c_str() );
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
        ExternalFunc external = { 0 };
        int opCode = 0;

        if ( mEnv->FindExternal( op->String, &external ) )
        {
            if ( external.Kind == External_Bytecode )
            {
                opCode = OP_CALLM;

                // TODO: Add this external call to the list of called functions
            }
            else if ( external.Kind == External_Native )
            {
                if ( external.Id >= 0x100 )
                    opCode = OP_CALLNATIVE;
                else
                    opCode = OP_CALLNATIVE_S;
            }
            else
            {
                assert( false );
                ThrowInternalError();
            }

            mCodeBinPtr[0] = opCode;
            mCodeBinPtr[1] = callFlags;
            mCodeBinPtr += 2;

            if ( opCode == OP_CALLNATIVE_S )
            {
                *(U8*) mCodeBinPtr = (U8) external.Id;
                mCodeBinPtr += 1;
            }
            else
            {
                WriteU32( mCodeBinPtr, external.Id );
            }
        }
        else
        {
            Function* func = AddForward( op->String );
            PushPatch( &func->Patches );
            mForwards++;

            mCodeBinPtr[0] = OP_CALL;
            mCodeBinPtr[1] = callFlags;
            mCodeBinPtr += 2;
            WriteU24( mCodeBinPtr, 0 );

            if ( mInFunc )
                mCurFunc->CalledFunctions.push_back( op->String );
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

void Compiler::GenerateLoop( Slist* list, const GenConfig& config, GenStatus& status )
{
    SymTable localTable;

    if ( list->Elements.size() < 8 )
        ThrowError( CERR_SEMANTICS, list, "'loop' takes 7 or more arguments" );

    MatchSymbol( list->Elements[1].get(), "for" );

    // Variable name

    if ( list->Elements[2]->Code != Elem_Symbol )
        ThrowError( CERR_SEMANTICS, list->Elements[2].get(), "Expected variable name" );

    Symbol* localSym = (Symbol*) list->Elements[2].get();
    Storage* local = AddLocal( localTable, localSym->String, mCurLocalCount );

    mCurLocalCount++;
    if ( mCurLocalCount > mMaxLocalCount )
        mMaxLocalCount = mCurLocalCount;

    MatchSymbol( list->Elements[3].get(), "from" );

    if ( list->Elements[5]->Code != Elem_Symbol )
        ThrowError( CERR_SEMANTICS, list->Elements[5].get(), "Expected symbol: to or downto" );

    int primitive;
    int step;

    if ( 0 == strcmp( ((Symbol*) list->Elements[5].get())->String.c_str(), "below" ) )
    {
        primitive = PRIM_GT;
        step = 1;
    }
    else if ( 0 == strcmp( ((Symbol*) list->Elements[5].get())->String.c_str(), "to" ) )
    {
        primitive = PRIM_GE;
        step = 1;
    }
    else if ( 0 == strcmp( ((Symbol*) list->Elements[5].get())->String.c_str(), "downto" ) )
    {
        primitive = PRIM_LE;
        step = -1;
    }
    else if ( 0 == strcmp( ((Symbol*) list->Elements[5].get())->String.c_str(), "above" ) )
    {
        primitive = PRIM_LT;
        step = -1;
    }
    else
    {
        ThrowError( CERR_SEMANTICS, list->Elements[5].get(), "Expected symbol: to, downto, above, below" );
    }

    if ( list->Elements[7]->Code != Elem_Symbol )
        ThrowError( CERR_SEMANTICS, list->Elements[7].get(), "Expected symbol: do, by" );

    int headerSize = 8;
    Element* stepExpr = nullptr;

    if ( 0 == strcmp( ((Symbol*) list->Elements[7].get())->String.c_str(), "by" ) )
    {
        if ( list->Elements.size() < 10 )
            ThrowError( CERR_SEMANTICS, list, "'loop' with a step takes 9 or more arguments" );

        stepExpr = list->Elements[8].get();
        headerSize = 10;
    }

    MatchSymbol( list->Elements[headerSize - 1].get(), "do" );

    mSymStack.push_back( &localTable );

    PatchChain  bodyChain;
    PatchChain  testChain;
    PatchChain  breakChain;
    PatchChain  nextChain;

    // Beginning expression
    Generate( list->Elements[4].get() );
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
    GenerateStatements( list, headerSize, config.WithLoop( &breakChain, &nextChain ), status );

    Patch( &nextChain );

    if ( stepExpr != nullptr )
        Generate( stepExpr );
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
    Generate( list->Elements[6].get() );

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

    mSymStack.pop_back();

    mCurLocalCount -= localTable.size();

    GenerateNilIfNeeded( config, status );
}

void Compiler::GenerateDo( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 2 )
        ThrowError( CERR_SEMANTICS, list, "'do' takes 1 or more arguments" );

    PatchChain  breakChain;
    PatchChain  nextChain;
    PatchChain  trueChain;

    U8* testPtr = mCodeBinPtr;

    // Test expression
    Generate( list->Elements[1].get(), GenConfig::Expr( &trueChain, &breakChain, false ) );

    ElideTrue( &trueChain, &breakChain );
    Patch( &trueChain );

    // Body
    GenerateStatements( list, 2, config.WithLoop( &breakChain, &nextChain ), status );

    PushPatch( &nextChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    Patch( &breakChain );
    Patch( &nextChain, testPtr );

    GenerateNilIfNeeded( config, status );
}

void Compiler::GenerateBreak( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( config.breakChain == nullptr )
        ThrowError( CERR_SEMANTICS, list, "Cannot use break outside of a loop" );

    PushPatch( config.breakChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    status.discarded = true;
}

void Compiler::GenerateNext( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( config.nextChain == nullptr )
        ThrowError( CERR_SEMANTICS, list, "Cannot use next outside of a loop" );

    PushPatch( config.nextChain );
    mCodeBinPtr[0] = OP_B;
    mCodeBinPtr += BranchInst::Size;

    status.discarded = true;
}

void Compiler::GenerateCase( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 2 )
        ThrowError( CERR_SEMANTICS, list, "'case' takes 1 or more arguments" );

    GenerateGeneralCase( list, config, status );
}

void Compiler::GenerateGeneralCase( Slist* list, const GenConfig& config, GenStatus& status )
{
    SymTable localTable;
    PatchChain exitChain;
    bool usesTempKeyform = false;

    const GenConfig& statementConfig = config;

    if ( list->Elements[1]->Code == Elem_Slist )
    {
        usesTempKeyform = true;

        std::unique_ptr<Symbol> localSym( new Symbol() );
        localSym->Code = Elem_Symbol;
        localSym->String = "$testKey";
        Storage* local = AddLocal( localTable, localSym->String, mCurLocalCount );

        mCurLocalCount++;
        if ( mCurLocalCount > mMaxLocalCount )
            mMaxLocalCount = mCurLocalCount;

        mSymStack.push_back( &localTable );

        Generate( list->Elements[1].get() );
        mCodeBinPtr[0] = OP_STLOC;
        mCodeBinPtr[1] = local->Offset;
        mCodeBinPtr += 2;
        DecreaseExprDepth();

        // Replace the keyform expression with the temporary variable
        list->Elements[1] = std::move( localSym );
    }

    for ( size_t i = 2; i < list->Elements.size(); i++ )
    {
        auto clauseElem = list->Elements[i].get();

        if ( clauseElem->Code != Elem_Slist )
            ThrowError( CERR_SEMANTICS, clauseElem, "" );

        auto clause = (Slist*) list->Elements[i].get();

        if ( clause->Elements[0]->Code == Elem_Symbol
            && (((Symbol*) clause->Elements[0].get())->String == "otherwise"
                || ((Symbol*) clause->Elements[0].get())->String == "true") )
        {
            if ( i != list->Elements.size() - 1 )
                ThrowError( CERR_SEMANTICS, clause, "" );

            GenerateImplicitProgn( clause, 1, statementConfig, status );
        }
        else
        {
            // If the key is not a list, then make it one, so that there's only one way to iterate.

            if ( clause->Elements[0]->Code != Elem_Slist )
            {
                std::unique_ptr<Slist> newList( new Slist() );
                newList->Code = Elem_Slist;
                newList->Elements.push_back( std::move( clause->Elements[0] ) );
                clause->Elements[0] = std::move( newList );
            }

            PatchChain falseChain;
            PatchChain trueChain;

            auto keyList = (Slist*) clause->Elements[0].get();
            int i = 0;

            for ( auto& key : keyList->Elements )
            {
                i++;

                if ( key->Code == Elem_Slist )
                    ThrowError( CERR_SEMANTICS, key.get(), "" );

                Generate( list->Elements[1].get() );
                Generate( key.get() );

                mCodeBinPtr[0] = OP_PRIM;
                mCodeBinPtr[1] = PRIM_EQ;
                mCodeBinPtr += 2;
                DecreaseExprDepth();

                if ( i == keyList->Elements.size() )
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

            GenerateImplicitProgn( clause, 1, statementConfig, status );

            PushPatch( &exitChain );
            mCodeBinPtr[0] = OP_B;
            mCodeBinPtr += BranchInst::Size;

            Patch( &falseChain );
        }
    }

    Patch( &exitChain );

    if ( usesTempKeyform )
    {
        mSymStack.pop_back();
        mCurLocalCount -= localTable.size();
    }
}

void Compiler::GenerateNot( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() != 2 )
        ThrowError( CERR_SEMANTICS, list, "'not' takes 1 argument" );

    Generate( list->Elements[1].get(), config.Invert(), status );
    status.kind = Expr_Logical;
}

void Compiler::GenerateComparison( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() != 3 )
        ThrowError( CERR_SEMANTICS, list, "comparison operators take 2 operands" );

    auto op = (Symbol*) list->Elements[0].get();
    int positivePrimitive;
    int negativePrimitive;

    if ( op->String == "=" )
    {
        positivePrimitive = PRIM_EQ;
        negativePrimitive = PRIM_NE;
    }
    else if ( op->String == "<>" )
    {
        positivePrimitive = PRIM_NE;
        negativePrimitive = PRIM_EQ;
    }
    else if ( op->String == "<" )
    {
        positivePrimitive = PRIM_LT;
        negativePrimitive = PRIM_GE;
    }
    else if ( op->String == "<=" )
    {
        positivePrimitive = PRIM_LE;
        negativePrimitive = PRIM_GT;
    }
    else if ( op->String == ">" )
    {
        positivePrimitive = PRIM_GT;
        negativePrimitive = PRIM_LE;
    }
    else if ( op->String == ">=" )
    {
        positivePrimitive = PRIM_GE;
        negativePrimitive = PRIM_LT;
    }
    else
    {
        assert( false );
        ThrowInternalError();
    }

    GenerateBinaryPrimitive( list, config.invert ? negativePrimitive : positivePrimitive, config, status );
    status.kind = Expr_Comparison;
}

void Compiler::GenerateAnd( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 3 )
        ThrowError( CERR_SEMANTICS, list, "'and' takes 2 or more operands" );

    ConjSpec spec = { &Compiler::GenerateAndClause, &Compiler::GenerateOrClause };
    GenerateConj( &spec, list, config );
    status.kind = Expr_Logical;

    if ( config.discard )
        status.discarded = true;
}

void Compiler::GenerateOr( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 3 )
        ThrowError( CERR_SEMANTICS, list, "'or' takes 2 or more operands" );

    ConjSpec spec = { &Compiler::GenerateOrClause, &Compiler::GenerateAndClause };
    GenerateConj( &spec, list, config );
    status.kind = Expr_Logical;

    if ( config.discard )
        status.discarded = true;
}

void Compiler::GenerateConj( ConjSpec* spec, Slist* list, const GenConfig& config )
{
    if ( config.trueChain == nullptr )
    {
        Atomize( spec, list, config.invert, config.discard );
        return;
    }

    for ( size_t i = 1; i < list->Elements.size() - 1; i++ )
    {
        if ( config.invert )
        {
            (this->*(spec->NegativeGenerator))( list->Elements[i].get(), config );
        }
        else
        {
            (this->*(spec->PositiveGenerator))( list->Elements[i].get(), config );
        }
    }

    if ( list->Elements.size() > 1 )
    {
        Generate( list->Elements.back().get(), config );
    }
}

void Compiler::GenerateAndClause( Element* elem, const GenConfig& config )
{
    PatchChain  newTrueChain;
    Generate( elem, config.WithTrue( &newTrueChain ) );
    ElideTrue( &newTrueChain, config.falseChain );
    Patch( &newTrueChain );
}

void Compiler::GenerateOrClause( Element* elem, const GenConfig& config )
{
    PatchChain  newFalseChain;
    Generate( elem, config.WithFalse( &newFalseChain ) );
    ElideFalse( config.trueChain, &newFalseChain );
    Patch( &newFalseChain );
}

void Compiler::Atomize( ConjSpec* spec, Slist* list, bool invert, bool discard )
{
    PatchChain  trueChain;
    PatchChain  falseChain;

    GenerateConj( spec, list, GenConfig::Expr( &trueChain, &falseChain, invert ) );

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
        ThrowInternalError();
    }
}

void Compiler::PushPatch( PatchChain* chain )
{
    InstPatch* link = new InstPatch;
    link->Inst = mCodeBinPtr;
    link->Next = chain->Next;
    chain->Next = link;
}

void Compiler::PopPatch( PatchChain* chain )
{
    assert( chain->Next != nullptr );

    auto link = chain->Next;
    chain->Next = chain->Next->Next;
    delete link;
}

void Compiler::ElideTrue( PatchChain* trueChain, PatchChain* falseChain )
{
    if (   trueChain->Next  == nullptr 
        || falseChain->Next == nullptr )
        return;

    U8* target = mCodeBinPtr;
    size_t diff = target - (trueChain->Next->Inst + BranchInst::Size);

    if ( diff == BranchInst::Size
        && mCodeBinPtr[-BranchInst::Size] == OP_B
        && &mCodeBinPtr[-BranchInst::Size] == falseChain->Next->Inst
        )
    {
        falseChain->Next->Inst = trueChain->Next->Inst;
        trueChain->Next->Inst[0] = InvertJump( trueChain->Next->Inst[0] );

        // Remove the branch instruction.
        PopPatch( trueChain );
        mCodeBinPtr -= BranchInst::Size;
    }
}

void Compiler::ElideFalse( PatchChain* trueChain, PatchChain* falseChain )
{
    if ( falseChain->Next == nullptr )
        return;

    U8* target = mCodeBinPtr;
    size_t diff = target - (falseChain->Next->Inst + BranchInst::Size);

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

    for ( InstPatch* link = chain->Next; link != nullptr; link = link->Next )
    {
        ptrdiff_t diff = target - (link->Inst + BranchInst::Size);

        if ( diff < BranchInst::OffsetMin || diff > BranchInst::OffsetMax )
            ThrowError( CERR_UNSUPPORTED, nullptr, "Branch target is too far." );

        BranchInst::StoreOffset( &link->Inst[1], diff );
    }
}

void Compiler::PatchCalls( PatchChain* chain, U32 addr )
{
    for ( InstPatch* link = chain->Next; link != nullptr; link = link->Next )
    {
        StoreU24( &link->Inst[2], addr );
    }
}

void Compiler::GenerateUnaryPrimitive( Element* elem, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        Generate( elem, config, status );
    }
    else
    {
        Generate( elem );

        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = 0;
        mCodeBinPtr[2] = OP_PRIM;
        mCodeBinPtr[3] = PRIM_SUB;
        mCodeBinPtr += 4;

        IncreaseExprDepth();
        DecreaseExprDepth();
    }
}

void Compiler::GenerateBinaryPrimitive( Slist* list, int primitive, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        GenerateDiscard( list->Elements[2].get() );
        GenerateDiscard( list->Elements[1].get() );
        status.discarded = true;
    }
    else
    {
        Generate( list->Elements[2].get() );
        Generate( list->Elements[1].get() );

        mCodeBinPtr[0] = OP_PRIM;
        mCodeBinPtr[1] = primitive;
        mCodeBinPtr += 2;

        DecreaseExprDepth();
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

        char name[256];

        sprintf_s( name, "$Lambda$%d", i );
        Function* func = AddFunc( name, address );

        GenerateProc( it->Definition, 1, func );
    }
}

void Compiler::GenerateProc( Slist* list, int startIndex, Function* func )
{
    assert( (size_t) startIndex < list->Elements.size() );

    mInFunc = true;
    mCurFunc = func;

    const size_t ArgsIndex = startIndex;
    const size_t BodyIndex = startIndex + 1;

    SymTable argTable;

    mSymStack.push_back( &argTable );

    if ( list->Elements[ArgsIndex]->Code != Elem_Slist )
        ThrowError( CERR_SEMANTICS, list->Elements[ArgsIndex].get(), "function parameter list is missing" );

    Slist* arglist = (Slist*) list->Elements[ArgsIndex].get();

    for ( size_t i = 0; i < arglist->Elements.size(); i++ )
    {
        if ( arglist->Elements[i]->Code != Elem_Symbol )
            ThrowError( CERR_UNSUPPORTED, arglist->Elements[i].get(), "only simple parameters are supported" );

        Symbol* argSym = (Symbol*) arglist->Elements[i].get();
        AddArg( argTable, argSym->String, argTable.size() );

        func->ArgCount++;
    }

    constexpr uint8_t PushInstSize = 2;

    U8* bodyPtr = mCodeBinPtr;
    U8* pushCountPatch = nullptr;

    // Assume that there are local variables
    *mCodeBinPtr = OP_PUSH;
    mCodeBinPtr++;
    pushCountPatch = mCodeBinPtr;
    mCodeBinPtr++;

    mMaxLocalCount = 0;
    mCurLocalCount = 0;
    mCurExprDepth = 0;
    mMaxExprDepth = 0;
    mLocalLambdas.clear();

    GenStatus status = { Expr_Other };
    GenerateImplicitProgn( list, BodyIndex, GenConfig::Statement(), status );

    if ( !status.tailRet )
    {
        mCodeBinPtr[0] = OP_RET;
        mCodeBinPtr += 1;
    }

    if ( mMaxLocalCount > 0 )
    {
        *pushCountPatch = mMaxLocalCount;
    }
    else
    {
        // No locals. So, delete the PUSH instruction
        memmove( bodyPtr, bodyPtr + PushInstSize, (mCodeBinPtr - bodyPtr) - PushInstSize );
        mCodeBinPtr -= PushInstSize;

        // If local lambda references were generated, then shift them
        for ( auto index : mLocalLambdas )
        {
            mLambdas[index].Patch -= PushInstSize;
        }
    }

    func->LocalCount = mMaxLocalCount;
    func->ExprDepth = mMaxExprDepth;

    mSymStack.pop_back();

    mCurFunc = nullptr;
    mInFunc = false;
}

void Compiler::GenerateImplicitProgn( Slist* list, int startIndex, const GenConfig& config, GenStatus& status )
{
    for ( size_t i = startIndex; i < list->Elements.size() - 1; i++ )
    {
        GenerateDiscard( list->Elements[i].get() );
    }

    if ( list->Elements.size() > (size_t) startIndex )
    {
        Generate( list->Elements.back().get(), config, status );
    }
    else    // There are no expressions
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
}

void Compiler::GenerateStatements( Slist* list, int startIndex, const GenConfig& config, GenStatus& status )
{
    for ( size_t i = startIndex; i < list->Elements.size(); i++ )
    {
        GenerateDiscard( list->Elements[i].get(), config );
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

void Compiler::MatchSymbol( Element* elem, const char* name )
{
    if ( elem->Code != Elem_Symbol || 0 != strcmp( name, ((Symbol*) elem)->String.c_str() ) )
        ThrowError( CERR_SEMANTICS, elem, "Expected symbol: %s", name );
}

void Compiler::GenerateSentinel()
{
    for ( int i = 0; i < SENTINEL_SIZE; i++ )
    {
        mCodeBinPtr[i] = OP_SENTINEL;
    }

    mCodeBinPtr += SENTINEL_SIZE;
}

Compiler::Declaration* Compiler::FindSymbol( const std::string& symbol )
{
    for ( auto stackIt = mSymStack.rbegin(); stackIt != mSymStack.rend(); stackIt++ )
    {
        auto tableIt = (*stackIt)->find( symbol );
        if ( tableIt != (*stackIt)->end() )
            return tableIt->second.get();
    }

    int offset = 0;

    if ( mEnv->FindGlobal( symbol, offset ) )
    {
        static Storage dummy {};
        dummy.Kind = Decl_Global;
        dummy.Offset = offset;
        return &dummy;
    }

    return nullptr;
}

Compiler::Storage* Compiler::AddArg( SymTable& table, const std::string& name, int offset )
{
    auto* arg = new Storage();
    arg->Kind = Decl_Arg;
    arg->Offset = offset;
    table.insert( SymTable::value_type( name, arg ) );
    return arg;
}

Compiler::Function* Compiler::AddFunc( const std::string& name, int address )
{
    auto* func = new Function();
    func->Kind = Decl_Func;
    func->Name = name;
    func->Address = address;
    mGlobalTable.insert( SymTable::value_type( name, func ) );
    return func;
}

Compiler::Function* Compiler::AddForward( const std::string& name )
{
    auto* func = new Function();
    func->Kind = Decl_Forward;
    func->Name = name;
    func->Address = 0;
    mGlobalTable.insert( SymTable::value_type( name, func ) );
    return func;
}

Compiler::Storage* Compiler::AddLocal( SymTable& table, const std::string& name, int offset )
{
    auto* local = new Storage();
    local->Kind = Decl_Local;
    local->Offset = offset;
    table.insert( SymTable::value_type( name, local ) );
    return local;
}

Compiler::ConstDecl* Compiler::AddConst( const std::string& name, int value )
{
    auto* constant = new ConstDecl();
    constant->Kind = Decl_Const;
    constant->Value = value;
    mConstTable.insert( SymTable::value_type( name, constant ) );
    return constant;
}

void Compiler::MakeStdEnv()
{
    AddConst( "false", 0 );
    AddConst( "true", 1 );
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
        if ( decl->Kind == Decl_Func )
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

            // Add 1 for the deepest return value in the call tree
            int16_t stackUsage = func->TreeStackUsage + func->ArgCount + 1;

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
            if ( it->second->Kind != Decl_Func )
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

void Compiler::ThrowError( CompilerErr exceptionCode, Element* elem, const char* format, ... )
{
    if ( mLog != nullptr )
    {
        int line = 0;
        int col = 0;

        if ( elem != nullptr )
        {
            line = elem->Line;
            col = elem->Column;
        }

        va_list args;
        va_start( args, format );
        Log( LOG_ERROR, line, col, format, args );
        va_end( args );
    }

    throw CompilerException( exceptionCode );
}

void Compiler::ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args )
{
    Log( LOG_ERROR, line, col, format, args );
    throw CompilerException( exceptionCode );
}

void Compiler::ThrowInternalError()
{
    ThrowInternalError( "Internal error" );
}

void Compiler::ThrowInternalError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CERR_INTERNAL, 0, 0, format, args );
    va_end( args );
}

void Compiler::ThrowUnresolvedFuncsError()
{
    if ( mLog != nullptr )
    {
        char msg[256] = "";

        for ( auto it = mGlobalTable.begin(); it != mGlobalTable.end(); it++ )
        {
            if ( it->second->Kind == Decl_Forward )
            {
                sprintf_s( msg, "unresolved function: %s", it->first.c_str() );
                mLog->Add( LOG_ERROR, 0, 0, msg );
            }
        }
    }

    ThrowError( CERR_SEMANTICS, nullptr, "there are %d unresolved functions", mForwards );
}

void Compiler::Log( LogCategory category, int line, int col, const char* format, va_list args )
{
    ::Log( mLog, category, line, col, format, args );
}

void Compiler::LogWarning( int line, int col, const char* format, ... )
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
