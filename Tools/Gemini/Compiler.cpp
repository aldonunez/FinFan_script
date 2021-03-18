#include "stdafx.h"
#include "Compiler.h"
#include "OpCodes.h"
#include <ctype.h>
#include <cstdarg>


Compiler::Compiler( const char* codeText, int codeTextLen, U8* codeBin, int codeBinLen, ICompilerEnv* env,
    ICompilerLog* log, int modIndex )
    :   mCodeTextPtr( codeText ),
        mCodeTextEnd( codeText + codeTextLen ),
        mCodeBin( codeBin ),
        mCodeBinPtr( codeBin ),
        mCodeBinEnd( codeBin + codeBinLen ),
        mLine( 1 ),
        mLineStart( codeText ),
        mCurToken( Token_Bof ),
        mCurNumber( 0 ),
        mTokLine( 0 ),
        mTokCol( 0 ),
        mForwards( 0 ),
        mEnv( env ),
        mLog( log ),
        mModIndex( modIndex ),
        mInFunc( false )
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
}

CompilerErr Compiler::Compile()
{
#if 0
    while ( NextToken() != Token_Eof )
    {
        printf( "%3d %12d %s\n", mCurToken, mCurNumber, mCurString.c_str() );
    }
#else
    try
    {
        std::unique_ptr<Slist> progTree( Parse() );

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
    }
    catch ( CompilerException& ex )
    {
        return ex.GetError();
    }
#endif

    return CERR_OK;
}

void Compiler::GetStats( CompilerStats& stats )
{
    stats.CodeBytesWritten = mCodeBinPtr - mCodeBin;
}

Compiler::TokenCode Compiler::NextToken()
{
    SkipWhitespace();

    mCurString.clear();
    mCurNumber = 0;
    mTokLine = mLine;
    mTokCol = GetColumn();

    if ( mCodeTextPtr >= mCodeTextEnd )
    {
        mCurToken = Token_Eof;
    }
    else if ( *mCodeTextPtr == '(' )
    {
        mCurToken = Token_LParen;
        mCodeTextPtr++;
    }
    else if ( *mCodeTextPtr == ')' )
    {
        mCurToken = Token_RParen;
        mCodeTextPtr++;
    }
    else if ( isdigit( *mCodeTextPtr ) )
    {
        mCurToken = Token_Number;
        ReadNumber();
    }
    else if ( *mCodeTextPtr == '-' && isdigit( mCodeTextPtr[1] ) )
    {
        mCurToken = Token_Number;
        ReadNumber();
    }
    else
    {
        mCurToken = Token_Symbol;
        ReadSymbol();
    }

    return mCurToken;
}

void Compiler::SkipWhitespace()
{
    while ( true )
    {
        while ( mCodeTextPtr < mCodeTextEnd
            && isspace( *mCodeTextPtr ) )
        {
            if ( *mCodeTextPtr == '\n' )
            {
                mLine++;
                mLineStart = mCodeTextPtr + 1;
            }

            mCodeTextPtr++;
        }

        if ( mCodeTextPtr == mCodeTextEnd
            || *mCodeTextPtr != ';' )
        {
            break;
        }

        while ( mCodeTextPtr < mCodeTextEnd
            && *mCodeTextPtr != '\n' )
        {
            mCodeTextPtr++;
        }
    }
}

void Compiler::ReadNumber()
{
    bool negate = false;

    if ( *mCodeTextPtr == '-' )
    {
        negate = true;
        mCodeTextPtr++;
    }

    while ( isdigit( *mCodeTextPtr ) )
    {
        mCurString.append( 1, *mCodeTextPtr );
        mCodeTextPtr++;
    }

    if ( mCodeTextPtr < mCodeTextEnd
        && !isspace( *mCodeTextPtr )
        && (*mCodeTextPtr != '(')
        && (*mCodeTextPtr != ')')
        && (*mCodeTextPtr != ';') )
        ThrowSyntaxError( "syntax error : bad number" );

    mCurNumber = atoi( mCurString.c_str() );
    if ( negate )
        mCurNumber = -mCurNumber;
}

void Compiler::ReadSymbol()
{
    while ( (*mCodeTextPtr != '(') 
        && (*mCodeTextPtr != ')')
        && (*mCodeTextPtr != ';')
        && !isspace( *mCodeTextPtr ) )
    {
        mCurString.append( 1, *mCodeTextPtr );
        mCodeTextPtr++;
    }
}

int Compiler::GetColumn()
{
    return mCodeTextPtr - mLineStart + 1;
}

Compiler::Slist* Compiler::Parse()
{
    std::unique_ptr<Slist> list( new Slist() );
    list->Code = Elem_Slist;

    while ( NextToken() != Token_Eof )
    {
        if ( mCurToken != Token_LParen )
            ThrowSyntaxError( "syntax error : expected list" );

        list->Elements.push_back( std::unique_ptr<Slist>( ParseSlist() ) );
    }

    return list.release();
}

Compiler::Slist* Compiler::ParseSlist()
{
    std::unique_ptr<Slist> list( new Slist() );
    list->Code = Elem_Slist;
    list->Line = mTokLine;
    list->Column = mTokCol;

    for ( ; ; )
    {
        switch ( NextToken() )
        {
        case Token_LParen:
            list->Elements.push_back( std::unique_ptr<Slist>( ParseSlist() ) );
            break;

        case Token_RParen:
            goto Done;

        case Token_Number:
            list->Elements.push_back( std::unique_ptr<Number>( ParseNumber() ) );
            break;

        case Token_Symbol:
            list->Elements.push_back( std::unique_ptr<Symbol>( ParseSymbol() ) );
            break;

        case Token_Eof:
            ThrowSyntaxError( "syntax error : unexpected end-of-file" );
            break;

        default:
            ThrowInternalError();
        }
    }
Done:

    return list.release();
}

Compiler::Number* Compiler::ParseNumber()
{
    Number* number = new Number();
    number->Code = Elem_Number;
    number->Value = mCurNumber;
    number->Line = mTokLine;
    number->Column = mTokCol;
    return number;
}

Compiler::Symbol* Compiler::ParseSymbol()
{
    Symbol* symbol = new Symbol();
    symbol->Code = Elem_Symbol;
    symbol->String = mCurString;
    symbol->Line = mTokLine;
    symbol->Column = mTokCol;
    return symbol;
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
    if ( elem->Code != Elem_Slist )
        return;

    GenStatus status = { Expr_Other };
    GenerateSlist( (Slist*) elem, GenConfig::Discard(), status );

    if ( !status.discarded )
    {
        *mCodeBinPtr = OP_POP;
        mCodeBinPtr++;
    }
}

void Compiler::GenerateNumber( Number* number, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    if ( (number->Value >= SCHAR_MIN) && (number->Value <= SCHAR_MAX) )
    {
        mCodeBinPtr[0] = OP_LDC_S;
        mCodeBinPtr[1] = number->Value;
        mCodeBinPtr += 2;
    }
    else
    {
        *mCodeBinPtr = OP_LDC;
        mCodeBinPtr++;
        WriteI32( mCodeBinPtr, number->Value );
    }
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
            mCodeBinPtr[0] = OP_LDGLO;
            mCodeBinPtr++;
            WriteU16( mCodeBinPtr, ((Storage*) decl)->Offset );
            break;

        case Decl_Local:
            mCodeBinPtr[0] = OP_LDLOC;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
            break;

        case Decl_Arg:
            mCodeBinPtr[0] = OP_LDARG;
            mCodeBinPtr[1] = ((Storage*) decl)->Offset;
            mCodeBinPtr += 2;
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
    }

    mCodeBinPtr[0] = OP_RET;
    mCodeBinPtr += 1;

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

    // True
    Generate( list->Elements[2].get(), statementConfig );
    mCodeBinPtr[0] = OP_B;
    leaveOffset = &mCodeBinPtr[1];
    mCodeBinPtr += BranchInst::Size;

    ElideFalse( &newTrueChain, &newFalseChain );

    U8* falsePtr = mCodeBinPtr;

    // False
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

    GenConfig statementConfig = GenConfig::Statement( config.discard );

    // TODO: check all the clauses for tail-return. If they all do, then set status.tailRet.

    for ( size_t i = 1; i < list->Elements.size(); i++ )
    {
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

            if ( config.discard )
            {
                mCodeBinPtr[0] = OP_DUP;
                mCodeBinPtr++;
            }

            PushPatch( &leaveChain );

            mCodeBinPtr[0] = OP_BTRUE;
            mCodeBinPtr += BranchInst::Size;

            if ( config.discard )
            {
                mCodeBinPtr[0] = OP_POP;
                mCodeBinPtr++;
            }
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

            PushPatch( &leaveChain );

            mCodeBinPtr[0] = OP_B;
            mCodeBinPtr += BranchInst::Size;

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
        status.discarded = true;
    else
        *mCodeBinPtr++ = OP_DUP;

    if ( list->Elements[1]->Code != Elem_Symbol )
        ThrowError( CERR_SEMANTICS, list->Elements[1].get(), "'set' : first argument must be a symbol" );

    auto targetSym = (Symbol*) list->Elements[1].get();
    auto decl = FindSymbol( targetSym->String );
    if ( decl != nullptr )
    {
        switch ( decl->Kind )
        {
        case Decl_Global:
            mCodeBinPtr[0] = OP_STGLO;
            mCodeBinPtr++;
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

    mInFunc = true;

    Symbol* funcSym = (Symbol*) list->Elements[1].get();
    U32 addr = (mCodeBinPtr - mCodeBin);

    SymTable::iterator it = mGlobalTable.find( funcSym->String );
    if ( it != mGlobalTable.end() )
    {
        if ( it->second->Kind == Decl_Forward )
        {
            Function* func = (Function*) it->second.get();
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
        Function* func = AddFunc( funcSym->String, addr );

        mEnv->AddExternal( funcSym->String, External_Bytecode, func->Address );
    }

    GenerateProc( list, 2 );

    mInFunc = false;
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

    WriteU32( mCodeBinPtr, 0 );
}

void Compiler::GenerateFuncall( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 2 )
        ThrowError( CERR_SEMANTICS, list, "'funcall' takes 1 or more arguments" );

    for ( size_t i = list->Elements.size() - 1; i > 0; i-- )
    {
        Generate( list->Elements[i].get() );
    }

    mCodeBinPtr[0] = OP_CALLI;
    mCodeBinPtr[1] = CallFlags::Build( list->Elements.size() - 2, config.discard );
    mCodeBinPtr += 2;
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

    U8 callFlags = CallFlags::Build( list->Elements.size() - 1, config.discard );

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
        }
    }

    if ( config.discard )
        status.discarded = true;
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
}

void Compiler::GenerateOr( Slist* list, const GenConfig& config, GenStatus& status )
{
    if ( list->Elements.size() < 3 )
        ThrowError( CERR_SEMANTICS, list, "'or' takes 2 or more operands" );

    ConjSpec spec = { &Compiler::GenerateOrClause, &Compiler::GenerateAndClause };
    GenerateConj( &spec, list, config );
    status.kind = Expr_Logical;
}

void Compiler::GenerateConj( ConjSpec* spec, Slist* list, const GenConfig& config )
{
    if ( config.trueChain == nullptr )
    {
        Atomize( spec, list, config.invert );
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

void Compiler::Atomize( ConjSpec* spec, Slist* list, bool invert )
{
    PatchChain  trueChain;
    PatchChain  falseChain;

    GenerateConj( spec, list, GenConfig::Expr( &trueChain, &falseChain, invert ) );

    ElideTrue( &trueChain, &falseChain );

    Patch( &trueChain );
    mCodeBinPtr[0] = OP_LDC_S;
    mCodeBinPtr[1] = 1;
    mCodeBinPtr[2] = OP_B;
    mCodeBinPtr += 3;

    // Offset of 2 to jump over LDC.S below.
    BranchInst::WriteOffset( mCodeBinPtr, 2 );

    Patch( &falseChain );
    mCodeBinPtr[0] = OP_LDC_S;
    mCodeBinPtr[1] = 0;
    mCodeBinPtr += 2;
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
        mCodeBinPtr[2] = OP_CALLP;
        mCodeBinPtr[3] = PRIM_SUB;
        mCodeBinPtr += 4;
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

        mCodeBinPtr[0] = OP_CALLP;
        mCodeBinPtr[1] = primitive;
        mCodeBinPtr += 2;
    }
}

void Compiler::GenerateLambdas()
{
    for ( auto it = mLambdas.begin(); it != mLambdas.end(); it++ )
    {
        int address = mCodeBinPtr - mCodeBin;
        int addrWord = CodeAddr::Build( address, mModIndex );
        StoreU32( it->Patch, addrWord );
        GenerateProc( it->Definition, 1 );
    }
}

void Compiler::GenerateProc( Slist* list, int startIndex )
{
    assert( (size_t) startIndex < list->Elements.size() );

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
    }

    bool hasLocals = false;
    U8* pushCountPatch = nullptr;

    for ( size_t i = BodyIndex; i < list->Elements.size(); i++ )
    {
        if ( HasLocals( list->Elements[i].get() ) )
        {
            hasLocals = true;
            break;
        }
    }

    if ( hasLocals )
    {
        *mCodeBinPtr = OP_PUSH;
        mCodeBinPtr++;
        pushCountPatch = mCodeBinPtr;
        mCodeBinPtr++;
    }

    mMaxLocalCount = 0;
    mCurLocalCount = 0;

    GenStatus status = { Expr_Other };
    GenerateImplicitProgn( list, BodyIndex, GenConfig::Statement(), status );

    if ( !status.tailRet )
    {
        mCodeBinPtr[0] = OP_RET;
        mCodeBinPtr += 1;
    }

    if ( hasLocals )
        *pushCountPatch = mMaxLocalCount;

    mSymStack.pop_back();
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
    else
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
        }
    }
}

bool Compiler::HasLocals( Element* elem )
{
    if ( elem->Code != Elem_Slist )
        return false;

    Slist* list = (Slist*) elem;

    if ( list->Elements.size() == 0 )
        return false;

    if ( list->Elements[0]->Code == Elem_Symbol )
    {
        auto* op = (Symbol*) list->Elements[0].get();
        if ( op->String == "let" )
            return true;
        else if ( op->String == "lambda" )
            return false;
    }

    for ( size_t i = 0; i < list->Elements.size(); i++ )
    {
        if ( HasLocals( list->Elements[i].get() ) )
            return true;
    }

    return false;
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
        static Storage dummy;
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
    AddConst( "#f", 0 );
    AddConst( "#t", 1 );
}

void Compiler::ThrowSyntaxError( const char* format, ... )
{
    if ( mLog != nullptr )
    {
        va_list args;
        va_start( args, format );
        Log( LOG_ERROR, mTokLine, mTokCol, format, args );
        va_end( args );
    }

    throw CompilerException( CERR_SYNTAX );
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

void Compiler::Log( LogCategory category, int line, int col, const char* format, va_list args )
{
    if ( mLog != nullptr )
    {
        char msg[256] = "";
        vsprintf_s( msg, format, args );
        mLog->Add( category, line, col, msg );
    }
}

void Compiler::ThrowInternalError()
{
    if ( mLog != nullptr )
        mLog->Add( LOG_ERROR, mLine, GetColumn(), "internal error" );

    throw CompilerException( CERR_INTERNAL );
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
