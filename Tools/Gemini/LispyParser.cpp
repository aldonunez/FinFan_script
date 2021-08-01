// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "stdafx.h"
#include "LispyParser.h"
#include <stdarg.h>


static const uint8_t sIdentifierInitialCharMap[] =
{
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0,
};

static const char* gTokenNames[] =
{
    "",
    "<End-of-file>",
    "(",
    ")",
    "<Number>",
    "<Symbol>",
};


LispyParser::LispyParser( const char* codeText, int codeTextLen, const char* fileName, ICompilerLog* log ) :
    mFileName( fileName != nullptr ? fileName : "" ),
    mUnitFileName(),
    mCodeTextPtr( codeText ),
    mCodeTextEnd( codeText + codeTextLen ),
    mLineStart( codeText ),
    mLine( 1 ),
    mCurChar( 0 ),
    mCurToken( TokenCode::Bof ),
    mCurNumber( 0 ),
    mTokLine( 0 ),
    mTokCol( 0 ),
    mRep( log )
{
    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCurChar = *mCodeTextPtr;
    }

    mParserMap =
    {
        { "+", &LispyParser::ParseBinary },
        { "*", &LispyParser::ParseBinary },
        { "/", &LispyParser::ParseBinary },
        { "%", &LispyParser::ParseBinary },
        { "-", &LispyParser::ParseNegate },
        { "=",  &LispyParser::ParseBinary},
        { "<>", &LispyParser::ParseBinary },
        { "<",  &LispyParser::ParseBinary },
        { "<=", &LispyParser::ParseBinary },
        { ">",  &LispyParser::ParseBinary },
        { ">=", &LispyParser::ParseBinary },
        { "and", &LispyParser::ParseBinary },
        { "or",  &LispyParser::ParseBinary },
        { "not", &LispyParser::ParseNot },
        { "eval*", &LispyParser::ParseEvalStar },
        { "lambda", &LispyParser::ParseLambda },
        { "function", &LispyParser::ParseFunction },
        { "funcall", &LispyParser::ParseFuncall },
        { "return", &LispyParser::ParseReturn },
        { "let", &LispyParser::ParseLet },
        { "defvar", &LispyParser::ParseGlobalError },
        { "aref", &LispyParser::ParseAref },
        { "set", &LispyParser::ParseSet },
        { "if", &LispyParser::ParseIf },
        { "cond", &LispyParser::ParseCond },
        { "loop", &LispyParser::ParseLoop },
        { "do", &LispyParser::ParseDo },
        { "break", &LispyParser::ParseBreak },
        { "next", &LispyParser::ParseNext },
        { "case", &LispyParser::ParseCase },
        { "progn", &LispyParser::ParseProgn },
        { "defun", &LispyParser::ParseGlobalError },
    };
}

int LispyParser::GetColumn()
{
    return mCodeTextPtr - mLineStart + 1;
}

int LispyParser::PeekChar() const
{
    return mCurChar;
}

int LispyParser::PeekChar( int index ) const
{
    if ( (mCodeTextEnd - mCodeTextPtr) < (index + 1) )
        return 0;

    return mCodeTextPtr[index];
}

void LispyParser::NextChar()
{
    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCodeTextPtr++;
    }

    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCurChar = *mCodeTextPtr;
    }
    else
    {
        mCurChar = 0;
    }
}

LispyParser::TokenCode LispyParser::ScanToken()
{
    SkipWhitespace();

    mCurString.clear();
    mCurNumber = 0;
    mTokLine = mLine;
    mTokCol = GetColumn();

    if ( PeekChar() == 0 )
    {
        mCurToken = TokenCode::Eof;
    }
    else if ( PeekChar() == '(' )
    {
        NextChar();
        mCurToken = TokenCode::LParen;
    }
    else if ( PeekChar() == ')' )
    {
        NextChar();
        mCurToken = TokenCode::RParen;
    }
    else if ( isdigit( PeekChar() ) )
    {
        ReadNumber();
    }
    else if ( PeekChar() == '-' && isdigit( PeekChar( 1 ) ) )
    {
        ReadNumber();
    }
    else if ( IsIdentifierInitial( PeekChar() ) )
    {
        ReadSymbol();
    }
    else
    {
        ThrowSyntaxError( "Bad character: U+%02X", PeekChar() );
    }

    return mCurToken;
}

LispyParser::TokenCode LispyParser::ScanToken( TokenCode code )
{
    AssertToken( code );
    return ScanToken();
}

LispyParser::TokenCode LispyParser::ScanLParen()
{
    return ScanToken( TokenCode::LParen );
}

LispyParser::TokenCode LispyParser::ScanRParen()
{
    return ScanToken( TokenCode::RParen );
}

std::string LispyParser::ScanSymbol()
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "syntax error : expected symbol" );

    std::string symbol = std::move( mCurString );
    ScanToken();
    return symbol;
}

LispyParser::TokenCode LispyParser::ScanSymbol( const char* str )
{
    if ( mCurToken != TokenCode::Symbol || mCurString != str )
        ThrowSyntaxError( "syntax error : expected symbol '%s'", str );

    return ScanToken();
}

void LispyParser::AssertToken( TokenCode code )
{
    if ( mCurToken != code )
        ThrowSyntaxError( "syntax error : expected %s", gTokenNames[(int) code] );
}

void LispyParser::SkipWhitespace()
{
    while ( true )
    {
        while ( isspace( PeekChar() ) )
        {
            int c = PeekChar();

            NextChar();

            if ( c == '\r'
                || c == '\n' )
            {
                if ( c == '\r'
                    && PeekChar() == '\n' )
                {
                    NextChar();
                }

                mLine++;
                mLineStart = mCodeTextPtr + 1;
            }
        }

        if ( PeekChar() != ';' )
        {
            break;
        }

        while ( PeekChar() != '\r'
            && PeekChar() != '\n' )
        {
            NextChar();
        }
    }
}

bool LispyParser::IsIdentifierInitial( int c )
{
    return (c < sizeof sIdentifierInitialCharMap)
        && sIdentifierInitialCharMap[c];
}

bool LispyParser::IsIdentifierCoda( int c )
{
    return IsIdentifierInitial( c ) || isdigit( c );
}

void LispyParser::ReadNumber()
{
    bool negate = false;

    if ( PeekChar() == '-' )
    {
        negate = true;
        NextChar();
    }

    while ( isdigit( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    if ( IsIdentifierInitial( PeekChar() ) )
        ThrowSyntaxError( "syntax error : bad number" );

    mCurNumber = atoi( mCurString.c_str() );
    if ( negate )
        mCurNumber = -mCurNumber;

    mCurToken = TokenCode::Number;
}

void LispyParser::ReadSymbol()
{
    while ( IsIdentifierCoda( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    mCurToken = TokenCode::Symbol;
}

Unique<Unit> LispyParser::Parse()
{
    auto unit = Make<Unit>( mFileName );

    mUnitFileName = unit->GetUnitFileName();

    ScanToken();

    while ( mCurToken != TokenCode::Eof )
    {
        ScanLParen();
        AssertToken( TokenCode::Symbol );

        if ( mCurString == "defun" )
        {
            unit->FuncDeclarations.push_back( ParseProc( true ) );
        }
        else if ( mCurString == "defnative" )
        {
            unit->DataDeclarations.push_back( ParseNative() );
        }
        else if ( mCurString == "defvar" )
        {
            unit->DataDeclarations.push_back( ParseDefvar() );
        }
        else if ( mCurString == "defconstant" )
        {
            unit->DataDeclarations.push_back( ParseDefconstant() );
        }
        else
        {
            ThrowSyntaxError( "'%s' is not valid at global scope", mCurString.c_str() );
        }
    }

    return unit;
}

Unique<ProcDecl> LispyParser::ParseProc( bool hasName )
{
    auto proc = Make<ProcDecl>();

    ScanToken();

    if ( hasName )
        proc->Name = ScanSymbol();

    proc->Params = ParseParamList();

    ParseImplicitProgn( proc->Body );

    ScanRParen();

    return proc;
}

std::vector<Unique<DataDecl>> LispyParser::ParseParamList()
{
    std::vector<Unique<DataDecl>> paramList;

    ScanLParen();

    while ( mCurToken != TokenCode::RParen )
    {
        paramList.push_back( ParseParameter() );
    }

    ScanToken();

    return paramList;
}

Unique<DataDecl> LispyParser::ParseParameter()
{
    return ParseLetBinding( Make<ParamDecl>(), true );
}

Unique<Syntax> LispyParser::ParseGlobalError()
{
    ThrowSyntaxError( "'%s' is only allowed at global scope", mCurString.c_str() );
}

Unique<Syntax> LispyParser::ParseExpression( bool isInit )
{
    if ( mCurToken == TokenCode::Number )
    {
        return ParseNumber();
    }
    else if ( mCurToken == TokenCode::Symbol )
    {
        return ParseSymbol();
    }
    else if ( mCurToken == TokenCode::LParen )
    {
        ScanToken();
        AssertToken( TokenCode::Symbol );

        if ( isInit )
        {
            if ( mCurString == "array" )
                return ParseArrayInitializer();
        }

        if ( auto it = mParserMap.find( mCurString );
            it != mParserMap.end() )
        {
            return (this->*it->second)();
        }
        else
        {
            return ParseCall();
        }
    }

    ThrowSyntaxError( "Expected number, symbol, or '('" );
}

Unique<NumberExpr> LispyParser::ParseNumber()
{
    auto node = Make<NumberExpr>( mCurNumber );
    ScanToken();
    return node;
}

Unique<NameExpr> LispyParser::ParseSymbol()
{
    auto node = Make<NameExpr>( std::move( mCurString ) );
    ScanToken();
    return node;
}

Unique<Syntax> LispyParser::ParseBinary()
{
    auto node = Make<BinaryExpr>();

    node->Op = ScanSymbol();
    node->Left = ParseExpression();
    node->Right = ParseExpression();

    ScanRParen();

    return node;
}

Unique<Syntax> LispyParser::ParseNegate()
{
    Unique<Syntax> node;

    std::string op = ScanSymbol();

    auto first = ParseExpression();

    if ( mCurToken == TokenCode::RParen )
    {
        auto unary = Make<UnaryExpr>();

        unary->Op = std::move( op );
        unary->Inner = std::move( first );

        node = std::move( unary );
    }
    else
    {
        auto binary = Make<BinaryExpr>();

        binary->Op = std::move( op );
        binary->Left = std::move( first );
        binary->Right = ParseExpression();

        node = std::move( binary );
    }

    ScanRParen();

    return node;
}

Unique<Syntax> LispyParser::ParseNot()
{
    auto node = Make<UnaryExpr>();

    ScanToken();

    node->Op = "not";
    node->Inner = ParseExpression();

    ScanRParen();

    return node;
}

Unique<Syntax> LispyParser::ParseCall()
{
    auto call = Make<CallExpr>();

    call->IsIndirect = false;
    call->Head = ParseSymbol();

    while ( mCurToken != TokenCode::RParen )
    {
        call->Arguments.push_back( ParseExpression() );
    }

    ScanToken();

    return call;
}

Unique<Syntax> LispyParser::ParseEvalStar()
{
    auto node = Make<CallOrSymbolExpr>();

    ScanToken();

    node->Symbol = ParseSymbol();

    ScanRParen();

    return node;
}

Unique<Syntax> LispyParser::ParseLambda()
{
    auto lambdaExpr = Make<LambdaExpr>();

    // Delegate the rest of the parsing to ParseProc

    lambdaExpr->Proc = ParseProc( false );

    return lambdaExpr;
}

Unique<NativeDecl> LispyParser::ParseNative()
{
    auto nativeDecl = Make<NativeDecl>();

    ScanToken();

    nativeDecl->Name = ScanSymbol();
    nativeDecl->Params = ParseParamList();

    ScanRParen();

    return nativeDecl;
}

Unique<Syntax> LispyParser::ParseFunction()
{
    auto addrOf = Make<AddrOfExpr>();

    ScanToken();

    addrOf->Inner = ParseSymbol();

    ScanRParen();

    return addrOf;
}

Unique<Syntax> LispyParser::ParseFuncall()
{
    auto call = Make<CallExpr>();

    ScanToken();

    call->IsIndirect = true;
    call->Head = ParseExpression();

    while ( mCurToken != TokenCode::RParen )
    {
        call->Arguments.push_back( ParseExpression() );
    }

    ScanToken();

    return call;
}

Unique<Syntax> LispyParser::ParseReturn()
{
    auto node = Make<ReturnStatement>();

    ScanToken();

    node->Inner = ParseExpression();

    ScanRParen();

    return node;
}

Unique<Syntax> LispyParser::ParseLet()
{
    auto node = Make<LetStatement>();

    ScanToken();
    ScanLParen();

    while ( mCurToken != TokenCode::RParen )
    {
        ScanLParen();
        node->Variables.push_back( ParseLetBinding( Make<VarDecl>() ) );
    }

    ScanRParen();

    ParseImplicitProgn( node->Body );
    ScanRParen();

    return node;
}

Unique<DataDecl> LispyParser::ParseLetBinding( Unique<DataDecl>&& varDecl, bool isParam )
{
    if ( mCurToken == TokenCode::Symbol )
    {
        varDecl->Name = ScanSymbol();

        if ( !isParam && mCurToken != TokenCode::RParen )
        {
            varDecl->Initializer = ParseExpression( true );
        }
    }
    else if ( mCurToken == TokenCode::LParen )
    {
        ScanToken();

        varDecl->Name = ScanSymbol();
        ScanSymbol( ":" );
        varDecl->TypeRef = ParseTypeRef();

        if ( !isParam && mCurToken != TokenCode::RParen )
        {
            varDecl->Initializer = ParseExpression( true );
        }
    }
    else
    {
        ThrowSyntaxError( "Expected name or name and type" );
    }

    if ( !isParam )
        ScanRParen();

    return varDecl;
}

Unique<TypeRef> LispyParser::ParseTypeRef( bool embedded )
{
    if ( !embedded )
    {
        if ( mCurToken == TokenCode::LParen )
        {
            ScanToken();
            // Now it's embedded
        }
        else
        {
            return ParseNameTypeRef( false );
        }
    }

    AssertToken( TokenCode::Symbol );

    if ( mCurString == "array" )
    {
        return ParseArrayTypeRef();
    }
    else if ( mCurString == "->" )
    {
        return ParsePtrFuncTypeRef();
    }
    else
    {
        return ParseNameTypeRef( true );
    }
}

Unique<TypeRef> LispyParser::ParseNameTypeRef( bool embedded )
{
    auto nameTypeRef = Make<NameTypeRef>();

    nameTypeRef->QualifiedName = ParseSymbol();

    if ( embedded )
        ScanRParen();

    return nameTypeRef;
}

Unique<TypeRef> LispyParser::ParsePtrFuncTypeRef()
{
    auto procTypeRef = Make<ProcTypeRef>();

    ScanToken();

    do
    {
        procTypeRef->Params.push_back( ParseTypeRef( false ) );

    } while ( mCurToken != TokenCode::RParen );

    ScanRParen();

    Unique<PointerTypeRef> pointerTypeRef( new PointerTypeRef() );

    pointerTypeRef->Target = std::move( procTypeRef );

    return pointerTypeRef;
}

Unique<TypeRef> LispyParser::ParseArrayTypeRef()
{
    auto typeRef = Make<ArrayTypeRef>();

    ScanSymbol( "array" );
    ScanLParen();

    typeRef->SizeExpr = ParseExpression();

    ScanRParen();
    ScanRParen();

    return typeRef;
}

Unique<Syntax> LispyParser::ParseArrayInitializer()
{
    auto initList = Make<InitList>();

    ScanToken();

    while ( mCurToken != TokenCode::RParen )
    {
        if ( mCurToken == TokenCode::Symbol && mCurString == "&repeat" )
        {
            initList->Fill = ArrayFill::Repeat;
            ScanToken();
        }
        else if ( mCurToken == TokenCode::Symbol && mCurString == "&extra" )
        {
            initList->Fill = ArrayFill::Extrapolate;
            ScanToken();
        }
        else
        {
            initList->Values.push_back( ParseExpression() );
        }
    }

    ScanRParen();

    return initList;
}

Unique<DataDecl> LispyParser::ParseDefvar()
{
    ScanToken();

    return ParseLetBinding( Make<VarDecl>() );
}

Unique<DataDecl> LispyParser::ParseDefconstant()
{
    ScanToken();

    return ParseLetBinding( Make<ConstDecl>() );
}

Unique<Syntax> LispyParser::ParseAref()
{
    auto indexExpr = Make<IndexExpr>();

    ScanToken();

    indexExpr->Head = ParseExpression();
    indexExpr->Index = ParseExpression();

    ScanRParen();

    return indexExpr;
}

Unique<Syntax> LispyParser::ParseSet()
{
    auto assignment = Make<AssignmentExpr>();

    ScanToken();

    assignment->Left = ParseExpression();
    assignment->Right = ParseExpression();

    ScanRParen();

    return assignment;
}

Unique<Syntax> LispyParser::ParseIf()
{
    auto condExpr = Make<CondExpr>();
    auto consequence = Make<CondClause>();

    condExpr->IsIf = true;

    ScanToken();

    consequence->Condition = ParseExpression();
    consequence->Body.Statements.push_back( ParseExpression() );
    condExpr->Clauses.push_back( std::move( consequence ) );

    if ( mCurToken != TokenCode::RParen )
    {
        auto alternative = Make<CondClause>();

        alternative->Condition = Make<NumberExpr>( 1 );
        alternative->Body.Statements.push_back( ParseExpression() );
        condExpr->Clauses.push_back( std::move( alternative ) );
    }

    ScanRParen();

    return condExpr;
}

Unique<Syntax> LispyParser::ParseCond()
{
    auto condExpr = Make<CondExpr>();

    ScanToken();

    while ( mCurToken != TokenCode::RParen )
    {
        condExpr->Clauses.push_back( ParseCondClause() );
    }

    ScanRParen();

    return condExpr;
}

Unique<CondClause> LispyParser::ParseCondClause()
{
    auto clause = Make<CondClause>();

    ScanLParen();

    clause->Condition = ParseExpression();
    ParseImplicitProgn( clause->Body );

    ScanRParen();

    return clause;
}

Unique<Syntax> LispyParser::ParseLoop()
{
    ScanToken();
    AssertToken( TokenCode::Symbol );

    if ( mCurString == "for" )
    {
        return ParseLoopFor();
    }
    else if ( mCurString == "do" )
    {
        return ParseLoopDo();
    }
    else
    {
        ThrowSyntaxError( "Missing for or do keyword" );
    }
}

Unique<Syntax> LispyParser::ParseLoopFor()
{
    auto forStmt = Make<ForStatement>();

    ScanToken();

    forStmt->IndexName = ScanSymbol();

    ScanSymbol( "from" );

    forStmt->First = ParseExpression();

    AssertToken( TokenCode::Symbol );
    if ( mCurString != "above"
        && mCurString != "below"
        && mCurString != "downto"
        && mCurString != "to" )
        ThrowSyntaxError( "Expected symbol: above, below, downto, to" );

    forStmt->Comparison = ScanSymbol();

    forStmt->Last = ParseExpression();

    AssertToken( TokenCode::Symbol );
    if ( mCurString == "by" )
    {
        ScanToken();
        forStmt->Step = ParseExpression();
    }

    ScanSymbol( "do" );

    ParseImplicitProgn( forStmt->Body );

    ScanRParen();

    return forStmt;
}

Unique<Syntax> LispyParser::ParseLoopDo()
{
    auto loopStmt = Make<LoopStatement>();

    ScanToken();

    while ( mCurToken != TokenCode::RParen )
    {
        if ( mCurToken == TokenCode::Symbol && mCurString == "while" )
        {
            ScanToken();
            loopStmt->Condition = ParseExpression();
            AssertToken( TokenCode::RParen );
        }
        else
        {
            loopStmt->Body.Statements.push_back( ParseExpression() );
        }
    }

    ScanToken();

    return loopStmt;
}

Unique<Syntax> LispyParser::ParseDo()
{
    auto whileStmt = Make<WhileStatement>();

    ScanToken();

    whileStmt->Condition = ParseExpression();

    ParseImplicitProgn( whileStmt->Body );

    ScanRParen();

    return whileStmt;
}

Unique<Syntax> LispyParser::ParseBreak()
{
    auto breakStmt = Make<BreakStatement>();

    ScanToken();
    ScanRParen();

    return breakStmt;
}

Unique<Syntax> LispyParser::ParseNext()
{
    auto nextStmt = Make<NextStatement>();

    ScanToken();
    ScanRParen();

    return nextStmt;
}

Unique<Syntax> LispyParser::ParseCase()
{
    auto caseExpr = Make<CaseExpr>();

    ScanToken();

    caseExpr->TestKey = ParseExpression();

    while ( mCurToken != TokenCode::RParen )
    {
        ScanLParen();

        if ( mCurToken == TokenCode::Symbol
            && (mCurString == "otherwise" || mCurString == "true") )
        {
            auto caseElse = Make<CaseElse>();

            ScanToken();
            ParseImplicitProgn( caseElse->Body );
            ScanRParen();

            caseExpr->Fallback = std::move( caseElse );
        }
        else
        {
            caseExpr->Clauses.push_back( ParseCaseWhen() );
        }
    }

    ScanRParen();

    return caseExpr;
}

Unique<CaseWhen> LispyParser::ParseCaseWhen()
{
    auto caseWhen = Make<CaseWhen>();

    if ( mCurToken == TokenCode::LParen )
    {
        ScanToken();

        do
        {
            caseWhen->Keys.push_back( ParseExpression() );
        } while ( mCurToken != TokenCode::RParen );

        ScanToken();
    }
    else
    {
        caseWhen->Keys.push_back( ParseExpression() );
    }

    ParseImplicitProgn( caseWhen->Body );
    ScanRParen();

    return caseWhen;
}

Unique<Syntax> LispyParser::ParseProgn()
{
    auto node = Make<LetStatement>();

    ScanToken();

    ParseImplicitProgn( node->Body );

    ScanRParen();

    return node;
}

void LispyParser::ParseImplicitProgn( StatementList& container )
{
    container.Line = mTokLine;
    container.Column = mTokCol;

    while ( mCurToken != TokenCode::RParen )
    {
        container.Statements.push_back( ParseExpression() );
    }
}

template <typename T, typename... Args>
Unique<T> LispyParser::Make( Args&&... args )
{
    T* syntax = new T( std::forward<Args>( args )... );
    syntax->Line = mTokLine;
    syntax->Column = mTokCol;
    syntax->FileName = mUnitFileName;
    return Unique<T>( syntax );
}

void LispyParser::ThrowSyntaxError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    mRep.ThrowError( CERR_SYNTAX, mUnitFileName, mTokLine, mTokCol, format, args );
    va_end( args );
}


#if 0
void GenerateIdCharTable()
{
    static const char sExtAlphaChars[] =
    {
        '_', '#', '@', '!',
        '$', '%', '&', '*',
        '+', '-', '.', '/',
        ':', '<', '=', '>',
        '?', '^', '~',
    };

    constexpr unsigned int TableSize = 128;
    bool table[TableSize] = { false };

    for ( int i = 0; i < _countof( sExtAlphaChars ); i++ )
    {
        table[sExtAlphaChars[i]] = true;
    }

    for ( unsigned int i = 0; i < TableSize; i++ )
    {
        printf( "%d, ", (bool) (table[i] || isalpha( i )) );

        if ( (i % 16) == 15 )
            printf( "\n" );
    }
}
#endif
