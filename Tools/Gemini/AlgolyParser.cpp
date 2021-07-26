// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "pch.h"
#include "AlgolyParser.h"
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdexcept>


static const char* gTokenNames[] =
{
    "<Bof>",
    "<Eof>",
    "<Eol>",
    ";",
    "<Number>",
    "<Symbol>",
    "(",
    ")",
    ",",
    "->",
    ".",
    "&",
    "@",
    "[",
    "]",
    "{",
    "}",
    ":=",
    ":",
    "..",
    "...",
    "+",
    "-",
    "*",
    "/",
    "%",
    "=",
    "<>",
    "<",
    "<=",
    ">",
    ">=",
    "Above",
    "And",
    "As",
    "Below",
    "Break",
    "By",
    "Case",
    "Const",
    "countof",
    "Def",
    "Do",
    "Downto",
    "Else",
    "Elsif",
    "End",
    "enum"
    "For",
    "If",
    "Import",
    "Lambda",
    "Loop",
    "Native",
    "Next",
    "Not",
    "of",
    "Or",
    "Proc",
    "record",
    "Return",
    "Then",
    "To",
    "type",
    "Var",
    "When",
    "While",
    "yield",
};


namespace Gemini
{

AlgolyParser::AlgolyParser( const char* codeText, size_t codeTextLen, const char* fileName, ICompilerLog* log ) :
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
    if ( codeText == nullptr )
        throw std::invalid_argument( "codeText" );

    if ( fileName == nullptr )
        throw std::invalid_argument( "fileName" );

    if ( log == nullptr )
        throw std::invalid_argument( "log" );

    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCurChar = *mCodeTextPtr;
    }
}

int AlgolyParser::GetColumn()
{
    auto diff = mCodeTextPtr - mLineStart;

    if ( diff >= INT_MAX )
        diff = INT_MAX - 1;

    return static_cast<int>(diff) + 1;
}

char AlgolyParser::PeekChar() const
{
    return mCurChar;
}

char AlgolyParser::PeekChar( int index ) const
{
    if ( (mCodeTextEnd - mCodeTextPtr) < (index + 1) )
        return 0;

    return mCodeTextPtr[index];
}

void AlgolyParser::NextChar()
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

void AlgolyParser::CollectChar()
{
    mCurString.append( 1, PeekChar() );
    NextChar();
}

AlgolyParser::TokenCode AlgolyParser::ScanToken()
{
    SkipWhitespace();

    mCurString.clear();
    mCurNumber = 0;
    mTokLine = mLine;
    mTokCol = GetColumn();

    if ( isdigit( PeekChar() ) )
    {
        ReadNumber();
        return mCurToken;
    }
    else if ( IsIdentifierInitial( PeekChar() ) )
    {
        ReadSymbolOrKeyword();
        return mCurToken;
    }

    switch ( PeekChar() )
    {
    case 0:
        mCurToken = TokenCode::Eof;
        break;

    case '\r':
    case '\n':
        mCurToken = TokenCode::Eol;
        ReadLineEnding();
        break;

    case ';':
        NextChar();
        mCurToken = TokenCode::Separator;
        break;

    case ':':
        NextChar();

        if ( PeekChar() == '=' )
        {
            NextChar();
            mCurToken = TokenCode::Assign;
        }
        else
        {
            mCurToken = TokenCode::Colon;
        }
        break;

    case '(':
        NextChar();
        mCurToken = TokenCode::LParen;
        break;

    case ')':
        NextChar();
        mCurToken = TokenCode::RParen;
        break;

    case ',':
        NextChar();
        mCurToken = TokenCode::Comma;
        break;

    case '&':
        NextChar();
        mCurToken = TokenCode::Ampersand;
        break;

    case '@':
        NextChar();
        mCurToken = TokenCode::At;
        break;

    case '[':
        NextChar();
        mCurToken = TokenCode::LBracket;
        break;

    case ']':
        NextChar();
        mCurToken = TokenCode::RBracket;
        break;

    case '{':
        NextChar();
        mCurToken = TokenCode::LBrace;
        break;

    case '}':
        NextChar();
        mCurToken = TokenCode::RBrace;
        break;

    case '.':
        NextChar();

        if ( PeekChar() == '.' && PeekChar( 1 ) == '.' )
        {
            NextChar();
            NextChar();
            mCurToken = TokenCode::Ellipsis;
        }
        else if ( PeekChar() == '.' )
        {
            NextChar();
            mCurToken = TokenCode::DotDot;
        }
        else
        {
            mCurToken = TokenCode::Dot;
        }
        break;

    case '+':
        CollectChar();
        mCurToken = TokenCode::Plus;
        break;

    case '-':
        CollectChar();

        if ( PeekChar() == '>' )
        {
            NextChar();
            mCurToken = TokenCode::RArrow;
        }
        else
        {
            mCurToken = TokenCode::Minus;
        }
        break;

    case '*':
        CollectChar();
        mCurToken = TokenCode::Star;
        break;

    case '/':
        CollectChar();
        mCurToken = TokenCode::Slash;
        break;

    case '%':
        CollectChar();
        mCurToken = TokenCode::Percent;
        break;

    case '=':
        CollectChar();
        mCurToken = TokenCode::EQ;
        break;

    case '<':
        CollectChar();

        if ( PeekChar() == '>' )
        {
            CollectChar();
            mCurToken = TokenCode::NE;
        }
        else if ( PeekChar() == '=' )
        {
            CollectChar();
            mCurToken = TokenCode::LE;
        }
        else
        {
            mCurToken = TokenCode::LT;
        }
        break;

    case '>':
        CollectChar();

        if ( PeekChar() == '=' )
        {
            CollectChar();
            mCurToken = TokenCode::GE;
        }
        else
        {
            mCurToken = TokenCode::GT;
        }
        break;

    default:
        ThrowSyntaxError( "Bad character: U+%02X", PeekChar() );
        break;
    }

    return mCurToken;
}

void AlgolyParser::ScanToken( TokenCode code )
{
    AssertToken( code );
    ScanToken();
}

void AlgolyParser::AssertToken( TokenCode code )
{
    if ( mCurToken != code )
        ThrowSyntaxError( "Expected token: %s", gTokenNames[(int) code] );
}

void AlgolyParser::ReadLineEnding()
{
    int c = PeekChar();

    NextChar();

    if ( c == '\r' && PeekChar() == '\n' )
        NextChar();

    mLine++;
    mLineStart = mCodeTextPtr;
}

void AlgolyParser::SkipWhitespace()
{
    while ( true )
    {
        if ( PeekChar() == '\\' )
        {
            NextChar();

            if ( PeekChar() != '\r' && PeekChar() != '\n' )
                ThrowSyntaxError( "Expected line ending after \\" );

            ReadLineEnding();
        }

        while ( isspace( PeekChar() )
            && PeekChar() != '\r'
            && PeekChar() != '\n' )
        {
            NextChar();
        }

        if ( PeekChar() != '#' )
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

void AlgolyParser::SkipLineEndings()
{
    while ( mCurToken == TokenCode::Eol )
    {
        ScanToken();
    }
}

void AlgolyParser::SkipLineSeparators()
{
    while ( mCurToken == TokenCode::Eol || mCurToken == TokenCode::Separator )
    {
        ScanToken();
    }
}

bool AlgolyParser::IsIdentifierInitial( int c )
{
    return isalpha( c )
        || (c == '_');
}

bool AlgolyParser::IsIdentifierCoda( int c )
{
    return IsIdentifierInitial( c ) || isdigit( c );
}

void AlgolyParser::ReadNumber()
{
    while ( isdigit( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    if ( IsIdentifierInitial( PeekChar() ) )
        ThrowSyntaxError( "syntax error : bad number" );

    unsigned long value = strtoul( mCurString.c_str(), NULL, 10 );

    // Leave INT32_MAX+1 in range for now, so it can be negated

    if ( value == ULONG_MAX && errno == ERANGE
        || value > (uint32_t) INT32_MAX + 1 )
        ThrowSyntaxError( "Number out of range" );

    mCurNumber = value;

    mCurToken = TokenCode::Number;
}

void AlgolyParser::ReadSymbolOrKeyword()
{
    static std::map<std::string, TokenCode> sStrToKeyword
    {
        { "above",  TokenCode::Above },
        { "and",    TokenCode::And },
        { "as",     TokenCode::As },
        { "below",  TokenCode::Below },
        { "break",  TokenCode::Break },
        { "by",     TokenCode::By },
        { "case",   TokenCode::Case },
        { "const",  TokenCode::Const },
        { "countof",TokenCode::Countof },
        { "def",    TokenCode::Def },
        { "do",     TokenCode::Do },
        { "downto", TokenCode::Downto },
        { "else",   TokenCode::Else },
        { "elsif",  TokenCode::Elsif },
        { "end",    TokenCode::End },
        { "enum",   TokenCode::Enum },
        { "for",    TokenCode::For },
        { "if",     TokenCode::If },
        { "import", TokenCode::Import },
        { "lambda", TokenCode::Lambda },
        { "loop",   TokenCode::Loop },
        { "native", TokenCode::Native },
        { "next",   TokenCode::Next },
        { "not",    TokenCode::Not },
        { "of",     TokenCode::Of },
        { "or",     TokenCode::Or },
        { "proc",   TokenCode::Proc },
        { "record", TokenCode::Record },
        { "return", TokenCode::Return },
        { "then",   TokenCode::Then },
        { "to",     TokenCode::To },
        { "type",   TokenCode::Type },
        { "var",    TokenCode::Var },
        { "when",   TokenCode::When },
        { "while",  TokenCode::While },
        { "yield",  TokenCode::Yield },
    };

    while ( IsIdentifierCoda( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    if ( auto it = sStrToKeyword.find( mCurString );
        it != sStrToKeyword.end() )
    {
        mCurToken = it->second;
    }
    else
    {
        mCurToken = TokenCode::Symbol;
    }
}

bool AlgolyParser::IsSeparatorKeyword( TokenCode tokenCode )
{
    return tokenCode == TokenCode::End
        || tokenCode == TokenCode::Else
        || tokenCode == TokenCode::Elsif
        || tokenCode == TokenCode::When
        || tokenCode == TokenCode::Do
        ;
}

bool AlgolyParser::IsStatementSeparator( TokenCode tokenCode )
{
    return tokenCode == TokenCode::Eol
        || tokenCode == TokenCode::Separator
        || IsSeparatorKeyword( tokenCode )
        ;
}

Unique<Unit> AlgolyParser::Parse()
{
    Unique<Unit> unit = Make<Unit>( mFileName );

    mUnitFileName = unit->GetUnitFileName();

    ScanToken();

    while ( mCurToken != TokenCode::Eof )
    {
        switch ( mCurToken )
        {
        case TokenCode::Def:
            unit->FuncDeclarations.push_back( ParseFunction() );
            break;

        case TokenCode::Native:
            unit->DataDeclarations.push_back( ParseNative() );
            break;

        case TokenCode::Var:
        case TokenCode::Const:
            ParseGlobalVars( unit.get() );
            break;

        case TokenCode::Import:
            unit->DataDeclarations.push_back( ParseImport() );
            break;

        case TokenCode::Type:
            ParseTypeDecls( unit.get() );
            break;

        default:
            ThrowSyntaxError( "syntax error : expected function" );
            break;
        }

        SkipLineSeparators();
    }

    return unit;
}

Unique<ImportDecl> AlgolyParser::ParseImport()
{
    Unique<ImportDecl> import( Make<ImportDecl>() );

    ScanToken();

    import->OriginalName = ParseRawSymbol();

    if ( mCurToken == TokenCode::As )
    {
        ScanToken();

        import->Name = ParseRawSymbol();
    }
    else
    {
        import->Name = import->OriginalName;
    }

    return import;
}

Unique<ProcDecl> AlgolyParser::ParseFunction()
{
    auto proc = ParseProc( true );

    SkipLineEndings();

    return proc;
}

Unique<LambdaExpr> AlgolyParser::ParseLambda()
{
    Unique<LambdaExpr> lambda( Make<LambdaExpr>() );

    lambda->Proc = ParseProc( false );

    return lambda;
}

Unique<NativeDecl> AlgolyParser::ParseNative()
{
    Unique<NativeDecl> native( Make<NativeDecl>() );

    ScanToken();

    native->Name = ParseRawSymbol();

    if ( mCurToken == TokenCode::LParen )
    {
        native->Params = ParseParamList();
    }

    if ( mCurToken == TokenCode::EQ )
    {
        ScanToken();

        if ( mCurToken == TokenCode::Number )
        {
            auto numberExpr = ParseNumber();

            if ( numberExpr->Value > INT32_MAX )
                ThrowSyntaxError( "Native ID is out of range" );

            native->OptionalId = std::move( numberExpr );
        }
        else if ( mCurToken == TokenCode::Symbol )
        {
            native->OptionalId = ParseSymbol();
        }
        else
        {
            ThrowSyntaxError( "Only a number or name can be used as a native ID" );
        }
    }

    return native;
}

Unique<ProcDecl> AlgolyParser::ParseProc( bool hasName )
{
    Unique<ProcDecl> proc( Make<ProcDecl>() );

    ScanToken();

    if ( hasName )
        proc->Name = ParseRawSymbol();

    if ( mCurToken == TokenCode::LParen )
    {
        proc->Params = ParseParamList();
    }

    if ( mCurToken == TokenCode::RArrow )
    {
        ScanToken();

        proc->ReturnTypeRef = ParseTypeRef();
    }

    SkipLineSeparators();

    ParseStatements( proc->Body );

    // Read past "end"
    ScanToken( TokenCode::End );

    return proc;
}

std::vector<Unique<DataDecl>> AlgolyParser::ParseParamList()
{
    std::vector<Unique<DataDecl>> paramList;

    // Read past left parenthesis
    ScanToken();
    SkipLineEndings();

    bool first = true;

    while ( mCurToken != TokenCode::RParen )
    {
        if ( !first )
        {
            if ( mCurToken != TokenCode::Comma )
                ThrowSyntaxError( "Expected , or )" );

            ScanToken();
            SkipLineEndings();
        }

        first = false;

        paramList.push_back( ParseParameter() );

        SkipLineEndings();
    }

    // Read past right parenthesis
    ScanToken();

    return paramList;
}

Unique<DataDecl> AlgolyParser::ParseParameter()
{
    auto paramDecl = Make<ParamDecl>();

    if ( mCurToken == TokenCode::Var )
    {
        ScanToken();
        paramDecl->Modifier = ParamModifier::Var;
    }
    else if ( mCurToken == TokenCode::Const )
    {
        ScanToken();
        paramDecl->Modifier = ParamModifier::Const;
    }

    return ParseVar( std::move( paramDecl ), std::nullopt );
}

void AlgolyParser::ParseStatements( StatementList& container )
{
    while ( !IsSeparatorKeyword( mCurToken ) )
    {
        container.Statements.push_back( ParseStatement() );
    }

    // Use the token after the statement list as a reference point

    container.Line = mTokLine;
    container.Column = mTokCol;
    container.FileName = mUnitFileName;
}

Unique<Syntax> AlgolyParser::ParseStatement()
{
    Unique<Syntax> elem;

    switch ( mCurToken )
    {
    case TokenCode::Const:
    case TokenCode::Var:
        elem = ParseLet();
        break;

    case TokenCode::Return:
        elem = ParseReturn();
        break;

    case TokenCode::For:
        elem = ParseFor();
        break;

    case TokenCode::Loop:
        elem = ParseLoop();
        break;

    case TokenCode::While:
        elem = ParseWhile();
        break;

    case TokenCode::Break:
        elem = ParseBreak();
        break;

    case TokenCode::Next:
        elem = ParseNext();
        break;

    case TokenCode::Yield:
        elem = ParseYield();
        break;

    default:
        elem = ParseExprStatement();
        break;
    }

    SkipLineSeparators();

    return elem;
}

Unique<Syntax> AlgolyParser::ParseExprStatement()
{
    Unique<Syntax> expr( ParseExpr() );

    if ( !IsStatementSeparator( mCurToken ) )
    {
        if ( expr->Kind == SyntaxKind::Name || expr->Kind == SyntaxKind::DotExpr )
        {
            // The parsed expression was only a symbol. But there are more tokens.
            // So, parse them into arguments for a call without parentheses.

            expr = ParseCall( std::move( expr ), false, false );
        }
        else
        {
            ThrowSyntaxError( "Expected end of statement" );
        }
    }
    else
    {
        if ( expr->Kind == SyntaxKind::Name
            || expr->Kind == SyntaxKind::DotExpr )
        {
            // The whole statement was a symbol. It can be a variable or a call without
            // parentheses and arguments. Use eval* to disambiguate them.

            Unique<CallOrSymbolExpr> eval = Make<CallOrSymbolExpr>();

            eval->Symbol = std::move( expr );
            expr = std::move( eval );
        }
    }

    return expr;
}

Unique<Syntax> AlgolyParser::ParseExpr()
{
    switch ( mCurToken )
    {
    case TokenCode::Lambda:
        return ParseLambda();

    case TokenCode::If:
        return ParseIf();

    case TokenCode::Case:
        return ParseCase();

    default:
        return ParseAssignment();
    }
}

Unique<Syntax> AlgolyParser::ParseAssignment()
{
    Unique<Syntax> first = ParseBinary( 0 );

    if ( mCurToken == TokenCode::Assign )
    {
        auto assignment = Make<AssignmentExpr>();

        assignment->Left = std::move( first );

        // Read past assignment operator
        ScanToken();
        SkipLineEndings();

        assignment->Right = ParseExpr();

        return assignment;
    }

    return first;
}

const AlgolyParser::TestOpFunc AlgolyParser::sTestOpFuncs[] =
{
    &AlgolyParser::IsTokenOrOp,
    &AlgolyParser::IsTokenAndOp,
    &AlgolyParser::IsTokenEqualityOp,
    &AlgolyParser::IsTokenComparisonOp,
    &AlgolyParser::IsTokenAdditiveOp,
    &AlgolyParser::IsTokenMultiplicativeOp,
};

bool AlgolyParser::IsTokenOrOp()
{
    return mCurToken == TokenCode::Or;
}

bool AlgolyParser::IsTokenAndOp()
{
    return mCurToken == TokenCode::And;
}

bool AlgolyParser::IsTokenEqualityOp()
{
    return mCurToken == TokenCode::EQ
        || mCurToken == TokenCode::NE;
}

bool AlgolyParser::IsTokenComparisonOp()
{
    return mCurToken == TokenCode::LT
        || mCurToken == TokenCode::LE
        || mCurToken == TokenCode::GT
        || mCurToken == TokenCode::GE;
}

bool AlgolyParser::IsTokenAdditiveOp()
{
    return mCurToken == TokenCode::Plus
        || mCurToken == TokenCode::Minus;
}

bool AlgolyParser::IsTokenMultiplicativeOp()
{
    return mCurToken == TokenCode::Star
        || mCurToken == TokenCode::Slash
        || mCurToken == TokenCode::Percent;
}

Unique<Syntax> AlgolyParser::ParseBinaryPart( int level )
{
    if ( level + 1 >= static_cast<int>( std::size( AlgolyParser::sTestOpFuncs ) ) )
        return ParseAsExpr();
    else
        return ParseBinary( level + 1 );
}

Unique<Syntax> AlgolyParser::ParseBinary( int level )
{
    Unique<Syntax> first( ParseBinaryPart( level ) );
    TestOpFunc testOpFunc = sTestOpFuncs[level];

    // Left-associative

    while ( (this->*testOpFunc)() )
    {
        auto binary = Make<BinaryExpr>();

        binary->Op = ParseAsRawSymbol();

        SkipLineEndings();

        binary->Left = std::move( first );
        binary->Right = ParseBinaryPart( level );

        if ( testOpFunc == &AlgolyParser::IsTokenComparisonOp && IsTokenComparisonOp() )
            ThrowSyntaxError( "Comparisons are binary only" );

        first = std::move( binary );
    }

    return first;
}

Unique<Syntax> AlgolyParser::ParseAsExpr()
{
    Unique<Syntax> first( ParseUnary() );

    // Left-associative

    while ( mCurToken == TokenCode::As )
    {
        auto asExpr = Make<AsExpr>();

        ScanToken();
        SkipLineEndings();

        asExpr->Inner = std::move( first );
        asExpr->TargetTypeRef = ParseNameTypeRef();

        first = std::move( asExpr );
    }

    return first;
}

Unique<Syntax> AlgolyParser::ParseUnary()
{
    if ( mCurToken == TokenCode::Minus
        || mCurToken == TokenCode::Not )
    {
        auto unary = Make<UnaryExpr>();

        unary->Op = ParseAsRawSymbol();
        unary->Inner = ParseUnary();

        return unary;
    }
    else if ( mCurToken == TokenCode::Ampersand
        || mCurToken == TokenCode::At )
    {
        auto addrOf = Make<AddrOfExpr>();

        ScanToken();

        addrOf->Inner = ParseSingle();

        return addrOf;
    }

    return ParseSingle();
}

Unique<Syntax> AlgolyParser::ParseSingle()
{
    Unique<Syntax> elem;
    bool indirect = false;

    // TODO: Or should we use a different syntax for indirect calls?

    switch ( mCurToken )
    {
    case TokenCode::LParen:
        ScanToken();
        SkipLineEndings();
        elem = ParseExpr();
        ScanToken( TokenCode::RParen );
        indirect = true;
        break;

    case TokenCode::Symbol:
        elem = ParseSymbol();
        break;

    case TokenCode::Number:
        elem = ParseNumber();
        break;

    case TokenCode::Countof:
        elem = ParseCountof();
        break;

    default:
        ThrowSyntaxError( "Expected expression" );
    }

    if ( mCurToken == TokenCode::LBracket
        || mCurToken == TokenCode::Dot )
    {
        elem = ParseIndexingOrDot( std::move( elem ) );
    }

    if ( mCurToken == TokenCode::LParen )
    {
        return ParseCall( std::move( elem ), indirect );
    }

    return elem;
}

Unique<Syntax> AlgolyParser::ParseCall( Unique<Syntax>&& head, bool indirect, bool parens )
{
    auto call = Make<CallExpr>();

    call->IsIndirect = indirect;
    call->Head = std::move( head );

    if ( parens )
    {
        // Read past left parenthesis
        ScanToken();
        SkipLineEndings();
    }

    bool first = true;

    while ( (parens && mCurToken != TokenCode::RParen)
        || (!parens && !IsStatementSeparator( mCurToken )) )
    {
        if ( !first )
        {
            if ( mCurToken != TokenCode::Comma )
                ThrowSyntaxError( "Expected , or )" );

            ScanToken();
            SkipLineEndings();
        }

        first = false;

        call->Arguments.push_back( ParseExpr() );

        if ( parens )
            SkipLineEndings();
    }

    if ( parens )
    {
        // Read past right parenthesis
        ScanToken();
    }

    return call;
}

Unique<Syntax> AlgolyParser::ParseIndexing( Unique<Syntax>&& head )
{
    // '[' index ']' | '[' [first] .. [last] ']'

    Unique<Syntax> expr;
    Unique<Syntax> index;

    ScanToken();

    if ( mCurToken != TokenCode::DotDot )
        index = ParseExpr();

    if ( mCurToken == TokenCode::DotDot )
    {
        auto slice = Make<SliceExpr>();

        ScanToken();

        if ( !index )
            index = MakeNumber( 0 );

        slice->Head = std::move( head );
        slice->FirstIndex = std::move( index );

        if ( mCurToken != TokenCode::RBracket )
            slice->LastIndex = ParseExpr();
        else
            slice->LastIndex = MakeNumber( -1 );

        expr = std::move( slice );
    }
    else
    {
        auto indexing = Make<IndexExpr>();

        indexing->Head = std::move( head );
        indexing->Index = std::move( index );

        expr = std::move( indexing );
    }

    ScanToken( TokenCode::RBracket );

    return expr;
}

Unique<Syntax> AlgolyParser::ParseDotExpr( Unique<Syntax>&& head )
{
    auto dotExpr = Make<DotExpr>();

    dotExpr->Head = std::move( head );

    ScanToken();

    dotExpr->Member = ParseRawSymbol();

    return dotExpr;
}

Unique<Syntax> AlgolyParser::ParseIndexingOrDot( Unique<Syntax>&& head )
{
    auto expr = std::move( head );

    while ( true )
    {
        if ( mCurToken == TokenCode::Dot )
        {
            expr = ParseDotExpr( std::move( expr ) );
        }
        else if ( mCurToken == TokenCode::LBracket )
        {
            expr = ParseIndexing( std::move( expr ) );
        }
        else
        {
            break;
        }
    }

    return expr;
}

Unique<Syntax> AlgolyParser::ParseQualifiedName()
{
    auto name = ParseSymbol();

    if ( mCurToken == TokenCode::Dot )
    {
        return ParseDotExpr( std::move( name ) );
    }

    return name;
}

Unique<Syntax> AlgolyParser::ParseCountof()
{
    auto countofExpr = Make<CountofExpr>();

    ScanToken();
    ScanToken( TokenCode::LParen );

    countofExpr->Expr = ParseExpr();

    ScanToken( TokenCode::RParen );

    return countofExpr;
}

Unique<Syntax> AlgolyParser::ParseLet()
{
    auto letNode = Make<LetStatement>();

    do
    {
        TokenCode keyword = mCurToken;

        ScanToken();
        SkipLineEndings();

        bool first = true;

        do
        {
            if ( !first )
            {
                if ( mCurToken != TokenCode::Comma )
                    ThrowSyntaxError( "Expected , or line separator" );

                ScanToken();
                SkipLineEndings();
            }

            first = false;

            Unique<DataDecl> varDecl;

            if ( keyword == TokenCode::Var )
                varDecl = ParseVarDecl();
            else
                varDecl = ParseConstDecl();

            letNode->Variables.push_back( std::move( varDecl ) );

        } while ( mCurToken != TokenCode::Eol
            && mCurToken != TokenCode::Separator
            && mCurToken != TokenCode::End );

        SkipLineSeparators();

    } while ( mCurToken == TokenCode::Var || mCurToken == TokenCode::Const );

    while ( mCurToken != TokenCode::End )
    {
        letNode->Body.Statements.push_back( ParseStatement() );
    }

    return letNode;
}

void AlgolyParser::ParseGlobalVars( Unit* unit )
{
    TokenCode declToken = mCurToken;

    ScanToken();
    SkipLineEndings();

    bool first = true;

    do
    {
        if ( !first )
        {
            ScanToken( TokenCode::Comma );
            SkipLineEndings();
        }

        first = false;

        if ( declToken == TokenCode::Var )
        {
            unit->DataDeclarations.push_back( ParseVarDecl() );
        }
        else if ( declToken == TokenCode::Const )
        {
            unit->DataDeclarations.push_back( ParseConstDecl() );
        }
        else
        {
            THROW_INTERNAL_ERROR( "" );
        }

    } while ( mCurToken != TokenCode::Eol
        && mCurToken != TokenCode::Separator
        && mCurToken != TokenCode::Eof );

    SkipLineSeparators();
}

Unique<DataDecl> AlgolyParser::ParseVar( Unique<DataDecl>&& varDecl, std::optional<TokenCode> assignToken )
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected variable name" );

    varDecl->Name = ParseRawSymbol();

    if ( mCurToken == TokenCode::Colon )
    {
        ScanToken();

        varDecl->TypeRef = ParseTypeRef();
    }

    if ( assignToken.has_value() )
    {
        ScanToken( assignToken.value() );
        SkipLineEndings();

        varDecl->Initializer = ParseInitExpr();
    }

    return std::move( varDecl );
}

Unique<DataDecl> AlgolyParser::ParseVarDecl()
{
    return ParseVar( Make<VarDecl>(), TokenCode::Assign );
}

Unique<DataDecl> AlgolyParser::ParseConstDecl()
{
    return ParseVar( Make<ConstDecl>(), TokenCode::EQ );
}

Unique<DataDecl> AlgolyParser::ParseNameDecl()
{
    auto varDecl = Make<VarDecl>();

    varDecl->Name = ParseRawSymbol();

    return varDecl;
}

Unique<DeclSyntax> AlgolyParser::ParseTypeDecl()
{
    auto typeDecl = Make<TypeDecl>();

    typeDecl->Name = ParseRawSymbol();

    ScanToken( TokenCode::EQ );

    typeDecl->TypeRef = ParseTypeDef();

    return typeDecl;
}

void AlgolyParser::ParseTypeDecls( Unit* unit )
{
    ScanToken();
    SkipLineEndings();

    bool first = true;

    do
    {
        if ( !first )
        {
            ScanToken( TokenCode::Comma );
            SkipLineEndings();
        }

        first = false;

        unit->DataDeclarations.push_back( ParseTypeDecl() );

    } while ( mCurToken != TokenCode::Eol
        && mCurToken != TokenCode::Separator
        && mCurToken != TokenCode::Eof );

    SkipLineSeparators();
}

Unique<TypeRef> AlgolyParser::ParseTypeDef()
{
    switch ( mCurToken )
    {
    case TokenCode::Record: return ParseRecordTypeDef();
    case TokenCode::Enum:   return ParseEnumTypeDef();
    default:
        return ParseTypeRef();
    }
}

Unique<TypeRef> AlgolyParser::ParseTypeRef()
{
    switch ( mCurToken )
    {
    case TokenCode::LBracket:   return ParseArrayTypeRef();
    case TokenCode::At:
    case TokenCode::Ampersand:  return ParsePtrFuncTypeRef();
    case TokenCode::Symbol:     return ParseNameTypeRef();
    default:
        ThrowSyntaxError( "Expected type denoter" );
    }
}

Unique<TypeRef> AlgolyParser::ParseRecordTypeDef()
{
    auto recordTypeRef = Make<RecordTypeRef>();

    ScanToken();
    SkipLineEndings();

    while ( mCurToken != TokenCode::End )
    {
        recordTypeRef->Fields.push_back( ParseVar( Make<FieldDecl>(), std::nullopt ) );

        SkipLineEndings();

        if ( mCurToken == TokenCode::Comma )
        {
            ScanToken();
            SkipLineEndings();
        }
    }

    ScanToken();

    return recordTypeRef;
}

Unique<TypeRef> AlgolyParser::ParseEnumTypeDef()
{
    auto enumTypeRef = Make<EnumTypeRef>();

    ScanToken();
    SkipLineEndings();

    while ( mCurToken != TokenCode::End )
    {
        auto member = Make<EnumMemberDef>();

        member->Name = ParseRawSymbol();

        SkipLineEndings();

        if ( mCurToken == TokenCode::EQ )
        {
            ScanToken();
            SkipLineEndings();

            member->Initializer = ParseExpr();
        }

        enumTypeRef->Members.push_back( std::move( member ) );

        if ( mCurToken == TokenCode::Comma )
        {
            ScanToken();
            SkipLineEndings();
        }
    }

    ScanToken();

    return enumTypeRef;
}

Unique<TypeRef> AlgolyParser::ParseNameTypeRef()
{
    auto nameTypeRef = Make<NameTypeRef>();

    nameTypeRef->QualifiedName = ParseQualifiedName();

    return nameTypeRef;
}

Unique<TypeRef> AlgolyParser::ParsePtrFuncTypeRef()
{
    auto procTypeRef = Make<ProcTypeRef>();

    ScanToken();
    ScanToken( TokenCode::Proc );

    if ( mCurToken == TokenCode::LParen )
    {
        ScanToken();

        bool first = true;

        while ( mCurToken != TokenCode::RParen )
        {
            if ( !first )
            {
                ScanToken( TokenCode::Comma );
                SkipLineEndings();
            }

            first = false;

            procTypeRef->Params.push_back( ParseAnonymousParameter() );

            SkipLineEndings();
        }

        ScanToken();
    }

    if ( mCurToken == TokenCode::RArrow )
    {
        ScanToken();

        procTypeRef->ReturnTypeRef = ParseTypeRef();
    }

    Unique<PointerTypeRef> pointerTypeRef( new PointerTypeRef() );

    pointerTypeRef->Target = std::move( procTypeRef );

    return pointerTypeRef;
}

ParamSpecRef AlgolyParser::ParseAnonymousParameter()
{
    ParamSpecRef param;

    if ( mCurToken == TokenCode::Var )
    {
        ScanToken();
        param.Modifier = ParamModifier::Var;
    }
    else if ( mCurToken == TokenCode::Const )
    {
        ScanToken();
        param.Modifier = ParamModifier::Const;
    }

    param.TypeRef = ParseTypeRef();

    return param;
}

Unique<TypeRef> AlgolyParser::ParseArrayTypeRef()
{
    auto arrayTypeRef = Make<ArrayTypeRef>();

    ScanToken( TokenCode::LBracket );

    if ( mCurToken != TokenCode::RBracket )
    {
        arrayTypeRef->SizeExpr = ParseExpr();
    }

    ScanToken( TokenCode::RBracket );

    if ( mCurToken == TokenCode::Of )
    {
        ScanToken();

        arrayTypeRef->ElementTypeRef = ParseTypeRef();
    }

    return arrayTypeRef;
}

Unique<Syntax> AlgolyParser::ParseArrayInitializer()
{
    ScanToken( TokenCode::LBracket );
    SkipLineEndings();

    auto initList = Make<InitList>();
    bool first = true;

    while ( mCurToken != TokenCode::RBracket )
    {
        if ( mCurToken == TokenCode::Ellipsis )
        {
            ScanToken();

            if ( mCurToken == TokenCode::Plus )
            {
                ScanToken();

                initList->Fill = ArrayFill::Extrapolate;
            }
            else
            {
                initList->Fill = ArrayFill::Repeat;
            }

            SkipLineEndings();
            break;
        }
        else if ( !first )
        {
            ScanToken( TokenCode::Comma );
            SkipLineEndings();
        }

        first = false;

        initList->Values.push_back( ParseInitExpr() );

        SkipLineEndings();
    }

    ScanToken( TokenCode::RBracket );

    return initList;
}

Unique<Syntax> AlgolyParser::ParseRecordInitializer()
{
    auto recordInitializer = Make<RecordInitializer>();

    ScanToken();
    SkipLineEndings();

    while ( mCurToken != TokenCode::RBrace )
    {
        auto fieldInit = Make<FieldInitializer>();

        fieldInit->Name = ParseRawSymbol();

        ScanToken( TokenCode::Colon );
        SkipLineEndings();

        fieldInit->Initializer = ParseInitExpr();

        recordInitializer->Fields.push_back( std::move( fieldInit ) );

        if ( mCurToken == TokenCode::Comma )
            ScanToken();

        SkipLineEndings();
    }

    ScanToken();

    return recordInitializer;
}

Unique<Syntax> AlgolyParser::ParseInitExpr()
{
    if ( mCurToken == TokenCode::LBracket )
    {
        return ParseArrayInitializer();
    }
    else if ( mCurToken == TokenCode::LBrace )
    {
        return ParseRecordInitializer();
    }
    else
    {
        return ParseExpr();
    }
}

Unique<Syntax> AlgolyParser::ParseReturn()
{
    auto retStmt = Make<ReturnStatement>();

    ScanToken();

    retStmt->Inner = ParseExpr();

    return retStmt;
}

Unique<Syntax> AlgolyParser::ParseIf()
{
    auto condExpr = Make<CondExpr>();

    condExpr->IsIf = true;
    condExpr->Clauses.push_back( ParseIfClause() );

    while ( mCurToken == TokenCode::Elsif )
    {
        condExpr->Clauses.push_back( ParseIfClause() );
    }

    if ( mCurToken == TokenCode::Else )
    {
        condExpr->Clauses.push_back( ParseElseClause() );
    }

    ScanToken( TokenCode::End );

    return condExpr;
}

Unique<CondClause> AlgolyParser::ParseIfClause()
{
    auto clause = Make<CondClause>();

    ScanToken();

    clause->Condition = ParseExpr();

    if ( mCurToken != TokenCode::Then )
        ThrowSyntaxError( "Expected then" );

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause->Body );

    return clause;
}

Unique<CondClause> AlgolyParser::ParseElseClause()
{
    auto clause = Make<CondClause>();

    ScanToken();
    SkipLineSeparators();

    clause->Condition = MakeNumber( 1 );

    ParseStatements( clause->Body );

    return clause;
}

Unique<Syntax> AlgolyParser::ParseFor()
{
    auto forStmt = Make<ForStatement>();

    ScanToken();

    forStmt->Index = ParseNameDecl();

    ScanToken( TokenCode::Assign );

    forStmt->First = ParseExpr();

    switch ( mCurToken )
    {
    case TokenCode::Above:  forStmt->Comparison = ForComparison::Above;  break;
    case TokenCode::Below:  forStmt->Comparison = ForComparison::Below;  break;
    case TokenCode::Downto: forStmt->Comparison = ForComparison::Downto; break;
    case TokenCode::To:     forStmt->Comparison = ForComparison::To;     break;
    default:
        ThrowSyntaxError( "Expected: above, below, downto, to" );
    }

    ScanToken();

    forStmt->Last = ParseExpr();

    if ( mCurToken == TokenCode::By )
    {
        ScanToken();

        forStmt->Step = ParseExpr();
    }

    ScanToken( TokenCode::Do );
    SkipLineSeparators();

    ParseStatements( forStmt->Body );

    ScanToken( TokenCode::End );

    return forStmt;
}

Unique<Syntax> AlgolyParser::ParseLoop()
{
    auto loopStmt = Make<LoopStatement>();

    ScanToken();
    SkipLineSeparators();

    ParseStatements( loopStmt->Body );

    if ( mCurToken == TokenCode::Do )
    {
        ScanToken();
        ScanToken( TokenCode::While );

        loopStmt->Condition = ParseExpr();
    }

    ScanToken( TokenCode::End );

    return loopStmt;
}

Unique<Syntax> AlgolyParser::ParseWhile()
{
    auto whileStmt = Make<WhileStatement>();

    ScanToken();
    whileStmt->Condition = ParseExpr();
    ScanToken( TokenCode::Do );

    ParseStatements( whileStmt->Body );

    ScanToken( TokenCode::End );

    return whileStmt;
}

Unique<Syntax> AlgolyParser::ParseCase()
{
    auto caseExpr = Make<CaseExpr>();

    ScanToken();

    caseExpr->TestKey = ParseExpr();

    SkipLineSeparators();

    while ( mCurToken == TokenCode::When )
    {
        caseExpr->Clauses.push_back( ParseCaseWhen() );
    }

    if ( mCurToken == TokenCode::Else )
    {
        caseExpr->Fallback = ParseCaseElse();
    }

    ScanToken( TokenCode::End );

    return caseExpr;
}

Unique<CaseWhen> AlgolyParser::ParseCaseWhen()
{
    auto clause = Make<CaseWhen>();

    ScanToken();

    bool first = true;

    while ( true )
    {
        if ( !first )
        {
            ScanToken( TokenCode::Comma );
            SkipLineEndings();
        }

        first = false;

        clause->Keys.push_back( ParseExpr() );

        if ( mCurToken == TokenCode::Then )
            break;
    }

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause->Body );

    return clause;
}

Unique<CaseElse> AlgolyParser::ParseCaseElse()
{
    auto clause = Make<CaseElse>();

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause->Body );

    return clause;
}

Unique<Syntax> AlgolyParser::ParseBreak()
{
    ScanToken();
    return Make<BreakStatement>();
}

Unique<Syntax> AlgolyParser::ParseNext()
{
    ScanToken();
    return Make<NextStatement>();
}

Unique<Syntax> AlgolyParser::ParseYield()
{
    ScanToken();
    return Make<YieldStatement>();
}

Unique<NumberExpr> AlgolyParser::ParseNumber()
{
    if ( mCurToken != TokenCode::Number )
        ThrowSyntaxError( "Expected number" );

    Unique<NumberExpr> elem( WrapNumber() );
    ScanToken();
    return elem;
}

std::string AlgolyParser::ParseRawSymbol()
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected symbol" );

    std::string symbol = std::move( mCurString );
    ScanToken();
    return symbol;
}

Unique<NameExpr> AlgolyParser::ParseSymbol()
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected symbol" );

    Unique<NameExpr> elem( WrapSymbol() );
    ScanToken();
    return elem;
}

std::string AlgolyParser::ParseAsRawSymbol()
{
    if ( mCurString.size() == 0 )
        THROW_INTERNAL_ERROR( "" );

    std::string symbol = std::move( mCurString );
    ScanToken();
    return symbol;
}

Unique<NumberExpr> AlgolyParser::WrapNumber()
{
    return MakeNumber( mCurNumber );
}

Unique<NameExpr> AlgolyParser::WrapSymbol()
{
    return MakeSymbol( mCurString.c_str() );
}

Unique<NumberExpr> AlgolyParser::MakeNumber( int64_t value )
{
    NumberExpr* number = new NumberExpr();
    number->Value = value;
    number->Line = mTokLine;
    number->Column = mTokCol;
    number->FileName = mUnitFileName;
    return Unique<NumberExpr>( number );
}

Unique<NameExpr> AlgolyParser::MakeSymbol( const char* string )
{
    NameExpr* symbol = new NameExpr();
    symbol->String = string;
    symbol->Line = mTokLine;
    symbol->Column = mTokCol;
    symbol->FileName = mUnitFileName;
    return Unique<NameExpr>( symbol );
}

template <typename T, typename... Args>
Unique<T> AlgolyParser::Make( Args&&... args )
{
    T* syntax = new T( std::forward<Args>( args )... );
    syntax->Line = mTokLine;
    syntax->Column = mTokCol;
    syntax->FileName = mUnitFileName;
    return Unique<T>( syntax );
}

void AlgolyParser::ThrowSyntaxError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    mRep.ThrowError( CompilerErr::SYNTAX, mUnitFileName, mTokLine, mTokCol, format, args );
    // No need to run va_end( args ), since an exception was thrown
}

}
