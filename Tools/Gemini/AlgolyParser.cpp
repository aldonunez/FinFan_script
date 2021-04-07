#include "stdafx.h"
#include "AlgolyParser.h"
#include <stdarg.h>


template <typename T>
using Unique  = std::unique_ptr<T>;

using Element = Compiler::Element;
using Number  = Compiler::Number;
using Symbol  = Compiler::Symbol;
using Slist   = Compiler::Slist;


AlgolyParser::AlgolyParser( const char* codeText, int codeTextLen, ICompilerLog* log ) :
    mCodeTextPtr( codeText ),
    mCodeTextEnd( codeText + codeTextLen ),
    mLineStart( codeText ),
    mLine( 1 ),
    mCurChar( 0 ),
    mCurToken( TokenCode::Bof ),
    mCurNumber( 0 ),
    mTokLine( 0 ),
    mTokCol( 0 ),
    mLog( log )
{
    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCurChar = *mCodeTextPtr;
    }
}

int AlgolyParser::GetColumn()
{
    return mCodeTextPtr - mLineStart + 1;
}

int AlgolyParser::PeekChar() const
{
    return mCurChar;
}

int AlgolyParser::PeekChar( int index ) const
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
            ThrowSyntaxError( "Bad character: U+%02X", ':' );
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

    case '+':
        CollectChar();
        mCurToken = TokenCode::Plus;
        break;

    case '-':
        if ( isdigit( PeekChar( 1 ) ) )
        {
            ReadNumber();
        }
        else
        {
            CollectChar();
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
        ThrowSyntaxError( "Expected token: %d", code );
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
        || (c == '_')
        || (c == '@');
}

bool AlgolyParser::IsIdentifierCoda( int c )
{
    return IsIdentifierInitial( c ) || isdigit( c );
}

void AlgolyParser::ReadNumber()
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

void AlgolyParser::ReadSymbolOrKeyword()
{
    static std::map<std::string, TokenCode> sStrToKeyword
    {
        { "above",  TokenCode::Above },
        { "and",    TokenCode::And },
        { "below",  TokenCode::Below },
        { "break",  TokenCode::Break },
        { "by",     TokenCode::By },
        { "case",   TokenCode::Case },
        { "def",    TokenCode::Def },
        { "do",     TokenCode::Do },
        { "downto", TokenCode::Downto },
        { "else",   TokenCode::Else },
        { "elsif",  TokenCode::Elsif },
        { "end",    TokenCode::End },
        { "for",    TokenCode::For },
        { "if",     TokenCode::If },
        { "lambda", TokenCode::Lambda },
        { "let",    TokenCode::Let },
        { "next",   TokenCode::Next },
        { "not",    TokenCode::Not },
        { "or",     TokenCode::Or },
        { "return", TokenCode::Return },
        { "then",   TokenCode::Then },
        { "to",     TokenCode::To },
        { "when",   TokenCode::When },
        { "while",  TokenCode::While },
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
        || tokenCode == TokenCode::When;
}

Compiler::Slist* AlgolyParser::Parse()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    ScanToken();

    while ( mCurToken != TokenCode::Eof )
    {
        if ( mCurToken != TokenCode::Def )
            ThrowSyntaxError( "syntax error : expected function" );

        list->Elements.push_back( ParseFunction() );

        SkipLineSeparators();
    }

    return list.release();
}

Unique<Compiler::Slist> AlgolyParser::ParseFunction()
{
    return ParseProc( "defun", true );
}

Unique<Compiler::Slist> AlgolyParser::ParseLambda()
{
    return ParseProc( "lambda", false );
}

Unique<Compiler::Slist> AlgolyParser::ParseProc( const char* head, bool hasName )
{
    std::unique_ptr<Slist> list( MakeSlist() );

    mCurString = head;
    list->Elements.push_back( ParseAsSymbol() );

    if ( hasName )
        list->Elements.push_back( ParseSymbol() );

    if ( mCurToken == TokenCode::LParen )
    {
        list->Elements.push_back( ParseParamList() );
    }
    else
    {
        list->Elements.push_back( MakeSlist() );
    }

    SkipLineSeparators();

    ParseStatements( list.get() );

    // Read past "end"
    ScanToken( TokenCode::End );
    SkipLineEndings();

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseParamList()
{
    std::unique_ptr<Slist> list( MakeSlist() );

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

        if ( mCurToken != TokenCode::Symbol )
            ThrowSyntaxError( "Expected parameter name" );

        list->Elements.push_back( ParseSymbol() );
    }

    // Read past right parenthesis
    ScanToken();

    return list;
}

void AlgolyParser::ParseStatements( Slist* container )
{
    while ( !IsSeparatorKeyword( mCurToken ) )
    {
        container->Elements.push_back( ParseStatement() );
    }
}

Unique<Compiler::Element> AlgolyParser::ParseStatement()
{
    std::unique_ptr<Element> elem;

    switch ( mCurToken )
    {
    case TokenCode::Let:
        elem = ParseLet();
        break;

    case TokenCode::Return:
        elem = ParseReturn();
        break;

    case TokenCode::For:
        elem = ParseFor();
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

    default:
        elem = ParseExprStatement();
        break;
    }

    SkipLineSeparators();

    return elem;
}

Unique<Compiler::Element> AlgolyParser::ParseExprStatement()
{
    std::unique_ptr<Element> expr( ParseExpr() );

    if ( mCurToken != TokenCode::Eol
        && mCurToken != TokenCode::Separator
        && !IsSeparatorKeyword( mCurToken ) )
    {
        ThrowSyntaxError( "Expected end of statement" );
    }

    return expr;
}

Unique<Compiler::Element> AlgolyParser::ParseExpr()
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
        return ParseBinary( 0 );
    }
}

const AlgolyParser::TestOpFunc AlgolyParser::sTestOpFuncs[] =
{
    &AlgolyParser::IsTokenOrOp,
    &AlgolyParser::IsTokenAndOp,
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

bool AlgolyParser::IsTokenComparisonOp()
{
    return mCurToken == TokenCode::EQ
        || mCurToken == TokenCode::NE
        || mCurToken == TokenCode::LT
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

Unique<Compiler::Element> AlgolyParser::ParseBinaryPart( int level )
{
    if ( level + 1 >= _countof( AlgolyParser::sTestOpFuncs ) )
        return ParseUnary();
    else
        return ParseBinary( level + 1 );
}

Unique<Compiler::Element> AlgolyParser::ParseBinary( int level )
{
    std::unique_ptr<Element> first( ParseBinaryPart( level ) );
    TestOpFunc testOpFunc = sTestOpFuncs[level];

    // Left-associative

    while ( (this->*testOpFunc)() )
    {
        std::unique_ptr<Slist> list = MakeSlist();

        list->Elements.push_back( ParseAsSymbol() );

        SkipLineEndings();

        list->Elements.push_back( std::move( first ) );
        list->Elements.push_back( ParseBinaryPart( level ) );

        if ( testOpFunc == &AlgolyParser::IsTokenComparisonOp && IsTokenComparisonOp() )
            ThrowSyntaxError( "Comparisons are binary only" );

        first = std::move( list );
    }

    return first;
}

Unique<Compiler::Element> AlgolyParser::ParseUnary()
{
    if ( mCurToken == TokenCode::Minus
        || mCurToken == TokenCode::Not )
    {
        std::unique_ptr<Slist> list( MakeSlist() );

        list->Elements.push_back( ParseAsSymbol() );
        list->Elements.push_back( ParseSingle() );

        return list;
    }

    return ParseSingle();
}

Unique<Compiler::Element> AlgolyParser::ParseSingle()
{
    std::unique_ptr<Element> elem;
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

    case TokenCode::Symbol: elem = ParseSymbol(); break;
    case TokenCode::Number: elem = ParseNumber(); break;

    default:
        ThrowSyntaxError( "Expected expression" );
    }

    if ( mCurToken == TokenCode::LParen )
    {
        return ParseCall( std::move( elem ), indirect );
    }
    else if ( mCurToken == TokenCode::Assign )
    {
        return ParseAssignment( std::move( elem ) );
    }

    return elem;
}

Unique<Compiler::Slist> AlgolyParser::ParseCall( std::unique_ptr<Element>&& head, bool indirect )
{
    std::unique_ptr<Slist> list( MakeSlist() );

    if ( indirect )
        list->Elements.push_back( MakeSymbol( "funcall" ) );

    list->Elements.push_back( std::move( head ) );

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

        list->Elements.push_back( ParseExpr() );
    }

    // Read past right parenthesis
    ScanToken();

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseAssignment( std::unique_ptr<Compiler::Element>&& head )
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( MakeSymbol( "set" ) );

    list->Elements.push_back( std::move( head ) );

    // Read past assignment operator
    ScanToken();
    SkipLineEndings();

    list->Elements.push_back( ParseExpr() );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseLet()
{
    std::unique_ptr<Slist> assignments( MakeSlist() );

    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( ParseAsSymbol() );
    list->Elements.push_back( std::unique_ptr<Element>() );

    SkipLineEndings();

    bool first = true;

    do
    {
        std::unique_ptr<Slist> pair( MakeSlist() );

        if ( !first )
        {
            if ( mCurToken != TokenCode::Comma )
                ThrowSyntaxError( "Expected , or line separator" );

            ScanToken();
            SkipLineEndings();
        }

        first = false;

        if ( mCurToken != TokenCode::Symbol )
            ThrowSyntaxError( "Expected variable name" );

        pair->Elements.push_back( ParseSymbol() );

        AssertToken( TokenCode::EQ );
        ScanToken();
        SkipLineEndings();

        pair->Elements.push_back( ParseExpr() );

        assignments->Elements.push_back( std::move( pair ) );

    } while ( mCurToken != TokenCode::Eol
        && mCurToken != TokenCode::Separator
        && mCurToken != TokenCode::End );

    list->Elements[1] = std::move( assignments );

    SkipLineSeparators();

    while ( mCurToken != TokenCode::End )
    {
        list->Elements.push_back( ParseStatement() );
    }

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseReturn()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( ParseAsSymbol() );
    list->Elements.push_back( ParseExpr() );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseIf()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( MakeSymbol( "cond" ) );
    list->Elements.push_back( ParseIfClause() );

    while ( mCurToken == TokenCode::Elsif )
    {
        list->Elements.push_back( ParseIfClause() );
    }

    if ( mCurToken == TokenCode::Else )
    {
        list->Elements.push_back( ParseElseClause() );
    }

    ScanToken( TokenCode::End );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseIfClause()
{
    std::unique_ptr<Slist> clause( MakeSlist() );

    ScanToken();

    clause->Elements.push_back( ParseExpr() );

    if ( mCurToken != TokenCode::Then )
        ThrowSyntaxError( "Expected then" );

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause.get() );

    return clause;
}

Unique<Compiler::Slist> AlgolyParser::ParseElseClause()
{
    std::unique_ptr<Slist> clause( MakeSlist() );

    ScanToken();
    SkipLineSeparators();

    clause->Elements.push_back( MakeNumber( 1 ) );

    ParseStatements( clause.get() );

    return clause;
}

Unique<Compiler::Slist> AlgolyParser::ParseFor()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( MakeSymbol( "loop" ) );
    list->Elements.push_back( ParseAsSymbol() );
    list->Elements.push_back( ParseSymbol() );

    ScanToken( TokenCode::Assign );
    list->Elements.push_back( MakeSymbol( "from" ) );
    list->Elements.push_back( ParseExpr() );

    if ( mCurToken == TokenCode::Above
        || mCurToken == TokenCode::Below
        || mCurToken == TokenCode::Downto
        || mCurToken == TokenCode::To )
    {
        list->Elements.push_back( ParseAsSymbol() );
    }
    else
    {
        ThrowSyntaxError( "Expected: above, below, downto, to" );
    }

    list->Elements.push_back( ParseExpr() );

    if ( mCurToken == TokenCode::By )
    {
        list->Elements.push_back( ParseAsSymbol() );
        list->Elements.push_back( ParseExpr() );
    }

    AssertToken( TokenCode::Do );
    list->Elements.push_back( ParseAsSymbol() );
    SkipLineSeparators();

    ParseStatements( list.get() );

    ScanToken( TokenCode::End );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseWhile()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    ScanToken();
    list->Elements.push_back( MakeSymbol( "do" ) );
    list->Elements.push_back( ParseExpr() );
    ScanToken( TokenCode::Do );

    ParseStatements( list.get() );

    ScanToken( TokenCode::End );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseCase()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( ParseAsSymbol() );
    list->Elements.push_back( ParseExpr() );

    SkipLineSeparators();

    while ( mCurToken == TokenCode::When )
    {
        list->Elements.push_back( ParseCaseWhen() );
    }

    if ( mCurToken == TokenCode::Else )
    {
        list->Elements.push_back( ParseCaseElse() );
    }

    ScanToken( TokenCode::End );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseCaseWhen()
{
    std::unique_ptr<Slist> clause( MakeSlist() );

    clause->Elements.push_back( MakeSlist() );

    auto keys = (Slist*) clause->Elements[0].get();

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

        if ( mCurToken == TokenCode::Symbol )
        {
            keys->Elements.push_back( ParseSymbol() );
        }
        else if ( mCurToken == TokenCode::Number )
        {
            keys->Elements.push_back( ParseNumber() );
        }
        else
        {
            ThrowSyntaxError( "Expected symbol or number" );
        }

        if ( mCurToken == TokenCode::Then )
            break;
    }

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause.get() );

    return clause;
}

Unique<Compiler::Slist> AlgolyParser::ParseCaseElse()
{
    std::unique_ptr<Slist> clause( MakeSlist() );

    clause->Elements.push_back( MakeSymbol( "otherwise" ) );

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause.get() );

    return clause;
}

Unique<Compiler::Slist> AlgolyParser::ParseBreak()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( ParseAsSymbol() );

    return list;
}

Unique<Compiler::Slist> AlgolyParser::ParseNext()
{
    std::unique_ptr<Slist> list( MakeSlist() );

    list->Elements.push_back( ParseAsSymbol() );

    return list;
}

Unique<Compiler::Number> AlgolyParser::ParseNumber()
{
    if ( mCurToken != TokenCode::Number )
        ThrowSyntaxError( "Expected number" );

    std::unique_ptr<Number> elem( WrapNumber() );
    ScanToken();
    return elem;
}

Unique<Compiler::Symbol> AlgolyParser::ParseSymbol()
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected symbol" );

    std::unique_ptr<Symbol> elem( WrapSymbol() );
    ScanToken();
    return elem;
}

Unique<Compiler::Symbol> AlgolyParser::ParseAsSymbol()
{
    if ( mCurString.size() == 0 )
        ThrowInternalError();

    std::unique_ptr<Symbol> elem( WrapSymbol() );
    ScanToken();
    return elem;
}

Unique<Compiler::Number> AlgolyParser::WrapNumber()
{
    return MakeNumber( mCurNumber );
}

Unique<Compiler::Symbol> AlgolyParser::WrapSymbol()
{
    return MakeSymbol( mCurString.c_str() );
}

Unique<Compiler::Number> AlgolyParser::MakeNumber( int32_t value )
{
    Number* number = new Number();
    number->Code = Compiler::Elem_Number;
    number->Value = value;
    number->Line = mTokLine;
    number->Column = mTokCol;
    return std::unique_ptr<Number>( number );
}

Unique<Compiler::Symbol> AlgolyParser::MakeSymbol( const char* string )
{
    Symbol* symbol = new Symbol();
    symbol->Code = Compiler::Elem_Symbol;
    symbol->String = string;
    symbol->Line = mTokLine;
    symbol->Column = mTokCol;
    return std::unique_ptr<Symbol>( symbol );
}

Unique<Compiler::Slist> AlgolyParser::MakeSlist()
{
    Slist* list = new Slist();
    list->Code = Compiler::Elem_Slist;
    list->Line = mTokLine;
    list->Column = mTokCol;
    return std::unique_ptr<Slist>( list );
}

void AlgolyParser::ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args )
{
    ::Log( mLog, LOG_ERROR, line, col, format, args );
    throw Compiler::CompilerException( exceptionCode );
}

void AlgolyParser::ThrowSyntaxError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CERR_SYNTAX, mTokLine, mTokCol, format, args );
    va_end( args );
}

void AlgolyParser::ThrowInternalError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CERR_INTERNAL, mLine, GetColumn(), format, args );
    va_end( args );
}

void AlgolyParser::ThrowInternalError()
{
    ThrowInternalError( "Internal error" );
}
