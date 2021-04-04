#include "stdafx.h"
#include "LispyParser.h"
#include <stdarg.h>


using Number  = Compiler::Number;
using Symbol  = Compiler::Symbol;
using Slist   = Compiler::Slist;


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


LispyParser::LispyParser( const char* codeText, int codeTextLen, ICompilerLog* log ) :
    mCodeTextPtr( codeText ),
    mCodeTextEnd( codeText + codeTextLen ),
    mLineStart( codeText ),
    mLine( 1 ),
    mCurChar( 0 ),
    mCurToken( Token_Bof ),
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
        mCurToken = Token_Eof;
    }
    else if ( PeekChar() == '(' )
    {
        NextChar();
        mCurToken = Token_LParen;
    }
    else if ( PeekChar() == ')' )
    {
        NextChar();
        mCurToken = Token_RParen;
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

    mCurToken = Token_Number;
}

void LispyParser::ReadSymbol()
{
    while ( IsIdentifierCoda( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    mCurToken = Token_Symbol;
}

Compiler::Slist* LispyParser::Parse()
{
    std::unique_ptr<Slist> list( new Slist() );
    list->Code = Compiler::Elem_Slist;

    while ( ScanToken() != Token_Eof )
    {
        if ( mCurToken != Token_LParen )
            ThrowSyntaxError( "syntax error : expected list" );

        list->Elements.push_back( std::unique_ptr<Slist>( ParseSlist() ) );
    }

    return list.release();
}

Compiler::Slist* LispyParser::ParseSlist()
{
    std::unique_ptr<Slist> list( new Slist() );
    list->Code = Compiler::Elem_Slist;
    list->Line = mTokLine;
    list->Column = mTokCol;

    for ( ; ; )
    {
        switch ( ScanToken() )
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

Compiler::Number* LispyParser::ParseNumber()
{
    Number* number = new Number();
    number->Code = Compiler::Elem_Number;
    number->Value = mCurNumber;
    number->Line = mTokLine;
    number->Column = mTokCol;
    return number;
}

Compiler::Symbol* LispyParser::ParseSymbol()
{
    Symbol* symbol = new Symbol();
    symbol->Code = Compiler::Elem_Symbol;
    symbol->String = mCurString;
    symbol->Line = mTokLine;
    symbol->Column = mTokCol;
    return symbol;
}

void LispyParser::ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args )
{
    ::Log( mLog, LOG_ERROR, line, col, format, args );
    throw Compiler::CompilerException( exceptionCode );
}

void LispyParser::ThrowSyntaxError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CERR_SYNTAX, mTokLine, mTokCol, format, args );
    va_end( args );
}

void LispyParser::ThrowInternalError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CERR_INTERNAL, mLine, GetColumn(), format, args );
    va_end( args );
}

void LispyParser::ThrowInternalError()
{
    ThrowInternalError( "Internal error" );
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
