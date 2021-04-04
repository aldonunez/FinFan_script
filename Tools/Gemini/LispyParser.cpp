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
    mLine( 1 ),
    mLineStart( codeText ),
    mCurToken( Token_Bof ),
    mCurNumber( 0 ),
    mTokLine( 0 ),
    mTokCol( 0 ),
    mLog( log )
{
}

LispyParser::TokenCode LispyParser::NextToken()
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
    else if ( IsIdentifierInitial() )
    {
        mCurToken = Token_Symbol;
        ReadSymbol();
    }
    else
    {
        ThrowSyntaxError( "Bad character: U+%02X", *mCodeTextPtr );
    }

    return mCurToken;
}

void LispyParser::SkipWhitespace()
{
    while ( true )
    {
        while ( mCodeTextPtr < mCodeTextEnd
            && isspace( *mCodeTextPtr ) )
        {
            if ( *mCodeTextPtr == '\r'
                || *mCodeTextPtr == '\n' )
            {
                if ( *mCodeTextPtr == '\r'
                    && (mCodeTextEnd - mCodeTextPtr) >= 2
                    && mCodeTextPtr[1] == '\n' )
                {
                    mCodeTextPtr++;
                }

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
            && *mCodeTextPtr != '\r'
            && *mCodeTextPtr != '\n' )
        {
            mCodeTextPtr++;
        }
    }
}

bool LispyParser::IsIdentifierInitial()
{
    return (*mCodeTextPtr < sizeof sIdentifierInitialCharMap)
        && sIdentifierInitialCharMap[*mCodeTextPtr];
}

bool LispyParser::IsIdentifierCoda()
{
    return IsIdentifierInitial() || isdigit( *mCodeTextPtr );
}

void LispyParser::ReadNumber()
{
    bool negate = false;

    if ( *mCodeTextPtr == '-' )
    {
        negate = true;
        mCodeTextPtr++;
    }

    while ( (mCodeTextPtr < mCodeTextEnd) && isdigit( *mCodeTextPtr ) )
    {
        mCurString.append( 1, *mCodeTextPtr );
        mCodeTextPtr++;
    }

    if ( (mCodeTextPtr < mCodeTextEnd) && IsIdentifierInitial() )
        ThrowSyntaxError( "syntax error : bad number" );

    mCurNumber = atoi( mCurString.c_str() );
    if ( negate )
        mCurNumber = -mCurNumber;
}

void LispyParser::ReadSymbol()
{
    while ( (mCodeTextPtr < mCodeTextEnd) && IsIdentifierCoda() )
    {
        mCurString.append( 1, *mCodeTextPtr );
        mCodeTextPtr++;
    }
}

int LispyParser::GetColumn()
{
    return mCodeTextPtr - mLineStart + 1;
}

Compiler::Slist* LispyParser::Parse()
{
    std::unique_ptr<Slist> list( new Slist() );
    list->Code = Compiler::Elem_Slist;

    while ( NextToken() != Token_Eof )
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
