#pragma once

#include "Compiler.h"
#include <string>


class LispyParser
{
    enum TokenCode
    {
        Token_Bof,
        Token_Eof,
        Token_LParen,
        Token_RParen,
        Token_Number,
        Token_Symbol,
    };

    const char*     mCodeTextPtr;
    const char*     mCodeTextEnd;
    const char*     mLineStart;
    int             mLine;
    int             mCurChar;

    TokenCode       mCurToken;
    std::string     mCurString;
    int             mCurNumber;
    int             mTokLine;
    int             mTokCol;

    ICompilerLog*   mLog;

public:
    LispyParser( const char* codeText, int codeTextLen, ICompilerLog* log );

    Compiler::Slist* Parse();

private:
    // Scanning

    int GetColumn();
    int PeekChar() const;
    int PeekChar( int index ) const;
    void NextChar();
    void SkipWhitespace();
    TokenCode ScanToken();
    void ReadNumber();
    void ReadSymbol();

    static bool IsIdentifierInitial( int c );
    static bool IsIdentifierCoda( int c );

    // Parsing

    Compiler::Slist* ParseSlist();
    Compiler::Number* ParseNumber();
    Compiler::Symbol* ParseSymbol();

    void ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args );
    void ThrowSyntaxError( const char* format, ... );
    void ThrowInternalError( const char* format, ... );
    void ThrowInternalError();
};
