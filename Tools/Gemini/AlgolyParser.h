#pragma once

#include "Compiler.h"
#include <memory>
#include <string>


class AlgolyParser
{
    template <typename T>
    using Unique = std::unique_ptr<T>;

    // TODO: Change statement break from colon to semicolon,
    //       and comment from semicolon to pound

    enum class TokenCode
    {
        Bof,
        Eof,
        Eol,
        Separator,
        Number,
        Symbol,
        Keyword,
        LParen,
        RParen,
        Comma,
        Assign,
        Plus,
        Minus,
        Star,
        Slash,
        Percent,
        EQ,
        NE,
        LT,
        LE,
        GT,
        GE,
        And,
        Def,
        Else,
        End,
        If,
        Lambda,
        Let,
        Not,
        Or,
        Return,
        Then,
    };

    typedef bool (AlgolyParser::*TestOpFunc)();

    static const TestOpFunc sTestOpFuncs[];

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
    AlgolyParser( const char* codeText, int codeTextLen, ICompilerLog* log );

    Compiler::Slist* Parse();

private:
    // Scanning

    int GetColumn();
    int PeekChar() const;
    void NextChar();
    void CollectChar();
    void SkipWhitespace();
    void SkipLineEndings();
    void SkipLineSeparators();
    TokenCode ScanToken();
    void ScanToken( TokenCode code );
    void ReadLineEnding();
    void ReadNumber();
    void ReadSymbolOrKeyword();

    bool IsIdentifierInitial( int c );
    bool IsIdentifierCoda( int c );

    void AssertToken( TokenCode code );

    // Parsing

    Unique<Compiler::Slist> ParseFunction();
    Unique<Compiler::Slist> ParseLambda();
    Unique<Compiler::Slist> ParseProc( const char* head, bool hasName );
    Unique<Compiler::Slist> ParseParamList();
    Unique<Compiler::Slist> ParseCall( std::unique_ptr<Compiler::Element>&& head, bool indirect );
    Unique<Compiler::Slist> ParseAssignment( std::unique_ptr<Compiler::Element>&& head );
    Unique<Compiler::Slist> ParseLet();
    Unique<Compiler::Slist> ParseReturn();
    Unique<Compiler::Slist> ParseIf();
    Unique<Compiler::Slist> ParseIfClause();
    Unique<Compiler::Slist> ParseElseClause();

    Unique<Compiler::Element> ParseStatement();
    Unique<Compiler::Element> ParseExpr();
    Unique<Compiler::Element> ParseBinaryPart( int level );
    Unique<Compiler::Element> ParseBinary( int level );
    Unique<Compiler::Element> ParseUnary();
    Unique<Compiler::Element> ParseSingle();

    bool IsTokenOrOp();
    bool IsTokenAndOp();
    bool IsTokenComparisonOp();
    bool IsTokenAdditiveOp();
    bool IsTokenMultiplicativeOp();

    Unique<Compiler::Number> ParseNumber();
    Unique<Compiler::Symbol> ParseSymbol();
    Unique<Compiler::Symbol> ParseAsSymbol();

    Unique<Compiler::Number> WrapNumber();
    Unique<Compiler::Symbol> WrapSymbol();

    Unique<Compiler::Number> MakeNumber( int32_t value );
    Unique<Compiler::Symbol> MakeSymbol( const char* string );
    Unique<Compiler::Slist>  MakeSlist();

    [[noreturn]] void ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args );
    [[noreturn]] void ThrowSyntaxError( const char* format, ... );
    [[noreturn]] void ThrowInternalError( const char* format, ... );
    [[noreturn]] void ThrowInternalError();
};
