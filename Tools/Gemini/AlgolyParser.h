#pragma once

#include "Compiler.h"
#include <memory>
#include <string>


class AlgolyParser
{
    template <typename T>
    using Unique = std::unique_ptr<T>;

    enum class TokenCode
    {
        Bof,
        Eof,
        Eol,
        Separator,
        Number,
        Symbol,
        LParen,
        RParen,
        Comma,
        Ampersand,
        LBracket,
        RBracket,
        Assign,
        Colon,
        Ellipsis,
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
        Above,
        And,
        Below,
        Break,
        By,
        Case,
        Const,
        Def,
        Do,
        Downto,
        Else,
        Elsif,
        End,
        For,
        If,
        Lambda,
        Let,
        Loop,
        Next,
        Not,
        Or,
        Return,
        Then,
        To,
        Var,
        When,
        While,
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

    Reporter        mRep;

public:
    AlgolyParser( const char* codeText, int codeTextLen, ICompilerLog* log );

    Unit* Parse();

private:
    // Scanning

    int GetColumn();
    int PeekChar() const;
    int PeekChar( int index ) const;
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

    static bool IsIdentifierInitial( int c );
    static bool IsIdentifierCoda( int c );

    void AssertToken( TokenCode code );

    // Parsing

    static bool IsSeparatorKeyword( TokenCode tokenCode );
    static bool IsStatementSeparator( TokenCode tokenCode );

    Unique<ProcDecl> ParseFunction();
    Unique<LambdaExpr> ParseLambda();
    Unique<ProcDecl> ParseProc( bool hasName );
    std::vector<std::unique_ptr<ParamDecl>> ParseParamList();
    Unique<Syntax> ParseCall( std::unique_ptr<Syntax>&& head, bool indirect, bool parens = true );
    Unique<Syntax> ParseAssignment( std::unique_ptr<Syntax>&& head );
    Unique<Syntax> ParseLet();

    void ParseGlobalVars( Unit* unit );
    Unique<DataDecl> ParseVar( Unique<DataDecl>&& varDecl, TokenCode assignToken );
    Unique<Syntax> ParseReturn();
    Unique<Syntax> ParseIf();
    Unique<CondClause> ParseIfClause();
    Unique<CondClause> ParseElseClause();
    Unique<Syntax> ParseFor();
    Unique<Syntax> ParseLoop();
    Unique<Syntax> ParseWhile();
    Unique<Syntax> ParseBreak();
    Unique<Syntax> ParseNext();
    Unique<Syntax> ParseCase();
    Unique<CaseWhen> ParseCaseWhen();
    Unique<CaseElse> ParseCaseElse();

    void ParseStatements( StatementList& cotainer );
    Unique<Syntax> ParseStatement();
    Unique<Syntax> ParseExprStatement();
    Unique<Syntax> ParseExpr();
    Unique<Syntax> ParseBinaryPart( int level );
    Unique<Syntax> ParseBinary( int level );
    Unique<Syntax> ParseUnary();
    Unique<Syntax> ParseSingle();
    Unique<Syntax> ParseIndexing( std::unique_ptr<Syntax>&& head );

    bool IsTokenOrOp();
    bool IsTokenAndOp();
    bool IsTokenEqualityOp();
    bool IsTokenComparisonOp();
    bool IsTokenAdditiveOp();
    bool IsTokenMultiplicativeOp();

    I32 ParseRawNumber();
    std::string ParseRawSymbol();
    std::string ParseAsRawSymbol();

    Unique<NumberExpr> ParseNumber();
    Unique<NameExpr> ParseSymbol();

    Unique<NumberExpr> WrapNumber();
    Unique<NameExpr> WrapSymbol();

    Unique<NumberExpr> MakeNumber( int32_t value );
    Unique<NameExpr> MakeSymbol( const char* string );

    template <typename T>
    Unique<T> Make();

    [[noreturn]] void ThrowSyntaxError( const char* format, ... );
};
