#pragma once

#include "Compiler.h"
#include <string>


class LispyParser
{
    enum class TokenCode
    {
        Bof,
        Eof,
        LParen,
        RParen,
        Number,
        Symbol,
    };

    template <typename T>
    using Unique = std::unique_ptr<T>;

    using ParseFunc = Unique<Syntax>( LispyParser::* )();
    using ParserMap = std::map<std::string, ParseFunc>;

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
    ParserMap       mParserMap;

public:
    LispyParser( const char* codeText, int codeTextLen, ICompilerLog* log );

    Unit* Parse();

private:
    // Scanning

    int GetColumn();
    int PeekChar() const;
    int PeekChar( int index ) const;
    void NextChar();
    void SkipWhitespace();
    TokenCode ScanToken();
    TokenCode ScanToken( TokenCode code );
    TokenCode ScanLParen();
    TokenCode ScanRParen();
    std::string ScanSymbol();
    TokenCode ScanSymbol( const char* str );
    void AssertToken( TokenCode code );
    void ReadNumber();
    void ReadSymbol();

    static bool IsIdentifierInitial( int c );
    static bool IsIdentifierCoda( int c );

    // Parsing

    Unique<Syntax> ParseExpression();
    Unique<NumberExpr> ParseNumber();
    Unique<NameExpr> ParseSymbol();

    Unique<Syntax> ParseBinary();
    Unique<Syntax> ParseNegate();
    Unique<Syntax> ParseNot();

    Unique<Syntax> ParseCall();
    Unique<Syntax> ParseEvalStar();
    Unique<Syntax> ParseLambda();
    Unique<Syntax> ParseFunction();
    Unique<Syntax> ParseFuncall();
    Unique<Syntax> ParseReturn();
    Unique<Syntax> ParseLet();
    Unique<DataDecl> ParseLetBinding( Unique<DataDecl>&& newVarDecl );
    Unique<Syntax> ParseAref();
    Unique<Syntax> ParseSet();

    Unique<Syntax> ParseIf();
    Unique<Syntax> ParseCond();
    Unique<CondClause> ParseCondClause();
    Unique<Syntax> ParseLoop();
    Unique<Syntax> ParseLoopFor();
    Unique<Syntax> ParseLoopDo();
    Unique<Syntax> ParseDo();
    Unique<Syntax> ParseBreak();
    Unique<Syntax> ParseNext();
    Unique<Syntax> ParseCase();
    Unique<CaseWhen> ParseCaseWhen();
    Unique<Syntax> ParseProgn();

    Unique<ProcDecl> ParseProc( bool hasName );
    Unique<DataDecl> ParseDefvar();
    Unique<DataDecl> ParseDefconstant();
    Unique<Syntax> ParseGlobalError();

    void ParseImplicitProgn( StatementList& container );

    template <typename T, typename... Args>
    std::unique_ptr<T> Make( Args&&... args );

    [[noreturn]] void ThrowSyntaxError( const char* format, ... );
};
