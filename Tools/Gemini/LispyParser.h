// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "LangCommon.h"
#include "Syntax.h"
#include <map>
#include <string>
#include <vector>


namespace Gemini
{

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

    using ParseFunc = Unique<Syntax>( LispyParser::* )();
    using ParserMap = std::map<std::string, ParseFunc>;

    std::string     mFileName;
    const char*     mUnitFileName;

    const char*     mCodeTextPtr;
    const char*     mCodeTextEnd;
    const char*     mLineStart;
    int             mLine;
    char            mCurChar;

    TokenCode       mCurToken;
    std::string     mCurString;
    int             mCurNumber;
    int             mTokLine;
    int             mTokCol;

    Reporter        mRep;
    ParserMap       mParserMap;

public:
    LispyParser( const char* codeText, size_t codeTextLen, const char* fileName, ICompilerLog* log );

    Unique<Unit> Parse();

private:
    // Scanning

    int GetColumn();
    char PeekChar() const;
    char PeekChar( int index ) const;
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

    Unique<Syntax> ParseExpression( bool isInit = false );
    Unique<NumberExpr> ParseNumber();
    Unique<NameExpr> ParseSymbol();

    Unique<Syntax> ParseBinary();
    Unique<Syntax> ParseNegate();
    Unique<Syntax> ParseNot();

    Unique<Syntax> ParseCall();
    Unique<Syntax> ParseEvalStar();
    Unique<Syntax> ParseLambda();
    Unique<NativeDecl> ParseNative();
    Unique<Syntax> ParseFunction();
    Unique<Syntax> ParseFuncall();
    Unique<Syntax> ParseReturn();
    Unique<Syntax> ParseLet();
    Unique<DataDecl> ParseLetBinding( Unique<DataDecl>&& newVarDecl, bool isParam = false );
    Unique<TypeRef> ParseTypeRef( bool embedded = true );
    Unique<TypeRef> ParseNameTypeRef( bool embedded );
    Unique<TypeRef> ParsePtrFuncTypeRef();
    Unique<TypeRef> ParseArrayTypeRef();
    Unique<Syntax> ParseArrayInitializer();
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
    Unique<Syntax> ParseYield();
    Unique<Syntax> ParseCase();
    Unique<CaseWhen> ParseCaseWhen();
    Unique<Syntax> ParseProgn();

    std::vector<Unique<DataDecl>> ParseParamList();
    ParamSpecRef ParseAnonymousParameter();
    Unique<DataDecl> ParseParameter();
    Unique<ProcDecl> ParseProc( bool hasName );
    Unique<DataDecl> ParseDefvar();
    Unique<DataDecl> ParseDefconstant();
    Unique<DataDecl> ParseNameDecl();
    Unique<Syntax> ParseGlobalError();

    void ParseImplicitProgn( StatementList& container );

    template <typename T, typename... Args>
    Unique<T> Make( Args&&... args );

    [[noreturn]] void ThrowSyntaxError( const char* format, ... );
};

}
