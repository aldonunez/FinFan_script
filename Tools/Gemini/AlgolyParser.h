// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "Compiler.h"
#include <memory>
#include <string>


class AlgolyParser
{
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
        RArrow,
        Dot,
        Ampersand,
        LBracket,
        RBracket,
        Assign,
        Colon,
        DotDot,
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
        As,
        Below,
        Break,
        By,
        Case,
        Const,
        Countof,
        Def,
        Do,
        Downto,
        Else,
        Elsif,
        End,
        For,
        If,
        Import,
        Lambda,
        Loop,
        Native,
        Next,
        Not,
        Of,
        Or,
        Proc,
        Return,
        Then,
        To,
        Type,
        Var,
        When,
        While,
    };

    typedef bool (AlgolyParser::*TestOpFunc)();

    static const TestOpFunc sTestOpFuncs[];

    std::string     mFileName;
    const char*     mUnitFileName;

    const char*     mCodeTextPtr;
    const char*     mCodeTextEnd;
    const char*     mLineStart;
    int             mLine;
    int             mCurChar;

    TokenCode       mCurToken;
    std::string     mCurString;
    int64_t         mCurNumber;

    int             mTokLine;
    int             mTokCol;

    Reporter        mRep;

public:
    AlgolyParser( const char* codeText, int codeTextLen, const char* fileName, ICompilerLog* log );

    Unique<Unit> Parse();

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

    Unique<ImportDecl> ParseImport();
    Unique<ProcDecl> ParseFunction();
    Unique<LambdaExpr> ParseLambda();
    Unique<NativeDecl> ParseNative();
    Unique<ProcDecl> ParseProc( bool hasName );
    std::vector<Unique<DataDecl>> ParseParamList();
    Unique<DataDecl> ParseParameter();
    Unique<Syntax> ParseCall( Unique<Syntax>&& head, bool indirect, bool parens = true );
    Unique<Syntax> ParseLet();

    void ParseGlobalVars( Unit* unit );
    Unique<DataDecl> ParseVar( Unique<DataDecl>&& newVarDecl, std::optional<TokenCode> assignToken );
    Unique<DataDecl> ParseVarDecl();
    Unique<DataDecl> ParseConstDecl();
    Unique<DeclSyntax> ParseTypeDecl();
    void ParseTypeDecls( Unit* unit );

    Unique<TypeRef> ParseTypeDef();
    Unique<TypeRef> ParseTypeRef();
    Unique<TypeRef> ParseNameTypeRef();
    Unique<TypeRef> ParsePtrFuncTypeRef();
    Unique<TypeRef> ParseArrayTypeRef();
    Unique<Syntax> ParseArrayInitializer();
    Unique<Syntax> ParseInitExpr();
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
    Unique<Syntax> ParseAssignment();
    Unique<Syntax> ParseBinaryPart( int level );
    Unique<Syntax> ParseBinary( int level );
    Unique<Syntax> ParseUnary();
    Unique<Syntax> ParseSingle();
    Unique<Syntax> ParseIndexing( Unique<Syntax>&& head );
    Unique<Syntax> ParseDotExpr( Unique<Syntax>&& head );
    Unique<Syntax> ParseIndexingOrDot( Unique<Syntax>&& head );
    Unique<Syntax> ParseCountof();

    bool IsTokenOrOp();
    bool IsTokenAndOp();
    bool IsTokenEqualityOp();
    bool IsTokenComparisonOp();
    bool IsTokenAdditiveOp();
    bool IsTokenMultiplicativeOp();

    std::string ParseRawSymbol();
    std::string ParseAsRawSymbol();

    Unique<NumberExpr> ParseNumber();
    Unique<NameExpr> ParseSymbol();

    Unique<NumberExpr> WrapNumber();
    Unique<NameExpr> WrapSymbol();

    Unique<NumberExpr> MakeNumber( int64_t value );
    Unique<NameExpr> MakeSymbol( const char* string );

    template <typename T, typename... Args>
    Unique<T> Make( Args&&... args );

    [[noreturn]] void ThrowSyntaxError( const char* format, ... );
};
