// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "GeminiCommon.h"
#include <string>
#include <vector>
#include <memory>
#include <map>
#include <optional>
#include <unordered_map>
#include "Syntax.h"


enum CompilerErr
{
    CERR_OK,
    CERR_INTERNAL,
    CERR_UNSUPPORTED,
    CERR_SYNTAX,
    CERR_SEMANTICS,
};

enum ExternalKind
{
    External_Bytecode,
    External_Native,
};

struct ExternalFunc
{
    int             Id;
    int             Address;
    ExternalKind    Kind;
};

class ICompilerEnv
{
public:
    virtual bool AddExternal( const std::string& name, ExternalKind kind, int address ) = 0;
    virtual bool FindExternal( const std::string& name, ExternalFunc* func ) = 0;
    virtual bool AddGlobal( const std::string& name, int offset ) = 0;
    virtual bool FindGlobal( const std::string& name, int& offset ) = 0;
};

enum LogCategory
{
    LOG_ERROR,
    LOG_WARNING,
};

class ICompilerLog
{
public:
    virtual void Add( LogCategory category, const char* fileName, int line, int column, const char* message ) = 0;
};

struct CallStats
{
    int16_t     MaxCallDepth;
    int16_t     MaxStackUsage;
    bool        Recurses;
};

struct CompilerStats
{
    int         CodeBytesWritten;
    bool        CallsIndirectly;
    CallStats   Lambda;
    CallStats   Static;
};


class LocalScope;

class CompilerException : public std::exception
{
    CompilerErr     mError;

public:
    CompilerException( CompilerErr error )
        : mError( error )
    {
    }

    CompilerErr GetError() const
    {
        return mError;
    }
};

class Reporter
{
    ICompilerLog* mLog;

public:
    Reporter( ICompilerLog* log );

    ICompilerLog* GetLog();

    void Log( LogCategory category, const char* fileName, int line, int col, const char* format, va_list args );
    void LogWarning( const char* fileName, int line, int col, const char* format, ... );

    [[noreturn]] void ThrowError( CompilerErr exceptionCode, Syntax* elem, const char* format, ... );
    [[noreturn]] void ThrowError( CompilerErr exceptionCode, const char* fileName, int line, int col, const char* format, va_list args );
    [[noreturn]] void ThrowInternalError();
    [[noreturn]] void ThrowInternalError( const char* format, ... );
};


class Compiler : public IVisitor
{
public:
    struct InstPatch
    {
        InstPatch*  Next;
        U8*         Inst;
    };

    struct PatchChain
    {
        InstPatch*  First;
        U8*         PatchedPtr;

        PatchChain() :
            First( nullptr ),
            PatchedPtr( nullptr )
        {
        }

        ~PatchChain()
        {
            while ( First != nullptr )
            {
                auto link = First;
                First = First->Next;
                delete link;
            }
        }

        PatchChain( PatchChain&& other ) noexcept
        {
            First = other.First;
            PatchedPtr = other.PatchedPtr;

            other.First = nullptr;
            other.PatchedPtr = nullptr;
        }

        PatchChain& operator=( PatchChain&& other ) noexcept
        {
            std::swap( First, other.First );
            PatchedPtr = other.PatchedPtr;
            return *this;
        }
    };

    using PatchMap = std::map<std::string, PatchChain>;

private:
    struct DeferredLambda
    {
        ProcDecl*   Definition;
        U8*         Patch;
    };

    enum class AddrRefKind
    {
        Lambda,
        Inst,
    };

    struct AddrRef
    {
        AddrRefKind Kind;
        union
        {
            size_t  LambdaIndex;
            U8**    InstPtr;
        };
    };

    enum class ExprKind
    {
        Other,
        Logical,
        Comparison,
    };

    struct GenStatus
    {
        ExprKind    kind;
        bool        discarded;
        bool        tailRet;

        Declaration*    baseDecl;
        int32_t         offset;
        bool            spilledAddr;
    };

    struct GenConfig
    {
        PatchChain* trueChain;
        PatchChain* falseChain;
        bool invert;
        bool discard;
        bool calcAddr;
        PatchChain* breakChain;
        PatchChain* nextChain;

        static GenConfig Discard()
        {
            GenConfig config = { nullptr, nullptr, false, true };
            return config;
        }

        static GenConfig Statement( bool discard = false )
        {
            GenConfig config = { nullptr, nullptr, false, discard };
            return config;
        }

        static GenConfig Expr( PatchChain* trueChain, PatchChain* falseChain, bool invert )
        {
            GenConfig config = { trueChain, falseChain, invert };
            return config;
        }

        GenConfig Invert() const
        {
            GenConfig config = { trueChain, falseChain, !invert, discard };
            return config;
        }

        GenConfig WithFalse( PatchChain* chain ) const
        {
            GenConfig config = { trueChain, chain, invert };
            return config;
        }

        GenConfig WithTrue( PatchChain* chain ) const
        {
            GenConfig config = { chain, falseChain, invert };
            return config;
        }

        GenConfig WithLoop( PatchChain* breakChain, PatchChain* nextChain ) const
        {
            GenConfig config = *this;
            config.breakChain = breakChain;
            config.nextChain = nextChain;
            return config;
        }

        GenConfig WithDiscard() const
        {
            GenConfig config = *this;
            config.discard = true;
            return config;
        }
    };

    typedef void (Compiler::*ConjClauseGenerator)( Syntax* elem, const GenConfig& config );

    struct ConjSpec
    {
        ConjClauseGenerator PositiveGenerator;
        ConjClauseGenerator NegativeGenerator;
    };

    typedef std::vector<DeferredLambda> LambdaVec;
    typedef std::vector<AddrRef> AddrRefVec;
    typedef std::vector<Unique<Unit>> UnitVec;
    typedef std::vector<std::shared_ptr<ModuleDeclaration>> ModVec;

    using GlobalVec = std::vector<I32>;

    struct GenParams
    {
        const GenConfig& config;
        GenStatus& status;
    };

    U8*             mCodeBin;
    U8*             mCodeBinPtr;
    U8*             mCodeBinEnd;
    GlobalVec       mGlobals;

    SymTable        mGlobalTable;
    SymTable        mModuleTable;
    SymTable        mPublicTable;
    PatchMap        mPatchMap;
    LambdaVec       mLambdas;
    AddrRefVec      mLocalAddrRefs;
    bool            mInFunc = false;
    Function*       mCurFunc = nullptr;
    int16_t         mCurExprDepth = 0;
    int16_t         mMaxExprDepth = 0;

    ICompilerEnv*   mEnv = nullptr;
    Reporter        mRep;
    int             mModIndex = 0;

    std::vector<GenParams> mGenStack;

    bool            mCompiled = false;
    bool            mCalculatedStats = false;
    CompilerStats   mStats = {};
    UnitVec         mUnits;

    std::shared_ptr<Declaration> mLoadedAddrDecl;

public:
    Compiler( U8* codeBin, int codeBinLen, ICompilerEnv* env, ICompilerLog* log, int modIndex = 0 );

    void AddUnit( Unique<Unit>&& unit );
    void AddModule( std::shared_ptr<ModuleDeclaration> moduleDecl );
    CompilerErr Compile();

    void GetStats( CompilerStats& stats );
    I32* GetData();
    size_t GetDataSize();
    std::shared_ptr<ModuleDeclaration> GetMetadata( const char* modName );

private:
    void BindAttributes();
    void FoldConstants();
    void GenerateCode();

    // Code generation

    const GenConfig& Config() const;
    GenStatus& Status();

    // Level 1
    void Generate( Syntax* elem );
    void Generate( Syntax* elem, const GenConfig& config );
    void Generate( Syntax* elem, const GenConfig& config, GenStatus& status );
    void GenerateDiscard( Syntax* elem );
    void GenerateDiscard( Syntax* elem, const GenConfig& config );

    // Level 2 - S-expressions
    void GenerateNumber( NumberExpr* number, const GenConfig& config, GenStatus& status );
    void GenerateSymbol( NameExpr* symbol, const GenConfig& config, GenStatus& status );
    void GenerateValue( Syntax* node, Declaration *decl, const GenConfig& config, GenStatus& status );
    void GenerateEvalStar( CallOrSymbolExpr* callOrSymbol, const GenConfig& config, GenStatus& status );
    void GenerateArefAddr( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status );
    void GenerateAref( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status );
    void GenerateDefvar( VarDecl* varDecl, const GenConfig& config, GenStatus& status );
    void GenerateGlobalInit( int32_t offset, Syntax* initializer );

    void CalcAddress( Syntax* dotExpr, Declaration*& baseDecl, int32_t& offset );

    void AddGlobalData( U32 offset, Syntax* valueElem );
    void AddGlobalDataArray( int32_t offset, Syntax* valueElem, size_t size );

    void EmitLoadConstant( int32_t value );
    void EmitLoadAddress( Syntax* node, Declaration* baseDecl, I32 offset );
    void EmitFuncAddress( Function* func, uint8_t*& dstPtr );
    void EmitLoadScalar( Syntax* node, Declaration* decl, int32_t offset );
    void EmitStoreScalar( Syntax* node, Declaration* decl, int32_t offset );
    void EmitSpilledAddrOffset( int32_t offset );

    // Level 3 - functions and special operators
    void GenerateArithmetic( BinaryExpr* binary, const GenConfig& config, GenStatus& status );
    void GenerateComparison( BinaryExpr* binary, const GenConfig& config, GenStatus& status );
    void GenerateAnd( BinaryExpr* binary, const GenConfig& config, GenStatus& status );
    void GenerateOr( BinaryExpr* binary, const GenConfig& config, GenStatus& status );
    void GenerateReturn( ReturnStatement* retStmt, const GenConfig& config, GenStatus& status );
    void GenerateCond( CondExpr* condExpr, const GenConfig& config, GenStatus& status );
    void GenerateSet( AssignmentExpr* assignment, const GenConfig& config, GenStatus& status );
    void GenerateLambda( LambdaExpr* lambdaExpr, const GenConfig& config, GenStatus& status );
    void GenerateFunction( AddrOfExpr* addrOf, const GenConfig& config, GenStatus& status );
    void GenerateFuncall( CallExpr* call, const GenConfig& config, GenStatus& status );
    void GenerateLet( LetStatement* letStmt, const GenConfig& config, GenStatus& status );
    void GenerateLetBinding( DataDecl* binding );
    void GenerateLocalInit( int32_t offset, Syntax* initializer );
    void AddLocalDataArray( int32_t offset, Syntax* valueElem, size_t size );

    void GenerateCall( CallExpr* call, const GenConfig& config, GenStatus& status );
    void GenerateCall( Declaration* decl, std::vector<Unique<Syntax>>& arguments, const GenConfig& config, GenStatus& status );
    void GenerateCallArgs( std::vector<Unique<Syntax>>& arguments );
    void GenerateFor( ForStatement* forStmt, const GenConfig& config, GenStatus& status );
    void GenerateSimpleLoop( LoopStatement* loopStmt, const GenConfig& config, GenStatus& status );
    void GenerateDo( WhileStatement* whileStmt, const GenConfig& config, GenStatus& status );
    void GenerateBreak( BreakStatement* breakStmt, const GenConfig& config, GenStatus& status );
    void GenerateNext( NextStatement* nextStmt, const GenConfig& config, GenStatus& status );
    void GenerateCase( CaseExpr* caseExpr, const GenConfig& config, GenStatus& status );
    void GenerateGeneralCase( CaseExpr* caseExpr, const GenConfig& config, GenStatus& status );

    void GenerateUnaryPrimitive( Syntax* elem, const GenConfig& config, GenStatus& status );
    void GenerateBinaryPrimitive( BinaryExpr* binary, int primitive, const GenConfig& config, GenStatus& status );

    void GenerateLambdas();
    void GenerateProc( ProcDecl* procDecl, Function* func );
    void GenerateImplicitProgn( StatementList* stmtList, const GenConfig& config, GenStatus& status );
    void GenerateStatements( StatementList* list, const GenConfig& config, GenStatus& status );
    void GenerateNilIfNeeded( const GenConfig& config, GenStatus& status );

    void GenerateSentinel();

    // And and Or
    void GenerateConj( ConjSpec* spec, BinaryExpr* binary, const GenConfig& config );
    void GenerateAndClause( Syntax* elem, const GenConfig& config );
    void GenerateOrClause( Syntax* elem, const GenConfig& config );
    void Atomize( ConjSpec* spec, BinaryExpr* binary, bool invert, bool discard );

    // And and Or plumbing
    void ElideTrue( PatchChain* trueChain, PatchChain* falseChain );
    void ElideFalse( PatchChain* trueChain, PatchChain* falseChain );
    U8 InvertJump( U8 opCode );

    // Backpatching
    void Patch( PatchChain* chain, U8* targetPtr = nullptr );
    void PatchCalls( PatchChain* chain, U32 addr );
    void PushPatch( PatchChain* chain, U8* patchPtr );
    void PushPatch( PatchChain* chain );
    void PopPatch( PatchChain* chain );
    PatchChain* PushFuncPatch( const std::string& name, U8* patchPtr );

    I32 GetSyntaxValue( Syntax* node, const char* message = nullptr );

    // Stack usage
    void IncreaseExprDepth();
    void DecreaseExprDepth( int amount = 1 );
    void CalculateStackDepth();
    void CalculateStackDepth( Function* func );


    // IVisitor
    virtual void VisitAddrOfExpr( AddrOfExpr* addrOf ) override;
    virtual void VisitAssignmentExpr( AssignmentExpr* assignment ) override;
    virtual void VisitBinaryExpr( BinaryExpr* binary ) override;
    virtual void VisitBreakStatement( BreakStatement* breakStmt ) override;
    virtual void VisitCallExpr( CallExpr* call ) override;
    virtual void VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol ) override;
    virtual void VisitCaseExpr( CaseExpr* caseExpr ) override;
    virtual void VisitCondExpr( CondExpr* condExpr ) override;
    virtual void VisitCountofExpr( CountofExpr* countofExpr ) override;
    virtual void VisitDotExpr( DotExpr* dotExpr ) override;
    virtual void VisitForStatement( ForStatement* forStmt ) override;
    virtual void VisitIndexExpr( IndexExpr* indexExpr ) override;
    virtual void VisitLambdaExpr( LambdaExpr* lambdaExpr ) override;
    virtual void VisitLetStatement( LetStatement* letStmt ) override;
    virtual void VisitLoopStatement( LoopStatement* loopStmt ) override;
    virtual void VisitNameExpr( NameExpr* nameExpr ) override;
    virtual void VisitNextStatement( NextStatement* nextStmt ) override;
    virtual void VisitNumberExpr( NumberExpr* numberExpr ) override;
    virtual void VisitProcDecl( ProcDecl* procDecl ) override;
    virtual void VisitReturnStatement( ReturnStatement* retStmt ) override;
    virtual void VisitSliceExpr( SliceExpr* sliceExpr ) override;
    virtual void VisitStatementList( StatementList* stmtList ) override;
    virtual void VisitUnaryExpr( UnaryExpr* unary ) override;
    virtual void VisitUnit( Unit* unit ) override;
    virtual void VisitVarDecl( VarDecl* varDecl ) override;
    virtual void VisitWhileStatement( WhileStatement* whileStmt ) override;
};
