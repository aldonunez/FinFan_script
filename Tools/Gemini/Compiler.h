#pragma once

#include "GeminiCommon.h"
#include <string>
#include <vector>
#include <memory>
#include <map>
#include <optional>
#include <unordered_map>


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
    virtual void Add( LogCategory category, int line, int column, const char* message ) = 0;
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


class Compiler
{
public:
    class CompilerException : public std::exception
    {
        CompilerErr     mError;

    public:
        CompilerException( CompilerErr error )
            :   mError( error )
        {
        }

        CompilerErr GetError() const
        {
            return mError;
        }
    };

    enum ElementCode
    {
        Elem_Slist,
        Elem_Number,
        Elem_Symbol,
    };

    struct Element
    {
        ElementCode Code;
        int         Line;
        int         Column;
        virtual ~Element() { }
    };

    using ElementVector = std::vector<std::unique_ptr<Element>>;

    struct Slist : public Element
    {
        ElementVector Elements;
    };

    struct Number : public Element
    {
        int Value;
    };

    struct Symbol : public Element
    {
        std::string String;
    };

private:
    friend class LocalScope;

    struct InstPatch
    {
        InstPatch*  Next;
        U8*         Inst;
    };

    struct PatchChain
    {
        InstPatch*  First;

        PatchChain()
            :   First( nullptr )
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
    };

    enum DeclKind
    {
        Decl_Const,
        Decl_Global,
        Decl_Local,
        Decl_Arg,
        Decl_Func,
        Decl_Forward,
    };

    struct Declaration
    {
        DeclKind  Kind;
        virtual ~Declaration() { }
    };

    struct ConstDecl : public Declaration
    {
        int Value;
    };

    struct Storage : public Declaration
    {
        int Offset;
    };

    struct Function : public Declaration
    {
        std::string Name;
        int         Address;
        PatchChain  Patches;

        int16_t     LocalCount;
        int16_t     ArgCount;
        int16_t     ExprDepth;

        int16_t     CallDepth;
        int16_t     IndividualStackUsage;
        int16_t     TreeStackUsage;

        bool        IsCalculating;
        bool        IsRecursive;
        bool        IsDepthKnown;
        bool        CallsIndirectly;

        std::list<std::string> CalledFunctions;
    };

    struct DeferredLambda
    {
        Slist*  Definition;
        U8*     Patch;
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

    enum ExprKind
    {
        Expr_Other,
        Expr_Logical,
        Expr_Comparison,
    };

    struct GenStatus
    {
        ExprKind    kind;
        bool        discarded;
        bool        tailRet;
    };

    struct GenConfig
    {
        PatchChain* trueChain;
        PatchChain* falseChain;
        bool invert;
        bool discard;
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

    typedef void (Compiler::*ConjClauseGenerator)( Element* elem, const GenConfig& config );

    struct ConjSpec
    {
        ConjClauseGenerator PositiveGenerator;
        ConjClauseGenerator NegativeGenerator;
    };

    typedef std::map<std::string, std::unique_ptr<Declaration>> SymTable;
    typedef std::vector<SymTable*> SymStack;
    typedef std::vector<DeferredLambda> LambdaVec;
    typedef std::vector<AddrRef> AddrRefVec;

    using GlobalVec = std::vector<I32>;

    typedef void (Compiler::*CallGenerator)( Slist* list, const GenConfig& config, GenStatus& status );
    typedef std::unordered_map<std::string, CallGenerator> GeneratorMap;


    U8*             mCodeBin;
    U8*             mCodeBinPtr;
    U8*             mCodeBinEnd;
    GlobalVec       mGlobals;

    SymTable        mConstTable;
    SymTable        mGlobalTable;
    SymStack        mSymStack;
    LambdaVec       mLambdas;
    AddrRefVec      mLocalAddrRefs;
    int             mCurLevelLocalCount;
    int             mCurLocalCount;
    int             mMaxLocalCount;
    int             mForwards;
    bool            mInFunc;
    Function*       mCurFunc;
    int16_t         mCurExprDepth;
    int16_t         mMaxExprDepth;

    ICompilerEnv*   mEnv;
    ICompilerLog*   mLog;
    int             mModIndex;

    GeneratorMap    mGeneratorMap;

    bool            mCompiled;
    bool            mCalculatedStats;
    CompilerStats   mStats;

public:
    Compiler( U8* codeBin, int codeBinLen, ICompilerEnv* env, ICompilerLog* log, int modIndex = 0 );

    CompilerErr Compile( Slist* progTree );
    void GetStats( CompilerStats& stats );
    I32* GetData();
    size_t GetDataSize();

private:
    // Code generation

    // Level 1
    void Generate( Element* elem );
    void Generate( Element* elem, const GenConfig& config );
    void Generate( Element* elem, const GenConfig& config, GenStatus& status );
    void GenerateDiscard( Element* elem );
    void GenerateDiscard( Element* elem, const GenConfig& config );

    // Level 2 - S-expressions
    void GenerateNumber( Number* number, const GenConfig& config, GenStatus& status );
    void GenerateSymbol( Symbol* symbol, const GenConfig& config, GenStatus& status );
    void GenerateSlist( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateEvalStar( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateAref( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateArrayElementRef( Slist* list );
    void GenerateDefvar( Slist* list, const GenConfig& config, GenStatus& status );

    void AddGlobalData( U32 offset, Element* valueElem );
    void AddGlobalDataArray( Storage* global, Element* valueElem, size_t size );

    void EmitLoadConstant( int32_t value );

    // Level 3 - functions and special operators
    void GenerateArithmetic( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateNegate( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateComparison( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateNot( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateAnd( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateOr( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateReturn( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateIf( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateCond( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateProgn( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateSet( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateDefun( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateLambda( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateFunction( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateFuncall( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateLet( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateLetBinding( Slist* localList );
    void AddLocalDataArray( Storage* global, Element* valueElem, size_t size );

    void GenerateCall( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateLoop( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateFor( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateSimpleLoop( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateDo( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateBreak( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateNext( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateCase( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateGeneralCase( Slist* list, const GenConfig& config, GenStatus& status );

    void GenerateUnaryPrimitive( Element* elem, const GenConfig& config, GenStatus& status );
    void GenerateBinaryPrimitive( Slist* list, int primitive, const GenConfig& config, GenStatus& status );

    void GenerateLambdas();
    void GenerateProc( Slist* list, int startIndex, Function* func );
    void GenerateImplicitProgn( Slist* list, int startIndex, const GenConfig& config, GenStatus& status );
    void GenerateStatements( Slist* list, size_t startIndex, const GenConfig& config, GenStatus& status );
    void GenerateStatements( Slist* list, size_t startIndex, size_t endIndex, const GenConfig& config, GenStatus& status );
    void GenerateNilIfNeeded( const GenConfig& config, GenStatus& status );

    void GenerateSentinel();

    // And and Or
    void GenerateConj( ConjSpec* spec, Slist* list, const GenConfig& config );
    void GenerateAndClause( Element* elem, const GenConfig& config );
    void GenerateOrClause( Element* elem, const GenConfig& config );
    void Atomize( ConjSpec* spec, Slist* list, bool invert, bool discard );

    // And and Or plumbing
    void ElideTrue( PatchChain* trueChain, PatchChain* falseChain );
    void ElideFalse( PatchChain* trueChain, PatchChain* falseChain );
    U8 InvertJump( U8 opCode );

    // Backpatching
    void Patch( PatchChain* chain, U8* targetPtr = nullptr );
    void PatchCalls( PatchChain* chain, U32 addr );
    void PushPatch( PatchChain* chain );
    void PopPatch( PatchChain* chain );

    // Symbol table
    Declaration* FindSymbol( const std::string& symbol );
    Storage* AddArg( SymTable& table, const std::string& name, int offset );
    Storage* AddLocal( SymTable& table, const std::string& name, int offset );
    Storage* AddLocal( const std::string& name, size_t size );
    Storage* AddGlobal( const std::string& name, size_t size );
    Function* AddFunc( const std::string& name, int address );
    Function* AddForward( const std::string& name );
    ConstDecl* AddConst( const std::string& name, int value );
    void MakeStdEnv();
    void CollectFunctionForwards( Slist* program );

    void MatchSymbol( Element* elem, const char* name, const char* message = nullptr );
    I32 GetElementValue( Element* elem, const char* message = nullptr );
    std::optional<I32> GetOptionalElementValue( Element* elem );

    // Stack usage
    void IncreaseExprDepth();
    void DecreaseExprDepth( int amount = 1 );
    void CalculateStackDepth();
    void CalculateStackDepth( Function* func );

    [[noreturn]] void ThrowError( CompilerErr exceptionCode, Element* elem, const char* format, ... );
    [[noreturn]] void ThrowError( CompilerErr exceptionCode, int line, int col, const char* format, va_list args );
    [[noreturn]] void ThrowInternalError();
    [[noreturn]] void ThrowInternalError( const char* format, ... );
    [[noreturn]] void ThrowUnresolvedFuncsError();

    void Log( LogCategory category, int line, int col, const char* format, va_list args );
    void LogWarning( int line, int col, const char* format, ... );
};


void Log( ICompilerLog* log, LogCategory category, int line, int col, const char* format, va_list args );
