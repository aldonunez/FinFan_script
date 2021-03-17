#pragma once

#include "GeminiCommon.h"


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
};

class ICompilerLog
{
public:
    virtual void Add( LogCategory category, int line, int column, const char* message ) = 0;
};

struct CompilerStats
{
    int     CodeBytesWritten;
};

class Compiler
{
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

    enum TokenCode
    {
        Token_Bof,
        Token_Eof,
        Token_LParen,
        Token_RParen,
        Token_Number,
        Token_Symbol,
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

    struct Slist : public Element
    {
        std::vector<std::unique_ptr<Element>> Elements;
    };

    struct Number : public Element
    {
        int Value;
    };

    struct Symbol : public Element
    {
        std::string String;
    };

    struct InstPatch
    {
        InstPatch*  Next;
        U8*         Inst;
    };

    struct PatchChain
    {
        InstPatch*  Next;

        PatchChain()
            :   Next( nullptr )
        {
        }

        ~PatchChain()
        {
            while ( Next != nullptr )
            {
                auto link = Next;
                Next = Next->Next;
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
    };

    struct DeferredLambda
    {
        Slist*  Definition;
        U8*     Patch;
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

    typedef void (Compiler::*CallGenerator)( Slist* list, const GenConfig& config, GenStatus& status );
    typedef std::unordered_map<std::string, CallGenerator> GeneratorMap;


    const char*     mCodeTextPtr;
    const char*     mCodeTextEnd;
    U8*             mCodeBin;
    U8*             mCodeBinPtr;
    U8*             mCodeBinEnd;
    int             mLine;
    const char*     mLineStart;

    TokenCode       mCurToken;
    std::string     mCurString;
    int             mCurNumber;
    int             mTokLine;
    int             mTokCol;

    SymTable        mConstTable;
    SymTable        mGlobalTable;
    SymStack        mSymStack;
    LambdaVec       mLambdas;
    int             mCurLocalCount;
    int             mMaxLocalCount;
    int             mForwards;
    bool            mInFunc;

    ICompilerEnv*   mEnv;
    ICompilerLog*   mLog;

    GeneratorMap    mGeneratorMap;

public:
    Compiler( const char* codeText, int codeTextLen, U8* codeBin, int codeBinLen, ICompilerEnv* env, 
        ICompilerLog* log );

    CompilerErr Compile();
    void GetStats( CompilerStats& stats );

private:
    // Scanning

    void SkipWhitespace();
    TokenCode NextToken();
    void ReadNumber();
    void ReadSymbol();
    int GetColumn();

    // Parsing

    Slist* Parse();
    Slist* ParseSlist();
    Number* ParseNumber();
    Symbol* ParseSymbol();

    // Code generation

    // Level 1
    void Generate( Element* elem );
    void Generate( Element* elem, const GenConfig& config );
    void Generate( Element* elem, const GenConfig& config, GenStatus& status );
    void GenerateDiscard( Element* elem );

    // Level 2 - S-expressions
    void GenerateNumber( Number* number, const GenConfig& config, GenStatus& status );
    void GenerateSymbol( Symbol* symbol, const GenConfig& config, GenStatus& status );
    void GenerateSlist( Slist* list, const GenConfig& config, GenStatus& status );

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
    void GenerateFuncall( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateLet( Slist* list, const GenConfig& config, GenStatus& status );
    void GenerateCall( Slist* list, const GenConfig& config, GenStatus& status );

    void GenerateUnaryPrimitive( Element* elem, const GenConfig& config, GenStatus& status );
    void GenerateBinaryPrimitive( Slist* list, int primitive, const GenConfig& config, GenStatus& status );

    void GenerateLambdas();
    void GenerateProc( Slist* list, int startIndex );
    void GenerateImplicitProgn( Slist* list, int startIndex, const GenConfig& config, GenStatus& status );

    // And and Or
    void GenerateConj( ConjSpec* spec, Slist* list, const GenConfig& config );
    void GenerateAndClause( Element* elem, const GenConfig& config );
    void GenerateOrClause( Element* elem, const GenConfig& config );
    void Atomize( ConjSpec* spec, Slist* list, bool invert );

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
    Function* AddFunc( const std::string& name, int address );
    Function* AddForward( const std::string& name );
    ConstDecl* AddConst( const std::string& name, int value );
    void MakeStdEnv();

    bool HasLocals( Element* elem );

    __declspec(noreturn) void ThrowSyntaxError( const char* format, ... );
    __declspec(noreturn) void ThrowError( CompilerErr exceptionCode, Element* elem, const char* format, ... );
    __declspec(noreturn) void ThrowInternalError();
    __declspec(noreturn) void ThrowUnresolvedFuncsError();
    void Log( LogCategory category, int line, int col, const char* format, va_list args );
};
