#pragma once

#include "Syntax.h"
#include "Compiler.h"


class BinderVisitor : public IVisitor
{
    using SymStack = std::vector<SymTable*>;

    // This relies on the syntax nodes sticking around outside of its control.
    // But since shared_ptr is used, instead of internal ref counts,
    // this is not robust.

    using LambdaVec = std::vector<LambdaExpr*>;

    friend class LocalScope;

    LambdaVec       mLambdas;
    SymStack        mSymStack;
    SymTable&       mGlobalTable;
    SymTable&       mModuleTable;
    SymTable&       mPublicTable;
    ICompilerEnv*   mEnv = nullptr;
    Reporter        mRep;

    Function*       mCurFunc = nullptr;

    int             mModIndex = 0;
    int             mCurLevelLocalCount = 0;
    int             mCurLocalCount = 0;
    int             mMaxLocalCount = 0;
    int             mGlobalSize = 0;
    int             mNextNativeId = 0;

    std::shared_ptr<TypeType>   mTypeType;
    std::shared_ptr<ModuleType> mModuleType;
    std::shared_ptr<XferType>   mXferType;
    std::shared_ptr<IntType>    mIntType;

public:
    BinderVisitor(
        int modIndex,
        SymTable& globalTable,
        SymTable& moduleTable,
        SymTable& publicTable,
        ICompilerEnv* env,
        ICompilerLog* log );

    void Declare( Unit* unit );
    void Bind( Unit* unit );

    size_t GetDataSize();

    // IVisitor
    virtual void VisitAddrOfExpr( AddrOfExpr* addrOf ) override;
    virtual void VisitArrayTypeRef( ArrayTypeRef* typeRef ) override;
    virtual void VisitAssignmentExpr( AssignmentExpr* assignment ) override;
    virtual void VisitBinaryExpr( BinaryExpr* binary ) override;
    virtual void VisitBreakStatement( BreakStatement* breakStmt ) override;
    virtual void VisitCallExpr( CallExpr* call ) override;
    virtual void VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol ) override;
    virtual void VisitCaseExpr( CaseExpr* caseExpr ) override;
    virtual void VisitCondExpr( CondExpr* condExpr ) override;
    virtual void VisitConstDecl( ConstDecl* constDecl ) override;
    virtual void VisitDotExpr( DotExpr* dotExpr ) override;
    virtual void VisitForStatement( ForStatement* forStmt ) override;
    virtual void VisitImportDecl( ImportDecl* importDecl ) override;
    virtual void VisitIndexExpr( IndexExpr* indexExpr ) override;
    virtual void VisitInitList( InitList* initList ) override;
    virtual void VisitLambdaExpr( LambdaExpr* lambdaExpr ) override;
    virtual void VisitLetStatement( LetStatement* letStmt ) override;
    virtual void VisitLoopStatement( LoopStatement* loopStmt ) override;
    virtual void VisitNameExpr( NameExpr* nameExpr ) override;
    virtual void VisitNameTypeRef( NameTypeRef* nameTypeRef ) override;
    virtual void VisitNativeDecl( NativeDecl* nativeDecl ) override;
    virtual void VisitNextStatement( NextStatement* nextStmt ) override;
    virtual void VisitNumberExpr( NumberExpr* numberExpr ) override;
    virtual void VisitParamDecl( ParamDecl* paramDecl ) override;
    virtual void VisitPointerTypeRef( PointerTypeRef* pointerTypeRef ) override;
    virtual void VisitProcDecl( ProcDecl* procDecl ) override;
    virtual void VisitProcTypeRef( ProcTypeRef* procTypeRef ) override;
    virtual void VisitReturnStatement( ReturnStatement* retStmt ) override;
    virtual void VisitStatementList( StatementList* stmtList ) override;
    virtual void VisitUnaryExpr( UnaryExpr* unary ) override;
    virtual void VisitUnit( Unit* unit ) override;
    virtual void VisitVarDecl( VarDecl* varDecl ) override;
    virtual void VisitWhileStatement( WhileStatement* whileStmt ) override;

private:
    void BindLambdas();

    void VisitProc( ProcDecl* procDecl );
    void VisitLetBinding( DataDecl* varDecl );
    void VisitStorage( DataDecl* varDecl, DeclKind declKind );
    std::shared_ptr<Type> VisitParamTypeRef( Unique<TypeRef>& typeRef );

    I32 Evaluate( Syntax* node, const char* message = nullptr );
    void CheckType(
        const std::shared_ptr<Type>& left,
        const std::shared_ptr<Type>& right,
        Syntax* node );
    void CheckType(
        Type* site,
        Type* type,
        Syntax* node );
    void CheckAssignableType( Syntax* node );
    void CheckAndConsolidateClauseType( StatementList& clause, std::shared_ptr<Type>& bodyType );
    void CheckAndConsolidateClauseType( Syntax* clause, std::shared_ptr<Type>& bodyType );

    // Symbol table
    std::shared_ptr<Declaration> FindSymbol( const std::string& symbol );
    std::shared_ptr<Storage> AddArg( const std::string& name );
    std::shared_ptr<Storage> AddLocal( SymTable& table, const std::string& name, int offset );
    std::shared_ptr<Storage> AddLocal( const std::string& name, size_t size );
    std::shared_ptr<Storage> AddGlobal( const std::string& name, size_t size );
    std::shared_ptr<Storage> AddStorage( const std::string& name, size_t size, DeclKind declKind );
    std::shared_ptr<Constant> AddConst( const std::string& name, int32_t value, bool isPublic );
    std::shared_ptr<Function> AddFunc( const std::string& name, int address );
    std::shared_ptr<Function> AddForward( const std::string& name );
    std::shared_ptr<TypeDeclaration> AddType( const std::string& name, std::shared_ptr<Type> type );
    void AddModule( const std::string& name, std::shared_ptr<ModuleDeclaration> moduleDecl );
    void CheckDuplicateGlobalSymbol( const std::string& name );

    void MakeStdEnv();
    void BindProcs( Unit* program );
    void BindNamedProc( ProcDecl* procDecl );

    void DeclareNode( DeclSyntax* node );
    std::shared_ptr<Declaration> DefineNode( const std::string& name, UndefinedDeclaration* decl );
    std::shared_ptr<FuncType> MakeFuncType( ProcDeclBase* procDecl );
};
