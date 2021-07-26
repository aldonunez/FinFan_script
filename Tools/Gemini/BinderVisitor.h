// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "LangCommon.h"
#include "Syntax.h"


namespace Gemini
{

class BinderVisitor final : public Visitor
{
    using SymStack = std::vector<SymTable*>;
    using LambdaVec = std::vector<Unique<ProcDecl>>;
    using NatTypeMap = std::map<int32_t, std::shared_ptr<Type>>;

    friend class LocalScope;
    friend class BorrowedScope;

    Unique<Syntax>  mReplacementNode;
    LambdaVec       mLambdas;
    SymStack        mSymStack;
    SymTable&       mGlobalTable;
    SymTable&       mModuleTable;
    SymTable&       mPublicTable;
    NatTypeMap      mNativeTypeMap;
    Reporter        mRep;

    Function*       mCurFunc = nullptr;
    bool            mInGlobalVarDef = false;

    ModSize         mModIndex = 0;
    LocalSize       mCurLevelLocalCount = 0;
    LocalSize       mCurLocalCount = 0;
    LocalSize       mMaxLocalCount = 0;
    ParamSize       mParamCount = 0;
    GlobalSize      mGlobalSize = 0;
    GlobalSize      mConstSize = 0;
    size_t          mTotalLambdas = 0;
    int32_t         mPrevNativeId = -1;

    std::shared_ptr<ErrorType>  mErrorType;
    std::shared_ptr<TypeType>   mTypeType;
    std::shared_ptr<ModuleType> mModuleType;
    std::shared_ptr<XferType>   mXferType;
    std::shared_ptr<IntType>    mIntType;

    CompilerAttrs&                  mGlobalAttrs;
    std::shared_ptr<ModuleAttrs>    mModuleAttrs;

public:
    BinderVisitor(
        ModSize modIndex,
        SymTable& globalTable,
        SymTable& moduleTable,
        SymTable& publicTable,
        CompilerAttrs& globalAttrs,
        ICompilerLog* log );

    void Declare( Unit* unit );
    void BindDeclarations( Unit* unit );
    void BindFunctionBodies( Unit* unit );

    size_t GetDataSize();
    size_t GetConstSize();
    std::shared_ptr<ModuleAttrs> GetModuleAttrs();

    // Visitor
    virtual void VisitAddrOfExpr( AddrOfExpr* addrOf ) override;
    virtual void VisitArrayTypeRef( ArrayTypeRef* typeRef ) override;
    virtual void VisitAsExpr( AsExpr* asExpr ) override;
    virtual void VisitAssignmentExpr( AssignmentExpr* assignment ) override;
    virtual void VisitBinaryExpr( BinaryExpr* binary ) override;
    virtual void VisitBreakStatement( BreakStatement* breakStmt ) override;
    virtual void VisitCallExpr( CallExpr* call ) override;
    virtual void VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol ) override;
    virtual void VisitCaseExpr( CaseExpr* caseExpr ) override;
    virtual void VisitCondExpr( CondExpr* condExpr ) override;
    virtual void VisitConstDecl( ConstDecl* constDecl ) override;
    virtual void VisitCountofExpr( CountofExpr* countofExpr ) override;
    virtual void VisitDotExpr( DotExpr* dotExpr ) override;
    virtual void VisitEnumTypeRef( EnumTypeRef* enumTypeRef ) override;
    virtual void VisitFieldDecl( FieldDecl* fieldDecl ) override;
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
    virtual void VisitRecordInitializer( RecordInitializer* recordInitializer ) override;
    virtual void VisitRecordTypeRef( RecordTypeRef* recordTypeRef ) override;
    virtual void VisitReturnStatement( ReturnStatement* retStmt ) override;
    virtual void VisitSliceExpr( SliceExpr* sliceExpr ) override;
    virtual void VisitStatementList( StatementList* stmtList ) override;
    virtual void VisitTypeDecl( TypeDecl* typeDecl ) override;
    virtual void VisitUnaryExpr( UnaryExpr* unary ) override;
    virtual void VisitUnit( Unit* unit ) override;
    virtual void VisitVarDecl( VarDecl* varDecl ) override;
    virtual void VisitWhileStatement( WhileStatement* whileStmt ) override;
    virtual void VisitYieldStatement( YieldStatement* yieldStmt ) override;

private:
    void Visit( Unique<Syntax>& child );

    void BindLambdas( Unit* unit );

    void RewriteCaseWithComplexKey( CaseExpr* caseExpr );

    void VisitProc( ProcDecl* procDecl );
    void VisitLetBinding( DataDecl* varDecl );
    void VisitConstBinding( ConstDecl* constDecl, ScopeKind scopeKind );
    void VisitStorage( DataDecl* varDecl, DeclKind declKind );
    ParamSpec VisitParamTypeRef( Unique<TypeRef>& typeRef, ParamModifier modifier );

    int32_t EvaluateInt( Syntax* node, const char* message = nullptr );
    ValueVariant EvaluateVariant( Syntax* node );
    std::optional<int32_t> EvaluateOptionalInt( Syntax* node );

    void EmitFuncAddress( std::optional<std::shared_ptr<Function>> optFunc, GlobalSize offset, int32_t* buffer, Syntax* valueNode );
    void CopyConstAggregateBlock( GlobalSize offset, int32_t* buffer, Syntax* valueNode );

    void CheckType(
        const std::shared_ptr<Type>& left,
        const std::shared_ptr<Type>& right,
        Syntax* node );
    void CheckType(
        Type* site,
        Type* type,
        Syntax* node );
    void CheckStatementType( Syntax* node );
    void CheckArgument(
        ParamMode mode,
        const std::shared_ptr<Type>& site,
        Syntax* argNode );
    void CheckAndConsolidateClauseType( StatementList& clause, std::shared_ptr<Type>& bodyType );
    void CheckAndConsolidateClauseType( Syntax* clause, std::shared_ptr<Type>& bodyType );
    void CheckStorageType(
        const std::shared_ptr<Type>& type,
        Syntax* node );
    void CheckConstType( Type& type, Syntax* node );
    void CheckInitializer(
        const std::shared_ptr<Type>& type,
        Unique<Syntax>& initializer );
    void CheckMissingRecordInitializer( const Unique<Syntax>& initializer );
    void CheckAllDescendantsHaveDefault( Type* type, Syntax* node );
    DataSize CheckArraySize( size_t rawSize, Type* elemType, Syntax* node );

    void ForbidExternalGlobalInGlobalInit( Declaration& decl, Syntax* node );

    // Symbol table
    std::shared_ptr<Declaration> FindSymbol( const std::string& symbol );
    std::shared_ptr<ParamStorage> AddParam( DeclSyntax* declNode, ParamSpec paramSpec );
    std::shared_ptr<LocalStorage> AddLocal( DeclSyntax* declNode, std::shared_ptr<Type> type, size_t size );
    std::shared_ptr<GlobalStorage> AddGlobal( DeclSyntax* declNode, std::shared_ptr<Type> type, size_t size );
    std::shared_ptr<Declaration> AddStorage( DeclSyntax* declNode, std::shared_ptr<Type> type, size_t size, DeclKind declKind );
    std::shared_ptr<Constant> AddConst( DeclSyntax* declNode, std::shared_ptr<Type> type, ValueVariant value, SymTable& table );
    std::shared_ptr<Constant> AddConst( DeclSyntax* declNode, std::shared_ptr<Type> type, ValueVariant value, bool isPublic );
    std::shared_ptr<Function> AddFunc( DeclSyntax* declNode, bool isPublic );
    std::shared_ptr<TypeDeclaration> AddType( DeclSyntax* declNode, std::shared_ptr<Type> type, bool isPublic );
    void AddModule( DeclSyntax* declNode, std::shared_ptr<ModuleDeclaration> moduleDecl );
    void CheckDuplicateGlobalSymbol( DeclSyntax* declNode );
    void CheckDuplicateSymbol( DeclSyntax* declNode, const SymTable& table );

    void MakeStdEnv();
    void BindProcs( Unit* program );
    void BindNamedProc( ProcDecl* procDecl );

    void DeclareNode( DeclSyntax* node );
    void PrepareToDefine( DeclSyntax* declNode );
    std::shared_ptr<Declaration> DefineNode( const std::string& name, UndefinedDeclaration* decl );
    std::shared_ptr<FuncType> MakeFuncType( ProcDeclBase* procDecl );
    std::shared_ptr<Type> VisitFuncReturnType( Unique<TypeRef>& typeRef );

    ParamSize GetParamSize( Type* type, ParamMode mode );
};

}
