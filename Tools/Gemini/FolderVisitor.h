#pragma once

#include "Syntax.h"
#include "Compiler.h"
#include <optional>


class FolderVisitor : public IVisitor
{
    Reporter                mRep;
    std::optional<int32_t>  mLastValue;
    bool                    mFoldNodes;

public:
    FolderVisitor( ICompilerLog* log );

    std::optional<int32_t> Evaluate( Syntax* node );
    void Fold( Syntax* node );

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
    void VisitProc( ProcDecl* procDecl );
    void VisitLetBinding( DataDecl* varDecl );

    void Fold( Unique<Syntax>& child );
};
