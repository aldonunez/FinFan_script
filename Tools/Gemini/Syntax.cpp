#include "stdafx.h"
#include "Syntax.h"


void AddrOfExpr::Accept( IVisitor* visitor )
{
    visitor->VisitAddrOfExpr( this );
}

void ArrayTypeRef::Accept( IVisitor* visitor )
{
    visitor->VisitArrayTypeRef( this );
}

void AssignmentExpr::Accept( IVisitor* visitor )
{
    visitor->VisitAssignmentExpr( this );
}

void BinaryExpr::Accept( IVisitor* visitor )
{
    visitor->VisitBinaryExpr( this );
}

void BreakStatement::Accept( IVisitor* visitor )
{
    visitor->VisitBreakStatement( this );
}

void CallExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCallExpr( this );
}

void CallOrSymbolExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCallOrSymbolExpr( this );
}

void CaseExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCaseExpr( this );
}

void CondExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCondExpr( this );
}

void ForStatement::Accept( IVisitor* visitor )
{
    visitor->VisitForStatement( this );
}

void IndexExpr::Accept( IVisitor* visitor )
{
    visitor->VisitIndexExpr( this );
}

void InitList::Accept( IVisitor* visitor )
{
    visitor->VisitInitList( this );
}

void LambdaExpr::Accept( IVisitor* visitor )
{
    visitor->VisitLambdaExpr( this );
}

void LetStatement::Accept( IVisitor* visitor )
{
    visitor->VisitLetStatement( this );
}

void LoopStatement::Accept( IVisitor* visitor )
{
    visitor->VisitLoopStatement( this );
}

void NameExpr::Accept( IVisitor* visitor )
{
    visitor->VisitNameExpr( this );
}

void NextStatement::Accept( IVisitor* visitor )
{
    visitor->VisitNextStatement( this );
}

void NumberExpr::Accept( IVisitor* visitor )
{
    visitor->VisitNumberExpr( this );
}

void ParamDecl::Accept( IVisitor* visitor )
{
    visitor->VisitParamDecl( this );
}

void ProcDecl::Accept( IVisitor* visitor )
{
    visitor->VisitProcDecl( this );
}

void ReturnStatement::Accept( IVisitor* visitor )
{
    visitor->VisitReturnStatement( this );
}

void StatementList::Accept( IVisitor* visitor )
{
    visitor->VisitStatementList( this );
}

void UnaryExpr::Accept( IVisitor* visitor )
{
    visitor->VisitUnaryExpr( this );
}

void Unit::Accept( IVisitor* visitor )
{
    visitor->VisitUnit( this );
}

void VarDecl::Accept( IVisitor* visitor )
{
    visitor->VisitVarDecl( this );
}

void WhileStatement::Accept( IVisitor* visitor )
{
    visitor->VisitWhileStatement( this );
}
