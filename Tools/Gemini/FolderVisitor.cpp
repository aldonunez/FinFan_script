#include "stdafx.h"
#include "FolderVisitor.h"


FolderVisitor::FolderVisitor( ICompilerLog* log ) :
    mFoldNodes( false ),
    mRep( log )
{
}

std::optional<int32_t> FolderVisitor::Evaluate( Syntax* node )
{
    mFoldNodes = false;
    node->Accept( this );

    return mLastValue;
}

void FolderVisitor::Fold( Syntax* node )
{
    mFoldNodes = true;
    node->Accept( this );
}

void FolderVisitor::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
    mLastValue.reset();
}

void FolderVisitor::VisitArrayTypeRef( ArrayTypeRef* typeRef )
{
    mLastValue.reset();
}

void FolderVisitor::VisitAssignmentExpr( AssignmentExpr* assignment )
{
    assignment->Left->Accept( this );
    Fold( assignment->Right );
    mLastValue.reset();
}

void FolderVisitor::VisitBinaryExpr( BinaryExpr* binary )
{
    Fold( binary->Left );

    std::optional<int32_t> leftOptVal = std::move( mLastValue );

    Fold( binary->Right );

    if ( leftOptVal.has_value() && mLastValue.has_value() )
    {
        int32_t left = leftOptVal.value();
        int32_t right = mLastValue.value();
        int32_t result = 0;

        if ( binary->Op == "+" )
            result = left + right;
        else if ( binary->Op == "-" )
            result = left - right;
        else if ( binary->Op == "*" )
            result = left * right;
        else if ( binary->Op == "/" )
        {
            if ( right == 0 )
                mRep.ThrowError( CERR_SEMANTICS, binary->Right.get(), "Division by 0" );

            result = left / right;
        }
        else if ( binary->Op == "%" )
        {
            if ( right == 0 )
                mRep.ThrowError( CERR_SEMANTICS, binary->Right.get(), "Division by 0" );

            result = left % right;
        }
        else if ( binary->Op == "=" )
            result = left == right;
        else if ( binary->Op == "<>" )
            result = left != right;
        else if ( binary->Op == "<" )
            result = left < right;
        else if ( binary->Op == "<=" )
            result = left <= right;
        else if ( binary->Op == ">" )
            result = left > right;
        else if ( binary->Op == ">=" )
            result = left >= right;
        else if ( binary->Op == "and" )
            result = left && right;
        else if ( binary->Op == "or" )
            result = left || right;
        else
            mRep.ThrowInternalError();

        mLastValue = result;
    }
    else
    {
        mLastValue.reset();
    }
}

void FolderVisitor::VisitBreakStatement( BreakStatement* breakStmt )
{
    mLastValue.reset();
}

void FolderVisitor::VisitCallExpr( CallExpr* call )
{
    for ( auto& arg : call->Arguments )
    {
        Fold( arg );
    }

    call->Head->Accept( this );

    mLastValue.reset();
}

void FolderVisitor::VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol )
{
    callOrSymbol->Symbol->Accept( this );
}

void FolderVisitor::VisitCaseExpr( CaseExpr* caseExpr )
{
    Fold( caseExpr->TestKey );

    for ( auto& clause : caseExpr->Clauses )
    {
        for ( auto& key : clause->Keys )
        {
            key->Accept( this );
        }

        clause->Body.Accept( this );
    }

    if ( caseExpr->Fallback != nullptr )
        caseExpr->Fallback->Body.Accept( this );

    mLastValue.reset();
}

void FolderVisitor::VisitCondExpr( CondExpr* condExpr )
{
    for ( auto& clause : condExpr->Clauses )
    {
        Fold( clause->Condition );
        clause->Body.Accept( this );
    }

    mLastValue.reset();
}

void FolderVisitor::VisitForStatement( ForStatement* forStmt )
{
    Fold( forStmt->First );
    Fold( forStmt->Last );

    if ( forStmt->Step )
        Fold( forStmt->Step );

    forStmt->Body.Accept( this );

    mLastValue.reset();
}

void FolderVisitor::VisitIndexExpr( IndexExpr* indexExpr )
{
    indexExpr->Head->Accept( this );
    Fold( indexExpr->Index );
    mLastValue.reset();
}

void FolderVisitor::VisitInitList( InitList* initList )
{
    for ( auto& value : initList->Values )
    {
        Fold( value );
    }

    mLastValue.reset();
}

void FolderVisitor::VisitLambdaExpr( LambdaExpr* lambdaExpr )
{
    VisitProc( lambdaExpr->Proc.get() );
    mLastValue.reset();
}

void FolderVisitor::VisitLetStatement( LetStatement* letStmt )
{
    for ( auto& binding : letStmt->Variables )
    {
        VisitLetBinding( binding.get() );
    }

    letStmt->Body.Accept( this );
    mLastValue.reset();
}

void FolderVisitor::VisitLetBinding( VarDecl* varDecl )
{
    if ( varDecl->Initializer != nullptr )
        Fold( varDecl->Initializer );
}

void FolderVisitor::VisitLoopStatement( LoopStatement* loopStmt )
{
    loopStmt->Body.Accept( this );

    if ( loopStmt->Condition != nullptr )
        Fold( loopStmt->Condition );

    mLastValue.reset();
}

void FolderVisitor::VisitNameExpr( NameExpr* nameExpr )
{
    if ( nameExpr->Decl->Kind == DeclKind::Const )
    {
        mLastValue = ((ConstDecl*) nameExpr->Decl.get())->Value;
    }
    else
    {
        mLastValue.reset();
    }
}

void FolderVisitor::VisitNextStatement( NextStatement* nextStmt )
{
    mLastValue.reset();
}

void FolderVisitor::VisitNumberExpr( NumberExpr* numberExpr )
{
    mLastValue = numberExpr->Value;
}

void FolderVisitor::VisitParamDecl( ParamDecl* paramDecl )
{
    mLastValue.reset();
}

void FolderVisitor::VisitProcDecl( ProcDecl* procDecl )
{
    VisitProc( procDecl );
    mLastValue.reset();
}

void FolderVisitor::VisitProc( ProcDecl* procDecl )
{
    for ( auto& parameter : procDecl->Params )
    {
        parameter->Accept( this );
    }

    procDecl->Body.Accept( this );
    mLastValue.reset();
}

void FolderVisitor::VisitReturnStatement( ReturnStatement* retStmt )
{
    Fold( retStmt->Inner );
    mLastValue.reset();
}

void FolderVisitor::VisitStatementList( StatementList* stmtList )
{
    for ( auto& stmt : stmtList->Statements )
    {
        Fold( stmt );
    }

    mLastValue.reset();
}

void FolderVisitor::VisitUnaryExpr( UnaryExpr* unary )
{
    Fold( unary->Inner );

    if ( mLastValue.has_value() )
    {
        if ( unary->Op == "-" )
            mLastValue = -mLastValue.value();
        else if ( unary->Op == "not" )
            mLastValue = !mLastValue.value();
        else
            mRep.ThrowInternalError();
    }
}

void FolderVisitor::VisitUnit( Unit* unit )
{
    for ( auto& varNode : unit->VarDeclarations )
        varNode->Accept( this );

    for ( auto& funcNode : unit->FuncDeclarations )
        funcNode->Accept( this );
}

void FolderVisitor::VisitVarDecl( VarDecl* varDecl )
{
    if ( varDecl->Initializer != nullptr )
        Fold( varDecl->Initializer );

    mLastValue.reset();
}

void FolderVisitor::VisitWhileStatement( WhileStatement* whileStmt )
{
    Fold( whileStmt->Condition );
    whileStmt->Body.Accept( this );
    mLastValue.reset();
}


void FolderVisitor::Fold( std::unique_ptr<Syntax>& child )
{
    child->Accept( this );

    if ( mFoldNodes && mLastValue.has_value() )
    {
        std::unique_ptr<NumberExpr> number( new NumberExpr( mLastValue.value() ) );

        child = std::move( number );
    }
}
