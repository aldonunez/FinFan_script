#include "stdafx.h"
#include "BinderVisitor.h"
#include <cstdarg>
#include "Compiler.h"
#include "FolderVisitor.h"


class LocalScope
{
    Compiler::SymTable  mLocalTable;
    BinderVisitor& mBinder;

public:
    explicit LocalScope( BinderVisitor& binder ) :
        mBinder( binder )
    {
        mBinder.mSymStack.push_back( &mLocalTable );
        mBinder.mCurLevelLocalCount = 0;
    }

    ~LocalScope()
    {
        mBinder.mCurLocalCount -= mBinder.mCurLevelLocalCount;
        mBinder.mCurLevelLocalCount = 0;
        mBinder.mSymStack.pop_back();
    }

    LocalScope( const LocalScope& ) = delete;
    LocalScope& operator=( const LocalScope& ) = delete;
};


bool IsFunctionDeclaration( DeclKind kind )
{
    return kind == DeclKind::Func
        || kind == DeclKind::Forward
        || kind == DeclKind::ExternalFunc;
}

bool IsCallableDeclaration( DeclKind kind )
{
    return kind == DeclKind::Func
        || kind == DeclKind::Forward
        || kind == DeclKind::ExternalFunc
        || kind == DeclKind::NativeFunc;
}


BinderVisitor::BinderVisitor(
    SymTable& constTable,
    SymTable& globalTable,
    ICompilerEnv* env,
    ICompilerLog* log )
    :
    mConstTable( constTable ),
    mGlobalTable( globalTable ),
    mEnv( env ),
    mRep( log ),
    mCurLevelLocalCount(),
    mCurLocalCount(),
    mMaxLocalCount(),
    mGlobalSize()
{
}

void BinderVisitor::Bind( Unit* unit )
{
    mSymStack.push_back( &mConstTable );
    mSymStack.push_back( &mGlobalTable );

    unit->Accept( this );

    BindLambdas();

    mSymStack.pop_back();
    mSymStack.pop_back();
}

size_t BinderVisitor::GetDataSize()
{
    return mGlobalSize;
}

void BinderVisitor::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
    addrOf->Inner->Accept( this );

    if ( !IsFunctionDeclaration( addrOf->Inner->Decl->Kind ) )
    {
        mRep.ThrowError( CERR_SEMANTICS, addrOf, "'%s' is not a function", addrOf->Inner->String.c_str() );
    }
}

void BinderVisitor::VisitArrayTypeRef( ArrayTypeRef* typeRef )
{
    typeRef->SizeExpr->Accept( this );

    typeRef->Size = GetElementValue( typeRef->SizeExpr.get(), "Expected a constant array size" );

    if ( typeRef->Size <= 0 )
        mRep.ThrowError( CERR_SEMANTICS, typeRef->SizeExpr.get(), "Array size must be positive" );
}

void BinderVisitor::VisitAssignmentExpr( AssignmentExpr* assignment )
{
    assignment->Left->Accept( this );
    assignment->Right->Accept( this );
}

void BinderVisitor::VisitBinaryExpr( BinaryExpr* binary )
{
    binary->Left->Accept( this );
    binary->Right->Accept( this );
}

void BinderVisitor::VisitBreakStatement( BreakStatement* breakStmt )
{
    // Nothing
}

void BinderVisitor::VisitCallExpr( CallExpr* call )
{
    call->Head->Accept( this );

    if ( !call->IsIndirect )
    {
        auto decl = call->Head->GetDecl();

        if ( decl == nullptr || !IsCallableDeclaration( decl->Kind ) )
            mRep.ThrowError( CERR_SEMANTICS, call->Head.get(), "Expected a function" );
    }

    for ( auto& arg : call->Arguments )
    {
        arg->Accept( this );
    }
}

void BinderVisitor::VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol )
{
    callOrSymbol->Symbol->Accept( this );
}

void BinderVisitor::VisitCaseExpr( CaseExpr* caseExpr )
{
    LocalScope localScope( *this );

    caseExpr->TestKey->Accept( this );

    if ( caseExpr->TestKey->Kind != SyntaxKind::Name
        && caseExpr->TestKey->Kind != SyntaxKind::Number )
    {
        // TODO: remove duplicate string in Compiler
        // TODO: ideally, add the local during code generation
        caseExpr->TestKeyDecl = AddLocal( "$testKey", 1 );
    }

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
}

void BinderVisitor::VisitCondExpr( CondExpr* condExpr )
{
    for ( auto& clause : condExpr->Clauses )
    {
        clause->Condition->Accept( this );
        clause->Body.Accept( this );
    }
}

void BinderVisitor::VisitConstDecl( ConstDecl* constDecl )
{
    // No need to make the type ref accept this visitor,
    // because only integer constants are supported

    if ( constDecl->TypeRef == nullptr )
    {
        int32_t value = 0;

        if ( constDecl->Initializer != nullptr )
        {
            constDecl->Initializer->Accept( this );

            value = GetElementValue( constDecl->Initializer.get(), "Constant initializer is not constant" );
        }
        else
        {
            mRep.ThrowInternalError( "Missing constant initializer" );
        }

        std::shared_ptr<Constant> constant = AddConst( constDecl->Name, value );

        constDecl->Decl = constant;
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, constDecl->TypeRef.get(), "Only integer constants are supported" );
    }
}

void BinderVisitor::VisitForStatement( ForStatement* forStmt )
{
    LocalScope localScope( *this );

    std::shared_ptr<Storage> local = AddLocal( forStmt->IndexName, 1 );

    forStmt->IndexDecl = local;

    forStmt->First->Accept( this );
    forStmt->Last->Accept( this );

    if ( forStmt->Step )
        forStmt->Step->Accept( this );

    forStmt->Body.Accept( this );
}

void BinderVisitor::VisitIndexExpr( IndexExpr* indexExpr )
{
    indexExpr->Head->Accept( this );
    indexExpr->Index->Accept( this );
}

void BinderVisitor::VisitInitList( InitList* initList )
{
    for ( auto& value : initList->Values )
    {
        value->Accept( this );
    }
}

void BinderVisitor::VisitLambdaExpr( LambdaExpr* lambdaExpr )
{
    // In order to limit the special processing and call stack depth,
    // defer the lambda until the the end where it can be treated as
    // a top level procedure

    mLambdas.push_back( lambdaExpr );
}

void BinderVisitor::VisitLetStatement( LetStatement* letStmt )
{
    LocalScope localScope( *this );

    for ( auto& binding : letStmt->Variables )
    {
        VisitLetBinding( binding.get() );
    }

    letStmt->Body.Accept( this );
}

void BinderVisitor::VisitLetBinding( DataDecl* varDecl )
{
    if ( varDecl->TypeRef != nullptr )
    {
        varDecl->TypeRef->Accept( this );
    }

    if ( varDecl->TypeRef == nullptr )
    {
        varDecl->Decl = AddLocal( varDecl->Name, 1 );
    }
    else if ( varDecl->TypeRef->Kind == SyntaxKind::Other )
    {
        auto type = (ArrayTypeRef*) varDecl->TypeRef.get();

        varDecl->Decl = AddLocal( varDecl->Name, type->Size );
    }

    if ( varDecl->Initializer != nullptr )
        varDecl->Initializer->Accept( this );
}

void BinderVisitor::VisitLoopStatement( LoopStatement* loopStmt )
{
    loopStmt->Body.Accept( this );

    if ( loopStmt->Condition != nullptr )
        loopStmt->Condition->Accept( this );
}

void BinderVisitor::VisitNameExpr( NameExpr* nameExpr )
{
    auto decl = FindSymbol( nameExpr->String );

    if ( decl != nullptr )
    {
        nameExpr->Decl = decl;
    }
    else
    {
        ExternalFunc external = { 0 };

        if ( mEnv->FindExternal( nameExpr->String, &external ) )
        {
            if ( external.Kind == External_Bytecode )
            {
                std::shared_ptr<ExternalFunction> extFunc( new ExternalFunction() );

                extFunc->Kind = DeclKind::ExternalFunc;
                extFunc->Id = external.Id;

                nameExpr->Decl = extFunc;
                mExtTable.insert( { nameExpr->String, extFunc } );
            }
            else if ( external.Kind == External_Native )
            {
                std::shared_ptr<NativeFunction> extFunc( new NativeFunction() );

                extFunc->Kind = DeclKind::NativeFunc;
                extFunc->Id = external.Id;

                nameExpr->Decl = extFunc;
                mExtTable.insert( { nameExpr->String, extFunc } );
            }
            else
            {
                mRep.ThrowError( CERR_SEMANTICS, nameExpr, "symbol not found '%s'", nameExpr->String.c_str() );
            }
        }
    }
}

void BinderVisitor::VisitNextStatement( NextStatement* nextStmt )
{
    // Nothing
}

void BinderVisitor::VisitNumberExpr( NumberExpr* numberExpr )
{
    // Nothing
}

void BinderVisitor::VisitParamDecl( ParamDecl* paramDecl )
{
    if ( paramDecl->TypeRef != nullptr )
        mRep.ThrowError( CERR_UNSUPPORTED, paramDecl->TypeRef.get(), "only simple parameters are supported" );

    paramDecl->Decl = AddArg( paramDecl->Name );
}

void BinderVisitor::VisitProcDecl( ProcDecl* procDecl )
{
    SymTable::iterator it = mGlobalTable.find( procDecl->Name );
    std::shared_ptr<Function> func;

    if ( it != mGlobalTable.end() )
    {
        if ( it->second->Kind == DeclKind::Forward )
        {
            func = (std::shared_ptr<Function>&) it->second;
            func->Kind = DeclKind::Func;
            func->Address = INT32_MAX;
            // TODO: look for forwards another way
        }
        else if ( it->second->Kind == DeclKind::Func )
        {
            mRep.ThrowError( CERR_SEMANTICS, procDecl, "the function '%s' is already defined", procDecl->Name.c_str() );
        }
        else
        {
            mRep.ThrowError( CERR_SEMANTICS, procDecl, "the symbol '%s' is already defined", procDecl->Name.c_str() );
        }
    }
    else
    {
        func = AddFunc( procDecl->Name, INT32_MAX );
    }

    procDecl->Decl = func;

    VisitProc( procDecl );
}

void BinderVisitor::VisitProc( ProcDecl* procDecl )
{
    LocalScope argScope( *this );

    auto func = (Function*) procDecl->Decl.get();

    if ( procDecl->Params.size() > ProcDecl::MaxArgs )
        mRep.ThrowError( CERR_SEMANTICS, procDecl, "'%s' has too many arguments. Max is %d",
            procDecl->Name.c_str(), ProcDecl::MaxArgs );

    for ( auto& parameter : procDecl->Params )
    {
        parameter->Accept( this );
    }

    mMaxLocalCount = 0;
    mCurLocalCount = 0;

    procDecl->Body.Accept( this );

    if ( mMaxLocalCount > ProcDecl::MaxLocals )
        mRep.ThrowError( CERR_SEMANTICS, procDecl, "'%s' has too many locals. Max is %d",
            procDecl->Name.c_str(), ProcDecl::MaxLocals );

    func->LocalCount = mMaxLocalCount;
    func->ArgCount = (int16_t) procDecl->Params.size();
}

void BinderVisitor::VisitReturnStatement( ReturnStatement* retStmt )
{
    retStmt->Inner->Accept( this );
}

void BinderVisitor::VisitStatementList( StatementList* stmtList )
{
    for ( auto& stmt : stmtList->Statements )
    {
        stmt->Accept( this );
    }
}

void BinderVisitor::VisitUnaryExpr( UnaryExpr* unary )
{
    unary->Inner->Accept( this );
}

void BinderVisitor::VisitUnit( Unit* unit )
{
    for ( auto& varNode : unit->DataDeclarations )
        varNode->Accept( this );

    for ( auto& funcNode : unit->FuncDeclarations )
        funcNode->Accept( this );
}

void BinderVisitor::VisitVarDecl( VarDecl* varDecl )
{
    if ( varDecl->TypeRef != nullptr )
    {
        varDecl->TypeRef->Accept( this );
    }

    if ( varDecl->TypeRef == nullptr )
    {
        varDecl->Decl = AddGlobal( varDecl->Name, 1 );
    }
    else if ( varDecl->TypeRef->Kind == SyntaxKind::Other )
    {
        auto type = (ArrayTypeRef*) varDecl->TypeRef.get();

        varDecl->Decl = AddGlobal( varDecl->Name, type->Size );
    }

    if ( varDecl->Initializer != nullptr )
        varDecl->Initializer->Accept( this );
}

void BinderVisitor::VisitWhileStatement( WhileStatement* whileStmt )
{
    whileStmt->Condition->Accept( this );
    whileStmt->Body.Accept( this );
}


void BinderVisitor::BindLambdas()
{
    int i = 0;

    for ( auto lambdaExpr : mLambdas )
    {
        char name[32];

        sprintf_s( name, "$Lambda$%d", i );
        i++;

        std::shared_ptr<Function> func = AddFunc( name, INT32_MAX );

        lambdaExpr->Proc->Name = name;
        lambdaExpr->Proc->Decl = func;

        VisitProc( lambdaExpr->Proc.get() );
    }
}


I32 BinderVisitor::GetElementValue( Syntax* elem, const char* message )
{
    FolderVisitor folder( mRep.GetLog() );

    auto optValue = folder.Evaluate( elem );

    if ( optValue.has_value() )
        return optValue.value();

    if ( message != nullptr )
        mRep.ThrowError( CERR_SEMANTICS, elem, message );
    else
        mRep.ThrowError( CERR_SEMANTICS, elem, "Expected a constant value" );
}


std::shared_ptr<Declaration> BinderVisitor::FindSymbol( const std::string& symbol )
{
    for ( auto table : mSymStack )
    {
        auto it = table->find( symbol );
        if ( it != table->end() )
            return it->second;
    }

    return nullptr;
}

std::shared_ptr<Storage> BinderVisitor::AddArg( const std::string& name )
{
    auto& table = *mSymStack.back();

    std::shared_ptr<Storage> arg( new Storage() );
    arg->Kind = DeclKind::Arg;
    arg->Offset = table.size();
    table.insert( SymTable::value_type( name, arg ) );
    return arg;
}

std::shared_ptr<Function> BinderVisitor::AddFunc( const std::string& name, int address )
{
    if ( mGlobalTable.find( name ) != mGlobalTable.end() )
        mRep.ThrowError( CERR_SEMANTICS, nullptr, "Duplicate symbol: %s", name.c_str() );

    std::shared_ptr<Function> func( new Function() );
    func->Kind = DeclKind::Func;
    func->Name = name;
    func->Address = address;
    mGlobalTable.insert( SymTable::value_type( name, func ) );
    return func;
}

std::shared_ptr<Storage> BinderVisitor::AddLocal( SymTable& table, const std::string& name, int offset )
{
    std::shared_ptr<Storage> local( new Storage() );
    local->Kind = DeclKind::Local;
    local->Offset = offset;
    table.insert( SymTable::value_type( name, local ) );
    return local;
}

std::shared_ptr<Storage> BinderVisitor::AddLocal( const std::string& name, size_t size )
{
    assert( size >= 1 );

    auto local = AddLocal( *mSymStack.back(), name, mCurLocalCount + size - 1 );

    mCurLocalCount += size;
    mCurLevelLocalCount += size;

    if ( mCurLocalCount > mMaxLocalCount )
        mMaxLocalCount = mCurLocalCount;

    if ( mMaxLocalCount > ProcDecl::MaxLocals )
        mRep.ThrowError( CERR_SEMANTICS, 0, 0, "Local exceeds capacity: %s", name.c_str() );

    return local;
}

std::shared_ptr<Storage> BinderVisitor::AddGlobal( const std::string& name, size_t size )
{
    if ( mGlobalTable.find( name ) != mGlobalTable.end() )
        mRep.ThrowError( CERR_SEMANTICS, nullptr, "Duplicate symbol: %s", name.c_str() );

    std::shared_ptr<Storage> global( new Storage() );
    global->Kind = DeclKind::Global;
    global->Offset = mGlobalSize;
    mGlobalTable.insert( SymTable::value_type( name, global ) );

    mGlobalSize += size;

    return global;
}

std::shared_ptr<Constant> BinderVisitor::AddConst( const std::string& name, int32_t value )
{
    std::shared_ptr<Constant> constant( new Constant() );
    constant->Kind = DeclKind::Const;
    constant->Value = value;
    mConstTable.insert( SymTable::value_type( name, constant ) );
    return constant;
}
