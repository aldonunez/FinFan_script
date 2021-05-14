#include "stdafx.h"
#include "BinderVisitor.h"
#include <cstdarg>
#include "Compiler.h"
#include "FolderVisitor.h"


class LocalScope
{
    Compiler::SymTable  mLocalTable;
    BinderVisitor&      mBinder;

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

bool IsVarDeclaration( DeclKind kind )
{
    return kind == DeclKind::Arg
        || kind == DeclKind::Global
        || kind == DeclKind::Local;
}


BinderVisitor::BinderVisitor(
    SymTable& globalTable,
    ICompilerEnv* env,
    ICompilerLog* log )
    :
    mGlobalTable( globalTable ),
    mEnv( env ),
    mRep( log )
{
}

void BinderVisitor::Bind( Unit* unit )
{
    MakeStdEnv();
    CollectFunctionForwards( unit );

    mSymStack.push_back( &mGlobalTable );

    unit->Accept( this );

    BindLambdas();

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
        mRep.ThrowError( CERR_SEMANTICS, addrOf->Inner.get(), "'%s' is not a function", addrOf->Inner->String.c_str() );
    }
}

void BinderVisitor::VisitArrayTypeRef( ArrayTypeRef* typeRef )
{
    typeRef->SizeExpr->Accept( this );

    typeRef->Size = GetFoldedSyntaxValue( typeRef->SizeExpr.get(), "Expected a constant array size" );

    if ( typeRef->Size <= 0 )
        mRep.ThrowError( CERR_SEMANTICS, typeRef->SizeExpr.get(), "Array size must be positive" );
}

void BinderVisitor::VisitAssignmentExpr( AssignmentExpr* assignment )
{
    assignment->Left->Accept( this );
    assignment->Right->Accept( this );

    if ( assignment->Left->Kind == SyntaxKind::Name )
    {
        auto decl = assignment->Left->GetDecl();

        if ( !IsVarDeclaration( decl->Kind ) )
            mRep.ThrowError( CERR_SEMANTICS, assignment->Left.get(), "Left side is not a variable object" );
    }

    // An indexing expression would have checked itself already
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

            value = GetFoldedSyntaxValue( constDecl->Initializer.get(), "Constant initializer is not constant" );
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

    auto decl = indexExpr->Head->GetDecl();

    if ( decl == nullptr || (decl->Kind != DeclKind::Local && decl->Kind != DeclKind::Global) )
        mRep.ThrowError( CERR_SEMANTICS, indexExpr->Head.get(), "Only named arrays can be indexed" );
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
    VisitStorage( varDecl, DeclKind::Local );
}

void BinderVisitor::VisitStorage( DataDecl* varDecl, DeclKind declKind )
{
    if ( varDecl->TypeRef != nullptr )
        varDecl->TypeRef->Accept( this );

    if ( varDecl->Initializer != nullptr )
        varDecl->Initializer->Accept( this );

    if ( varDecl->TypeRef == nullptr
        && varDecl->Initializer != nullptr
        && varDecl->Initializer->Kind == SyntaxKind::ArrayInitializer )
    {
        int32_t size = ((InitList*) varDecl->Initializer.get())->Values.size();

        varDecl->TypeRef = std::unique_ptr<ArrayTypeRef>( new ArrayTypeRef( size ) );
    }
    else if ( varDecl->TypeRef != nullptr
        && varDecl->TypeRef->Kind == SyntaxKind::ArrayTypeRef
        && varDecl->Initializer != nullptr
        && varDecl->Initializer->Kind != SyntaxKind::ArrayInitializer )
    {
        mRep.ThrowError( CERR_SEMANTICS, varDecl->Initializer.get(), "Arrays are initialized with arrays" );
    }

    if ( varDecl->TypeRef == nullptr )
    {
        varDecl->Decl = AddStorage( varDecl->Name, 1, declKind );
    }
    else if ( varDecl->TypeRef->Kind == SyntaxKind::ArrayTypeRef )
    {
        auto type = (ArrayTypeRef*) varDecl->TypeRef.get();

        varDecl->Decl = AddStorage( varDecl->Name, type->Size, declKind );
    }
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
        mRep.ThrowError( CERR_SEMANTICS, nameExpr, "symbol not found '%s'", nameExpr->String.c_str() );
    }
}

void BinderVisitor::VisitNativeDecl( NativeDecl* nativeDecl )
{
    CheckDuplicateGlobalSymbol( nativeDecl->Name );

    std::shared_ptr<NativeFunction> native( new NativeFunction() );
    native->Kind = DeclKind::NativeFunc;
    native->Id = mNextNativeId;
    mGlobalTable.insert( SymTable::value_type( nativeDecl->Name, native ) );

    mNextNativeId++;

    nativeDecl->Decl = native;
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
            func = std::static_pointer_cast<Function>( it->second );
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
    VisitStorage( varDecl, DeclKind::Global );
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


I32 BinderVisitor::GetFoldedSyntaxValue( Syntax* node, const char* message )
{
    FolderVisitor folder( mRep.GetLog() );

    auto optValue = folder.Evaluate( node );

    if ( optValue.has_value() )
        return optValue.value();

    if ( message != nullptr )
        mRep.ThrowError( CERR_SEMANTICS, node, message );
    else
        mRep.ThrowError( CERR_SEMANTICS, node, "Expected a constant value" );
}


std::shared_ptr<Declaration> BinderVisitor::FindSymbol( const std::string& symbol )
{
    for ( auto stackIt = mSymStack.rbegin(); stackIt != mSymStack.rend(); stackIt++ )
    {
        auto table = *stackIt;

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
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<Storage> global( new Storage() );
    global->Kind = DeclKind::Global;
    global->Offset = mGlobalSize;
    mGlobalTable.insert( SymTable::value_type( name, global ) );

    mGlobalSize += size;

    return global;
}

std::shared_ptr<Storage> BinderVisitor::AddStorage( const std::string& name, size_t size, DeclKind declKind )
{
    switch ( declKind )
    {
    case DeclKind::Global:  return AddGlobal( name, size );
    case DeclKind::Local:   return AddLocal( name, size );
    default:
        mRep.ThrowInternalError();
    }
}

std::shared_ptr<Constant> BinderVisitor::AddConst( const std::string& name, int32_t value )
{
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<Constant> constant( new Constant() );
    constant->Kind = DeclKind::Const;
    constant->Value = value;
    mGlobalTable.insert( SymTable::value_type( name, constant ) );
    return constant;
}

std::shared_ptr<Function> BinderVisitor::AddFunc( const std::string& name, int address )
{
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<Function> func( new Function() );
    func->Kind = DeclKind::Func;
    func->Name = name;
    func->Address = address;
    mGlobalTable.insert( SymTable::value_type( name, func ) );
    return func;
}

std::shared_ptr<Function> BinderVisitor::AddForward( const std::string& name )
{
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<Function> func( new Function() );
    func->Kind = DeclKind::Forward;
    func->Name = name;
    func->Address = INT32_MAX;
    mGlobalTable.insert( SymTable::value_type( name, func ) );
    return func;
}

void BinderVisitor::CheckDuplicateGlobalSymbol( const std::string& name )
{
    if ( mGlobalTable.find( name ) != mGlobalTable.end() )
        mRep.ThrowError( CERR_SEMANTICS, nullptr, "Duplicate symbol: %s", name.c_str() );
}

void BinderVisitor::MakeStdEnv()
{
    AddConst( "false", 0 );
    AddConst( "true", 1 );
}

void BinderVisitor::CollectFunctionForwards( Unit* program )
{
    for ( auto& elem : program->FuncDeclarations )
    {
        auto funcDecl = (ProcDecl*) elem.get();

        AddForward( funcDecl->Name );
    }
}
