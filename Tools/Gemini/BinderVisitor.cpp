// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "stdafx.h"
#include "BinderVisitor.h"
#include <cstdarg>
#include "Compiler.h"
#include "FolderVisitor.h"


class LocalScope
{
    SymTable            mLocalTable;
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


static bool IsFunctionDeclaration( DeclKind kind )
{
    return kind == DeclKind::Func
        || kind == DeclKind::Forward
        ;
}

static bool IsCallableDeclaration( DeclKind kind )
{
    return kind == DeclKind::Func
        || kind == DeclKind::Forward
        || kind == DeclKind::NativeFunc
        ;
}

static bool IsVarDeclaration( DeclKind kind )
{
    return kind == DeclKind::Param
        || kind == DeclKind::Global
        || kind == DeclKind::Local;
}

static bool IsAddressableDeclaration( DeclKind kind )
{
    return kind == DeclKind::Func
        || kind == DeclKind::Forward
        ;
}

bool IsScalarType( TypeKind kind )
{
    return kind == TypeKind::Int
        || kind == TypeKind::Pointer
        ;
}

bool IsIntegralType( TypeKind kind )
{
    return kind == TypeKind::Int
        ;
}

static bool IsAddressableType( TypeKind kind )
{
    return kind == TypeKind::Func;
}

static bool IsStatementType( TypeKind kind )
{
    return IsScalarType( kind )
        || kind == TypeKind::Xfer
        ;
}

static bool IsAssignableType( TypeKind kind )
{
    return IsScalarType( kind )
        || kind == TypeKind::Xfer
        ;
}

static bool IsEquatable( TypeKind kind )
{
    return IsScalarType( kind )
        ;
}

static bool IsBoolean( TypeKind kind )
{
    return kind == TypeKind::Int;
}

static bool IsAllowedPointerTarget( TypeKind kind )
{
    return kind == TypeKind::Func;
}

static bool IsAllowedParamType( TypeKind kind )
{
    return IsScalarType( kind )
        ;
}

static bool IsStorageType( TypeKind kind )
{
    return IsScalarType( kind )
        || kind == TypeKind::Array
        ;
}

template <typename T, typename... Args>
std::shared_ptr<T> Make( Args&&... args )
{
    T* type = new T( std::forward<Args>( args )... );
    return std::shared_ptr<T>( type );
}


BinderVisitor::BinderVisitor(
    int modIndex,
    SymTable& globalTable,
    SymTable& moduleTable,
    SymTable& publicTable,
    ICompilerEnv* env,
    ICompilerLog* log )
    :
    mGlobalTable( globalTable ),
    mModuleTable( moduleTable ),
    mPublicTable( publicTable ),
    mEnv( env ),
    mRep( log ),
    mModIndex( modIndex )
{
    mSymStack.push_back( &mGlobalTable );

    MakeStdEnv();
}

void BinderVisitor::Declare( Unit* unit )
{
    for ( auto& varNode : unit->DataDeclarations )
        DeclareNode( varNode.get() );

    for ( auto& funcNode : unit->FuncDeclarations )
        DeclareNode( funcNode.get() );
}

void BinderVisitor::Bind( Unit* unit )
{
    unit->Accept( this );

    BindProcs( unit );
    BindLambdas();
}

size_t BinderVisitor::GetDataSize()
{
    return mGlobalSize;
}

void BinderVisitor::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
    addrOf->Inner->Accept( this );

    auto innerType = addrOf->Inner->Type;
    auto decl = addrOf->Inner->GetDecl();

    if ( !innerType || !IsAddressableType( innerType->GetKind() )
        || decl == nullptr || !IsAddressableDeclaration( decl->Kind ) )
    {
        mRep.ThrowError( CERR_SEMANTICS, addrOf->Inner.get(), "Expected a function" );
    }

    addrOf->Type = Make<PointerType>( innerType );
}

void BinderVisitor::VisitArrayTypeRef( ArrayTypeRef* typeRef )
{
    typeRef->SizeExpr->Accept( this );

    std::shared_ptr<Type> elemType;

    if ( typeRef->ElementTypeRef )
    {
        typeRef->ElementTypeRef->Accept( this );

        elemType = typeRef->ElementTypeRef->ReferentType;
    }
    else
    {
        elemType = mIntType;
    }

    int32_t size = Evaluate( typeRef->SizeExpr.get(), "Expected a constant array size" );

    if ( size <= 0 )
        mRep.ThrowError( CERR_SEMANTICS, typeRef->SizeExpr.get(), "Array size must be positive" );

    if ( !IsStorageType( elemType->GetKind() ) )
        mRep.ThrowError( CERR_SEMANTICS, typeRef->SizeExpr.get(), "Element type is not allowed" );

    typeRef->Type = mTypeType;
    typeRef->ReferentType = Make<ArrayType>( size, elemType );
}

void BinderVisitor::VisitAssignmentExpr( AssignmentExpr* assignment )
{
    assignment->Left->Accept( this );
    assignment->Right->Accept( this );

    if ( assignment->Left->Kind == SyntaxKind::Name )
    {
        // Don't allow constants

        auto decl = assignment->Left->GetDecl();

        if ( !IsVarDeclaration( decl->Kind ) )
            mRep.ThrowError( CERR_SEMANTICS, assignment->Left.get(), "Left side is not a variable object" );
    }

    CheckAssignableType( assignment->Left.get() );

    // An indexing expression would have checked itself already

    CheckType( assignment->Left->Type, assignment->Right->Type, assignment );

    assignment->Type = assignment->Left->Type;
}

void BinderVisitor::VisitBinaryExpr( BinaryExpr* binary )
{
    binary->Left->Accept( this );
    binary->Right->Accept( this );

    if ( binary->Op == "=" || binary->Op == "<>" )
    {
        if ( !IsEquatable( binary->Left->Type->GetKind() )
            || !IsEquatable( binary->Right->Type->GetKind() ) )
        {
            mRep.ThrowError( CERR_SEMANTICS, binary, "Equality expressions only support scalars" );
        }

        CheckType( binary->Left->Type, binary->Right->Type, binary );

        binary->Type = mIntType;
    }
    else
    {
        if ( binary->Left->Type->GetKind() != TypeKind::Int
            || binary->Right->Type->GetKind() != TypeKind::Int )
        {
            mRep.ThrowError( CERR_SEMANTICS, binary, "Binary expressions only support integers" );
        }

        binary->Type = binary->Left->Type;
    }
}

void BinderVisitor::VisitBreakStatement( BreakStatement* breakStmt )
{
    breakStmt->Type = mXferType;
}

void BinderVisitor::VisitCallExpr( CallExpr* call )
{
    call->Head->Accept( this );

    std::shared_ptr<FuncType> funcType;

    if ( call->IsIndirect )
    {
        auto type = call->Head->Type.get();

        if ( type == nullptr
            || type->GetKind() != TypeKind::Pointer
            || ((PointerType*) type)->TargetType->GetKind() != TypeKind::Func )
        {
            mRep.ThrowError( CERR_SEMANTICS, call->Head.get(), "Expected a function pointer" );
        }

        funcType = std::static_pointer_cast<FuncType>( ((PointerType*) type)->TargetType );
    }
    else
    {
        if ( call->Head->Type->GetKind() != TypeKind::Func )
            mRep.ThrowError( CERR_SEMANTICS, call->Head.get(), "Expected a function" );

        funcType = std::static_pointer_cast<FuncType>( call->Head->Type );
    }

    if ( call->Arguments.size() != funcType->ParamTypes.size() )
        mRep.ThrowError( CERR_SEMANTICS, call, "Function does not take %u arguments", call->Arguments.size() );

    int i = 0;

    for ( auto& arg : call->Arguments )
    {
        arg->Accept( this );

        CheckType( funcType->ParamTypes[i], arg->Type, arg.get() );
        i++;
    }

    call->Type = funcType->ReturnType;
}

void BinderVisitor::VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol )
{
    callOrSymbol->Symbol->Accept( this );

    auto decl = callOrSymbol->Symbol->GetDecl();

    if ( IsCallableDeclaration( decl->Kind ) )
    {
        auto funcType = (FuncType*) decl->Type.get();

        if ( funcType->ParamTypes.size() > 0 )
            mRep.ThrowError( CERR_SEMANTICS, callOrSymbol, "Too few arguments" );

        callOrSymbol->Type = funcType->ReturnType;
    }
    else
    {
        callOrSymbol->Type = decl->Type;
    }
}

void BinderVisitor::VisitCaseExpr( CaseExpr* caseExpr )
{
    LocalScope localScope( *this );

    caseExpr->TestKey->Accept( this );

    if ( caseExpr->TestKey->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, caseExpr->TestKey.get(), "Case only supports integers" );

    if ( caseExpr->TestKey->Kind != SyntaxKind::Name
        && caseExpr->TestKey->Kind != SyntaxKind::Number )
    {
        // TODO: Ideally simplify a complex test key in one place
        caseExpr->TestKeyDecl = AddLocal( "$testKey", caseExpr->TestKey->Type, 1 );
    }

    std::shared_ptr<Type> bodyType;

    for ( auto& clause : caseExpr->Clauses )
    {
        for ( auto& key : clause->Keys )
        {
            key->Accept( this );

            if ( key->Type->GetKind() != TypeKind::Int )
                mRep.ThrowError( CERR_SEMANTICS, key.get(), "Case key only supports integers" );
        }

        clause->Body.Accept( this );

        CheckAndConsolidateClauseType( clause->Body, bodyType );
    }

    if ( caseExpr->Fallback )
    {
        caseExpr->Fallback->Body.Accept( this );

        CheckAndConsolidateClauseType( caseExpr->Fallback->Body, bodyType );
    }
    else if ( caseExpr->Clauses.size() > 0 )
    {
        if ( bodyType->GetKind() != TypeKind::Int
            && bodyType->GetKind() != TypeKind::Xfer )
            mRep.ThrowError( CERR_SEMANTICS, caseExpr, "Case without else must yield integers or nothing" );
    }
    else
    {
        bodyType = mIntType;
    }

    caseExpr->Type = bodyType;
}

void BinderVisitor::VisitCondExpr( CondExpr* condExpr )
{
    std::shared_ptr<Type> bodyType;

    for ( auto& clause : condExpr->Clauses )
    {
        clause->Condition->Accept( this );

        if ( !IsBoolean( clause->Condition->Type->GetKind() ) )
            mRep.ThrowError( CERR_SEMANTICS, clause->Condition.get(), "Expected scalar type" );

        clause->Body.Accept( this );

        Syntax* sourceNode = nullptr;

        if ( clause->Body.Statements.size() == 0 && !condExpr->IsIf )
            sourceNode = clause->Condition.get();
        else
            sourceNode = &clause->Body;

        CheckAndConsolidateClauseType( sourceNode, bodyType );
    }

    if ( condExpr->Clauses.size() > 0 )
    {
        auto optVal = GetOptionalSyntaxValue( condExpr->Clauses.back()->Condition.get() );

        if ( !optVal.has_value() || optVal.value() == 0 )
        {
            if ( bodyType->GetKind() != TypeKind::Int
                && bodyType->GetKind() != TypeKind::Xfer )
            {
                mRep.ThrowError( CERR_SEMANTICS, condExpr, "If without else must yield integers or nothing" );
            }
        }
    }
    else
    {
        bodyType = mIntType;
    }

    condExpr->Type = bodyType;
}

void BinderVisitor::VisitConstDecl( ConstDecl* constDecl )
{
    if ( constDecl->Decl )
        return;

    mGlobalTable.erase( constDecl->Name );

    VisitConstBinding( constDecl, ScopeKind::Global );
}

void BinderVisitor::VisitConstBinding( ConstDecl* constDecl, ScopeKind scopeKind )
{
    if ( !constDecl->Initializer )
        mRep.ThrowInternalError( "Missing constant initializer" );

    constDecl->Initializer->Accept( this );

    if ( constDecl->TypeRef )
    {
        constDecl->TypeRef->Accept( this );

        CheckType( constDecl->TypeRef->ReferentType, constDecl->Initializer->Type, constDecl->Initializer.get() );
    }

    std::shared_ptr<Type> type = constDecl->Initializer->Type;

    if ( type->GetKind() == TypeKind::Int )
    {
        int32_t value = Evaluate( constDecl->Initializer.get(), "Constant initializer is not constant" );

        std::shared_ptr<Constant> constant;

        if ( scopeKind == ScopeKind::Global )
            constant = AddConst( constDecl->Name, type, value, true );
        else
            constant = AddConst( constDecl->Name, type, value, *mSymStack.back() );

        constDecl->Decl = constant;
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, constDecl, "Only integer constants are supported" );
    }
}

void BinderVisitor::VisitCountofExpr( CountofExpr* countofExpr )
{
    countofExpr->Expr->Accept( this );

    if ( countofExpr->Expr->Type->GetKind() != TypeKind::Array )
        mRep.ThrowError( CERR_SEMANTICS, countofExpr->Expr.get(), "Countof applies to arrays" );

    countofExpr->Type = mIntType;
}

void BinderVisitor::VisitDotExpr( DotExpr* dotExpr )
{
    dotExpr->Head->Accept( this );

    if ( dotExpr->Head->Type->GetKind() == TypeKind::Module )
    {
        auto decl = dotExpr->Head->GetDecl();

        assert( decl->Kind == DeclKind::Module );

        auto modDecl = (ModuleDeclaration*) decl;

        auto it = modDecl->Table.find( dotExpr->Member );

        if ( it == modDecl->Table.end() )
            mRep.ThrowError( CERR_SEMANTICS, dotExpr, "Member not found: %s", dotExpr->Member.c_str() );

        dotExpr->Decl = it->second;
        dotExpr->Type = dotExpr->Decl->Type;
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, dotExpr->Head.get(), "Can only access members of a module" );
    }
}

void BinderVisitor::VisitForStatement( ForStatement* forStmt )
{
    LocalScope localScope( *this );

    auto local = AddLocal( forStmt->IndexName, mIntType, 1 );

    forStmt->IndexDecl = local;

    forStmt->First->Accept( this );
    forStmt->Last->Accept( this );

    if ( forStmt->Step )
        forStmt->Step->Accept( this );

    forStmt->Body.Accept( this );

    if ( forStmt->First->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, forStmt->First.get(), "For bounds only support integers" );

    if ( forStmt->Last->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, forStmt->Last.get(), "For bounds only support integers" );

    if ( forStmt->Step )
    {
        if ( forStmt->Step->Type->GetKind() != TypeKind::Int )
            mRep.ThrowError( CERR_SEMANTICS, forStmt->Step.get(), "For step only supports integers" );
    }

    forStmt->Type = mIntType;
}

void BinderVisitor::VisitImportDecl( ImportDecl* importDecl )
{
    if ( importDecl->Decl )
        return;

    mGlobalTable.erase( importDecl->Name );

    auto it = mModuleTable.find( importDecl->OriginalName );

    if ( it == mModuleTable.end() )
        mRep.ThrowError( CERR_SEMANTICS, importDecl, "Module not found" );

    AddModule( importDecl->Name, std::static_pointer_cast<ModuleDeclaration>( it->second ) );
}

void BinderVisitor::VisitIndexExpr( IndexExpr* indexExpr )
{
    indexExpr->Head->Accept( this );
    indexExpr->Index->Accept( this );

    if ( indexExpr->Head->Type->GetKind() != TypeKind::Array )
        mRep.ThrowError( CERR_SEMANTICS, indexExpr->Head.get(), "Only arrays can be indexed" );

    if ( indexExpr->Index->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, indexExpr->Index.get(), "Index only supports integers" );

    auto arrayType = (ArrayType*) indexExpr->Head->Type.get();

    if ( arrayType->Count > 0 )
    {
        std::optional<int32_t> optIndexVal = GetOptionalSyntaxValue( indexExpr->Index.get() );

        if ( optIndexVal.has_value() )
        {
            if ( optIndexVal.value() < 0 || optIndexVal.value() >= arrayType->Count )
                mRep.ThrowError( CERR_SEMANTICS, indexExpr->Index.get(), "Index must be within bounds of array" );
        }
    }

    indexExpr->Type = arrayType->ElemType;
}

void BinderVisitor::VisitInitList( InitList* initList )
{
    std::shared_ptr<Type> elemType;

    for ( auto& value : initList->Values )
    {
        value->Accept( this );

        if ( !elemType )
            elemType = value->Type;
        else
            CheckType( elemType, value->Type, value.get() );
    }

    if ( !elemType )
        elemType = mIntType;

    initList->Type = Make<ArrayType>( initList->Values.size(), elemType );
}

void BinderVisitor::VisitLambdaExpr( LambdaExpr* lambdaExpr )
{
    // In order to limit the special processing and call stack depth,
    // defer the lambda until the the end where it can be treated as
    // a top level procedure

    mLambdas.push_back( lambdaExpr );

    auto funcType = MakeFuncType( lambdaExpr->Proc.get() );

    lambdaExpr->Type = Make<PointerType>( funcType );

    {
        char name[32];

        sprintf_s( name, "$Lambda$%d", mLambdas.size() - 1 );

        std::shared_ptr<Function> func = AddFunc( name, INT32_MAX );

        lambdaExpr->Proc->Name = name;
        lambdaExpr->Proc->Decl = func;

        func->IsLambda = true;
        func->Type = funcType;
    }
}

void BinderVisitor::VisitLetStatement( LetStatement* letStmt )
{
    LocalScope localScope( *this );

    for ( auto& binding : letStmt->Variables )
    {
        if ( binding->Kind == SyntaxKind::VarDecl )
        {
            VisitLetBinding( binding.get() );
        }
        else if ( binding->Kind == SyntaxKind::ConstDecl )
        {
            VisitConstBinding( (ConstDecl*) binding.get(), ScopeKind::Local );
        }
    }

    letStmt->Body.Accept( this );

    CheckStatementType( &letStmt->Body );

    letStmt->Type = letStmt->Body.Type;
}

void BinderVisitor::VisitLetBinding( DataDecl* varDecl )
{
    VisitStorage( varDecl, DeclKind::Local );
}

void BinderVisitor::VisitStorage( DataDecl* varDecl, DeclKind declKind )
{
    std::shared_ptr<Type> type;

    if ( varDecl->TypeRef == nullptr )
    {
        if ( varDecl->Initializer != nullptr )
            varDecl->Initializer->Accept( this );

        if ( varDecl->Initializer == nullptr )
            type = mIntType;
        else
            type = varDecl->Initializer->Type;
    }
    else
    {
        varDecl->TypeRef->Accept( this );

        type = varDecl->TypeRef->ReferentType;

        if ( varDecl->Initializer != nullptr )
            CheckInitializer( type, varDecl->Initializer );
        else
            CheckAllDescendantsHaveDefault( type.get(), varDecl );
    }

    if ( !IsStorageType( type->GetKind() ) )
        mRep.ThrowError( CERR_SEMANTICS, varDecl, "Variables cannot take this type" );

    int32_t size = type->GetSize();

    if ( size == 0 )
    {
        mRep.ThrowInternalError( "Bad type" );
    }

    varDecl->Decl = AddStorage( varDecl->Name, type, size, declKind );
    varDecl->Type = type;
}

void BinderVisitor::CheckInitializer(
    const std::shared_ptr<Type>& type,
    const Unique<Syntax>& initializer )
{
    if ( initializer->Kind == SyntaxKind::ArrayInitializer )
    {
        if ( type->GetKind() != TypeKind::Array )
            mRep.ThrowError( CERR_SEMANTICS, initializer.get(), "Array initializer is invalid here" );

        auto& arrayInit = (InitList&) *initializer;
        auto& arrayType = (ArrayType&) *type;

        std::shared_ptr<Type> elemType = arrayType.ElemType;

        if ( arrayType.Count < (int32_t) arrayInit.Values.size() )
        {
            mRep.ThrowError( CERR_SEMANTICS, initializer.get(), "Wrong number of array elements" );
        }

        for ( auto& value : arrayInit.Values )
        {
            CheckInitializer( elemType, value );
        }

        if ( arrayInit.Fill == ArrayFill::Repeat )
        {
            if ( arrayInit.Values.size() < 1 )
                mRep.ThrowError( CERR_SEMANTICS, &arrayInit, "Element repetition requires at least one element" );
        }
        else if ( arrayInit.Fill == ArrayFill::Extrapolate )
        {
            if ( arrayInit.Values.size() < 2 )
                mRep.ThrowError( CERR_SEMANTICS, &arrayInit, "Element extrapolation requires at least two elements" );

            if ( !IsIntegralType( elemType->GetKind() ) )
                mRep.ThrowError( CERR_SEMANTICS, &arrayInit, "Elements must be integral to extrapolate them" );
        }
        else
        {
            if ( arrayInit.Values.size() < (size_t) arrayType.Count )
                CheckAllDescendantsHaveDefault( elemType.get(), initializer.get() );
        }

        initializer->Type = type;
    }
    else
    {
        initializer->Accept( this );

        CheckType( type, initializer->Type, initializer.get() );
    }
}

void BinderVisitor::CheckAllDescendantsHaveDefault( Type* type, Syntax* node )
{
    if ( type->GetKind() == TypeKind::Array )
    {
        auto arrayType = (ArrayType*) type;

        CheckAllDescendantsHaveDefault( arrayType->ElemType.get(), node );
    }
    else if ( type->GetKind() == TypeKind::Pointer )
    {
        mRep.ThrowError( CERR_SEMANTICS, node, "Pointers must be initialized" );
    }
}

void BinderVisitor::VisitLoopStatement( LoopStatement* loopStmt )
{
    loopStmt->Body.Accept( this );

    if ( loopStmt->Condition != nullptr )
    {
        loopStmt->Condition->Accept( this );

        if ( !IsBoolean( loopStmt->Condition->Type->GetKind() ) )
            mRep.ThrowError( CERR_SEMANTICS, loopStmt->Condition.get(), "Loop condition only supports integers" );
    }

    loopStmt->Type = mIntType;
}

void BinderVisitor::VisitNameExpr( NameExpr* nameExpr )
{
    auto decl = FindSymbol( nameExpr->String );

    if ( decl != nullptr )
    {
        if ( decl->Kind == DeclKind::Undefined )
            decl = DefineNode( nameExpr->String, (UndefinedDeclaration*) decl.get() );

        nameExpr->Decl = decl;
    }
    else
    {
        mRep.ThrowError( CERR_SEMANTICS, nameExpr, "symbol not found '%s'", nameExpr->String.c_str() );
    }

    nameExpr->Type = nameExpr->Decl->Type;
}

void BinderVisitor::VisitNameTypeRef( NameTypeRef* nameTypeRef )
{
    nameTypeRef->QualifiedName->Accept( this );

    auto decl = nameTypeRef->QualifiedName->GetDecl();

    if ( decl->Kind != DeclKind::Type )
        mRep.ThrowError( CERR_SEMANTICS, nameTypeRef, "Expected a type name" );

    nameTypeRef->Type = mTypeType;
    nameTypeRef->ReferentType = ((TypeDeclaration*) decl)->ReferentType;
}

void BinderVisitor::VisitNativeDecl( NativeDecl* nativeDecl )
{
    if ( nativeDecl->Decl )
        return;

    mGlobalTable.erase( nativeDecl->Name );

    std::shared_ptr<NativeFunction> native( new NativeFunction() );
    native->Kind = DeclKind::NativeFunc;
    native->Id = mNextNativeId;
    mGlobalTable.insert( SymTable::value_type( nativeDecl->Name, native ) );

    mNextNativeId++;

    native->Type = MakeFuncType( nativeDecl );

    nativeDecl->Decl = native;
}

void BinderVisitor::VisitNextStatement( NextStatement* nextStmt )
{
    nextStmt->Type = mXferType;
}

void BinderVisitor::VisitNumberExpr( NumberExpr* numberExpr )
{
    numberExpr->Type = mIntType;
}

void BinderVisitor::VisitParamDecl( ParamDecl* paramDecl )
{
    auto type = VisitParamTypeRef( paramDecl->TypeRef );

    paramDecl->Decl = AddParam( paramDecl->Name, type );
}

std::shared_ptr<Type> BinderVisitor::VisitParamTypeRef( Unique<TypeRef>& typeRef )
{
    std::shared_ptr<Type> type;

    if ( typeRef )
    {
        typeRef->Accept( this );

        if ( !IsAllowedParamType( typeRef->ReferentType->GetKind() ) )
            mRep.ThrowError( CERR_SEMANTICS, typeRef.get(), "This type is not allowed for parameters" );

        type = typeRef->ReferentType;
    }
    else
    {
        type = mIntType;
    }

    return type;
}

void BinderVisitor::VisitPointerTypeRef( PointerTypeRef* pointerTypeRef )
{
    pointerTypeRef->Target->Accept( this );

    if ( !IsAllowedPointerTarget( pointerTypeRef->Target->ReferentType->GetKind() ) )
        mRep.ThrowError( CERR_SEMANTICS, pointerTypeRef->Target.get(), "This type is not allowed for pointers" );

    auto pointerType = Make<PointerType>( pointerTypeRef->Target->ReferentType );

    pointerTypeRef->Type = mTypeType;
    pointerTypeRef->ReferentType = pointerType;
}

void BinderVisitor::VisitProcDecl( ProcDecl* procDecl )
{
    if ( procDecl->Decl )
        return;

    mGlobalTable.erase( procDecl->Name );

    auto forward = AddForward( procDecl->Name );

    forward->Type = MakeFuncType( procDecl );

    procDecl->Decl = forward;
}

void BinderVisitor::BindNamedProc( ProcDecl* procDecl )
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
        mRep.ThrowInternalError( "Function wasn't previously declared" );
    }

    VisitProc( procDecl );
}

void BinderVisitor::VisitProc( ProcDecl* procDecl )
{
    LocalScope paramScope( *this );

    auto func = (Function*) procDecl->Decl.get();

    if ( procDecl->Params.size() > ProcDecl::MaxParams )
    {
        mRep.ThrowError( CERR_SEMANTICS, procDecl, "'%s' has too many parameters. Max is %d",
            procDecl->Name.c_str(), ProcDecl::MaxParams );
    }

    for ( auto& parameter : procDecl->Params )
    {
        parameter->Accept( this );
    }

    mMaxLocalCount = 0;
    mCurLocalCount = 0;

    mCurFunc = func;

    procDecl->Body.Accept( this );

    mCurFunc = nullptr;

    if ( mMaxLocalCount > ProcDecl::MaxLocals )
    {
        mRep.ThrowError( CERR_SEMANTICS, procDecl, "'%s' has too many locals. Max is %d",
            procDecl->Name.c_str(), ProcDecl::MaxLocals );
    }

    func->LocalCount = mMaxLocalCount;
    func->ParamCount = (int16_t) procDecl->Params.size();

    auto funcType = (FuncType*) func->Type.get();

    CheckType( funcType->ReturnType, procDecl->Body.Type, &procDecl->Body );
}

void BinderVisitor::VisitProcTypeRef( ProcTypeRef* procTypeRef )
{
    std::shared_ptr<Type> returnType = VisitFuncReturnType( procTypeRef->ReturnTypeRef );

    auto funcType = Make<FuncType>( returnType );

    for ( auto& param : procTypeRef->Params )
    {
        param->Accept( this );

        funcType->ParamTypes.push_back( param->ReferentType );
    }

    procTypeRef->Type = mTypeType;
    procTypeRef->ReferentType = funcType;
}

void BinderVisitor::VisitReturnStatement( ReturnStatement* retStmt )
{
    retStmt->Inner->Accept( this );

    auto funcType = (FuncType*) mCurFunc->Type.get();

    CheckType( funcType->ReturnType, retStmt->Inner->Type, retStmt );

    retStmt->Type = mXferType;
}

void BinderVisitor::VisitSliceExpr( SliceExpr* sliceExpr )
{
    sliceExpr->Head->Accept( this );
    sliceExpr->FirstIndex->Accept( this );
    sliceExpr->LastIndex->Accept( this );

    if ( sliceExpr->Head->Type->GetKind() != TypeKind::Array )
        mRep.ThrowError( CERR_SEMANTICS, sliceExpr->Head.get(), "Only arrays can be sliced" );

    if ( sliceExpr->FirstIndex->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, sliceExpr->FirstIndex.get(), "Range bounds must be integers" );

    if ( sliceExpr->LastIndex->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, sliceExpr->LastIndex.get(), "Range bounds must be integers" );

    auto arrayType = (ArrayType*) sliceExpr->Head->Type.get();

    int32_t firstVal = Evaluate( sliceExpr->FirstIndex.get() );
    int32_t lastVal = Evaluate( sliceExpr->LastIndex.get() );

    if ( firstVal >= lastVal )
        mRep.ThrowError( CERR_SEMANTICS, sliceExpr->LastIndex.get(), "Range is not in increasing order" );

    if ( firstVal < 0 )
        mRep.ThrowError( CERR_SEMANTICS, sliceExpr->LastIndex.get(), "Slices must be within bounds of array" );

    if ( arrayType->Count > 0 )
    {
        if ( lastVal > arrayType->Count )
            mRep.ThrowError( CERR_SEMANTICS, sliceExpr->LastIndex.get(), "Slices must be within bounds of array" );
    }

    int32_t size = lastVal - firstVal;

    auto slicedArrayType = Make<ArrayType>( size, arrayType->ElemType );

    sliceExpr->Type = slicedArrayType;
}

void BinderVisitor::VisitStatementList( StatementList* stmtList )
{
    for ( auto& stmt : stmtList->Statements )
    {
        stmt->Accept( this );
    }

    if ( stmtList->Statements.size() == 0 )
    {
        stmtList->Type = mIntType;
    }
    else
    {
        if ( !IsStatementType( stmtList->Statements.back()->Type->GetKind() ) )
        {
            // REWRITING TREE:
            Unique<NumberExpr> zero( new NumberExpr() );

            zero->Accept( this );

            stmtList->Statements.push_back( std::move( zero ) );
        }

        stmtList->Type = stmtList->Statements.back()->Type;
    }
}

void BinderVisitor::VisitTypeDecl( TypeDecl* typeDecl )
{
    if ( typeDecl->Decl )
        return;

    mGlobalTable.erase( typeDecl->Name );

    typeDecl->TypeRef->Accept( this );

    AddType( typeDecl->Name, typeDecl->TypeRef->ReferentType, true );
}

void BinderVisitor::VisitUnaryExpr( UnaryExpr* unary )
{
    unary->Inner->Accept( this );

    if ( unary->Inner->Type->GetKind() != TypeKind::Int )
        mRep.ThrowError( CERR_SEMANTICS, unary->Inner.get(), "Unary expression only supports integers" );

    unary->Type = unary->Inner->Type;
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
    if ( varDecl->Decl )
        return;

    mGlobalTable.erase( varDecl->Name );

    VisitStorage( varDecl, DeclKind::Global );
}

void BinderVisitor::VisitWhileStatement( WhileStatement* whileStmt )
{
    whileStmt->Condition->Accept( this );

    if ( !IsBoolean( whileStmt->Condition->Type->GetKind() ) )
        mRep.ThrowError( CERR_SEMANTICS, whileStmt->Condition.get(), "While condition only supports integers" );

    whileStmt->Body.Accept( this );

    whileStmt->Type = mIntType;
}


void BinderVisitor::BindLambdas()
{
    for ( auto lambdaExpr : mLambdas )
    {
        VisitProc( lambdaExpr->Proc.get() );
    }
}


void BinderVisitor::CheckType(
    const std::shared_ptr<Type>& site,
    const std::shared_ptr<Type>& type,
    Syntax* node )
{
    CheckType( site.get(), type.get(), node );
}

void BinderVisitor::CheckType(
    Type* site,
    Type* type,
    Syntax* node )
{
    if ( site->GetKind() != TypeKind::Xfer
        && type->GetKind() != TypeKind::Xfer
        && !site->IsAssignableFrom( type ) )
    {
        mRep.ThrowError( CERR_SEMANTICS, node, "Incompatible assignment" );
    }
}

void BinderVisitor::CheckStatementType( Syntax* node )
{
    if ( !IsStatementType( node->Type->GetKind() ) )
        mRep.ThrowError( CERR_SEMANTICS, node, "Expected scalar type" );
}

void BinderVisitor::CheckAssignableType( Syntax* node )
{
    if ( !IsAssignableType( node->Type->GetKind() ) )
        mRep.ThrowError( CERR_SEMANTICS, node, "Expected scalar type" );
}

void BinderVisitor::CheckAndConsolidateClauseType( StatementList& clause, std::shared_ptr<Type>& bodyType )
{
    CheckAndConsolidateClauseType( &clause, bodyType );
}

void BinderVisitor::CheckAndConsolidateClauseType( Syntax* clause, std::shared_ptr<Type>& bodyType )
{
    CheckStatementType( clause );

    if ( !bodyType || bodyType->GetKind() == TypeKind::Xfer )
        bodyType = clause->Type;
    else
        CheckType( bodyType, clause->Type, clause );
}

I32 BinderVisitor::Evaluate( Syntax* node, const char* message )
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

std::optional<int32_t> BinderVisitor::GetOptionalSyntaxValue( Syntax* node )
{
    FolderVisitor folder( mRep.GetLog() );

    return folder.Evaluate( node );
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

std::shared_ptr<ParamStorage> BinderVisitor::AddParam( const std::string& name, std::shared_ptr<Type> type )
{
    auto& table = *mSymStack.back();

    std::shared_ptr<ParamStorage> param( new ParamStorage() );
    param->Kind = DeclKind::Param;
    param->Offset = table.size();
    param->Type = type;
    table.insert( SymTable::value_type( name, param ) );
    return param;
}

std::shared_ptr<LocalStorage> BinderVisitor::AddLocal( SymTable& table, const std::string& name, std::shared_ptr<Type> type, int offset )
{
    std::shared_ptr<LocalStorage> local( new LocalStorage() );
    local->Kind = DeclKind::Local;
    local->Offset = offset;
    local->Type = type;
    table.insert( SymTable::value_type( name, local ) );
    return local;
}

std::shared_ptr<LocalStorage> BinderVisitor::AddLocal( const std::string& name, std::shared_ptr<Type> type, size_t size )
{
    assert( size >= 1 );

    auto local = AddLocal( *mSymStack.back(), name, type, mCurLocalCount + size - 1 );

    mCurLocalCount += size;
    mCurLevelLocalCount += size;

    if ( mCurLocalCount > mMaxLocalCount )
        mMaxLocalCount = mCurLocalCount;

    if ( mMaxLocalCount > ProcDecl::MaxLocals )
        mRep.ThrowError( CERR_SEMANTICS, 0, 0, "Local exceeds capacity: %s", name.c_str() );

    return local;
}

std::shared_ptr<GlobalStorage> BinderVisitor::AddGlobal( const std::string& name, std::shared_ptr<Type> type, size_t size )
{
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<GlobalStorage> global( new GlobalStorage() );
    global->Kind = DeclKind::Global;
    global->Offset = mGlobalSize;
    global->ModIndex = mModIndex;
    global->Type = type;
    mGlobalTable.insert( SymTable::value_type( name, global ) );

    mGlobalSize += size;

    mPublicTable.insert( SymTable::value_type( name, global ) );

    return global;
}

std::shared_ptr<Declaration> BinderVisitor::AddStorage( const std::string& name, std::shared_ptr<Type> type, size_t size, DeclKind declKind )
{
    switch ( declKind )
    {
    case DeclKind::Global:  return AddGlobal( name, type, size );
    case DeclKind::Local:   return AddLocal( name, type, size );
    default:
        mRep.ThrowInternalError();
    }
}

std::shared_ptr<Constant> BinderVisitor::AddConst( const std::string& name, std::shared_ptr<Type> type, int32_t value, SymTable& table )
{
    std::shared_ptr<Constant> constant( new Constant() );
    constant->Kind = DeclKind::Const;
    constant->Type = type;
    constant->Value = value;
    table.insert( SymTable::value_type( name, constant ) );
    return constant;
}

std::shared_ptr<Constant> BinderVisitor::AddConst( const std::string& name, std::shared_ptr<Type> type, int32_t value, bool isPublic )
{
    CheckDuplicateGlobalSymbol( name );

    auto constant = AddConst( name, type, value, mGlobalTable );

    if ( isPublic )
        mPublicTable.insert( SymTable::value_type( name, constant ) );

    return constant;
}

std::shared_ptr<Function> BinderVisitor::AddFunc( const std::string& name, int address )
{
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<Function> func( new Function() );
    func->Kind = DeclKind::Func;
    func->Name = name;
    func->Address = address;
    func->ModIndex = mModIndex;
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
    func->ModIndex = mModIndex;
    mGlobalTable.insert( SymTable::value_type( name, func ) );

    mPublicTable.insert( SymTable::value_type( name, func ) );

    return func;
}

std::shared_ptr<TypeDeclaration> BinderVisitor::AddType( const std::string& name, std::shared_ptr<Type> type, bool isPublic )
{
    CheckDuplicateGlobalSymbol( name );

    std::shared_ptr<TypeDeclaration> typeDecl( new TypeDeclaration() );
    typeDecl->Kind = DeclKind::Type;
    typeDecl->Type = mTypeType;
    typeDecl->ReferentType = type;
    mGlobalTable.insert( SymTable::value_type( name, typeDecl ) );

    if ( isPublic )
        mPublicTable.insert( SymTable::value_type( name, typeDecl ) );

    return typeDecl;
}

void BinderVisitor::AddModule( const std::string& name, std::shared_ptr<ModuleDeclaration> moduleDecl )
{
    CheckDuplicateGlobalSymbol( name );

    mGlobalTable.insert( SymTable::value_type( name, moduleDecl ) );
}

void BinderVisitor::CheckDuplicateGlobalSymbol( const std::string& name )
{
    if ( mGlobalTable.find( name ) != mGlobalTable.end() )
        mRep.ThrowError( CERR_SEMANTICS, nullptr, "Duplicate symbol: %s", name.c_str() );
}

void BinderVisitor::MakeStdEnv()
{
    mTypeType.reset( new TypeType() );
    mModuleType.reset( new ModuleType() );
    mXferType.reset( new XferType() );
    mIntType.reset( new IntType() );

    AddType( "int", mIntType, false );
    AddConst( "false", mIntType, 0, false );
    AddConst( "true", mIntType, 1, false );
}

void BinderVisitor::BindProcs( Unit* program )
{
    for ( auto& elem : program->FuncDeclarations )
    {
        BindNamedProc( elem.get() );
    }
}

void BinderVisitor::DeclareNode( DeclSyntax* node )
{
    CheckDuplicateGlobalSymbol( node->Name );

    std::shared_ptr<UndefinedDeclaration> undef( new UndefinedDeclaration() );
    undef->Kind = DeclKind::Undefined;
    undef->Node = node;
    mGlobalTable.insert( SymTable::value_type( node->Name, undef ) );
}

std::shared_ptr<Declaration> BinderVisitor::DefineNode( const std::string& name, UndefinedDeclaration* decl )
{
    // The first thing that the declaration nodes do is to erase the "undefined" declaration
    // to make room for the defined declaration. This also prevents getting stuck in loops.
    // Save the node that the undefined declaration refers to.

    Syntax* node = decl->Node;

    node->Accept( this );

    return FindSymbol( name );
}

std::shared_ptr<FuncType> BinderVisitor::MakeFuncType( ProcDeclBase* procDecl )
{
    std::shared_ptr<Type> returnType = VisitFuncReturnType( procDecl->ReturnTypeRef );

    auto funcType = Make<FuncType>( returnType );

    for ( auto& paramDecl : procDecl->Params )
    {
        auto type = VisitParamTypeRef( paramDecl->TypeRef );

        funcType->ParamTypes.push_back( type );
    }

    return funcType;
}

std::shared_ptr<Type> BinderVisitor::VisitFuncReturnType( Unique<TypeRef>& typeRef )
{
    if ( typeRef )
    {
        typeRef->Accept( this );

        if ( !IsScalarType( typeRef->ReferentType->GetKind() ) )
            mRep.ThrowError( CERR_SEMANTICS, typeRef.get(), "Only scalar types can be returned" );

        return typeRef->ReferentType;
    }

    return mIntType;
}
