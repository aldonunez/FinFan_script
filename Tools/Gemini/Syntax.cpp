// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "stdafx.h"
#include "Syntax.h"


Declaration* Syntax::GetDecl()
{
    return nullptr;
}

NameExpr::NameExpr()
{
    Kind = SyntaxKind::Name;
}

NameExpr::NameExpr( const std::string& str ) :
    String( str )
{
    Kind = SyntaxKind::Name;
}

NameExpr::NameExpr( std::string&& str ) :
    String( str )
{
    Kind = SyntaxKind::Name;
}

Declaration* NameExpr::GetDecl()
{
    return Decl.get();
}

NumberExpr::NumberExpr() :
    NumberExpr( 0 )
{
}

NumberExpr::NumberExpr( int64_t value ) :
    Value( value )
{
    Kind = SyntaxKind::Number;
}

ArrayTypeRef::ArrayTypeRef()
{
    Kind = SyntaxKind::ArrayTypeRef;
}

InitList::InitList()
{
    Kind = SyntaxKind::ArrayInitializer;
}

Declaration* DeclSyntax::GetDecl()
{
    return Decl.get();
}

ConstDecl::ConstDecl()
{
    Kind = SyntaxKind::ConstDecl;
}

VarDecl::VarDecl()
{
    Kind = SyntaxKind::VarDecl;
}

AddrOfExpr::AddrOfExpr()
{
    Kind = SyntaxKind::AddrOfExpr;
}

IndexExpr::IndexExpr()
{
    Kind = SyntaxKind::Index;
}

DotExpr::DotExpr()
{
    Kind = SyntaxKind::DotExpr;
}

Declaration* DotExpr::GetDecl()
{
    return Decl.get();
}

Unit::Unit( const std::string& fileName )
{
    mFileName.resize( fileName.size() + 1 );

    memcpy( mFileName.data(), fileName.data(), fileName.size() );
    mFileName[fileName.size()] = '\0';

    FileName = GetUnitFileName();
}

const char* Unit::GetUnitFileName()
{
    return mFileName.data();
}


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

void CaseElse::Accept( IVisitor* visitor )
{
    // There's no entry in IVisitor for this node
}

void CaseExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCaseExpr( this );
}

void CaseWhen::Accept( IVisitor* visitor )
{
    // There's no entry in IVisitor for this node
}

void CondClause::Accept( IVisitor* visitor )
{
    // There's no entry in IVisitor for this node
}

void CondExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCondExpr( this );
}

void ConstDecl::Accept( IVisitor* visitor )
{
    visitor->VisitConstDecl( this );
}

void CountofExpr::Accept( IVisitor* visitor )
{
    visitor->VisitCountofExpr( this );
}

void DotExpr::Accept( IVisitor* visitor )
{
    visitor->VisitDotExpr( this );
}

void ForStatement::Accept( IVisitor* visitor )
{
    visitor->VisitForStatement( this );
}

void ImportDecl::Accept( IVisitor* visitor )
{
    visitor->VisitImportDecl( this );
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

void NameTypeRef::Accept( IVisitor* visitor )
{
    visitor->VisitNameTypeRef( this );
}

void NextStatement::Accept( IVisitor* visitor )
{
    visitor->VisitNextStatement( this );
}

void NativeDecl::Accept( IVisitor* visitor )
{
    visitor->VisitNativeDecl( this );
}

void NumberExpr::Accept( IVisitor* visitor )
{
    visitor->VisitNumberExpr( this );
}

void ParamDecl::Accept( IVisitor* visitor )
{
    visitor->VisitParamDecl( this );
}

void PointerTypeRef::Accept( IVisitor* visitor )
{
    visitor->VisitPointerTypeRef( this );
}

void ProcDecl::Accept( IVisitor* visitor )
{
    visitor->VisitProcDecl( this );
}

void ProcTypeRef::Accept( IVisitor* visitor )
{
    visitor->VisitProcTypeRef( this );
}

void ReturnStatement::Accept( IVisitor* visitor )
{
    visitor->VisitReturnStatement( this );
}

void SliceExpr::Accept( IVisitor* visitor )
{
    visitor->VisitSliceExpr( this );
}

void StatementList::Accept( IVisitor* visitor )
{
    visitor->VisitStatementList( this );
}

void TypeDecl::Accept( IVisitor* visitor )
{
    visitor->VisitTypeDecl( this );
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


std::optional<int32_t> GetOptionalSyntaxValue( Syntax* node )
{
    if ( node->Kind == SyntaxKind::Number )
    {
        auto number = (NumberExpr*) node;

        assert( number->Value >= INT32_MIN && number->Value <= INT32_MAX );

        return (int32_t) number->Value;
    }

    return std::optional<int32_t>();
}


//----------------------------------------------------------------------------
//  Visitors
//----------------------------------------------------------------------------

void IVisitor::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
}

void IVisitor::VisitArrayTypeRef( ArrayTypeRef* typeRef )
{
}

void IVisitor::VisitAssignmentExpr( AssignmentExpr* assignment )
{
}

void IVisitor::VisitBinaryExpr( BinaryExpr* binary )
{
}

void IVisitor::VisitBreakStatement( BreakStatement* breakStmt )
{
}

void IVisitor::VisitCallExpr( CallExpr* call )
{
}

void IVisitor::VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol )
{
}

void IVisitor::VisitCaseExpr( CaseExpr* caseExpr )
{
}

void IVisitor::VisitCondExpr( CondExpr* condExpr )
{
}

void IVisitor::VisitConstDecl( ConstDecl* constDecl )
{
}

void IVisitor::VisitCountofExpr( CountofExpr* countofExpr )
{
}

void IVisitor::VisitDotExpr( DotExpr* dotExpr )
{
}

void IVisitor::VisitForStatement( ForStatement* forStmt )
{
}

void IVisitor::VisitImportDecl( ImportDecl* importDecl )
{
}

void IVisitor::VisitIndexExpr( IndexExpr* indexExpr )
{
}

void IVisitor::VisitInitList( InitList* initList )
{
}

void IVisitor::VisitLambdaExpr( LambdaExpr* lambdaExpr )
{
}

void IVisitor::VisitLetStatement( LetStatement* letStmt )
{
}

void IVisitor::VisitLoopStatement( LoopStatement* loopStmt )
{
}

void IVisitor::VisitNameExpr( NameExpr* nameExpr )
{
}

void IVisitor::VisitNameTypeRef( NameTypeRef* nameTypeRef )
{
}

void IVisitor::VisitNativeDecl( NativeDecl* nativeDecl )
{
}

void IVisitor::VisitNextStatement( NextStatement* nextStmt )
{
}

void IVisitor::VisitNumberExpr( NumberExpr* numberExpr )
{
}

void IVisitor::VisitParamDecl( ParamDecl* paramDecl )
{
}

void IVisitor::VisitPointerTypeRef( PointerTypeRef* pointerTypeRef )
{
}

void IVisitor::VisitProcDecl( ProcDecl* procDecl )
{
}

void IVisitor::VisitProcTypeRef( ProcTypeRef* procTypeRef )
{
}

void IVisitor::VisitReturnStatement( ReturnStatement* retStmt )
{
}

void IVisitor::VisitSliceExpr( SliceExpr* sliceExpr )
{
}

void IVisitor::VisitStatementList( StatementList* stmtmList )
{
}

void IVisitor::VisitTypeDecl( TypeDecl* typeDecl )
{
}

void IVisitor::VisitUnaryExpr( UnaryExpr* unary )
{
}

void IVisitor::VisitUnit( Unit* unit )
{
}

void IVisitor::VisitVarDecl( VarDecl* varDecl )
{
}

void IVisitor::VisitWhileStatement( WhileStatement* whileStmt )
{
}


//----------------------------------------------------------------------------
//  Types
//----------------------------------------------------------------------------

Type::Type( TypeKind kind ) :
    mKind( kind )
{
}

TypeKind Type::GetKind() const
{
    return mKind;
}

bool Type::IsEqual( Type* other ) const
{
    return false;
}

bool Type::IsAssignableFrom( Type* other ) const
{
    return IsEqual( other );
}

int32_t Type::GetSize() const
{
    return 0;
}


TypeType::TypeType() :
    Type( TypeKind::Type )
{
}


ModuleType::ModuleType() :
    Type( TypeKind::Module )
{
}


XferType::XferType() :
    Type( TypeKind::Xfer )
{
}

bool XferType::IsEqual( Type* other ) const
{
    return other != nullptr
        && other->GetKind() == TypeKind::Xfer;
}


IntType::IntType() :
    Type( TypeKind::Int )
{
}

bool IntType::IsEqual( Type* other ) const
{
    return other != nullptr
        && (other->GetKind() == TypeKind::Int);
}

bool IntType::IsAssignableFrom( Type* other ) const
{
    return other != nullptr
        && (other->GetKind() == TypeKind::Int
            || other->GetKind() == TypeKind::Xfer
            );
}

int32_t IntType::GetSize() const
{
    return 1;
}


ArrayType::ArrayType( int32_t count, std::shared_ptr<Type> elemType ) :
    Type( TypeKind::Array ),
    Count( count ),
    ElemType( elemType )
{
}

bool ArrayType::IsEqual( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Array )
        return false;

    auto otherArray = (ArrayType*) other;

    if ( !ElemType->IsEqual( otherArray->ElemType.get() ) )
        return false;

    return Count == otherArray->Count;
}

bool ArrayType::IsAssignableFrom( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Array )
        return false;

    auto otherArray = (ArrayType*) other;

    if ( !ElemType->IsEqual( otherArray->ElemType.get() ) )
        return false;

    if ( ElemType->GetKind() == TypeKind::Pointer )
        return Count == otherArray->Count;

    return Count >= otherArray->Count;
}

int32_t ArrayType::GetSize() const
{
    return Count * ElemType->GetSize();
}


FuncType::FuncType( std::shared_ptr<Type> returnType ) :
    Type( TypeKind::Func ),
    ReturnType( returnType )
{
}

bool FuncType::IsEqual( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Func )
        return false;

    auto otherFunc = (FuncType*) other;

    if ( !ReturnType->IsEqual( otherFunc->ReturnType.get() )
        || ParamTypes.size() != otherFunc->ParamTypes.size() )
        return false;

    for ( int i = 0; i < (int) ParamTypes.size(); i++ )
    {
        if ( !ParamTypes[i]->IsEqual( otherFunc->ParamTypes[i].get() ) )
            return false;
    }

    return true;
}


PointerType::PointerType( std::shared_ptr<Type> target ) :
    Type( TypeKind::Pointer ),
    TargetType( target )
{
}

bool PointerType::IsEqual( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Pointer )
        return false;

    auto otherPointer = (PointerType*) other;

    return TargetType->IsEqual( otherPointer->TargetType.get() );
}

int32_t PointerType::GetSize() const
{
    return 1;
}
