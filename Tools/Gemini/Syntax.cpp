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

NumberExpr::NumberExpr( int32_t value ) :
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


std::optional<int32_t> GetOptionalSyntaxValue( Syntax* node )
{
    if ( node->Kind == SyntaxKind::Number )
    {
        auto number = (NumberExpr*) node;
        return number->Value;
    }
    else if ( node->Kind == SyntaxKind::Name )
    {
        auto decl = ((NameExpr*) node)->Decl.get();

        if ( decl != nullptr && decl->Kind == DeclKind::Const )
        {
            auto constant = (Constant*) decl;
            return constant->Value;
        }
    }

    return std::optional<int32_t>();
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

bool Type::IsAssignableFrom( Type* other ) const
{
    return false;
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

bool XferType::IsAssignableFrom( Type* other ) const
{
    return other != nullptr
        && other->GetKind() == TypeKind::Xfer;
}

IntType::IntType() :
    Type( TypeKind::Int )
{
}

bool IntType::IsAssignableFrom( Type* other ) const
{
    return other != nullptr
        && (other->GetKind() == TypeKind::Int
            || other->GetKind() == TypeKind::Xfer);
}

int32_t IntType::GetSize() const
{
    return 1;
}

ArrayType::ArrayType( int32_t size, std::shared_ptr<Type> elemType ) :
    Type( TypeKind::Array ),
    Size( size ),
    ElemType( elemType )
{
}

bool ArrayType::IsAssignableFrom( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Array )
        return false;

    auto otherArray = (ArrayType*) other;

    return Size >= otherArray->Size
        && ElemType->IsAssignableFrom( otherArray->ElemType.get() );
}

int32_t ArrayType::GetSize() const
{
    return Size;
}

FuncType::FuncType( std::shared_ptr<Type> returnType ) :
    Type( TypeKind::Func ),
    ReturnType( returnType )
{
}

bool FuncType::IsAssignableFrom( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Func )
        return false;

    auto otherFunc = (FuncType*) other;

    if ( !ReturnType->IsAssignableFrom( otherFunc->ReturnType.get() )
        || ParamTypes.size() != otherFunc->ParamTypes.size() )
        return false;

    for ( int i = 0; i < (int) ParamTypes.size(); i++ )
    {
        if ( !ParamTypes[i]->IsAssignableFrom( otherFunc->ParamTypes[i].get() ) )
            return false;
    }

    return true;
}

PointerType::PointerType( std::shared_ptr<Type> target ) :
    Type( TypeKind::Pointer ),
    TargetType( target )
{
}

bool PointerType::IsAssignableFrom( Type* other ) const
{
    if ( other == nullptr || other->GetKind() != TypeKind::Pointer )
        return false;

    auto otherPointer = (PointerType*) other;

    return TargetType->IsAssignableFrom( otherPointer->TargetType.get() );
}

int32_t PointerType::GetSize() const
{
    return 1;
}
