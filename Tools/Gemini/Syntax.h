// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>


enum class ScopeKind
{
    Global,
    Local,
};


enum class SyntaxKind
{
    Number,
    Name,
    AddrOfExpr,
    Index,
    DotExpr,
    ArrayTypeRef,
    ArrayInitializer,
    ConstDecl,
    VarDecl,
    Other,
};


class IVisitor;

class Syntax;
class ProcDecl;

struct Declaration;

class Type;


template <class T = Syntax>
using Unique = std::unique_ptr<T, std::default_delete<Syntax>>;


class Syntax
{
public:
    SyntaxKind Kind = SyntaxKind::Other;
    int Line = 0;
    int Column = 0;
    const char* FileName = nullptr;

    // All nodes in the same syntax tree refer to the file name string in the root Unit

    std::shared_ptr<Type>   Type;

    virtual ~Syntax() {}
    virtual void Accept( IVisitor* visitor ) = 0;
    virtual Declaration* GetDecl();
};

class StatementList : public Syntax
{
public:
    std::vector<Unique<Syntax>> Statements;

    virtual void Accept( IVisitor* visitor ) override;
};

class NameExpr : public Syntax
{
public:
    std::shared_ptr<Declaration> Decl;
    std::string String;

    NameExpr();
    NameExpr( const std::string& str );
    NameExpr( std::string&& str );

    virtual void Accept( IVisitor* visitor ) override;
    virtual Declaration* GetDecl() override;
};

class NumberExpr : public Syntax
{
public:
    int64_t Value;

    NumberExpr();
    NumberExpr( int64_t value );

    virtual void Accept( IVisitor* visitor ) override;
};

class TypeRef : public Syntax
{
public:
    std::shared_ptr<::Type> ReferentType;
};

class NameTypeRef : public TypeRef
{
public:
    Unique<Syntax>  QualifiedName;

    virtual void Accept( IVisitor* visitor ) override;
};

class ArrayTypeRef : public TypeRef
{
public:
    Unique<Syntax>  SizeExpr;
    Unique<TypeRef> ElementTypeRef;

    ArrayTypeRef();

    virtual void Accept( IVisitor* visitor ) override;
};

class ProcTypeRef : public TypeRef
{
public:
    std::vector<Unique<TypeRef>> Params;
    Unique<TypeRef>              ReturnTypeRef;

    virtual void Accept( IVisitor* visitor ) override;
};

class PointerTypeRef : public TypeRef
{
public:
    Unique<TypeRef> Target;

    virtual void Accept( IVisitor* visitor ) override;
};

enum class ArrayFill
{
    None,
    Repeat,
    Extrapolate,
};

class InitList : public Syntax
{
public:
    ArrayFill Fill = ArrayFill::None;

    std::vector<Unique<Syntax>> Values;

    InitList();

    virtual void Accept( IVisitor* visitor ) override;
};

class DeclSyntax : public Syntax
{
public:
    std::shared_ptr<Declaration> Decl;

    std::string Name;

    virtual Declaration* GetDecl() override;
};

class DataDecl : public DeclSyntax
{
public:
    Unique<TypeRef>    TypeRef;
    Unique<Syntax>     Initializer;
};

class ConstDecl : public DataDecl
{
public:
    ConstDecl();

    virtual void Accept( IVisitor* visitor ) override;
};

class VarDecl : public DataDecl
{
public:
    VarDecl();

    virtual void Accept( IVisitor* visitor ) override;
};

class ParamDecl : public DataDecl
{
public:
    virtual void Accept( IVisitor* visitor ) override;
};

class TypeDecl : public DeclSyntax
{
public:
    Unique<TypeRef>    TypeRef;

    virtual void Accept( IVisitor* visitor ) override;
};

class LambdaExpr : public Syntax
{
public:
    Unique<ProcDecl> Proc;

    virtual void Accept( IVisitor* visitor ) override;
};

class CondClause : public Syntax
{
public:
    Unique<Syntax> Condition;
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class CondExpr : public Syntax
{
public:
    std::vector<Unique<CondClause>> Clauses;
    bool IsIf = false;

    virtual void Accept( IVisitor* visitor ) override;
};

class CaseWhen : public Syntax
{
public:
    std::vector<Unique<Syntax>> Keys;
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class CaseElse : public Syntax
{
public:
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class CaseExpr : public Syntax
{
public:
    Unique<Syntax> TestKey;
    Unique<CaseElse> Fallback;
    std::vector<Unique<CaseWhen>> Clauses;

    std::shared_ptr<Declaration> TestKeyDecl;

    virtual void Accept( IVisitor* visitor ) override;
};

class BinaryExpr : public Syntax
{
public:
    std::string Op;
    Unique<Syntax> Left;
    Unique<Syntax> Right;

    virtual void Accept( IVisitor* visitor ) override;
};

class UnaryExpr : public Syntax
{
public:
    std::string Op;
    Unique<Syntax> Inner;

    virtual void Accept( IVisitor* visitor ) override;
};

class AddrOfExpr : public Syntax
{
public:
    Unique<Syntax> Inner;

    AddrOfExpr();

    virtual void Accept( IVisitor* visitor ) override;
};

class IndexExpr : public Syntax
{
public:
    Unique<Syntax> Head;
    Unique<Syntax> Index;

    IndexExpr();

    virtual void Accept( IVisitor* visitor ) override;
};

class SliceExpr : public Syntax
{
public:
    Unique<Syntax> Head;
    Unique<Syntax> FirstIndex;
    Unique<Syntax> LastIndex;

    virtual void Accept( IVisitor* visitor ) override;
};

class DotExpr : public Syntax
{
public:
    Unique<Syntax> Head;
    std::string Member;

    std::shared_ptr<Declaration> Decl;

    DotExpr();

    virtual void Accept( IVisitor* visitor ) override;
    virtual Declaration* GetDecl() override;
};

class CallExpr : public Syntax
{
public:
    bool IsIndirect = false;
    Unique<Syntax> Head;
    std::vector<Unique<Syntax>> Arguments;

    virtual void Accept( IVisitor* visitor ) override;
};

class CallOrSymbolExpr : public Syntax
{
public:
    Unique<Syntax> Symbol;

    virtual void Accept( IVisitor* visitor ) override;
};

class AssignmentExpr : public Syntax
{
public:
    Unique<Syntax> Left;
    Unique<Syntax> Right;

    virtual void Accept( IVisitor* visitor ) override;
};

class CountofExpr : public Syntax
{
public:
    Unique<Syntax>      Expr;

    virtual void Accept( IVisitor* visitor ) override;
};

class LetStatement : public Syntax
{
public:
    std::vector<Unique<DataDecl>> Variables;
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class ReturnStatement : public Syntax
{
public:
    Unique<Syntax> Inner;

    virtual void Accept( IVisitor* visitor ) override;
};

class ForStatement : public Syntax
{
public:
    std::string IndexName;
    std::string Comparison;
    Unique<Syntax> First;
    Unique<Syntax> Last;
    Unique<Syntax> Step;
    StatementList Body;

    std::shared_ptr<Declaration> IndexDecl;

    virtual void Accept( IVisitor* visitor ) override;
};

class LoopStatement : public Syntax
{
public:
    StatementList Body;
    Unique<Syntax> Condition;

    virtual void Accept( IVisitor* visitor ) override;
};

class WhileStatement : public Syntax
{
public:
    Unique<Syntax> Condition;
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class BreakStatement : public Syntax
{
public:
    virtual void Accept( IVisitor* visitor ) override;
};

class NextStatement : public Syntax
{
public:
    virtual void Accept( IVisitor* visitor ) override;
};

class ProcDeclBase : public DeclSyntax
{
public:
    constexpr static int16_t MaxParams = 127;
    constexpr static int16_t MaxLocals = 127;

    std::vector<Unique<DataDecl>>   Params;
    Unique<TypeRef>                 ReturnTypeRef;
};

class ProcDecl : public ProcDeclBase
{
public:
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class NativeDecl : public ProcDeclBase
{
public:
    virtual void Accept( IVisitor* visitor ) override;
};

class ImportDecl : public DeclSyntax
{
public:
    std::string OriginalName;

    virtual void Accept( IVisitor* visitor ) override;
};

class Unit : public Syntax
{
    // All nodes in the syntax tree rooted in this Unit refer to this string
    std::vector<char> mFileName;

public:
    std::vector<Unique<DeclSyntax>> DataDeclarations;
    std::vector<Unique<ProcDecl>> FuncDeclarations;

    Unit( const std::string& fileName );

    const char* GetUnitFileName();

    virtual void Accept( IVisitor* visitor ) override;
};

std::optional<int32_t> GetOptionalSyntaxValue( Syntax* node );


//----------------------------------------------------------------------------
//  Visitors
//----------------------------------------------------------------------------

class IVisitor
{
public:
    virtual void VisitAddrOfExpr( AddrOfExpr* addrOf );
    virtual void VisitArrayTypeRef( ArrayTypeRef* typeRef );
    virtual void VisitAssignmentExpr( AssignmentExpr* assignment );
    virtual void VisitBinaryExpr( BinaryExpr* binary );
    virtual void VisitBreakStatement( BreakStatement* breakStmt );
    virtual void VisitCallExpr( CallExpr* call );
    virtual void VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol );
    virtual void VisitCaseExpr( CaseExpr* caseExpr );
    virtual void VisitCondExpr( CondExpr* condExpr );
    virtual void VisitConstDecl( ConstDecl* constDecl );
    virtual void VisitCountofExpr( CountofExpr* countofExpr );
    virtual void VisitDotExpr( DotExpr* dotExpr );
    virtual void VisitForStatement( ForStatement* forStmt );
    virtual void VisitImportDecl( ImportDecl* importDecl );
    virtual void VisitIndexExpr( IndexExpr* indexExpr );
    virtual void VisitInitList( InitList* initList );
    virtual void VisitLambdaExpr( LambdaExpr* lambdaExpr );
    virtual void VisitLetStatement( LetStatement* letStmt );
    virtual void VisitLoopStatement( LoopStatement* loopStmt );
    virtual void VisitNameExpr( NameExpr* nameExpr );
    virtual void VisitNameTypeRef( NameTypeRef* nameTypeRef );
    virtual void VisitNativeDecl( NativeDecl* nativeDecl );
    virtual void VisitNextStatement( NextStatement* nextStmt );
    virtual void VisitNumberExpr( NumberExpr* numberExpr );
    virtual void VisitParamDecl( ParamDecl* paramDecl );
    virtual void VisitPointerTypeRef( PointerTypeRef* pointerTypeRef );
    virtual void VisitProcDecl( ProcDecl* procDecl );
    virtual void VisitProcTypeRef( ProcTypeRef* procTypeRef );
    virtual void VisitReturnStatement( ReturnStatement* retStmt );
    virtual void VisitSliceExpr( SliceExpr* sliceExpr );
    virtual void VisitStatementList( StatementList* stmtmList );
    virtual void VisitTypeDecl( TypeDecl* typeDecl );
    virtual void VisitUnaryExpr( UnaryExpr* unary );
    virtual void VisitUnit( Unit* unit );
    virtual void VisitVarDecl( VarDecl* varDecl );
    virtual void VisitWhileStatement( WhileStatement* whileStmt );
};


//----------------------------------------------------------------------------
//  Declarations
//----------------------------------------------------------------------------

enum class DeclKind
{
    Undefined,
    Const,
    Global,
    Local,
    Param,
    Func,
    Forward,
    NativeFunc,
    Type,
    Module,
    LoadedAddress,
};

struct Declaration
{
    DeclKind  Kind;
    std::shared_ptr<Type>   Type;

    virtual ~Declaration() { }
};

using SymTable = std::map<std::string, std::shared_ptr<Declaration>>;

struct UndefinedDeclaration : public Declaration
{
    Syntax* Node;
};

struct Constant : public Declaration
{
    int Value;
};

struct GlobalStorage : public Declaration
{
    int Offset;
    int ModIndex;
};

struct LocalStorage : public Declaration
{
    int Offset;
};

struct ParamStorage : public Declaration
{
    int Offset;
};

struct CallSite
{
    int16_t     ExprDepth;
    std::string FunctionName;
};

struct Function : public Declaration
{
    std::string Name;
    int         Address;
    int         ModIndex;
    bool        IsLambda;

    int16_t     LocalCount;
    int16_t     ParamCount;
    int16_t     ExprDepth;

    int16_t     CallDepth;
    int16_t     IndividualStackUsage;
    int16_t     TreeStackUsage;

    bool        IsCalculating;
    bool        IsRecursive;
    bool        IsDepthKnown;
    bool        CallsIndirectly;

    std::list<CallSite> CalledFunctions;
};

struct NativeFunction : public Declaration
{
    int32_t Id;
};

struct TypeDeclaration : public Declaration
{
    std::shared_ptr<::Type> ReferentType;
};

struct ModuleDeclaration : public Declaration
{
    std::string Name;
    SymTable    Table;
};

struct LoadedAddressDeclaration : public Declaration
{
};


//----------------------------------------------------------------------------
//  Types
//----------------------------------------------------------------------------

enum class TypeKind
{
    Type,
    Module,
    Xfer,
    Int,
    Array,
    Func,
    Pointer,
};

class Type
{
    TypeKind    mKind;

protected:
    Type( TypeKind kind );

public:
    TypeKind GetKind() const;
    virtual bool IsEqual( Type* other ) const;
    virtual bool IsAssignableFrom( Type* other ) const;
    virtual int32_t GetSize() const;
};

class TypeType : public Type
{
public:
    TypeType();
};

class ModuleType : public Type
{
public:
    ModuleType();
};

class XferType : public Type
{
public:
    XferType();

    virtual bool IsEqual( Type* other ) const override;
};

class IntType : public Type
{
public:
    IntType();

    virtual bool IsEqual( Type* other ) const override;
    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual int32_t GetSize() const override;
};

class ArrayType : public Type
{
public:
    int32_t Count;
    std::shared_ptr<Type> ElemType;

    ArrayType( int32_t count, std::shared_ptr<Type> elemType );

    virtual bool IsEqual( Type* other ) const override;
    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual int32_t GetSize() const override;
};

class FuncType : public Type
{
public:
    std::shared_ptr<Type>               ReturnType;
    std::vector<std::shared_ptr<Type>>  ParamTypes;

    FuncType( std::shared_ptr<Type> returnType );

    virtual bool IsEqual( Type* other ) const override;
};

class PointerType : public Type
{
public:
    std::shared_ptr<Type>   TargetType;

    PointerType( std::shared_ptr<Type> target );

    virtual bool IsEqual( Type* other ) const override;
    virtual int32_t GetSize() const override;
};
