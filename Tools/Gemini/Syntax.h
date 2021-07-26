// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "Common.h"
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>


namespace Gemini
{

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
    AsExpr,
    Index,
    Slice,
    DotExpr,
    ArrayInitializer,
    RecordInitializer,
    ConstDecl,
    VarDecl,
    ParamDecl,
    Other,
};


constexpr CodeSize      UndefinedAddr = 16777215;


class Visitor;

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

    std::shared_ptr<Gemini::Type>   Type;

    virtual ~Syntax() {}
    virtual void Accept( Visitor* visitor ) = 0;
    virtual Declaration* GetDecl();
    virtual std::shared_ptr<Declaration> GetSharedDecl();
};

class StatementList : public Syntax
{
public:
    std::vector<Unique<Syntax>> Statements;

    virtual void Accept( Visitor* visitor ) override;
};

class NameExpr : public Syntax
{
public:
    std::shared_ptr<Declaration> Decl;
    std::string String;

    NameExpr();
    NameExpr( const std::string& str );
    NameExpr( std::string&& str );

    virtual void Accept( Visitor* visitor ) override;
    virtual Declaration* GetDecl() override;
    virtual std::shared_ptr<Declaration> GetSharedDecl() override;
};

class NumberExpr : public Syntax
{
public:
    int64_t Value;

    NumberExpr();
    NumberExpr( int64_t value );

    virtual void Accept( Visitor* visitor ) override;
};

class TypeRef : public Syntax
{
public:
    std::shared_ptr<Gemini::Type> ReferentType;
};

class NameTypeRef : public TypeRef
{
public:
    Unique<Syntax>  QualifiedName;

    virtual void Accept( Visitor* visitor ) override;
};

class EnumMemberDef;

class EnumTypeRef : public TypeRef
{
public:
    std::vector<Unique<EnumMemberDef>> Members;

    virtual void Accept( Visitor* visitor ) override;
};

class ArrayTypeRef : public TypeRef
{
public:
    Unique<Syntax>  SizeExpr;
    Unique<TypeRef> ElementTypeRef;

    virtual void Accept( Visitor* visitor ) override;
};

enum class ParamModifier
{
    None,
    Var,
    Const,
};

struct ParamSpecRef
{
    Unique<Gemini::TypeRef> TypeRef;
    ParamModifier           Modifier = ParamModifier::None;

    ParamSpecRef() = default;

    ParamSpecRef( ParamSpecRef&& other ) noexcept :
        TypeRef( std::move( other.TypeRef ) ),
        Modifier( other.Modifier )
    {
    }

    ParamSpecRef( const ParamSpecRef& ) = delete;
    ParamSpecRef& operator=( const ParamSpecRef& ) = delete;
};

class ProcTypeRef : public TypeRef
{
public:
    std::vector<ParamSpecRef>   Params;
    Unique<TypeRef>             ReturnTypeRef;

    virtual void Accept( Visitor* visitor ) override;
};

class PointerTypeRef : public TypeRef
{
public:
    Unique<TypeRef> Target;

    virtual void Accept( Visitor* visitor ) override;
};

class DataDecl;

class RecordTypeRef : public TypeRef
{
public:
    std::vector<Unique<DataDecl>> Fields;

    virtual void Accept( Visitor* visitor ) override;
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

    virtual void Accept( Visitor* visitor ) override;
};

class FieldInitializer;

class RecordInitializer : public Syntax
{
public:
    RecordInitializer();

    std::vector<Unique<FieldInitializer>> Fields;

    virtual void Accept( Visitor* visitor ) override;
};

class DeclSyntax : public Syntax
{
public:
    std::shared_ptr<Declaration> Decl;

    std::string Name;

    virtual Declaration* GetDecl() override;
    virtual std::shared_ptr<Declaration> GetSharedDecl() override;
};

class DataDecl : public DeclSyntax
{
public:
    Unique<Gemini::TypeRef>     TypeRef;
    Unique<Syntax>              Initializer;
};

class ConstDecl : public DataDecl
{
public:
    ConstDecl();

    virtual void Accept( Visitor* visitor ) override;
};

class VarDecl : public DataDecl
{
public:
    VarDecl();
    VarDecl( std::string_view name );

    virtual void Accept( Visitor* visitor ) override;
};

class ParamDecl : public DataDecl
{
public:
    ParamModifier   Modifier = ParamModifier::None;

    ParamDecl();

    virtual void Accept( Visitor* visitor ) override;
};

class EnumMemberDef : public DataDecl
{
public:
    virtual void Accept( Visitor* visitor ) override;
};

class FieldDecl : public DataDecl
{
public:
    virtual void Accept( Visitor* visitor ) override;
};

class FieldInitializer : public DataDecl
{
public:
    virtual void Accept( Visitor* visitor ) override;
};

class TypeDecl : public DeclSyntax
{
public:
    Unique<Gemini::TypeRef>     TypeRef;

    virtual void Accept( Visitor* visitor ) override;
};

class AsExpr : public Syntax
{
public:
    Unique<Syntax>              Inner;
    Unique<Gemini::TypeRef>     TargetTypeRef;

    virtual void Accept( Visitor* visitor ) override;
};

class LambdaExpr : public Syntax
{
public:
    Unique<ProcDecl> Proc;

    virtual void Accept( Visitor* visitor ) override;
};

class CondClause : public Syntax
{
public:
    Unique<Syntax> Condition;
    StatementList Body;

    virtual void Accept( Visitor* visitor ) override;
};

class CondExpr : public Syntax
{
public:
    std::vector<Unique<CondClause>> Clauses;
    bool IsIf = false;

    virtual void Accept( Visitor* visitor ) override;
};

class CaseWhen : public Syntax
{
public:
    std::vector<Unique<Syntax>> Keys;
    StatementList Body;

    virtual void Accept( Visitor* visitor ) override;
};

class CaseElse : public Syntax
{
public:
    StatementList Body;

    virtual void Accept( Visitor* visitor ) override;
};

class CaseExpr : public Syntax
{
public:
    Unique<Syntax> TestKey;
    Unique<CaseElse> Fallback;
    std::vector<Unique<CaseWhen>> Clauses;

    virtual void Accept( Visitor* visitor ) override;
};

class BinaryExpr : public Syntax
{
public:
    std::string Op;
    Unique<Syntax> Left;
    Unique<Syntax> Right;

    virtual void Accept( Visitor* visitor ) override;
};

class UnaryExpr : public Syntax
{
public:
    std::string Op;
    Unique<Syntax> Inner;

    virtual void Accept( Visitor* visitor ) override;
};

class AddrOfExpr : public Syntax
{
public:
    Unique<Syntax> Inner;

    AddrOfExpr();

    virtual void Accept( Visitor* visitor ) override;
};

class IndexExpr : public Syntax
{
public:
    Unique<Syntax> Head;
    Unique<Syntax> Index;

    IndexExpr();

    virtual void Accept( Visitor* visitor ) override;
};

class SliceExpr : public Syntax
{
public:
    Unique<Syntax> Head;
    Unique<Syntax> FirstIndex;
    Unique<Syntax> LastIndex;

    SliceExpr();

    virtual void Accept( Visitor* visitor ) override;
};

class DotExpr : public Syntax
{
public:
    Unique<Syntax> Head;
    std::string Member;

    std::shared_ptr<Declaration> Decl;

    DotExpr();

    virtual void Accept( Visitor* visitor ) override;
    virtual Declaration* GetDecl() override;
    virtual std::shared_ptr<Declaration> GetSharedDecl() override;
};

class CallExpr : public Syntax
{
public:
    bool IsIndirect = false;
    Unique<Syntax> Head;
    std::vector<Unique<Syntax>> Arguments;

    virtual void Accept( Visitor* visitor ) override;
};

class CallOrSymbolExpr : public Syntax
{
public:
    Unique<Syntax> Symbol;

    virtual void Accept( Visitor* visitor ) override;
};

class AssignmentExpr : public Syntax
{
public:
    Unique<Syntax> Left;
    Unique<Syntax> Right;

    virtual void Accept( Visitor* visitor ) override;
};

class CountofExpr : public Syntax
{
public:
    Unique<Syntax>      Expr;

    virtual void Accept( Visitor* visitor ) override;
};

class LetStatement : public Syntax
{
public:
    std::vector<Unique<DataDecl>> Variables;
    StatementList Body;

    virtual void Accept( Visitor* visitor ) override;
};

class ReturnStatement : public Syntax
{
public:
    Unique<Syntax> Inner;

    virtual void Accept( Visitor* visitor ) override;
};

enum class ForComparison
{
    Above,
    Below,
    Downto,
    To,
};

class ForStatement : public Syntax
{
public:
    ForComparison Comparison = ForComparison::Above;
    Unique<DataDecl> Index;
    Unique<Syntax> First;
    Unique<Syntax> Last;
    Unique<Syntax> Step;
    StatementList Body;

    std::shared_ptr<Declaration> IndexDecl;

    virtual void Accept( Visitor* visitor ) override;
};

class LoopStatement : public Syntax
{
public:
    StatementList Body;
    Unique<Syntax> Condition;

    virtual void Accept( Visitor* visitor ) override;
};

class WhileStatement : public Syntax
{
public:
    Unique<Syntax> Condition;
    StatementList Body;

    virtual void Accept( Visitor* visitor ) override;
};

class BreakStatement : public Syntax
{
public:
    virtual void Accept( Visitor* visitor ) override;
};

class NextStatement : public Syntax
{
public:
    virtual void Accept( Visitor* visitor ) override;
};

class YieldStatement : public Syntax
{
public:
    virtual void Accept( Visitor* visitor ) override;
};

class ProcDeclBase : public DeclSyntax
{
public:
    constexpr static int16_t MaxParams = ParamSizeMax;
    constexpr static int16_t MaxLocals = LocalSizeMax;

    std::vector<Unique<DataDecl>>   Params;
    Unique<TypeRef>                 ReturnTypeRef;
};

class ProcDecl : public ProcDeclBase
{
public:
    StatementList Body;

    virtual void Accept( Visitor* visitor ) override;
};

class NativeDecl : public ProcDeclBase
{
public:
    Unique<Syntax> OptionalId;

    virtual void Accept( Visitor* visitor ) override;
};

class ImportDecl : public DeclSyntax
{
public:
    std::string OriginalName;

    virtual void Accept( Visitor* visitor ) override;
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

    virtual void Accept( Visitor* visitor ) override;
};


std::optional<int32_t> GetFinalOptionalSyntaxValue( Syntax* node );

void CopyBaseSyntax( Syntax& dest, const Syntax& source );


//----------------------------------------------------------------------------
//  Visitors
//----------------------------------------------------------------------------

class Visitor
{
public:
    virtual ~Visitor() { }

    virtual void VisitAddrOfExpr( AddrOfExpr* addrOf );
    virtual void VisitArrayTypeRef( ArrayTypeRef* typeRef );
    virtual void VisitAsExpr( AsExpr* asExpr );
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
    virtual void VisitEnumMemberDef( EnumMemberDef* enumMemberDef );
    virtual void VisitEnumTypeRef( EnumTypeRef* enumTypeRef );
    virtual void VisitFieldDecl( FieldDecl* fieldDecl );
    virtual void VisitFieldInitializer( FieldInitializer* fieldInit );
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
    virtual void VisitRecordInitializer( RecordInitializer* recordInitializer );
    virtual void VisitRecordTypeRef( RecordTypeRef* recordTypeRef );
    virtual void VisitReturnStatement( ReturnStatement* retStmt );
    virtual void VisitSliceExpr( SliceExpr* sliceExpr );
    virtual void VisitStatementList( StatementList* stmtmList );
    virtual void VisitTypeDecl( TypeDecl* typeDecl );
    virtual void VisitUnaryExpr( UnaryExpr* unary );
    virtual void VisitUnit( Unit* unit );
    virtual void VisitVarDecl( VarDecl* varDecl );
    virtual void VisitWhileStatement( WhileStatement* whileStmt );
    virtual void VisitYieldStatement( YieldStatement* yieldStmt );
};


//----------------------------------------------------------------------------
//  Declarations
//----------------------------------------------------------------------------

enum class DeclKind
{
    Undefined,
    Const,
    Enum,
    Global,
    Local,
    Param,
    Field,
    Func,
    NativeFunc,
    Type,
    Module,
    LoadedAddress,
};

struct Declaration
{
    const DeclKind          Kind;
    bool                    IsReadOnly = false;

    virtual ~Declaration() { }
    virtual std::shared_ptr<Type> GetType() const = 0;

protected:
    Declaration( DeclKind kind );
};

struct CommonDeclaration : public Declaration
{
    std::shared_ptr<Gemini::Type>   Type;

    CommonDeclaration( DeclKind kind );

    virtual std::shared_ptr<Gemini::Type> GetType() const override
    {
        return Type;
    }
};

using SymTable = std::map<std::string, std::shared_ptr<Declaration>>;

struct UndefinedDeclaration : public CommonDeclaration
{
    // It would be safer if here we kept a weak_ptr to a Syntax node.
    // But that would mean changing all Unique<Syntax> to Shared<Syntax>.

    Syntax*     Node = nullptr;

    UndefinedDeclaration();
};

class ModuleAttrs;

struct ConstRef
{
    std::shared_ptr<ModuleAttrs>    Module;
    GlobalSize                      Offset;
};

enum class ValueKind
{
    Integer,
    Function,
    Aggregate,
};

struct Function;

class ValueVariant
{
    using Variant = std::variant<
        int32_t,
        std::shared_ptr<Function>,
        ConstRef>;

    Variant mVariant;

public:
    ValueVariant() = default;
    ValueVariant( const ValueVariant& ) = default;
    ValueVariant( ValueVariant&& ) = default;

    template <typename T,
        std::enable_if_t<!std::is_same_v<std::decay_t<T>, ValueVariant>,
            int> = 0>
    ValueVariant( T&& t ) noexcept :
        mVariant( std::move( t ) )
    {
    }

    template <typename T,
        std::enable_if_t<!std::is_same_v<std::decay_t<T>, ValueVariant>,
        int> = 0>
        ValueVariant( const T& t ) noexcept :
        mVariant( t )
    {
    }

    ValueVariant& operator=( const ValueVariant& ) = default;
    ValueVariant& operator=( ValueVariant&& ) = default;

    bool Is( ValueKind kind ) const
    {
        return mVariant.index() == static_cast<size_t>(kind);
    }

    int32_t& GetInteger()
    {
        return std::get<0>( mVariant );
    }

    std::shared_ptr<Function>& GetFunction()
    {
        return std::get<1>( mVariant );
    }

    ConstRef& GetAggregate()
    {
        return std::get<2>( mVariant );
    }

    void SetInteger( int32_t value )
    {
        mVariant = value;
    }

    void SetFunction( std::shared_ptr<Function> value )
    {
        mVariant = value;
    }

    void SetAggregate( ConstRef value )
    {
        mVariant = value;
    }
};

struct Constant : public Declaration
{
    ValueVariant    Value;
    GlobalSize      Offset = 0;
    ModSize         ModIndex = 0;
    bool            Serialized = false;

    std::shared_ptr<Gemini::Type>   Type;

    Constant();

    virtual std::shared_ptr<Gemini::Type> GetType() const override
    {
        return Type;
    }
};

struct GlobalStorage : public CommonDeclaration
{
    GlobalSize  Offset = 0;
    ModSize     ModIndex = 0;

    GlobalStorage();
};

struct LocalStorage : public CommonDeclaration
{
    LocalSize   Offset = 0;

    LocalStorage();
};

enum class ParamMode
{
    Value,
    ValueIn,
    RefInOut,
    RefIn,
};

struct ParamStorage : public CommonDeclaration
{
    ParamSize   Offset = 0;
    ParamMode   Mode = ParamMode::Value;
    ParamSize   Size = 0;

    ParamStorage();
};

struct FieldStorage : public CommonDeclaration
{
    DataSize    Offset = 0;

    FieldStorage();
};

struct CallSite
{
    std::string FunctionName;
    int16_t     ExprDepth;
    uint8_t     ModIndex;
};

struct Function : public CommonDeclaration
{
    std::string Name;
    CodeSize    Address = UndefinedAddr;
    ModSize     ModIndex = 0;
    bool        IsLambda = false;

    LocalSize   LocalCount = 0;
    ParamSize   ParamCount = 0;
    LocalSize   ExprDepth = 0;

    uint32_t    CallDepth = 0;
    uint32_t    IndividualStackUsage = 0;
    uint32_t    TreeStackUsage = 0;

    bool        IsCalculating = false;
    bool        IsRecursive = false;
    bool        IsDepthKnown = false;
    bool        CallsIndirectly = false;

    std::list<CallSite> CalledFunctions;

    Function();
};

struct NativeFunction : public CommonDeclaration
{
    int32_t     Id = 0;

    NativeFunction();
};

struct TypeDeclaration : public CommonDeclaration
{
    std::shared_ptr<Gemini::Type> ReferentType;

    TypeDeclaration();
};

struct ModuleDeclaration : public CommonDeclaration
{
    std::string Name;
    SymTable    Table;
    int32_t     Index = 0;

    ModuleDeclaration();
};

struct LoadedAddressDeclaration : public CommonDeclaration
{
    LoadedAddressDeclaration();
};

class EnumType;

struct EnumMember : public Declaration
{
    // Use a weak reference to avoid a circular reference.
    // But the parent type must always be available.
    // This is easy to guarantee since, in the language,
    // these only show up in the context of the parent.

    const std::weak_ptr<EnumType>   ParentType;
    int32_t                         Value;

    EnumMember( int32_t value, std::shared_ptr<EnumType> parentType );

    virtual std::shared_ptr<Type> GetType() const override;
};


//----------------------------------------------------------------------------
//  Types
//----------------------------------------------------------------------------

enum class TypeKind
{
    Error,
    Type,
    Module,
    Xfer,
    Int,
    Array,
    Func,
    Pointer,
    Record,
    Enum,
};

class Type
{
    TypeKind    mKind;

protected:
    Type( TypeKind kind );

public:
    virtual ~Type() { }

    TypeKind GetKind() const;
    virtual bool IsEqual( Type* other ) const;
    virtual bool IsAssignableFrom( Type* other ) const;
    virtual bool IsPassableFrom( Type* other, ParamMode mode ) const;
    virtual DataSize GetSize() const;
};


template <typename Derived, TypeKind kind>
class SimpleType : public Type
{
public:
    SimpleType() : Type( kind ) {}
};


class ErrorType : public SimpleType<ErrorType, TypeKind::Error>
{
};

class TypeType : public SimpleType<TypeType, TypeKind::Type>
{
};

class ModuleType : public SimpleType<ModuleType, TypeKind::Module>
{
};

class XferType : public SimpleType<XferType, TypeKind::Xfer>
{
public:
    virtual bool IsEqual( Type* other ) const override;
};

class IntType : public SimpleType<IntType, TypeKind::Int>
{
public:
    virtual bool IsEqual( Type* other ) const override;
    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual DataSize GetSize() const override;
};


class ArrayType : public Type
{
public:
    DataSize Count;
    std::shared_ptr<Type> ElemType;

    ArrayType( DataSize count, std::shared_ptr<Type> elemType );

    virtual bool IsEqual( Type* other ) const override;
    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual bool IsPassableFrom( Type* other, ParamMode mode ) const override;
    virtual DataSize GetSize() const override;
};

struct ParamSpec
{
    std::shared_ptr<Gemini::Type>   Type;
    ParamMode                       Mode = ParamMode::Value;
    ParamSize                       Size = 0;
};

class FuncType : public Type
{
public:
    std::shared_ptr<Type>               ReturnType;
    std::vector<ParamSpec>              Params;

    FuncType( std::shared_ptr<Type> returnType );

    virtual bool IsEqual( Type* other ) const override;
};

class PointerType : public Type
{
public:
    std::shared_ptr<Type>   TargetType;

    PointerType( std::shared_ptr<Type> target );

    virtual bool IsEqual( Type* other ) const override;
    virtual DataSize GetSize() const override;
};


class RecordType : public Type
{
public:
    using FieldVec = std::vector<std::shared_ptr<FieldStorage>>;

private:
    mutable DataSize mSize = 0;

    FieldVec    OrderedFields;
    SymTable    Fields;

public:
    RecordType();

    SymTable& GetFields();
    FieldVec& GetOrderedFields();

    virtual bool IsEqual( Type* other ) const override;
    virtual DataSize GetSize() const override;
};


class EnumType : public Type
{
    SymTable    MembersByName;

public:
    EnumType();

    SymTable& GetMembersByName();

    virtual bool IsEqual( Type* other ) const override;
    virtual DataSize GetSize() const override;
};

}
