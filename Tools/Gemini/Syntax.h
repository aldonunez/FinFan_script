#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>


enum class SyntaxKind
{
    Number,
    Name,
    Index,
    ArrayTypeRef,
    ArrayInitializer,
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
    int32_t Value;

    NumberExpr();
    NumberExpr( int32_t value );

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
    Unique<NameExpr> Symbol;

    virtual void Accept( IVisitor* visitor ) override;
};

class ArrayTypeRef : public TypeRef
{
public:
    Unique<Syntax> SizeExpr;

    ArrayTypeRef();

    virtual void Accept( IVisitor* visitor ) override;
};

class ProcTypeRef : public TypeRef
{
public:
    std::vector<Unique<TypeRef>> Params;

    virtual void Accept( IVisitor* visitor ) override;
};

class PointerTypeRef : public TypeRef
{
public:
    Unique<TypeRef> Target;

    virtual void Accept( IVisitor* visitor ) override;
};

class InitList : public Syntax
{
public:
    bool HasExtra = false;

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
    virtual void Accept( IVisitor* visitor ) override;
};

class VarDecl : public DataDecl
{
public:
    virtual void Accept( IVisitor* visitor ) override;
};

class ParamDecl : public DataDecl
{
public:
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
    Unique<NameExpr> Inner;

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
    Unique<NameExpr> Symbol;

    virtual void Accept( IVisitor* visitor ) override;
};

class AssignmentExpr : public Syntax
{
public:
    Unique<Syntax> Left;
    Unique<Syntax> Right;

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
    constexpr static int16_t MaxArgs = 127;
    constexpr static int16_t MaxLocals = 127;

    std::vector<Unique<DataDecl>> Params;
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
    virtual void VisitAddrOfExpr( AddrOfExpr* addrOf ) = 0;
    virtual void VisitArrayTypeRef( ArrayTypeRef* typeRef ) = 0;
    virtual void VisitAssignmentExpr( AssignmentExpr* assignment ) = 0;
    virtual void VisitBinaryExpr( BinaryExpr* binary ) = 0;
    virtual void VisitBreakStatement( BreakStatement* breakStmt ) = 0;
    virtual void VisitCallExpr( CallExpr* call ) = 0;
    virtual void VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol ) = 0;
    virtual void VisitCaseExpr( CaseExpr* caseExpr ) = 0;
    virtual void VisitCondExpr( CondExpr* condExpr ) = 0;
    virtual void VisitConstDecl( ConstDecl* constDecl ) = 0;
    virtual void VisitForStatement( ForStatement* forStmt ) = 0;
    virtual void VisitIndexExpr( IndexExpr* indexExpr ) = 0;
    virtual void VisitInitList( InitList* initList ) = 0;
    virtual void VisitLambdaExpr( LambdaExpr* lambdaExpr ) = 0;
    virtual void VisitLetStatement( LetStatement* letStmt ) = 0;
    virtual void VisitLoopStatement( LoopStatement* loopStmt ) = 0;
    virtual void VisitNameExpr( NameExpr* nameExpr ) = 0;
    virtual void VisitNameTypeRef( NameTypeRef* nameTypeRef ) = 0;
    virtual void VisitNativeDecl( NativeDecl* nativeDecl ) = 0;
    virtual void VisitNextStatement( NextStatement* nextStmt ) = 0;
    virtual void VisitNumberExpr( NumberExpr* numberExpr ) = 0;
    virtual void VisitParamDecl( ParamDecl* paramDecl ) = 0;
    virtual void VisitPointerTypeRef( PointerTypeRef* pointerTypeRef ) = 0;
    virtual void VisitProcDecl( ProcDecl* procDecl ) = 0;
    virtual void VisitProcTypeRef( ProcTypeRef* procTypeRef ) = 0;
    virtual void VisitReturnStatement( ReturnStatement* retStmt ) = 0;
    virtual void VisitStatementList( StatementList* stmtmList ) = 0;
    virtual void VisitUnaryExpr( UnaryExpr* unary ) = 0;
    virtual void VisitUnit( Unit* unit ) = 0;
    virtual void VisitVarDecl( VarDecl* varDecl ) = 0;
    virtual void VisitWhileStatement( WhileStatement* whileStmt ) = 0;
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
    Arg,
    Func,
    Forward,
    ExternalFunc,
    NativeFunc,
    Type,
};

struct Declaration
{
    DeclKind  Kind;
    std::shared_ptr<Type>   Type;
    virtual ~Declaration() { }
};

struct UndefinedDeclaration : public Declaration
{
    Syntax* Node;
};

struct Constant : public Declaration
{
    int Value;
};

struct Storage : public Declaration
{
    int Offset;
};

struct Function : public Declaration
{
    std::string Name;
    int         Address;

    int16_t     LocalCount;
    int16_t     ArgCount;
    int16_t     ExprDepth;

    int16_t     CallDepth;
    int16_t     IndividualStackUsage;
    int16_t     TreeStackUsage;

    bool        IsCalculating;
    bool        IsRecursive;
    bool        IsDepthKnown;
    bool        CallsIndirectly;

    std::list<std::string> CalledFunctions;
};

struct ExternalFunction : public Declaration
{
    int32_t Id;
};

struct NativeFunction : public Declaration
{
    int32_t Id;
};

struct TypeDeclaration : public Declaration
{
    std::shared_ptr<::Type> ReferentType;
};


//----------------------------------------------------------------------------
//  Types
//----------------------------------------------------------------------------

enum class TypeKind
{
    Type,
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
    virtual bool IsAssignableFrom( Type* other ) const;
    virtual int32_t GetSize() const;
};

class TypeType : public Type
{
public:
    TypeType();
};

class XferType : public Type
{
public:
    XferType();

    virtual bool IsAssignableFrom( Type* other ) const override;
};

class IntType : public Type
{
public:
    IntType();

    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual int32_t GetSize() const override;
};

class ArrayType : public Type
{
public:
    int32_t Size;
    std::shared_ptr<Type> ElemType;

    ArrayType( int32_t size, std::shared_ptr<Type> elemType );

    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual int32_t GetSize() const override;
};

class FuncType : public Type
{
public:
    std::shared_ptr<Type>               ReturnType;
    std::vector<std::shared_ptr<Type>>  ParamTypes;

    FuncType( std::shared_ptr<Type> returnType );

    virtual bool IsAssignableFrom( Type* other ) const override;
};

class PointerType : public Type
{
public:
    std::shared_ptr<Type>   TargetType;

    PointerType( std::shared_ptr<Type> target );

    virtual bool IsAssignableFrom( Type* other ) const override;
    virtual int32_t GetSize() const override;
};
