#pragma once

#include <memory>
#include <string>
#include <vector>


enum class SyntaxKind
{
    Number,
    Name,
    Index,
    Other,
};


class IVisitor;

class ProcDecl;

struct Declaration;


class Syntax
{
public:
    SyntaxKind Kind = SyntaxKind::Other;
    int Line = 0;
    int Column = 0;

    virtual ~Syntax() {}
    virtual void Accept( IVisitor* visitor ) = 0;
    virtual Declaration* GetDecl();
};

class StatementList : public Syntax
{
public:
    std::vector<std::unique_ptr<Syntax>> Statements;

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
};

class ArrayTypeRef : public TypeRef
{
public:
    int32_t Size;

    std::unique_ptr<Syntax> SizeExpr;

    virtual void Accept( IVisitor* visitor ) override;
};

class InitList : public Syntax
{
public:
    bool HasExtra = false;

    std::vector<std::unique_ptr<Syntax>> Values;

    virtual void Accept( IVisitor* visitor ) override;
};

class DataDecl : public Syntax
{
public:
    std::shared_ptr<Declaration> Decl;

    std::string Name;
    std::unique_ptr<TypeRef> TypeRef;
    std::unique_ptr<Syntax> Initializer;

    virtual Declaration* GetDecl() override;
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

class LambdaExpr : public Syntax
{
public:
    std::unique_ptr<ProcDecl> Proc;

    virtual void Accept( IVisitor* visitor ) override;
};

class CondClause
{
public:
    std::unique_ptr<Syntax> Condition;
    StatementList Body;
};

class CondExpr : public Syntax
{
public:
    std::vector<std::unique_ptr<CondClause>> Clauses;

    virtual void Accept( IVisitor* visitor ) override;
};

class CaseWhen
{
public:
    std::vector<std::unique_ptr<Syntax>> Keys;
    StatementList Body;
};

class CaseElse
{
public:
    StatementList Body;
};

class CaseExpr : public Syntax
{
public:
    std::unique_ptr<Syntax> TestKey;
    std::unique_ptr<CaseElse> Fallback;
    std::vector<std::unique_ptr<CaseWhen>> Clauses;

    std::shared_ptr<Declaration> TestKeyDecl;

    virtual void Accept( IVisitor* visitor ) override;
};

class BinaryExpr : public Syntax
{
public:
    std::string Op;
    std::unique_ptr<Syntax> Left;
    std::unique_ptr<Syntax> Right;

    virtual void Accept( IVisitor* visitor ) override;
};

class UnaryExpr : public Syntax
{
public:
    std::string Op;
    std::unique_ptr<Syntax> Inner;

    virtual void Accept( IVisitor* visitor ) override;
};

class AddrOfExpr : public Syntax
{
public:
    std::unique_ptr<NameExpr> Inner;

    virtual void Accept( IVisitor* visitor ) override;
};

class IndexExpr : public Syntax
{
public:
    std::unique_ptr<Syntax> Head;
    std::unique_ptr<Syntax> Index;

    IndexExpr();

    virtual void Accept( IVisitor* visitor ) override;
};

class CallExpr : public Syntax
{
public:
    bool IsIndirect = false;
    std::unique_ptr<Syntax> Head;
    std::vector<std::unique_ptr<Syntax>> Arguments;

    virtual void Accept( IVisitor* visitor ) override;
};

class CallOrSymbolExpr : public Syntax
{
public:
    std::unique_ptr<NameExpr> Symbol;

    virtual void Accept( IVisitor* visitor ) override;
};

class AssignmentExpr : public Syntax
{
public:
    std::unique_ptr<Syntax> Left;
    std::unique_ptr<Syntax> Right;

    virtual void Accept( IVisitor* visitor ) override;
};

class LetStatement : public Syntax
{
public:
    std::vector<std::unique_ptr<DataDecl>> Variables;
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
};

class ReturnStatement : public Syntax
{
public:
    std::unique_ptr<Syntax> Inner;

    virtual void Accept( IVisitor* visitor ) override;
};

class ForStatement : public Syntax
{
public:
    std::string IndexName;
    std::string Comparison;
    std::unique_ptr<Syntax> First;
    std::unique_ptr<Syntax> Last;
    std::unique_ptr<Syntax> Step;
    StatementList Body;

    std::shared_ptr<Declaration> IndexDecl;

    virtual void Accept( IVisitor* visitor ) override;
};

class LoopStatement : public Syntax
{
public:
    StatementList Body;
    std::unique_ptr<Syntax> Condition;

    virtual void Accept( IVisitor* visitor ) override;
};

class WhileStatement : public Syntax
{
public:
    std::unique_ptr<Syntax> Condition;
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

class ParamDecl : public Syntax
{
public:
    std::shared_ptr<Declaration> Decl;
    std::string Name;
    std::unique_ptr<TypeRef> TypeRef;

    virtual void Accept( IVisitor* visitor ) override;
    virtual Declaration* GetDecl() override;
};

class ProcDecl : public Syntax
{
public:
    constexpr static int16_t MaxArgs = 127;
    constexpr static int16_t MaxLocals = 127;

    std::shared_ptr<Declaration> Decl;

    std::string Name;
    std::vector<std::unique_ptr<ParamDecl>> Params;
    StatementList Body;

    virtual void Accept( IVisitor* visitor ) override;
    virtual Declaration* GetDecl() override;
};

class Unit : public Syntax
{
public:
    std::vector<std::unique_ptr<DataDecl>> DataDeclarations;
    std::vector<std::unique_ptr<ProcDecl>> FuncDeclarations;

    virtual void Accept( IVisitor* visitor ) override;
};


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
    virtual void VisitNextStatement( NextStatement* nextStmt ) = 0;
    virtual void VisitNumberExpr( NumberExpr* numberExpr ) = 0;
    virtual void VisitParamDecl( ParamDecl* paramDecl ) = 0;
    virtual void VisitProcDecl( ProcDecl* procDecl ) = 0;
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
    Const,
    Global,
    Local,
    Arg,
    Func,
    Forward,
    ExternalFunc,
    NativeFunc,
};

struct Declaration
{
    DeclKind  Kind;
    virtual ~Declaration() { }
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
