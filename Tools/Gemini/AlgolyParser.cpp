#include "stdafx.h"
#include "AlgolyParser.h"
#include <stdarg.h>


template <typename T>
using Unique  = std::unique_ptr<T>;


AlgolyParser::AlgolyParser( const char* codeText, int codeTextLen, ICompilerLog* log ) :
    mCodeTextPtr( codeText ),
    mCodeTextEnd( codeText + codeTextLen ),
    mLineStart( codeText ),
    mLine( 1 ),
    mCurChar( 0 ),
    mCurToken( TokenCode::Bof ),
    mCurNumber( 0 ),
    mTokLine( 0 ),
    mTokCol( 0 ),
    mRep( log )
{
    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCurChar = *mCodeTextPtr;
    }
}

int AlgolyParser::GetColumn()
{
    return mCodeTextPtr - mLineStart + 1;
}

int AlgolyParser::PeekChar() const
{
    return mCurChar;
}

int AlgolyParser::PeekChar( int index ) const
{
    if ( (mCodeTextEnd - mCodeTextPtr) < (index + 1) )
        return 0;

    return mCodeTextPtr[index];
}

void AlgolyParser::NextChar()
{
    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCodeTextPtr++;
    }

    if ( mCodeTextPtr < mCodeTextEnd )
    {
        mCurChar = *mCodeTextPtr;
    }
    else
    {
        mCurChar = 0;
    }
}

void AlgolyParser::CollectChar()
{
    mCurString.append( 1, PeekChar() );
    NextChar();
}

AlgolyParser::TokenCode AlgolyParser::ScanToken()
{
    SkipWhitespace();

    mCurString.clear();
    mCurNumber = 0;
    mTokLine = mLine;
    mTokCol = GetColumn();

    if ( isdigit( PeekChar() ) )
    {
        ReadNumber();
        return mCurToken;
    }
    else if ( IsIdentifierInitial( PeekChar() ) )
    {
        ReadSymbolOrKeyword();
        return mCurToken;
    }

    switch ( PeekChar() )
    {
    case 0:
        mCurToken = TokenCode::Eof;
        break;

    case '\r':
    case '\n':
        mCurToken = TokenCode::Eol;
        ReadLineEnding();
        break;

    case ';':
        NextChar();
        mCurToken = TokenCode::Separator;
        break;

    case ':':
        NextChar();

        if ( PeekChar() == '=' )
        {
            NextChar();
            mCurToken = TokenCode::Assign;
        }
        else
        {
            mCurToken = TokenCode::Colon;
        }
        break;

    case '(':
        NextChar();
        mCurToken = TokenCode::LParen;
        break;

    case ')':
        NextChar();
        mCurToken = TokenCode::RParen;
        break;

    case ',':
        NextChar();
        mCurToken = TokenCode::Comma;
        break;

    case '&':
        NextChar();
        mCurToken = TokenCode::Ampersand;
        break;

    case '[':
        NextChar();
        mCurToken = TokenCode::LBracket;
        break;

    case ']':
        NextChar();
        mCurToken = TokenCode::RBracket;
        break;

    case '.':
        NextChar();

        if ( PeekChar() == '.' && PeekChar( 1 ) == '.' )
        {
            NextChar();
            NextChar();
            mCurToken = TokenCode::Ellipsis;
        }
        else
        {
            ThrowSyntaxError( "Bad character: U+%02X", '.' );
        }
        break;

    case '+':
        CollectChar();
        mCurToken = TokenCode::Plus;
        break;

    case '-':
        CollectChar();
        mCurToken = TokenCode::Minus;
        break;

    case '*':
        CollectChar();
        mCurToken = TokenCode::Star;
        break;

    case '/':
        CollectChar();
        mCurToken = TokenCode::Slash;
        break;

    case '%':
        CollectChar();
        mCurToken = TokenCode::Percent;
        break;

    case '=':
        CollectChar();
        mCurToken = TokenCode::EQ;
        break;

    case '<':
        CollectChar();

        if ( PeekChar() == '>' )
        {
            CollectChar();
            mCurToken = TokenCode::NE;
        }
        else if ( PeekChar() == '=' )
        {
            CollectChar();
            mCurToken = TokenCode::LE;
        }
        else
        {
            mCurToken = TokenCode::LT;
        }
        break;

    case '>':
        CollectChar();

        if ( PeekChar() == '=' )
        {
            CollectChar();
            mCurToken = TokenCode::GE;
        }
        else
        {
            mCurToken = TokenCode::GT;
        }
        break;

    default:
        ThrowSyntaxError( "Bad character: U+%02X", PeekChar() );
        break;
    }

    return mCurToken;
}

void AlgolyParser::ScanToken( TokenCode code )
{
    AssertToken( code );
    ScanToken();
}

void AlgolyParser::AssertToken( TokenCode code )
{
    if ( mCurToken != code )
        ThrowSyntaxError( "Expected token: %d", code );
}

void AlgolyParser::ReadLineEnding()
{
    int c = PeekChar();

    NextChar();

    if ( c == '\r' && PeekChar() == '\n' )
        NextChar();

    mLine++;
    mLineStart = mCodeTextPtr;
}

void AlgolyParser::SkipWhitespace()
{
    while ( true )
    {
        if ( PeekChar() == '\\' )
        {
            NextChar();

            if ( PeekChar() != '\r' && PeekChar() != '\n' )
                ThrowSyntaxError( "Expected line ending after \\" );

            ReadLineEnding();
        }

        while ( isspace( PeekChar() )
            && PeekChar() != '\r'
            && PeekChar() != '\n' )
        {
            NextChar();
        }

        if ( PeekChar() != '#' )
        {
            break;
        }

        while ( PeekChar() != '\r'
            && PeekChar() != '\n' )
        {
            NextChar();
        }
    }
}

void AlgolyParser::SkipLineEndings()
{
    while ( mCurToken == TokenCode::Eol )
    {
        ScanToken();
    }
}

void AlgolyParser::SkipLineSeparators()
{
    while ( mCurToken == TokenCode::Eol || mCurToken == TokenCode::Separator )
    {
        ScanToken();
    }
}

bool AlgolyParser::IsIdentifierInitial( int c )
{
    return isalpha( c )
        || (c == '_')
        || (c == '@');
}

bool AlgolyParser::IsIdentifierCoda( int c )
{
    return IsIdentifierInitial( c ) || isdigit( c );
}

void AlgolyParser::ReadNumber()
{
    bool negate = false;

    if ( PeekChar() == '-' )
    {
        negate = true;
        NextChar();
    }

    while ( isdigit( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    if ( IsIdentifierInitial( PeekChar() ) )
        ThrowSyntaxError( "syntax error : bad number" );

    mCurNumber = atoi( mCurString.c_str() );
    if ( negate )
        mCurNumber = -mCurNumber;

    mCurToken = TokenCode::Number;
}

void AlgolyParser::ReadSymbolOrKeyword()
{
    static std::map<std::string, TokenCode> sStrToKeyword
    {
        { "above",  TokenCode::Above },
        { "and",    TokenCode::And },
        { "below",  TokenCode::Below },
        { "break",  TokenCode::Break },
        { "by",     TokenCode::By },
        { "case",   TokenCode::Case },
        { "const",  TokenCode::Const },
        { "def",    TokenCode::Def },
        { "do",     TokenCode::Do },
        { "downto", TokenCode::Downto },
        { "else",   TokenCode::Else },
        { "elsif",  TokenCode::Elsif },
        { "end",    TokenCode::End },
        { "for",    TokenCode::For },
        { "if",     TokenCode::If },
        { "lambda", TokenCode::Lambda },
        { "loop",   TokenCode::Loop },
        { "next",   TokenCode::Next },
        { "not",    TokenCode::Not },
        { "or",     TokenCode::Or },
        { "return", TokenCode::Return },
        { "then",   TokenCode::Then },
        { "to",     TokenCode::To },
        { "var",    TokenCode::Var },
        { "when",   TokenCode::When },
        { "while",  TokenCode::While },
    };

    while ( IsIdentifierCoda( PeekChar() ) )
    {
        mCurString.append( 1, PeekChar() );
        NextChar();
    }

    if ( auto it = sStrToKeyword.find( mCurString );
        it != sStrToKeyword.end() )
    {
        mCurToken = it->second;
    }
    else
    {
        mCurToken = TokenCode::Symbol;
    }
}

bool AlgolyParser::IsSeparatorKeyword( TokenCode tokenCode )
{
    return tokenCode == TokenCode::End
        || tokenCode == TokenCode::Else
        || tokenCode == TokenCode::Elsif
        || tokenCode == TokenCode::When
        || tokenCode == TokenCode::Do
        ;
}

bool AlgolyParser::IsStatementSeparator( TokenCode tokenCode )
{
    return tokenCode == TokenCode::Eol
        || tokenCode == TokenCode::Separator
        || IsSeparatorKeyword( tokenCode )
        ;
}

Unit* AlgolyParser::Parse()
{
    Unique<Unit> unit = Make<Unit>();

    ScanToken();

    while ( mCurToken != TokenCode::Eof )
    {
        if ( mCurToken == TokenCode::Def )
        {
            unit->FuncDeclarations.push_back( ParseFunction() );
        }
        else if ( mCurToken == TokenCode::Var
            || mCurToken == TokenCode::Const )
        {
            ParseGlobalVars( unit.get() );
        }
        else
        {
            ThrowSyntaxError( "syntax error : expected function" );
        }

        SkipLineSeparators();
    }

    return unit.release();
}

Unique<ProcDecl> AlgolyParser::ParseFunction()
{
    auto proc = ParseProc( true );

    SkipLineEndings();

    return proc;
}

Unique<LambdaExpr> AlgolyParser::ParseLambda()
{
    Unique<LambdaExpr> lambda( Make<LambdaExpr>() );

    lambda->Proc = ParseProc( false );

    return lambda;
}

Unique<ProcDecl> AlgolyParser::ParseProc( bool hasName )
{
    Unique<ProcDecl> proc( Make<ProcDecl>() );

    ScanToken();

    if ( hasName )
        proc->Name = ParseRawSymbol();

    if ( mCurToken == TokenCode::LParen )
    {
        proc->Params = ParseParamList();
    }

    SkipLineSeparators();

    ParseStatements( proc->Body );

    // Read past "end"
    ScanToken( TokenCode::End );

    return proc;
}

std::vector<std::unique_ptr<ParamDecl>> AlgolyParser::ParseParamList()
{
    std::vector<std::unique_ptr<ParamDecl>> paramList;

    // Read past left parenthesis
    ScanToken();
    SkipLineEndings();

    bool first = true;

    while ( mCurToken != TokenCode::RParen )
    {
        if ( !first )
        {
            if ( mCurToken != TokenCode::Comma )
                ThrowSyntaxError( "Expected , or )" );

            ScanToken();
            SkipLineEndings();
        }

        first = false;

        Unique<ParamDecl> param = Make<ParamDecl>();

        if ( mCurToken != TokenCode::Symbol )
            ThrowSyntaxError( "Expected parameter name" );

        param->Name = ParseRawSymbol();

        paramList.push_back( std::move( param ) );
    }

    // Read past right parenthesis
    ScanToken();

    return paramList;
}

void AlgolyParser::ParseStatements( StatementList& container )
{
    container.Line = mTokLine;
    container.Column = mTokCol;

    while ( !IsSeparatorKeyword( mCurToken ) )
    {
        container.Statements.push_back( ParseStatement() );
    }
}

Unique<Syntax> AlgolyParser::ParseStatement()
{
    std::unique_ptr<Syntax> elem;

    switch ( mCurToken )
    {
    case TokenCode::Var:
        elem = ParseLet();
        break;

    case TokenCode::Return:
        elem = ParseReturn();
        break;

    case TokenCode::For:
        elem = ParseFor();
        break;

    case TokenCode::Loop:
        elem = ParseLoop();
        break;

    case TokenCode::While:
        elem = ParseWhile();
        break;

    case TokenCode::Break:
        elem = ParseBreak();
        break;

    case TokenCode::Next:
        elem = ParseNext();
        break;

    default:
        elem = ParseExprStatement();
        break;
    }

    SkipLineSeparators();

    return elem;
}

Unique<Syntax> AlgolyParser::ParseExprStatement()
{
    std::unique_ptr<Syntax> expr( ParseExpr() );

    if ( !IsStatementSeparator( mCurToken ) )
    {
        if ( expr->Kind == SyntaxKind::Name )
        {
            // The parsed expression was only a symbol. But there are more tokens.
            // So, parse them into arguments for a call without parentheses.

            expr = ParseCall( std::move( expr ), false, false );
        }
        else
        {
            ThrowSyntaxError( "Expected end of statement" );
        }
    }
    else
    {
        if ( expr->Kind == SyntaxKind::Name )
        {
            // The whole statement was a symbol. It can be a variable or a call without
            // parentheses and arguments. Use eval* to disambiguate them.

            Unique<CallOrSymbolExpr> eval = Make<CallOrSymbolExpr>();

            eval->Symbol = std::move( (Unique<NameExpr>&) expr );
            expr = std::move( eval );
        }
    }

    return expr;
}

Unique<Syntax> AlgolyParser::ParseExpr()
{
    switch ( mCurToken )
    {
    case TokenCode::Lambda:
        return ParseLambda();

    case TokenCode::If:
        return ParseIf();

    case TokenCode::Case:
        return ParseCase();

    default:
        return ParseAssignment();
    }
}

Unique<Syntax> AlgolyParser::ParseAssignment()
{
    Unique<Syntax> first = ParseBinary( 0 );

    if ( mCurToken == TokenCode::Assign )
    {
        if ( first->Kind != SyntaxKind::Name
            && first->Kind != SyntaxKind::Index )
        {
            mRep.ThrowError( CERR_SYNTAX, first.get(), "Left side of assignment must be modifiable" );
        }

        auto assignment = Make<AssignmentExpr>();

        assignment->Left = std::move( first );

        // Read past assignment operator
        ScanToken();
        SkipLineEndings();

        assignment->Right = ParseExpr();

        return assignment;
    }

    return first;
}

const AlgolyParser::TestOpFunc AlgolyParser::sTestOpFuncs[] =
{
    &AlgolyParser::IsTokenOrOp,
    &AlgolyParser::IsTokenAndOp,
    &AlgolyParser::IsTokenEqualityOp,
    &AlgolyParser::IsTokenComparisonOp,
    &AlgolyParser::IsTokenAdditiveOp,
    &AlgolyParser::IsTokenMultiplicativeOp,
};

bool AlgolyParser::IsTokenOrOp()
{
    return mCurToken == TokenCode::Or;
}

bool AlgolyParser::IsTokenAndOp()
{
    return mCurToken == TokenCode::And;
}

bool AlgolyParser::IsTokenEqualityOp()
{
    return mCurToken == TokenCode::EQ
        || mCurToken == TokenCode::NE;
}

bool AlgolyParser::IsTokenComparisonOp()
{
    return mCurToken == TokenCode::LT
        || mCurToken == TokenCode::LE
        || mCurToken == TokenCode::GT
        || mCurToken == TokenCode::GE;
}

bool AlgolyParser::IsTokenAdditiveOp()
{
    return mCurToken == TokenCode::Plus
        || mCurToken == TokenCode::Minus;
}

bool AlgolyParser::IsTokenMultiplicativeOp()
{
    return mCurToken == TokenCode::Star
        || mCurToken == TokenCode::Slash
        || mCurToken == TokenCode::Percent;
}

Unique<Syntax> AlgolyParser::ParseBinaryPart( int level )
{
    if ( level + 1 >= _countof( AlgolyParser::sTestOpFuncs ) )
        return ParseUnary();
    else
        return ParseBinary( level + 1 );
}

Unique<Syntax> AlgolyParser::ParseBinary( int level )
{
    std::unique_ptr<Syntax> first( ParseBinaryPart( level ) );
    TestOpFunc testOpFunc = sTestOpFuncs[level];

    // Left-associative

    while ( (this->*testOpFunc)() )
    {
        auto binary = Make<BinaryExpr>();

        binary->Op = ParseAsRawSymbol();

        SkipLineEndings();

        binary->Left = std::move( first );
        binary->Right = ParseBinaryPart( level );

        if ( testOpFunc == &AlgolyParser::IsTokenComparisonOp && IsTokenComparisonOp() )
            ThrowSyntaxError( "Comparisons are binary only" );

        first = std::move( binary );
    }

    return first;
}

Unique<Syntax> AlgolyParser::ParseUnary()
{
    if ( mCurToken == TokenCode::Minus
        || mCurToken == TokenCode::Not )
    {
        auto unary = Make<UnaryExpr>();

        unary->Op = ParseAsRawSymbol();
        unary->Inner = ParseSingle();

        return unary;
    }
    else if ( mCurToken == TokenCode::Ampersand )
    {
        auto addrOf = Make<AddrOfExpr>();

        ScanToken();

        addrOf->Inner = ParseSymbol();

        return addrOf;
    }

    return ParseSingle();
}

Unique<Syntax> AlgolyParser::ParseSingle()
{
    std::unique_ptr<Syntax> elem;
    bool indirect = false;

    // TODO: Or should we use a different syntax for indirect calls?

    switch ( mCurToken )
    {
    case TokenCode::LParen:
        ScanToken();
        SkipLineEndings();
        elem = ParseExpr();
        ScanToken( TokenCode::RParen );
        indirect = true;
        break;

    case TokenCode::Symbol: elem = ParseSymbol(); break;
    case TokenCode::Number: elem = ParseNumber(); break;

    default:
        ThrowSyntaxError( "Expected expression" );
    }

    if ( mCurToken == TokenCode::LBracket )
    {
        elem = ParseIndexing( std::move( elem ) );
    }

    if ( mCurToken == TokenCode::LParen )
    {
        return ParseCall( std::move( elem ), indirect );
    }

    return elem;
}

Unique<Syntax> AlgolyParser::ParseCall( std::unique_ptr<Syntax>&& head, bool indirect, bool parens )
{
    auto call = Make<CallExpr>();

    if ( head->Kind == SyntaxKind::Number )
        ThrowSyntaxError( "A number cannot designate a procedure to call" );

    call->IsIndirect = indirect;
    call->Head = std::move( head );

    if ( parens )
    {
        // Read past left parenthesis
        ScanToken();
        SkipLineEndings();
    }

    bool first = true;

    while ( (parens && mCurToken != TokenCode::RParen)
        || (!parens && !IsStatementSeparator( mCurToken )) )
    {
        if ( !first )
        {
            if ( mCurToken != TokenCode::Comma )
                ThrowSyntaxError( "Expected , or )" );

            ScanToken();
            SkipLineEndings();
        }

        first = false;

        call->Arguments.push_back( ParseExpr() );

        if ( parens )
            SkipLineEndings();
    }

    if ( parens )
    {
        // Read past right parenthesis
        ScanToken();
    }

    return call;
}

Unique<Syntax> AlgolyParser::ParseIndexing( std::unique_ptr<Syntax>&& head )
{
    auto indexing = Make<IndexExpr>();

    indexing->Head = std::move( head );

    ScanToken();

    indexing->Index = ParseExpr();

    ScanToken( TokenCode::RBracket );

    return indexing;
}

Unique<Syntax> AlgolyParser::ParseLet()
{
    auto letNode = Make<LetStatement>();

    ScanToken();
    SkipLineEndings();

    bool first = true;

    do
    {
        if ( !first )
        {
            if ( mCurToken != TokenCode::Comma )
                ThrowSyntaxError( "Expected , or line separator" );

            ScanToken();
            SkipLineEndings();
        }

        first = false;

        auto varDecl = ParseVar( Make<VarDecl>(), TokenCode::Assign );

        letNode->Variables.push_back( std::move( varDecl ) );

    } while ( mCurToken != TokenCode::Eol
        && mCurToken != TokenCode::Separator
        && mCurToken != TokenCode::End );

    SkipLineSeparators();

    while ( mCurToken != TokenCode::End )
    {
        letNode->Body.Statements.push_back( ParseStatement() );
    }

    return letNode;
}

void AlgolyParser::ParseGlobalVars( Unit* unit )
{
    TokenCode declToken = mCurToken;

    ScanToken();
    SkipLineEndings();

    bool first = true;

    do
    {
        if ( !first )
        {
            ScanToken( TokenCode::Comma );
            SkipLineEndings();
        }

        first = false;

        if ( declToken == TokenCode::Var )
        {
            unit->DataDeclarations.push_back( ParseVar( Make<VarDecl>(), TokenCode::Assign ) );
        }
        else if ( declToken == TokenCode::Const )
        {
            unit->DataDeclarations.push_back( ParseVar( Make<ConstDecl>(), TokenCode::EQ ) );
        }
        else
        {
            mRep.ThrowInternalError();
        }

    } while ( mCurToken != TokenCode::Eol
        && mCurToken != TokenCode::Separator );

    SkipLineSeparators();
}

Unique<DataDecl> AlgolyParser::ParseVar( Unique<DataDecl>&& varDecl, TokenCode assignToken )
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected variable name" );

    varDecl->Name = ParseRawSymbol();

    if ( mCurToken == TokenCode::Colon )
    {
        auto arrayTypeRef = Make<ArrayTypeRef>();

        ScanToken();
        ScanToken( TokenCode::LBracket );

        arrayTypeRef->SizeExpr = ParseExpr();

        ScanToken( TokenCode::RBracket );
        ScanToken( assignToken );
        SkipLineEndings();
        ScanToken( TokenCode::LBracket );

        auto initList = Make<InitList>();
        bool first = true;

        while ( mCurToken != TokenCode::RBracket )
        {
            if ( mCurToken == TokenCode::Ellipsis )
            {
                ScanToken();
                initList->HasExtra = true;
                break;
            }
            else if ( !first )
            {
                ScanToken( TokenCode::Comma );
                SkipLineEndings();
            }

            first = false;

            initList->Values.push_back( ParseExpr() );
        }

        ScanToken( TokenCode::RBracket );

        varDecl->TypeRef = std::move( arrayTypeRef );
        varDecl->Initializer = std::move( initList );
    }
    else
    {
        ScanToken( assignToken );
        SkipLineEndings();

        varDecl->Initializer = ParseExpr();
    }

    return varDecl;
}

Unique<Syntax> AlgolyParser::ParseReturn()
{
    auto retStmt = Make<ReturnStatement>();

    ScanToken();

    retStmt->Inner = ParseExpr();

    return retStmt;
}

Unique<Syntax> AlgolyParser::ParseIf()
{
    auto condExpr = Make<CondExpr>();

    condExpr->IsIf = true;
    condExpr->Clauses.push_back( ParseIfClause() );

    while ( mCurToken == TokenCode::Elsif )
    {
        condExpr->Clauses.push_back( ParseIfClause() );
    }

    if ( mCurToken == TokenCode::Else )
    {
        condExpr->Clauses.push_back( ParseElseClause() );
    }

    ScanToken( TokenCode::End );

    return condExpr;
}

Unique<CondClause> AlgolyParser::ParseIfClause()
{
    std::unique_ptr<CondClause> clause( new CondClause() );

    ScanToken();

    clause->Condition = ParseExpr();

    if ( mCurToken != TokenCode::Then )
        ThrowSyntaxError( "Expected then" );

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause->Body );

    return clause;
}

Unique<CondClause> AlgolyParser::ParseElseClause()
{
    std::unique_ptr<CondClause> clause( new CondClause() );

    ScanToken();
    SkipLineSeparators();

    clause->Condition = MakeNumber( 1 );

    ParseStatements( clause->Body );

    return clause;
}

Unique<Syntax> AlgolyParser::ParseFor()
{
    auto forStmt = Make<ForStatement>();

    ScanToken();

    forStmt->IndexName = ParseRawSymbol();

    ScanToken( TokenCode::Assign );

    forStmt->First = ParseExpr();

    if ( mCurToken == TokenCode::Above
        || mCurToken == TokenCode::Below
        || mCurToken == TokenCode::Downto
        || mCurToken == TokenCode::To )
    {
        forStmt->Comparison = ParseAsRawSymbol();
    }
    else
    {
        ThrowSyntaxError( "Expected: above, below, downto, to" );
    }

    forStmt->Last = ParseExpr();

    if ( mCurToken == TokenCode::By )
    {
        ScanToken();

        forStmt->Step = ParseExpr();
    }

    ScanToken( TokenCode::Do );
    SkipLineSeparators();

    ParseStatements( forStmt->Body );

    ScanToken( TokenCode::End );

    return forStmt;
}

Unique<Syntax> AlgolyParser::ParseLoop()
{
    auto loopStmt = Make<LoopStatement>();

    ScanToken();
    SkipLineSeparators();

    ParseStatements( loopStmt->Body );

    if ( mCurToken == TokenCode::Do )
    {
        ScanToken();
        ScanToken( TokenCode::While );

        loopStmt->Condition = ParseExpr();
    }

    ScanToken( TokenCode::End );

    return loopStmt;
}

Unique<Syntax> AlgolyParser::ParseWhile()
{
    auto whileStmt = Make<WhileStatement>();

    ScanToken();
    whileStmt->Condition = ParseExpr();
    ScanToken( TokenCode::Do );

    ParseStatements( whileStmt->Body );

    ScanToken( TokenCode::End );

    return whileStmt;
}

Unique<Syntax> AlgolyParser::ParseCase()
{
    auto caseExpr = Make<CaseExpr>();

    ScanToken();

    caseExpr->TestKey = ParseExpr();

    SkipLineSeparators();

    while ( mCurToken == TokenCode::When )
    {
        caseExpr->Clauses.push_back( ParseCaseWhen() );
    }

    if ( mCurToken == TokenCode::Else )
    {
        caseExpr->Fallback = ParseCaseElse();
    }

    ScanToken( TokenCode::End );

    return caseExpr;
}

Unique<CaseWhen> AlgolyParser::ParseCaseWhen()
{
    std::unique_ptr<CaseWhen> clause( new CaseWhen() );

    ScanToken();

    bool first = true;

    while ( true )
    {
        if ( !first )
        {
            ScanToken( TokenCode::Comma );
            SkipLineEndings();
        }

        first = false;

        if ( mCurToken == TokenCode::Symbol )
        {
            clause->Keys.push_back( ParseSymbol() );
        }
        else if ( mCurToken == TokenCode::Number )
        {
            clause->Keys.push_back( ParseNumber() );
        }
        else
        {
            ThrowSyntaxError( "Expected symbol or number" );
        }

        if ( mCurToken == TokenCode::Then )
            break;
    }

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause->Body );

    return clause;
}

Unique<CaseElse> AlgolyParser::ParseCaseElse()
{
    std::unique_ptr<CaseElse> clause( new CaseElse() );

    ScanToken();
    SkipLineSeparators();

    ParseStatements( clause->Body );

    return clause;
}

Unique<Syntax> AlgolyParser::ParseBreak()
{
    ScanToken();
    return Make<BreakStatement>();
}

Unique<Syntax> AlgolyParser::ParseNext()
{
    ScanToken();
    return Make<NextStatement>();
}

I32 AlgolyParser::ParseRawNumber()
{
    if ( mCurToken != TokenCode::Number )
        ThrowSyntaxError( "Expected number" );

    I32 number = mCurNumber;
    ScanToken();
    return number;
}

Unique<NumberExpr> AlgolyParser::ParseNumber()
{
    if ( mCurToken != TokenCode::Number )
        ThrowSyntaxError( "Expected number" );

    std::unique_ptr<NumberExpr> elem( WrapNumber() );
    ScanToken();
    return elem;
}

std::string AlgolyParser::ParseRawSymbol()
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected symbol" );

    std::string symbol = std::move( mCurString );
    ScanToken();
    return symbol;
}

Unique<NameExpr> AlgolyParser::ParseSymbol()
{
    if ( mCurToken != TokenCode::Symbol )
        ThrowSyntaxError( "Expected symbol" );

    std::unique_ptr<NameExpr> elem( WrapSymbol() );
    ScanToken();
    return elem;
}

std::string AlgolyParser::ParseAsRawSymbol()
{
    if ( mCurString.size() == 0 )
        mRep.ThrowInternalError();

    std::string symbol = std::move( mCurString );
    ScanToken();
    return symbol;
}

Unique<NumberExpr> AlgolyParser::WrapNumber()
{
    return MakeNumber( mCurNumber );
}

Unique<NameExpr> AlgolyParser::WrapSymbol()
{
    return MakeSymbol( mCurString.c_str() );
}

Unique<NumberExpr> AlgolyParser::MakeNumber( int32_t value )
{
    NumberExpr* number = new NumberExpr();
    number->Kind = SyntaxKind::Number;
    number->Value = value;
    number->Line = mTokLine;
    number->Column = mTokCol;
    return std::unique_ptr<NumberExpr>( number );
}

Unique<NameExpr> AlgolyParser::MakeSymbol( const char* string )
{
    NameExpr* symbol = new NameExpr();
    symbol->Kind = SyntaxKind::Name;
    symbol->String = string;
    symbol->Line = mTokLine;
    symbol->Column = mTokCol;
    return std::unique_ptr<NameExpr>( symbol );
}

template <typename T>
Unique<T> AlgolyParser::Make()
{
    T* syntax = new T();
    syntax->Line = mTokLine;
    syntax->Column = mTokCol;
    return std::unique_ptr<T>( syntax );
}

void AlgolyParser::ThrowSyntaxError( const char* format, ... )
{
    va_list args;
    va_start( args, format );
    mRep.ThrowError( CERR_SYNTAX, mTokLine, mTokCol, format, args );
    va_end( args );
}
