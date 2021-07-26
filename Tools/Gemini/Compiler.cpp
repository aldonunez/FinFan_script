// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "pch.h"
#include "Compiler.h"
#include "BinderVisitor.h"
#include "Disassembler.h"
#include "FolderVisitor.h"
#include "OpCodes.h"
#include "VmCommon.h"
#include <algorithm>
#include <stdarg.h>
#include <stdexcept>
#include <string.h>

#define ENABLE_DISASSEMBLY 0


namespace Gemini
{

#if ENABLE_DISASSEMBLY
    void Disassemble( const uint8_t* program, int size );

    #define DISASSEMBLE( code, size ) Disassemble( code, size )
#else
    #define DISASSEMBLE( code, size )
#endif


Compiler::Compiler( ICompilerEnv* env, ICompilerLog* log, CompilerAttrs& globalAttrs, ModSize modIndex ) :
    mEnv( env ),
    mRep( log ),
    mModIndex( modIndex ),
    mGlobalAttrs( globalAttrs )
{
    if ( env == nullptr )
        throw std::invalid_argument( "env" );

    if ( log == nullptr )
        throw std::invalid_argument( "log" );

    if ( modIndex >= ModSizeMax )
        throw std::invalid_argument( "modIndex" );

    mLoadedAddrDecl.reset( new LoadedAddressDeclaration() );
    mLoadedAddrDecl->Type = mErrorType;

    mLoadedAddrDeclConst.reset( new LoadedAddressDeclaration() );
    mLoadedAddrDeclConst->Type = mErrorType;
    mLoadedAddrDeclConst->IsReadOnly = true;
}

void Compiler::AddUnit( Unique<Unit>&& unit )
{
    if ( !unit )
        throw std::invalid_argument( "unit" );

    mUnits.push_back( std::move( unit ) );
}

void Compiler::AddModule( std::shared_ptr<ModuleDeclaration> moduleDecl )
{
    if ( !moduleDecl )
        throw std::invalid_argument( "moduleDecl" );

    auto modTabResult = mModuleTable.insert( SymTable::value_type( moduleDecl->Name, moduleDecl ) );

    if ( !modTabResult.second )
        throw std::invalid_argument( "Module name already used" );

    auto modIdResult = mModulesById.insert( ModIdMap::value_type( moduleDecl->Index, moduleDecl ) );

    if ( !modIdResult.second )
        throw std::invalid_argument( "Module index already used" );
}

CompilerErr Compiler::Compile()
{
    if ( mStatus != CompilerErr::NONE )
        return mStatus;

    try
    {
        BindAttributes();
        FoldConstants();
        GenerateCode();

        CopyDeferredGlobals();

        GenerateSentinel();
        FinalizeConstData();

        mStatus = CompilerErr::OK;
    }
    catch ( CompilerException& ex )
    {
        mStatus = ex.GetError();
    }

    return mStatus;
}

void Compiler::GetStats( CompilerStats& stats )
{
    if ( mStatus == CompilerErr::OK && !mCalculatedStats )
    {
        mStats.CodeBytesWritten = static_cast<CodeSize>( mCodeBin.size() );

        CalculateStackDepth();

        mCalculatedStats = true;
    }

    stats = mStats;
}

uint8_t* Compiler::GetCode()
{
    return mCodeBin.data();
}

size_t Compiler::GetCodeSize()
{
    return mCodeBin.size();
}

int32_t* Compiler::GetData()
{
    return mGlobals.data();
}

size_t Compiler::GetDataSize()
{
    return mGlobals.size();
}

int32_t* Compiler::GetConst()
{
    return mConsts.data();
}

size_t Compiler::GetConstSize()
{
    return mConsts.size();
}

std::shared_ptr<ModuleDeclaration> Compiler::GetMetadata( const char* modName )
{
    if ( modName == nullptr )
        throw std::invalid_argument( "modName" );

    std::shared_ptr<ModuleDeclaration> modDecl( new ModuleDeclaration() );

    modDecl->Name = modName;
    modDecl->Index = mModIndex;
    modDecl->Table = std::move( mPublicTable );
    modDecl->Type = mModuleType;

    return modDecl;
}

void Compiler::BindAttributes()
{
    BinderVisitor binder( mModIndex, mGlobalTable, mModuleTable, mPublicTable, mGlobalAttrs, mRep.GetLog() );

    for ( auto& unit : mUnits )
        binder.Declare( unit.get() );

    for ( auto& unit : mUnits )
        binder.BindDeclarations( unit.get() );

    for ( auto& unit : mUnits )
        binder.BindFunctionBodies( unit.get() );

    mGlobals.resize( binder.GetDataSize() );
    mConsts.resize( binder.GetConstSize() );

    mModuleAttrs = binder.GetModuleAttrs();
}

void Compiler::FoldConstants()
{
#if !defined( GEMINIVM_DISABLE_FOLDING_PASS )
    FolderVisitor folder( mRep.GetLog() );

    for ( auto& unit : mUnits )
        folder.Fold( unit.get() );
#endif
}

void Compiler::GenerateCode()
{
    for ( auto& unit : mUnits )
        unit->Accept( this );
}

void Compiler::VisitUnit( Unit* unit )
{
    for ( auto& varNode : unit->DataDeclarations )
        Generate( varNode.get() );

    for ( auto& funcNode : unit->FuncDeclarations )
        funcNode->Accept( this );
}

void Compiler::Generate( Syntax* elem )
{
    GenStatus status = { ExprKind::Other };
    Generate( elem, GenConfig::Statement(), status );
}

void Compiler::Generate( Syntax* elem, const GenConfig& config )
{
    GenStatus status = { ExprKind::Other };
    Generate( elem, config, status );
}

void Compiler::Generate( Syntax* node, const GenConfig& config, GenStatus& status )
{
    mGenStack.push_back( { config, status } );

    node->Accept( this );

    mGenStack.pop_back();

    if ( status.kind != ExprKind::Logical )
    {
        if ( config.trueChain != nullptr )
        {
            OpCode opcode = (config.invert && status.kind != ExprKind::Comparison) ? OP_BFALSE : OP_BTRUE;
            EmitBranch( opcode, config.trueChain );
            DecreaseExprDepth();

            EmitBranch( OP_B, config.falseChain );
        }
        else if ( config.invert && status.kind != ExprKind::Comparison )
        {
            if ( !config.discard )
            {
                Emit( OP_NOT );
            }
        }
    }
}

void Compiler::GenerateDiscard( Syntax* elem )
{
    GenConfig config{};

    GenerateDiscard( elem, config );
}

void Compiler::GenerateDiscard( Syntax* elem, const GenConfig& config )
{
    GenConfig configDiscard = config.WithDiscard();
    GenStatus status = { ExprKind::Other };

    mGenStack.push_back( { configDiscard, status } );

    elem->Accept( this );

    mGenStack.pop_back();

    if ( !status.discarded )
    {
        assert( status.discarded );
        mRep.LogWarning( elem->FileName, elem->Line, elem->Column, "Deprecated: POP was emitted." );

        Emit( OP_POP );

        DecreaseExprDepth();
    }

    assert( mCurExprDepth == 0 );
}

void Compiler::VisitNumberExpr( NumberExpr* numberExpr )
{
    GenerateNumber( numberExpr, Config(), Status() );
}

void Compiler::GenerateNumber( NumberExpr* number, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    EmitLoadConstant( (int32_t) number->Value );
}

void Compiler::EmitLoadConstant( int32_t value )
{
    if ( (value >= INT8_MIN) && (value <= INT8_MAX) )
    {
        EmitU8( OP_LDC_S, static_cast<uint8_t>(value) );
    }
    else
    {
        EmitU32( OP_LDC, value );
    }

    IncreaseExprDepth();
}

void Compiler::VisitNameExpr( NameExpr* nameExpr )
{
    GenerateSymbol( nameExpr, Config(), Status() );
}

void Compiler::GenerateSymbol( NameExpr* symbol, const GenConfig& config, GenStatus& status )
{
    GenerateValue( symbol, symbol->GetDecl(), config, status );
}

void Compiler::GenerateValue( Syntax* node, Declaration* decl, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        status.baseDecl = decl;
        return;
    }

    if ( IsScalarType( node->Type->GetKind() ) )
    {
        EmitLoadScalar( node, decl, 0 );
    }
    else
    {
        EmitLoadAddress( node, decl, 0 );
    }
}

void Compiler::EmitLoadScalar( Syntax* node, Declaration* decl, int32_t offset )
{
    switch ( decl->Kind )
    {
    case DeclKind::Global:
        assert( offset >= 0 && offset < GlobalSizeMax );
        assert( offset < (GlobalSizeMax - ((GlobalStorage*) decl)->Offset) );

        EmitModAccess(
            OP_LDMOD,
            ((GlobalStorage*) decl)->ModIndex,
            static_cast<uint16_t>(((GlobalStorage*) decl)->Offset + offset) );
        IncreaseExprDepth();
        break;

    case DeclKind::Local:
        assert( offset >= 0 && offset < LocalSizeMax );
        assert( offset <= ((LocalStorage*) decl)->Offset );

        EmitU8( OP_LDLOC, static_cast<uint8_t>(((LocalStorage*) decl)->Offset - offset) );
        IncreaseExprDepth();
        break;

    case DeclKind::Param:
        {
            auto param = (ParamStorage*) decl;

            if ( param->Mode == ParamMode::Value
                || param->Mode == ParamMode::ValueIn )
            {
                assert( offset >= 0 && offset < ParamSizeMax );
                assert( offset < (ParamSizeMax - param->Offset) );

                EmitU8( OP_LDARG, static_cast<uint8_t>(param->Offset + offset) );
                IncreaseExprDepth();
            }
            else if ( param->Mode == ParamMode::RefInOut
                || param->Mode == ParamMode::RefIn )
            {
                EmitU8( OP_LDARG, param->Offset );
                IncreaseExprDepth();

                if ( offset > 0 )
                    EmitSpilledAddrOffset( offset );

                Emit( OP_LOADI );
            }
            else
            {
                THROW_INTERNAL_ERROR( "EmitLoadScalar: Bad parameter mode" );
            }
        }
        break;

    case DeclKind::Func:
        mRep.ThrowSemanticsError( node, "functions don't have values" );
        break;

    case DeclKind::Const:
        {
            assert( offset >= 0 && offset < GlobalSizeMax );

            auto constant = (Constant*) decl;
            ValueVariant value;

            if ( constant->Value.Is( ValueKind::Aggregate ) )
            {
                assert( (GlobalSize) offset < (constant->Value.GetAggregate().Module->GetConsts().size() - constant->Value.GetAggregate().Offset) );

                FolderVisitor folder( mRep.GetLog() );

                GlobalSize bufOffset = static_cast<GlobalSize>(constant->Value.GetAggregate().Offset + offset);

                value = folder.ReadConstValue( *node->Type, constant->Value.GetAggregate().Module, bufOffset );
            }
            else
            {
                assert( offset == 0 );

                value = constant->Value;
            }

            if ( value.Is( ValueKind::Integer ) )
            {
                EmitLoadConstant( value.GetInteger() );
            }
            else if ( value.Is( ValueKind::Function ) )
            {
                EmitLoadFuncAddress( value.GetFunction().get() );
            }
            else
            {
                THROW_INTERNAL_ERROR( "" );
            }
        }
        break;

    case DeclKind::Enum:
        assert( offset == 0 );
        EmitLoadConstant( ((EnumMember*) decl)->Value );
        break;

    case DeclKind::LoadedAddress:
        assert( offset >= 0 && offset < DataSizeMax );

        if ( offset > 0 )
            EmitSpilledAddrOffset( offset );

        Emit( OP_LOADI );
        DecreaseExprDepth();
        IncreaseExprDepth();
        break;

    default:
        THROW_INTERNAL_ERROR( "EmitLoadScalar: DeclKind" );
    }
}

void Compiler::EmitSpilledAddrOffset( int32_t offset )
{
    EmitU24( OP_OFFSET, offset );
}

void Compiler::GenerateEvalStar( CallOrSymbolExpr* callOrSymbol, const GenConfig& config, GenStatus& status )
{
    auto& symbol = callOrSymbol->Symbol;
    auto decl = symbol->GetDecl();

    if ( decl->Kind == DeclKind::Func
        || decl->Kind == DeclKind::NativeFunc )
    {
        std::vector<Unique<Syntax>> args;

        GenerateCall( decl, args, config, status );
    }
    else
    {
        Generate( symbol.get() );
    }
}

void Compiler::VisitCallOrSymbolExpr( CallOrSymbolExpr* callOrSymbol )
{
    GenerateEvalStar( callOrSymbol, Config(), Status() );
}

void Compiler::GenerateArithmetic( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    auto&   op = binary->Op;
    uint8_t primitive;

    if ( op == "+" )
        primitive = PRIM_ADD;
    else if ( op == "-" )
        primitive = PRIM_SUB;
    else if ( op == "*" )
        primitive = PRIM_MUL;
    else if ( op == "/" )
        primitive = PRIM_DIV;
    else if ( op == "%" )
        primitive = PRIM_MOD;
    else
        THROW_INTERNAL_ERROR( "" );

    GenerateBinaryPrimitive( binary, primitive, config, status );
}

void Compiler::VisitBinaryExpr( BinaryExpr* binary )
{
    if ( binary->Op[0] == '='
        || binary->Op[0] == '<'
        || binary->Op[0] == '>' )
    {
        GenerateComparison( binary, Config(), Status() );
    }
    else if ( binary->Op == "and" )
    {
        GenerateAnd( binary, Config(), Status() );
    }
    else if ( binary->Op == "or" )
    {
        GenerateOr( binary, Config(), Status() );
    }
    else
    {
        GenerateArithmetic( binary, Config(), Status() );
    }
}

void Compiler::GenerateReturn( ReturnStatement* retStmt, const GenConfig& config, GenStatus& status )
{
    if ( retStmt->Inner != nullptr )
    {
        Generate( retStmt->Inner.get() );
    }
    else
    {
        // Currently not allowed in the language
        EmitU8( OP_LDC_S, 0 );
        IncreaseExprDepth();
    }

    Emit( OP_RET );

    if ( config.discard )
        DecreaseExprDepth();

    status.discarded = true;
    status.tailRet = true;
}

void Compiler::VisitReturnStatement( ReturnStatement* retStmt )
{
    GenerateReturn( retStmt, Config(), Status() );
}

void Compiler::VisitCountofExpr( CountofExpr* countofExpr )
{
    if ( Config().discard )
    {
        Status().discarded = true;
        return;
    }

    EmitCountofArray( countofExpr->Expr.get() );
}

void Compiler::EmitCountofArray( Syntax* arrayNode )
{
    ArrayType& arrayType = (ArrayType&) *arrayNode->Type;

    if ( arrayType.Count != 0 )
    {
        EmitLoadConstant( arrayType.Count );
    }
    else
    {
        auto addr = CalcAddress( arrayNode );

        if ( addr.decl->Kind == DeclKind::Param )
        {
            auto param = (ParamStorage*) addr.decl;

            EmitU8( OP_LDARG, param->Offset + 1 );
            IncreaseExprDepth();
        }
        else if ( addr.decl->Kind == DeclKind::LoadedAddress )
        {
            Emit( OP_POP );
            DecreaseExprDepth();
        }
        else
        {
            THROW_INTERNAL_ERROR( "" );
        }
    }
}

void Compiler::GenerateCond( CondExpr* condExpr, const GenConfig& config, GenStatus& status )
{
    PatchChain  falseChain;
    PatchChain  leaveChain;
    bool        foundCatchAll = false;
    LocalSize   exprDepth = mCurExprDepth;
    int32_t     startLoc = static_cast<int32_t>( mCodeBin.size() );

    GenConfig statementConfig = GenConfig::Statement( config.discard )
        .WithLoop( config.breakChain, config.nextChain );

    // TODO: check all the clauses for tail-return. If they all do, then set status.tailRet.

    for ( int i = 0; i < (int) condExpr->Clauses.size(); i++ )
    {
        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        auto clause = condExpr->Clauses[i].get();

        auto optVal = GetFinalOptionalSyntaxValue( clause->Condition.get() );

        bool isConstantTrue = optVal.has_value() && optVal.value() != 0;

        if ( isConstantTrue )
        {
            if ( clause->Body.Statements.size() == 0 && !condExpr->IsIf )
            {
                GenStatus clauseStatus = { ExprKind::Other };
                Generate( clause->Condition.get(), statementConfig, clauseStatus );
            }
            else
            {
                GenStatus clauseStatus = { ExprKind::Other };
                GenerateImplicitProgn( &clause->Body, statementConfig, clauseStatus );
            }
            foundCatchAll = true;
            break;
        }

        if ( clause->Body.Statements.size() == 0 && !condExpr->IsIf )
        {
            Generate( clause->Condition.get() );

            if ( !config.discard )
            {
                Emit( OP_DUP );
                IncreaseExprDepth();
            }

            EmitBranch( OP_BTRUE, &leaveChain );
            DecreaseExprDepth();
        }
        else
        {
            PatchChain  trueChain;

            falseChain = PatchChain();

            Generate( clause->Condition.get(), GenConfig::Expr( &trueChain, &falseChain, false ) );
            ElideTrue( &trueChain, &falseChain );
            Patch( &trueChain );

            // True
            GenStatus clauseStatus = { ExprKind::Other };
            GenerateImplicitProgn( &clause->Body, statementConfig, clauseStatus );

            if ( i < (int) condExpr->Clauses.size() - 1 || !config.discard )
            {
                // Not the last clause; or last one is not catch-all and not discarding.
                // So, we'll emit LDC.S 0 below. And here we emit a jump over it.

                EmitBranch( OP_B, &leaveChain );
            }

            ElideFalse( &trueChain, &falseChain );
            Patch( &falseChain );
        }
    }

    if ( !foundCatchAll )
    {
        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        if ( !config.discard )
        {
            EmitU8( OP_LDC_S, 0 );
            IncreaseExprDepth();
        }
    }

    // This is not very important, because it optimizes a case like:
    //   if n then B() else 3 end
    // ... in a discarding context

    if ( (mCodeBin.size() - startLoc) >= BranchInst::Size
        && mCodeBin[mCodeBin.size() - BranchInst::Size] == OP_B
        && leaveChain.First != nullptr
        && leaveChain.First->Ref == ((int32_t) mCodeBin.size() - BranchInst::Size)
        && falseChain.PatchedInstIndex == (int32_t) mCodeBin.size() )
    {
        PopPatch( &leaveChain );
        DeleteCode( BranchInst::Size );
        Patch( &falseChain );
    }

    Patch( &leaveChain );

    if ( config.discard )
        status.discarded = true;
}

void Compiler::VisitCondExpr( CondExpr* condExpr )
{
    GenerateCond( condExpr, Config(), Status() );
}

void Compiler::VisitAsExpr( AsExpr* asExpr )
{
    Generate( asExpr->Inner.get(), Config(), Status() );
}

void Compiler::GenerateSetScalar( AssignmentExpr* assignment, const GenConfig& config, GenStatus& status )
{
    // Value
    Generate( assignment->Right.get() );

    if ( config.discard )
    {
        status.discarded = true;
    }
    else
    {
        Emit( OP_DUP );
        IncreaseExprDepth();
    }

    auto addr = CalcAddress( assignment->Left.get() );

    EmitStoreScalar( assignment->Left.get(), addr.decl, addr.offset );
}

void Compiler::EmitStoreScalar( Syntax* node, Declaration* decl, int32_t offset )
{
    if ( decl->IsReadOnly )
        mRep.ThrowSemanticsError( node, "Constants can't be changed" );

    switch ( decl->Kind )
    {
    case DeclKind::Global:
        assert( offset >= 0 && offset < GlobalSizeMax );
        assert( offset < (GlobalSizeMax - ((GlobalStorage*) decl)->Offset) );

        EmitModAccess(
            OP_STMOD,
            ((GlobalStorage*) decl)->ModIndex,
            static_cast<uint16_t>(((GlobalStorage*) decl)->Offset + offset) );
        break;

    case DeclKind::Local:
        assert( offset >= 0 && offset < LocalSizeMax );
        assert( offset <= ((LocalStorage*) decl)->Offset );

        EmitU8( OP_STLOC, static_cast<uint8_t>(((LocalStorage*) decl)->Offset - offset) );
        break;

    case DeclKind::Param:
        {
            auto param = (ParamStorage*) decl;

            if ( param->Mode == ParamMode::Value )
            {
                assert( offset >= 0 && offset < ParamSizeMax );
                assert( offset < (ParamSizeMax - param->Offset) );

                EmitU8( OP_STARG, static_cast<uint8_t>(param->Offset + offset) );
            }
            else if ( param->Mode == ParamMode::RefInOut )
            {
                EmitU8( OP_LDARG, param->Offset );
                IncreaseExprDepth();

                if ( offset > 0 )
                    EmitSpilledAddrOffset( offset );

                Emit( OP_STOREI );
                DecreaseExprDepth();
            }
            else
            {
                THROW_INTERNAL_ERROR( "EmitStoreScalar: Bad parameter mode" );
            }
        }
        break;

    case DeclKind::Func:
    case DeclKind::NativeFunc:
        mRep.ThrowSemanticsError( node, "functions can't be assigned a value" );
        break;

    case DeclKind::Const:
    case DeclKind::Enum:
        mRep.ThrowSemanticsError( node, "Constants can't be changed" );
        break;

    case DeclKind::LoadedAddress:
        assert( offset >= 0 && offset < DataSizeMax );

        if ( offset > 0 )
            EmitSpilledAddrOffset( offset );

        Emit( OP_STOREI );
        DecreaseExprDepth();
        break;

    default:
        THROW_INTERNAL_ERROR( "EmitStoreScalar: DeclKind" );
    }

    DecreaseExprDepth();
}

void Compiler::GenerateSetAggregate( AssignmentExpr* assignment, const GenConfig& config, GenStatus& status )
{
    if ( IsOpenArrayType( *assignment->Left->Type ) || IsOpenArrayType( *assignment->Right->Type ) )
    {
        auto& leftArrayType = (ArrayType&) *assignment->Left->Type;
        auto& rightArrayType = (ArrayType&) *assignment->Right->Type;

        GenerateRef( *assignment->Right, leftArrayType, false );

        if ( config.discard )
        {
            status.discarded = true;
        }
        else
        {
            Emit( OP_OVER );
            Emit( OP_OVER );
            IncreaseExprDepth( 2 );
        }

        if ( IsClosedArrayType( leftArrayType ) )
            EmitLoadConstant( leftArrayType.Count );

        auto addr = CalcAddress( assignment->Left.get(), true );

        EmitLoadAddress( assignment->Left.get(), addr.decl, addr.offset );

        EmitU24( OP_COPYARRAY, rightArrayType.ElemType->GetSize() );

        DecreaseExprDepth( 4 );

        // Generating a return value or argument value would need PUSHBLOCK
    }
    else
    {
        // Value
        Generate( assignment->Right.get() );

        if ( config.discard )
        {
            status.discarded = true;
        }
        else
        {
            Emit( OP_DUP );
            IncreaseExprDepth();
        }

        auto addr = CalcAddress( assignment->Left.get(), true );

        EmitLoadAddress( assignment->Left.get(), addr.decl, addr.offset );

        EmitU24( OP_COPYBLOCK, assignment->Right->Type->GetSize() );

        DecreaseExprDepth( 2 );
    }
}

void Compiler::VisitAssignmentExpr( AssignmentExpr* assignment )
{
    if ( IsScalarType( assignment->Left->Type->GetKind() ) )
        GenerateSetScalar( assignment, Config(), Status() );
    else
        GenerateSetAggregate( assignment, Config(), Status() );
}

void Compiler::VisitProcDecl( ProcDecl* procDecl )
{
    uint32_t addr = static_cast<CodeSize>( mCodeBin.size() );

    auto func = (Function*) procDecl->Decl.get();

    func->Address = addr;

    mGlobalAttrs.AddFunctionByAddress( std::static_pointer_cast<Function>(procDecl->Decl) );

    auto patchIt = mFuncPatchMap.find( func->Name );
    if ( patchIt != mFuncPatchMap.end() )
        PatchCalls( &patchIt->second, addr );

    mEnv->AddExternal( procDecl->Name, ExternalKind::Bytecode, func->Address );

    GenerateProc( procDecl, func );
}

void Compiler::VisitLambdaExpr( LambdaExpr* lambdaExpr )
{
    THROW_INTERNAL_ERROR( "LambdaExpr was not transformed" );
}

void Compiler::GenerateFunction( AddrOfExpr* addrOf, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }

    auto func = (Function*) addrOf->Inner->GetDecl();

    EmitLoadFuncAddress( func );
}

void Compiler::EmitLoadFuncAddress( Function* func )
{
    size_t curIndex = ReserveCode( 5 );
    mCodeBin[curIndex] = OP_LDC;

    EmitFuncAddress( func, { CodeRefKind::Code, static_cast<int32_t>( curIndex + 1 ) } );

    IncreaseExprDepth();

    DISASSEMBLE( &mCodeBin[curIndex], 5 );
}

void Compiler::EmitFuncAddress( Function* func, CodeRef funcRef )
{
    if ( func->Address == UndefinedAddr )
    {
        PushFuncPatch( func->Name, funcRef );
    }

    uint32_t addrWord = CodeAddr::Build( func->Address, func->ModIndex );

    switch ( funcRef.Kind )
    {
    case CodeRefKind::Code:
        StoreU32( &mCodeBin[funcRef.Location], addrWord );
        break;

    case CodeRefKind::Data:
        mGlobals[funcRef.Location] = addrWord;
        break;

    case CodeRefKind::Const:
        mConsts[funcRef.Location] = addrWord;
        break;

    default:
        THROW_INTERNAL_ERROR( "EmitFuncAddress: CodeRef::Kind" );
    }
}

void Compiler::VisitAddrOfExpr( AddrOfExpr* addrOf )
{
    GenerateFunction( addrOf, Config(), Status() );
}

void Compiler::GenerateFuncall( CallExpr* call, const GenConfig& config, GenStatus& status )
{
    auto ptrType = (PointerType*) call->Head->Type.get();
    auto funcType = (FuncType*) ptrType->TargetType.get();

    ParamSize argCount = GenerateCallArgs( call->Arguments, funcType );

    Generate( call->Head.get() );

    EmitU8( OP_CALLI, CallFlags::Build( argCount, config.discard ) );

    DecreaseExprDepth();

    if ( config.discard )
        status.discarded = true;
    else
        IncreaseExprDepth();

    DecreaseExprDepth( argCount );

    if ( mInFunc )
        mCurFunc->CallsIndirectly = true;
}

void Compiler::GenerateLet( LetStatement* letStmt, const GenConfig& config, GenStatus& status )
{
    for ( auto& binding : letStmt->Variables )
    {
        if ( binding->Kind == SyntaxKind::VarDecl )
            GenerateLetBinding( binding.get() );
    }

    GenerateImplicitProgn( &letStmt->Body, config, status );
}

void Compiler::GenerateLetBinding( DataDecl* binding )
{
    auto local = (LocalStorage*) binding->GetDecl();

    GenerateLocalInit( local->Offset, local->Type.get(), binding->Initializer.get() );
}

void Compiler::GenerateLocalInit( LocalSize offset, Type* localType, Syntax* initializer )
{
    if ( initializer == nullptr )
        return;

    auto type = initializer->Type.get();

    if ( IsScalarType( type->GetKind() ) )
    {
        Generate( initializer );
        EmitU8( OP_STLOC, offset );
        DecreaseExprDepth();
    }
    else if ( initializer->Kind == SyntaxKind::ArrayInitializer )
    {
        auto arrayType = (ArrayType*) type;

        EmitLocalArrayInitializer( offset, (ArrayType*) localType, (InitList*) initializer, arrayType->Count );
    }
    else if ( initializer->Kind == SyntaxKind::RecordInitializer )
    {
        EmitLocalRecordInitializer( offset, (RecordType*) localType, (RecordInitializer*) initializer );
    }
    else
    {
        EmitLocalAggregateCopyBlock( offset, localType, initializer );
    }
}

void Compiler::VisitLetStatement( LetStatement* letStmt )
{
    GenerateLet( letStmt, Config(), Status() );
}

void Compiler::EmitLocalAggregateCopyBlock( LocalSize offset, Type* localType, Syntax* valueElem )
{
    if ( IsOpenArrayType( *valueElem->Type ) )
    {
        Generate( valueElem );

        auto dstArrayType = (ArrayType&) *localType;

        EmitLoadConstant( dstArrayType.Count );
        EmitU8( OP_LDLOCA, offset );
        EmitU24( OP_COPYARRAY, dstArrayType.ElemType->GetSize() );

        IncreaseExprDepth();
        DecreaseExprDepth( 4 );
    }
    else
    {
        Generate( valueElem );

        EmitU8( OP_LDLOCA, offset );
        EmitU24( OP_COPYBLOCK, valueElem->Type->GetSize() );

        IncreaseExprDepth();
        DecreaseExprDepth( 2 );
    }
}

void Compiler::EmitLocalArrayInitializer( LocalSize offset, ArrayType* localType, InitList* initList, size_t size )
{
    LocalSize   locIndex = offset;
    LocalSize   i = 0;

    auto        localElemType = localType->ElemType.get();

    if ( initList->Values.size() > size )
        mRep.ThrowSemanticsError( initList, "Array has too many initializers" );

    for ( auto& entry : initList->Values )
    {
        assert( locIndex+1 >= entry->Type->GetSize() );

        GenerateLocalInit( locIndex, localElemType, entry.get() );
        i++;
        locIndex -= static_cast<LocalSize>(entry->Type->GetSize());
    }

    if ( initList->Fill == ArrayFill::Extrapolate && initList->Values.size() > 0 )
    {
        // Use unsigned values for well defined overflow

        size_t   count = initList->Values.size();
        uint32_t prevValue = 0;
        uint32_t step = 0;

        prevValue = GetSyntaxValue( initList->Values.back().get(), "Array initializer extrapolation requires a constant" );

        if ( initList->Values.size() > 1 )
        {
            auto prevValue2 = GetFinalOptionalSyntaxValue( initList->Values[count - 2].get() );
            if ( prevValue2.has_value() )
                step = VmSub( prevValue, prevValue2.value() );
        }

        for ( ; i < size; i++ )
        {
            uint32_t newValue = VmAdd( prevValue, step );

            EmitLoadConstant( newValue );
            EmitU8( OP_STLOC, static_cast<uint8_t>(locIndex) );
            locIndex--;
            DecreaseExprDepth();

            prevValue = newValue;
        }
    }
    else if ( initList->Fill == ArrayFill::Repeat && initList->Values.size() > 0 )
    {
        Syntax* lastNode = initList->Values.back().get();

        for ( ; i < size; i++ )
        {
            assert( locIndex+1 >= lastNode->Type->GetSize() );

            GenerateLocalInit( locIndex, localElemType, lastNode );
            locIndex -= static_cast<LocalSize>(lastNode->Type->GetSize());
        }
    }
}

void Compiler::EmitLocalRecordInitializer( LocalSize offset, RecordType* localType, RecordInitializer* recordInit )
{
    for ( auto& fieldInit : recordInit->Fields )
    {
        auto fieldDecl = (FieldStorage*) fieldInit->GetDecl();

        assert( offset >= fieldDecl->Offset );

        auto itLocalField = localType->GetFields().find( fieldInit->Name );
        auto localFieldType = itLocalField->second->GetType();

        GenerateLocalInit( static_cast<LocalSize>(offset - fieldDecl->Offset), localFieldType.get(), fieldInit->Initializer.get() );
    }
}

void Compiler::GenerateRef( Syntax& node, Type& siteType, bool writable )
{
    if ( IsOpenArrayType( siteType ) && IsClosedArrayType( *node.Type ) )
        EmitCountofArray( &node );

    auto addr = CalcAddress( &node, writable );

    if ( !addr.spilled )
    {
        EmitLoadAddress( &node, addr.decl, addr.offset );
    }
    else if ( addr.offset > 0 )
    {
        EmitSpilledAddrOffset( addr.offset );
    }
}

void Compiler::GenerateArg( Syntax& node, ParamSpec& paramSpec )
{
    switch ( paramSpec.Mode )
    {
    case ParamMode::Value:
    case ParamMode::ValueIn:
        Generate( &node );
        break;

    case ParamMode::RefInOut:
    case ParamMode::RefIn:
        GenerateRef( node, *paramSpec.Type, (paramSpec.Mode == ParamMode::RefInOut) );
        break;

    default:
        THROW_INTERNAL_ERROR( "GenerateArg: ParamMode" );
    }
}

ParamSize Compiler::GenerateCallArgs( std::vector<Unique<Syntax>>& arguments, FuncType* funcType )
{
    assert( arguments.size() == funcType->Params.size() );

    ParamSize argCount = 0;

    for ( auto i = static_cast<ptrdiff_t>(arguments.size()) - 1; i >= 0; i-- )
    {
        GenerateArg( *arguments[i], funcType->Params[i] );

        argCount += funcType->Params[i].Size;
    }

    return argCount;
}

void Compiler::GenerateCall( CallExpr* call, const GenConfig& config, GenStatus& status )
{
    GenerateCall( call->Head->GetDecl(), call->Arguments, config, status );
}

void Compiler::GenerateCall( Declaration* decl, std::vector<Unique<Syntax>>& arguments, const GenConfig& config, GenStatus& status )
{
    auto funcType = (FuncType*) decl->GetType().get();

    ParamSize argCount = GenerateCallArgs( arguments, funcType );

    uint8_t callFlags = CallFlags::Build( argCount, config.discard );

    if ( decl == nullptr )
    {
        THROW_INTERNAL_ERROR( "Call head has no declaration" );
    }
    else if ( decl->Kind == DeclKind::Func
        && ((Function*) decl)->ModIndex == mModIndex )
    {
        Function* func = (Function*) decl;

        size_t curIndex = ReserveCode( 5 );
        mCodeBin[curIndex+0] = OP_CALL;
        mCodeBin[curIndex+1] = callFlags;

        if ( func->Address == UndefinedAddr )
        {
            PushFuncPatch( func->Name, { CodeRefKind::Code, static_cast<int32_t>( curIndex + 2 ) } );
        }

        StoreU24( &mCodeBin[curIndex+2], func->Address );

        DISASSEMBLE( &mCodeBin[curIndex], 5 );

        if ( mInFunc )
            mCurFunc->CalledFunctions.push_back( { func->Name, mCurExprDepth, mModIndex } );
    }
    else
    {
        uint8_t opCode = 0;
        int32_t id = 0;

        if ( decl->Kind == DeclKind::Func )
        {
            auto func = (Function*) decl;

            opCode = OP_CALLM;
            id = CodeAddr::Build( func->Address, func->ModIndex );

            mCurFunc->CalledFunctions.push_back( { func->Name, mCurExprDepth, func->ModIndex } );
        }
        else if ( decl->Kind == DeclKind::NativeFunc )
        {
            id = ((NativeFunction*) decl)->Id;

            if ( id >= 0x100 )
                opCode = OP_CALLNATIVE;
            else
                opCode = OP_CALLNATIVE_S;
        }
        else
        {
            THROW_INTERNAL_ERROR( "" );
        }

        if ( opCode == OP_CALLNATIVE_S )
        {
            size_t curIndex = ReserveCode( 3 );
            mCodeBin[curIndex + 0] = opCode;
            mCodeBin[curIndex + 1] = callFlags;
            mCodeBin[curIndex + 2] = static_cast<uint8_t>(id);

            DISASSEMBLE( &mCodeBin[curIndex], 3 );
        }
        else
        {
            size_t curIndex = ReserveCode( 6 );
            mCodeBin[curIndex + 0] = opCode;
            mCodeBin[curIndex + 1] = callFlags;

            StoreU32( &mCodeBin[curIndex + 2], id );

            DISASSEMBLE( &mCodeBin[curIndex], 6 );
        }
    }

    IncreaseExprDepth();

    if ( config.discard )
    {
        status.discarded = true;
        DecreaseExprDepth();
    }

    DecreaseExprDepth( argCount );
}

void Compiler::VisitCallExpr( CallExpr* call )
{
    if ( call->IsIndirect )
        GenerateFuncall( call, Config(), Status() );
    else
        GenerateCall( call, Config(), Status() );
}

void Compiler::GenerateFor( ForStatement* forStmt, const GenConfig& config, GenStatus& status )
{
    // Variable name

    auto local = (LocalStorage*) forStmt->IndexDecl.get();

    uint8_t primitive;
    int32_t step;

    if ( forStmt->Comparison == ForComparison::Below )
    {
        primitive = PRIM_LT;
        step = 1;
    }
    else if ( forStmt->Comparison == ForComparison::To )
    {
        primitive = PRIM_LE;
        step = 1;
    }
    else if ( forStmt->Comparison == ForComparison::Downto )
    {
        primitive = PRIM_GE;
        step = -1;
    }
    else if ( forStmt->Comparison == ForComparison::Above )
    {
        primitive = PRIM_GT;
        step = -1;
    }
    else
    {
        mRep.ThrowSemanticsError( forStmt, "Expected symbol: to, downto, above, below" );
    }

    PatchChain  bodyChain;
    PatchChain  testChain;
    PatchChain  breakChain;
    PatchChain  nextChain;

    // Beginning expression
    Generate( forStmt->First.get() );
    Emit( OP_DUP );
    EmitU8( OP_STLOC, local->Offset );
    IncreaseExprDepth();
    DecreaseExprDepth();

    // The unconditional jump below leaves one value on the expression stack.
    // Decrease the depth here, because this value overlaps the first one pushed
    // for a usual test.
    DecreaseExprDepth();

    EmitBranch( OP_B, &testChain );

    int32_t bodyLoc = static_cast<int32_t>( mCodeBin.size() );

    // Body
    GenerateStatements( &forStmt->Body, config.WithLoop( &breakChain, &nextChain ), status );

    Patch( &nextChain );

    if ( forStmt->Step != nullptr )
        Generate( forStmt->Step.get() );
    else
        EmitLoadConstant( step );

    EmitU8( OP_LDLOC, local->Offset );
    EmitU8( OP_PRIM, PRIM_ADD );
    Emit( OP_DUP );
    EmitU8( OP_STLOC, local->Offset );
    IncreaseExprDepth();
    DecreaseExprDepth();
    IncreaseExprDepth();
    DecreaseExprDepth();

    Patch( &testChain );

    // Ending expression
    Generate( forStmt->Last.get() );

    EmitU8( OP_PRIM, primitive );
    DecreaseExprDepth();

    EmitBranch( OP_BTRUE, &bodyChain );
    DecreaseExprDepth();

    Patch( &bodyChain, bodyLoc );
    Patch( &breakChain );

    GenerateNilIfNeeded( config, status );
}

void Compiler::VisitForStatement( ForStatement* forStmt )
{
    GenerateFor( forStmt, Config(), Status() );
}

void Compiler::GenerateSimpleLoop( LoopStatement* loopStmt, const GenConfig& config, GenStatus& status )
{
    PatchChain  breakChain;
    PatchChain  nextChain;

    int32_t     bodyLoc = static_cast<int32_t>(mCodeBin.size());

    // Body
    GenerateStatements( &loopStmt->Body, config.WithLoop( &breakChain, &nextChain ), status );

    if ( loopStmt->Condition == nullptr )
    {
        EmitBranch( OP_B, &nextChain );

        Patch( &nextChain, bodyLoc );
    }
    else
    {
        GenStatus exprStatus;
        PatchChain bodyChain;

        Patch( &nextChain );

        Generate( loopStmt->Condition.get(), GenConfig::Expr( &bodyChain, &breakChain, false ), exprStatus );
        ElideFalse( &bodyChain, &breakChain );

        Patch( &bodyChain, bodyLoc );
    }

    Patch( &breakChain );

    GenerateNilIfNeeded( config, status );
}

void Compiler::VisitLoopStatement( LoopStatement* loopStmt )
{
    GenerateSimpleLoop( loopStmt, Config(), Status() );
}

void Compiler::GenerateDo( WhileStatement* whileStmt, const GenConfig& config, GenStatus& status )
{
    PatchChain  breakChain;
    PatchChain  nextChain;
    PatchChain  trueChain;

    int32_t testLoc = static_cast<int32_t>(mCodeBin.size());

    // Test expression
    Generate( whileStmt->Condition.get(), GenConfig::Expr( &trueChain, &breakChain, false ) );

    ElideTrue( &trueChain, &breakChain );
    Patch( &trueChain );

    // Body
    GenerateStatements( &whileStmt->Body, config.WithLoop( &breakChain, &nextChain ), status );

    EmitBranch( OP_B, &nextChain );

    Patch( &breakChain );
    Patch( &nextChain, testLoc );

    GenerateNilIfNeeded( config, status );
}

void Compiler::VisitWhileStatement( WhileStatement* whileStmt )
{
    GenerateDo( whileStmt, Config(), Status() );
}

void Compiler::GenerateBreak( BreakStatement* breakStmt, const GenConfig& config, GenStatus& status )
{
    if ( config.breakChain == nullptr )
        mRep.ThrowSemanticsError( breakStmt, "Cannot use break outside of a loop" );

    EmitBranch( OP_B, config.breakChain );

    status.discarded = true;
}

void Compiler::VisitBreakStatement( BreakStatement* breakStmt )
{
    GenerateBreak( breakStmt, Config(), Status() );
}

void Compiler::GenerateNext( NextStatement* nextStmt, const GenConfig& config, GenStatus& status )
{
    if ( config.nextChain == nullptr )
        mRep.ThrowSemanticsError( nextStmt, "Cannot use next outside of a loop" );

    EmitBranch( OP_B, config.nextChain );

    status.discarded = true;
}

void Compiler::VisitNextStatement( NextStatement* nextStmt )
{
    GenerateNext( nextStmt, Config(), Status() );
}

void Compiler::VisitYieldStatement( YieldStatement* nextStmt )
{
    Emit( OP_YIELD );

    GenerateNilIfNeeded( Config(), Status() );
}

void Compiler::GenerateCase( CaseExpr* caseExpr, const GenConfig& config, GenStatus& status )
{
    GenerateGeneralCase( caseExpr, config, status );
}

void Compiler::GenerateGeneralCase( CaseExpr* caseExpr, const GenConfig& config, GenStatus& status )
{
    PatchChain  exitChain;
    LocalSize   exprDepth = mCurExprDepth;

    const GenConfig& statementConfig = config;

    for ( auto& clause : caseExpr->Clauses )
    {
        PatchChain falseChain;
        PatchChain trueChain;

        // Restore the expression depth, so that it doesn't accumulate
        mCurExprDepth = exprDepth;

        size_t i = 0;

        for ( auto& key : clause->Keys )
        {
            i++;

            Generate( caseExpr->TestKey.get() );
            Generate( key.get() );

            EmitU8( OP_PRIM, PRIM_EQ );
            DecreaseExprDepth();

            if ( i == clause->Keys.size() )
            {
                EmitBranch( OP_BFALSE, &falseChain );
                DecreaseExprDepth();
            }
            else
            {
                EmitBranch( OP_BTRUE, &trueChain );
                DecreaseExprDepth();
            }
        }

        Patch( &trueChain );

        GenerateImplicitProgn( &clause->Body, statementConfig, status );

        EmitBranch( OP_B, &exitChain );

        Patch( &falseChain );
    }

    // Restore the expression depth, so that it doesn't accumulate
    mCurExprDepth = exprDepth;

    if ( caseExpr->Fallback != nullptr )
    {
        GenerateImplicitProgn( &caseExpr->Fallback->Body, statementConfig, status );
    }
    else
    {
        GenerateNilIfNeeded( config, status );
    }

    Patch( &exitChain );
}

void Compiler::VisitCaseExpr( CaseExpr* caseExpr )
{
    GenerateCase( caseExpr, Config(), Status() );
}

void Compiler::VisitUnaryExpr( UnaryExpr* unary )
{
    if ( unary->Op == "not" )
    {
        Generate( unary->Inner.get(), Config().Invert(), Status() );
        Status().kind = ExprKind::Logical;
    }
    else if ( unary->Op == "-" )
    {
        GenerateUnaryPrimitive( unary->Inner.get(), Config(), Status() );
    }
    else
    {
        THROW_INTERNAL_ERROR( "" );
    }
}

void Compiler::GenerateComparison( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    auto&   op = binary->Op;
    uint8_t positivePrimitive;
    uint8_t negativePrimitive;

    if ( op == "=" )
    {
        positivePrimitive = PRIM_EQ;
        negativePrimitive = PRIM_NE;
    }
    else if ( op == "<>" )
    {
        positivePrimitive = PRIM_NE;
        negativePrimitive = PRIM_EQ;
    }
    else if ( op == "<" )
    {
        positivePrimitive = PRIM_LT;
        negativePrimitive = PRIM_GE;
    }
    else if ( op == "<=" )
    {
        positivePrimitive = PRIM_LE;
        negativePrimitive = PRIM_GT;
    }
    else if ( op == ">" )
    {
        positivePrimitive = PRIM_GT;
        negativePrimitive = PRIM_LE;
    }
    else if ( op == ">=" )
    {
        positivePrimitive = PRIM_GE;
        negativePrimitive = PRIM_LT;
    }
    else
    {
        THROW_INTERNAL_ERROR( "" );
    }

    GenerateBinaryPrimitive( binary, config.invert ? negativePrimitive : positivePrimitive, config, status );
    status.kind = ExprKind::Comparison;
}

void Compiler::GenerateAnd( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    ConjSpec spec = { &Compiler::GenerateAndClause, &Compiler::GenerateOrClause };
    GenerateConj( &spec, binary, config );
    status.kind = ExprKind::Logical;

    if ( config.discard )
        status.discarded = true;
}

void Compiler::GenerateOr( BinaryExpr* binary, const GenConfig& config, GenStatus& status )
{
    ConjSpec spec = { &Compiler::GenerateOrClause, &Compiler::GenerateAndClause };
    GenerateConj( &spec, binary, config );
    status.kind = ExprKind::Logical;

    if ( config.discard )
        status.discarded = true;
}

void Compiler::GenerateConj( ConjSpec* spec, BinaryExpr* binary, const GenConfig& config )
{
    if ( config.trueChain == nullptr )
    {
        Atomize( spec, binary, config.invert, config.discard );
        return;
    }

    if ( config.invert )
    {
        (this->*(spec->NegativeGenerator))( binary->Left.get(), config );
    }
    else
    {
        (this->*(spec->PositiveGenerator))( binary->Left.get(), config );
    }

    Generate( binary->Right.get(), config );
}

void Compiler::GenerateAndClause( Syntax* elem, const GenConfig& config )
{
    PatchChain  newTrueChain;
    Generate( elem, config.WithTrue( &newTrueChain ) );
    ElideTrue( &newTrueChain, config.falseChain );
    Patch( &newTrueChain );
}

void Compiler::GenerateOrClause( Syntax* elem, const GenConfig& config )
{
    PatchChain  newFalseChain;
    Generate( elem, config.WithFalse( &newFalseChain ) );
    ElideFalse( config.trueChain, &newFalseChain );
    Patch( &newFalseChain );
}

void Compiler::Atomize( ConjSpec* spec, BinaryExpr* binary, bool invert, bool discard )
{
    PatchChain  trueChain;
    PatchChain  falseChain;
    PatchChain  endChain;

    GenerateConj( spec, binary, GenConfig::Expr( &trueChain, &falseChain, invert ) );

    ElideTrue( &trueChain, &falseChain );

    Patch( &trueChain );

    if ( !discard )
    {
        EmitU8( OP_LDC_S, 1 );

        EmitBranch( OP_B, &endChain );
    }

    Patch( &falseChain );

    if ( !discard )
    {
        EmitU8( OP_LDC_S, 0 );

        Patch( &endChain );

        IncreaseExprDepth();
    }
}

uint8_t Compiler::InvertJump( uint8_t opCode )
{
    switch ( opCode )
    {
    case OP_BTRUE:  return OP_BFALSE;
    case OP_BFALSE: return OP_BTRUE;
    default:
        THROW_INTERNAL_ERROR( "" );
    }
}

void Compiler::PushPatch( PatchChain* chain )
{
    PushPatch( chain, static_cast<int32_t>(mCodeBin.size()) );
}

void Compiler::PushPatch( PatchChain* chain, int32_t patchLoc )
{
    PushBasicPatch( chain, patchLoc );
}

template <typename TRef>
void Compiler::PushBasicPatch( BasicPatchChain<TRef>* chain, TRef patchLoc )
{
    BasicInstPatch<TRef>* link = new BasicInstPatch<TRef>;
    link->Ref = patchLoc;
    link->Next = chain->First;
    chain->First = link;
}

void Compiler::PopPatch( PatchChain* chain )
{
    assert( chain->First != nullptr );

    auto link = chain->First;
    chain->First = chain->First->Next;
    delete link;
}

void Compiler::PushFuncPatch( const std::string& name, CodeRef codeRef )
{
    auto patchIt = mFuncPatchMap.find( name );
    if ( patchIt == mFuncPatchMap.end() )
    {
        auto result = mFuncPatchMap.insert( FuncPatchMap::value_type( { name, FuncPatchChain() } ) );
        patchIt = std::move( result.first );
    }

    PushBasicPatch( &patchIt->second, codeRef );

    if ( codeRef.Kind == CodeRefKind::Code )
    {
        AddrRef ref = { AddrRefKind::Inst };
        ref.InstIndexPtr = &patchIt->second.First->Ref.Location;
        mLocalAddrRefs.push_back( ref );
    }
}

void Compiler::ElideTrue( PatchChain* trueChain, PatchChain* falseChain )
{
    if (   trueChain->First  == nullptr
        || falseChain->First == nullptr )
        return;

    int32_t target = static_cast<int32_t>(mCodeBin.size());
    size_t diff = target - (trueChain->First->Ref + BranchInst::Size);

    if ( diff == BranchInst::Size
        && mCodeBin[mCodeBin.size() - BranchInst::Size] == OP_B
        && ((int32_t) mCodeBin.size() - BranchInst::Size) == falseChain->First->Ref
        )
    {
        falseChain->First->Ref = trueChain->First->Ref;

        mCodeBin[trueChain->First->Ref] = InvertJump( mCodeBin[trueChain->First->Ref] );

        // Remove the branch instruction.
        PopPatch( trueChain );

        DeleteCode( BranchInst::Size );
    }
}

void Compiler::ElideFalse( PatchChain* trueChain, PatchChain* falseChain )
{
    if ( falseChain->First == nullptr )
        return;

    int32_t target = static_cast<int32_t>(mCodeBin.size());
    size_t diff = target - (falseChain->First->Ref + BranchInst::Size);

    if ( diff == 0 )
    {
        // Remove the branch instruction.
        PopPatch( falseChain );

        DeleteCode( BranchInst::Size );
    }
}

void Compiler::Patch( PatchChain* chain, int32_t targetIndex )
{
    int32_t target = (targetIndex >= 0) ? targetIndex : static_cast<int32_t>(mCodeBin.size());

    for ( InstPatch* link = chain->First; link != nullptr; link = link->Next )
    {
        ptrdiff_t diff = target - (link->Ref + BranchInst::Size);

        if ( diff < BranchInst::OffsetMin || diff > BranchInst::OffsetMax )
            mRep.ThrowSemanticsError( nullptr, "Branch target is too far." );

        BranchInst::StoreOffset( &mCodeBin[link->Ref + 1], static_cast<BranchInst::TOffset>(diff) );
    }

    chain->PatchedInstIndex = target;
}

void Compiler::PatchCalls( FuncPatchChain* chain, uint32_t addr )
{
    for ( FuncInstPatch* link = chain->First; link != nullptr; link = link->Next )
    {
        void* refPtr = nullptr;

        switch ( link->Ref.Kind )
        {
        case CodeRefKind::Code:  refPtr = &mCodeBin[link->Ref.Location]; break;
        case CodeRefKind::Data:  refPtr = &mGlobals[link->Ref.Location]; break;
        case CodeRefKind::Const: refPtr = &mConsts[link->Ref.Location]; break;
        default:
            THROW_INTERNAL_ERROR( "" );
        }

        StoreU24( (uint8_t*) refPtr, addr );
    }
}

void Compiler::GenerateUnaryPrimitive( Syntax* elem, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        Generate( elem, config, status );
    }
    else
    {
        EmitU8( OP_LDC_S, 0 );

        Generate( elem );

        EmitU8( OP_PRIM, PRIM_SUB );

        IncreaseExprDepth();
        DecreaseExprDepth();
    }
}

void Compiler::GenerateBinaryPrimitive( BinaryExpr* binary, uint8_t primitive, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        GenerateDiscard( binary->Left.get() );
        GenerateDiscard( binary->Right.get() );
        status.discarded = true;
    }
    else
    {
        Generate( binary->Left.get() );
        Generate( binary->Right.get() );

        EmitU8( OP_PRIM, primitive );

        DecreaseExprDepth();
    }
}

void Compiler::EmitLoadAddress( Syntax* node, Declaration* baseDecl, int32_t offset )
{
    if ( baseDecl == nullptr )
    {
        THROW_INTERNAL_ERROR( "Missing declaration" );
    }
    else
    {
        uint32_t addrWord;

        switch ( baseDecl->Kind )
        {
        case DeclKind::Global:
            assert( offset >= 0 && offset < GlobalSizeMax );
            assert( offset < (GlobalSizeMax - ((GlobalStorage*) baseDecl)->Offset) );

            addrWord = CodeAddr::Build(
                ((GlobalStorage*) baseDecl)->Offset + offset,
                ((GlobalStorage*) baseDecl)->ModIndex );
            EmitU32( OP_LDC, addrWord );
            IncreaseExprDepth();
            break;

        case DeclKind::Local:
            assert( offset >= 0 && offset < LocalSizeMax );
            assert( offset <= ((LocalStorage*) baseDecl)->Offset );

            EmitU8( OP_LDLOCA, static_cast<uint8_t>(((LocalStorage*) baseDecl)->Offset - offset) );
            IncreaseExprDepth();
            break;

        case DeclKind::LoadedAddress:
            assert( offset >= 0 && offset < DataSizeMax );

            if ( offset > 0 )
                EmitSpilledAddrOffset( offset );
            break;

        case DeclKind::Param:
            {
                auto param = (ParamStorage*) baseDecl;

                if ( IsOpenArrayType( *param->Type ) )
                {
                    EmitU8( OP_LDARG, static_cast<uint8_t>(param->Offset + 1) );
                    EmitU8( OP_LDARG, static_cast<uint8_t>(param->Offset + 0) );
                    IncreaseExprDepth( 2 );
                }
                else if ( param->Mode == ParamMode::Value )
                {
                    assert( offset >= 0 && offset < ParamSizeMax );
                    assert( offset < (ParamSizeMax - param->Offset) );

                    EmitU8( OP_LDARGA, static_cast<uint8_t>(param->Offset + offset) );
                    IncreaseExprDepth();
                }
                else if ( param->Mode == ParamMode::RefInOut
                    || param->Mode == ParamMode::RefIn )
                {
                    EmitU8( OP_LDARG, param->Offset );

                    if ( offset != 0 )
                        EmitSpilledAddrOffset( offset );

                    IncreaseExprDepth();
                }
                else
                {
                    THROW_INTERNAL_ERROR( "EmitLoadAddress: Bad parameter mode" );
                }
            }
            break;

        case DeclKind::Const:
            {
                auto    constant = (Constant*) baseDecl;
                ModSize modIndex = constant->ModIndex | CONST_SECTION_MOD_INDEX_MASK;

                assert( offset >= 0 && offset < GlobalSizeMax );
                assert( offset < (GlobalSizeMax - constant->Offset) );

                if ( !constant->Serialized )
                    SerializeConstant( constant );

                addrWord = CodeAddr::Build(
                    constant->Offset + offset,
                    modIndex );
                EmitU32( OP_LDC, addrWord );
                IncreaseExprDepth();
            }
            break;

        default:
            mRep.ThrowSemanticsError( node, "Can't take address of declaration" );
        }
    }
}

void Compiler::GenerateArefAddrBase( Syntax* fullExpr, Syntax* head, Syntax* index, const GenConfig& config, GenStatus& status )
{
    assert( config.calcAddr );

    auto& arrayType = (ArrayType&) *head->Type;

    Generate( head, config, status );

    if ( arrayType.Count > 0 )
    {
        std::optional<int32_t> optIndexVal;

        optIndexVal = GetFinalOptionalSyntaxValue( index );

        if ( optIndexVal.has_value() )
        {
            assert( optIndexVal.value() < DataSizeMax );

            status.offset += static_cast<DataSize>(optIndexVal.value()) * arrayType.ElemType->GetSize();
            return;
        }
    }
    // Else, open arrays need to check the index

    if ( !status.spilledAddr )
    {
        EmitLoadAddress( fullExpr, status.baseDecl, status.offset );

        // Set this after emitting the original decl's address above
        if ( status.baseDecl->IsReadOnly )
            status.baseDecl = mLoadedAddrDeclConst.get();
        else
            status.baseDecl = mLoadedAddrDecl.get();

        status.offset = 0;
        status.spilledAddr = true;
    }

    if ( status.offset > 0 )
    {
        EmitSpilledAddrOffset( status.offset );

        status.offset = 0;
    }

    // A slice of [..] only passes thru to the head of the expression

    if ( fullExpr->Kind == SyntaxKind::Slice )
    {
        auto firstVal = GetFinalOptionalSyntaxValue( index );
        auto lastVal = GetFinalOptionalSyntaxValue( ((SliceExpr*) fullExpr)->LastIndex.get() );

        if ( firstVal.has_value() && firstVal == 0
            && lastVal.has_value() && lastVal == -1 )
        {
            return;
        }
    }

    Generate( index );

    if ( arrayType.Count == 0 )
    {
        if ( fullExpr->Kind == SyntaxKind::Index )
        {
            EmitU24( OP_INDEXOPEN, arrayType.ElemType->GetSize() );
            DecreaseExprDepth();
        }
        else
        {
            Generate( ((SliceExpr*) fullExpr)->LastIndex.get() );

            auto firstVal = GetFinalOptionalSyntaxValue( index );
            auto lastVal = GetFinalOptionalSyntaxValue( ((SliceExpr*) fullExpr)->LastIndex.get() );

            if ( firstVal.has_value() && lastVal.has_value() && lastVal != -1 )
            {
                EmitU24( OP_RANGEOPENCLOSED, arrayType.ElemType->GetSize() );
                DecreaseExprDepth( 2 );
            }
            else
            {
                EmitU24( OP_RANGEOPEN, arrayType.ElemType->GetSize() );
                DecreaseExprDepth( 1 );
            }
        }
    }
    else
    {
        if ( fullExpr->Kind == SyntaxKind::Index )
        {
            EmitOpenIndex( OP_INDEX, arrayType.ElemType->GetSize(), arrayType.Count );
        }
        else
        {
            Generate( ((SliceExpr*) fullExpr)->LastIndex.get() );

            EmitOpenIndex( OP_RANGE, arrayType.ElemType->GetSize(), arrayType.Count );
        }
    }

    DecreaseExprDepth();
}

void Compiler::GenerateArefAddr( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status )
{
    GenerateArefAddrBase( indexExpr, indexExpr->Head.get(), indexExpr->Index.get(), config, status );
}

void Compiler::GenerateAref( IndexExpr* indexExpr, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        GenerateArefAddr( indexExpr, config, status );
        return;
    }

    auto& arrayType = (ArrayType&) *indexExpr->Head->Type;
    auto& elemType = *arrayType.ElemType;

    EmitCopyPartOfAggregate( indexExpr, &elemType );
}

void Compiler::EmitCopyPartOfAggregate( Syntax* partNode, Type* partType )
{
    // Calculate address in each clause below instead of once here,
    // because it might spill. And in the case of array, the spilled address
    // must be right after the size.

    if ( IsScalarType( partType->GetKind() ) )
    {
        auto addr = CalcAddress( partNode );

        EmitLoadScalar( partNode, addr.decl, addr.offset );
    }
    else
    {
        auto addr = CalcAddress( partNode );

        EmitLoadAddress( partNode, addr.decl, addr.offset );
    }
}

void Compiler::VisitIndexExpr( IndexExpr* indexExpr )
{
    GenerateAref( indexExpr, Config(), Status() );
}

Compiler::CalculatedAddress Compiler::CalcAddress( Syntax* expr, bool writable )
{
    GenConfig config{};
    GenStatus status{};

    config.calcAddr = true;

    Generate( expr, config, status );

    if ( status.baseDecl == nullptr )
        mRep.ThrowSemanticsError( expr, "Expression has no address" );

    if ( writable && status.baseDecl->IsReadOnly )
        mRep.ThrowSemanticsError( expr, "Constants cannot be changed" );

    CalculatedAddress addr{};

    addr.decl = status.baseDecl;
    addr.offset = status.offset;
    addr.spilled = status.spilledAddr;

    return addr;
}

void Compiler::VisitSliceExpr( SliceExpr* sliceExpr )
{
    const GenConfig& config = Config();
    GenStatus& status = Status();

    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        GenerateArefAddrBase( sliceExpr, sliceExpr->Head.get(), sliceExpr->FirstIndex.get(), config, status );
        return;
    }

    auto addr = CalcAddress( sliceExpr );

    EmitLoadAddress( sliceExpr, addr.decl, addr.offset );
}

void Compiler::GenerateFieldAccess( DotExpr* dotExpr, const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
        return;
    }
    else if ( config.calcAddr )
    {
        Generate( dotExpr->Head.get(), config, status );

        auto field = (FieldStorage*) dotExpr->GetDecl();

        status.offset += field->Offset;
        return;
    }

    auto    field = (FieldStorage*) dotExpr->GetDecl();
    auto&   fieldType = *field->GetType();

    EmitCopyPartOfAggregate( dotExpr, &fieldType );
}

void Compiler::VisitDotExpr( DotExpr* dotExpr )
{
    auto decl = dotExpr->GetDecl();

    if ( decl->Kind == DeclKind::Field )
    {
        GenerateFieldAccess( dotExpr, Config(), Status() );
    }
    else
    {
        GenerateValue( dotExpr, decl, Config(), Status() );
    }
}

void Compiler::GenerateDefvar( VarDecl* varDecl, const GenConfig& config, GenStatus& status )
{
    auto global = (GlobalStorage*) varDecl->GetDecl();

    mGlobalDataGenerator.GenerateGlobalInit( global->Offset, varDecl->Initializer.get() );
}

void Compiler::VisitVarDecl( VarDecl* varDecl )
{
    GenerateDefvar( varDecl, Config(), Status() );
}

void Compiler::EmitGlobalFuncAddress( std::optional<std::shared_ptr<Function>> optFunc, GlobalSize offset, int32_t* buffer, Syntax* initializer )
{
    if ( optFunc.has_value() )
    {
        EmitFuncAddress( optFunc.value().get(), { CodeRefKind::Data, offset } );
    }
    else
    {
        // We don't need to check if it's writable, because we'll explicitly check that it's a global
        auto addr = CalcAddress( initializer );

        if ( addr.decl->Kind != DeclKind::Global )
            mRep.ThrowSemanticsError( initializer, "Const or global expected" );

        auto       global    = (GlobalStorage*) addr.decl;
        GlobalSize srcOffset = global->Offset + addr.offset;

        PushDeferredGlobal( *initializer->Type, ModuleSection::Data, global->ModIndex, srcOffset, offset );
    }
}

void Compiler::PushDeferredGlobal( Type& type, ModuleSection srcSection, ModSize srcModIndex, GlobalSize srcOffset, GlobalSize dstOffset )
{
    MemTransfer  transfer;

    transfer.SrcSection = srcSection;
    transfer.SrcModIndex = srcModIndex;

    transfer.Src = srcOffset;
    transfer.Dst = dstOffset;
    transfer.Size = type.GetSize();

    mDeferredGlobals.push_back( transfer );
}

void Compiler::CopyGlobalAggregateBlock( GlobalSize offset, Syntax* valueNode )
{
    // Defer these globals until all function addresses are known and put in source blocks

    auto srcAddr = CalcAddress( valueNode );

    if ( srcAddr.decl->Kind == DeclKind::Global )
    {
        auto       global    = (GlobalStorage*) srcAddr.decl;
        GlobalSize srcOffset = global->Offset + srcAddr.offset;

        PushDeferredGlobal( *valueNode->Type, ModuleSection::Data, global->ModIndex, srcOffset, offset );
    }
    else if ( srcAddr.decl->Kind == DeclKind::Const )
    {
        assert( ((Constant*) srcAddr.decl)->Serialized );
        assert( ((Constant*) srcAddr.decl)->Value.Is( ValueKind::Aggregate ) );

        auto       constant  = (Constant*) srcAddr.decl;
        GlobalSize srcOffset = constant->Offset + srcAddr.offset;

        PushDeferredGlobal( *valueNode->Type, ModuleSection::Const, constant->ModIndex, srcOffset, offset );
    }
    else
    {
        THROW_INTERNAL_ERROR( "CopyGlobalAggregateBlock: DeclKind" );
    }
}

void Compiler::VisitConstDecl( ConstDecl* constDecl )
{
    // Constants inside functions are serialized on demand
    if ( mInFunc )
        return;

    // All global constants except scalars are serialized, because they're public,
    // and can be accessed from any module.

    // Scalar constants are skipped, because their addresses are never needed:
    // Scalar parameters with RefIn mode are changed to ValueIn mode.

    if ( IsSerializableConstType( *constDecl->Decl->GetType() ) )
        SerializeConstant( (Constant*) constDecl->GetDecl() );
}

void Compiler::SerializeConstant( Constant* constant )
{
    // Constants in other modules would have been serialized already
    assert( constant->ModIndex == mModIndex );
    assert( !constant->Serialized );

    auto type = constant->GetType();

    assert( type->GetSize() <= (mConsts.size() - mTotalConst) );

    constant->Serialized = true;
    constant->Offset = mTotalConst;

    // Scalar constants are not serialized

    if ( constant->Value.Is( ValueKind::Aggregate ) )
    {
        auto& aggregate = constant->Value.GetAggregate();

        SerializeConstPart( type.get(), aggregate.Module->GetConsts(), aggregate.Offset, mConsts, mTotalConst );
    }
    else
    {
        THROW_INTERNAL_ERROR( "SerializeConstant: ValueKind" );
    }

    mTotalConst += type->GetSize();
}

void Compiler::SerializeConstPart( Type* type, GlobalVec& srcBuffer, GlobalSize srcOffset, GlobalVec& dstBuffer, GlobalSize dstOffset )
{
    if ( IsIntegralType( type->GetKind() ) )
    {
        dstBuffer[dstOffset] = srcBuffer[srcOffset];
    }
    else if ( IsPtrFuncType( *type ) )
    {
        auto func = mGlobalAttrs.GetFunction( srcBuffer[srcOffset] );

        EmitFuncAddress( func.get(), { CodeRefKind::Const, dstOffset }  );
    }
    else if ( type->GetKind() == TypeKind::Array )
    {
        auto arrayType = (ArrayType*) type;
        auto elemType = arrayType->ElemType.get();

        for ( GlobalSize i = 0; i < arrayType->Count; i++ )
        {
            SerializeConstPart( elemType, srcBuffer, srcOffset, dstBuffer, dstOffset );

            srcOffset += elemType->GetSize();
            dstOffset += elemType->GetSize();
        }
    }
    else if ( type->GetKind() == TypeKind::Record )
    {
        auto recordType = (RecordType*) type;

        for ( auto& f : recordType->GetOrderedFields() )
        {
            SerializeConstPart( f->GetType().get(), srcBuffer, srcOffset + f->Offset, dstBuffer, dstOffset + f->Offset );
        }
    }
    else
    {
        THROW_INTERNAL_ERROR( "SerializeConstPart: TypeKind" );
    }
}

void Compiler::GenerateProc( ProcDecl* procDecl, Function* func )
{
    mInFunc = true;
    mCurFunc = func;

    constexpr uint8_t PushInstSize = 2;

    int32_t bodyLoc = static_cast<int32_t>(mCodeBin.size());

    // Assume that there are local variables
    EmitU8( OP_PUSH, 0 );

    mCurExprDepth = 0;
    mMaxExprDepth = 0;
    mLocalAddrRefs.clear();

    GenConfig config = GenConfig::Statement();
    GenStatus status = { ExprKind::Other };

    mGenStack.push_back( { config, status } );

    procDecl->Body.Accept( this );

    mGenStack.pop_back();

    assert( mCurExprDepth == 1 );

    if ( !status.tailRet )
    {
        Emit( OP_RET );
    }

    if ( func->LocalCount > 0 )
    {
        size_t index = bodyLoc + 1;
        mCodeBin[index] = (uint8_t) func->LocalCount;
    }
    else
    {
        // No locals. So, delete the PUSH instruction
        DeleteCode( bodyLoc, PushInstSize );

        // If local lambda references were generated, then shift them
        // This also includes references to any function

        for ( const auto& ref : mLocalAddrRefs )
        {
            int32_t* instIndexPtr = nullptr;

            switch ( ref.Kind )
            {
            case AddrRefKind::Inst:
                instIndexPtr = ref.InstIndexPtr;
                break;

            default:
                THROW_INTERNAL_ERROR( "" );
            }

            *instIndexPtr -= PushInstSize;
        }
    }

    func->ExprDepth = mMaxExprDepth;

    mCurFunc = nullptr;
    mInFunc = false;
}

void Compiler::GenerateImplicitProgn( StatementList* stmtList, const GenConfig& config, GenStatus& status )
{
    for ( int i = 0; i < (int) stmtList->Statements.size() - 1; i++ )
    {
        GenerateDiscard( stmtList->Statements[i].get() );
    }

    if ( stmtList->Statements.size() >= 1 )
    {
        Generate( stmtList->Statements.back().get(), config, status );
    }
    else    // There are no expressions
    {
        GenerateNilIfNeeded( config, status );
    }
}

void Compiler::VisitStatementList( StatementList* stmtList )
{
    GenerateImplicitProgn( stmtList, Config(), Status() );
}

void Compiler::GenerateStatements( StatementList* list, const GenConfig& config, GenStatus& status )
{
    for ( auto& node : list->Statements )
    {
        GenerateDiscard( node.get(), config );
    }
}

void Compiler::GenerateNilIfNeeded( const GenConfig& config, GenStatus& status )
{
    if ( config.discard )
    {
        status.discarded = true;
    }
    else
    {
        EmitU8( OP_LDC_S, 0 );

        IncreaseExprDepth();
    }
}

const Compiler::GenConfig& Compiler::Config() const
{
    assert( mGenStack.size() >= 1 );
    return mGenStack.back().config;
}

Compiler::GenStatus& Compiler::Status()
{
    assert( mGenStack.size() >= 1 );
    return mGenStack.back().status;
}

void Compiler::GenerateSentinel()
{
    constexpr auto ALIGN = MODULE_CODE_ALIGNMENT;

    size_t alignedSize = ((mCodeBin.size() + SENTINEL_SIZE + (ALIGN - 1)) / ALIGN) * ALIGN;
    size_t paddingSize = alignedSize - mCodeBin.size();
    size_t curIndex = ReserveProgram( paddingSize );

    for ( size_t i = 0; i < paddingSize; i++ )
    {
        mCodeBin[curIndex++] = OP_SENTINEL;
    }
}

void Compiler::FinalizeConstData()
{
    mConsts.resize( mTotalConst );

    // Replace original constants with final values and layout

    mModuleAttrs->GetConsts() = mConsts;

    // Point references to their values in the final layout

    for ( auto& [name, decl] : mPublicTable )
    {
        if ( decl->Kind == DeclKind::Const )
        {
            auto& constant = (Constant&) *decl;

            if ( constant.Value.Is( ValueKind::Aggregate ) )
            {
                assert( constant.Serialized );

                constant.Value.GetAggregate().Offset = constant.Offset;
            }
        }
    }
}

int32_t Compiler::GetSyntaxValue( Syntax* node, const char* message )
{
    auto optValue = GetFinalOptionalSyntaxValue( node );

    if ( optValue.has_value() )
        return optValue.value();

    if ( message != nullptr )
        mRep.ThrowSemanticsError( node, message );
    else
        mRep.ThrowSemanticsError( node, "Expected a constant value" );
}

std::optional<int32_t> Compiler::GetFinalOptionalSyntaxValue( Syntax* node )
{
#if defined( GEMINIVM_DISABLE_FOLDING_PASS )
    if ( node->Kind != SyntaxKind::Number )
    {
        FolderVisitor folder( mRep.GetLog(), mConstIndexFuncMap );

        return folder.EvaluateInt( node );
    }
#endif

    return Gemini::GetFinalOptionalSyntaxValue( node );
}

void Compiler::IncreaseExprDepth( LocalSize amount )
{
    if ( amount > (LocalSizeMax - mCurExprDepth) )
        mRep.ThrowSemanticsError( NULL, "Expression is too deep" );

    mCurExprDepth += amount;

    if ( mMaxExprDepth < mCurExprDepth )
        mMaxExprDepth = mCurExprDepth;
}

void Compiler::DecreaseExprDepth( LocalSize amount )
{
    assert( amount >= 0 );
    assert( amount <= mCurExprDepth );

    mCurExprDepth -= amount;
}

void Compiler::CopyDeferredGlobals()
{
    for ( const auto& transfer : mDeferredGlobals )
    {
        int32_t* pSrc = nullptr;

        switch ( transfer.SrcSection )
        {
        case ModuleSection::Data:
            pSrc = &mGlobals[transfer.Src];
            break;

        case ModuleSection::Const:
            if ( transfer.SrcModIndex == mModIndex )
                pSrc = &mConsts[transfer.Src];
            else
                pSrc = &mGlobalAttrs.GetModule( transfer.SrcModIndex )->GetConsts()[transfer.Src];
            break;

        default:
            THROW_INTERNAL_ERROR( "CopyDeferredGlobals: ModuleSection" );
        }

        std::copy_n( pSrc, transfer.Size, &mGlobals[transfer.Dst] );
    }
}

void Compiler::CalculateStackDepth()
{
    mStats.Lambda = {};
    mStats.Static = {};
    mStats.CallsIndirectly = false;

    for ( const auto& [name, decl] : mGlobalTable )
    {
        if ( decl->Kind == DeclKind::Func )
        {
            Function*  func = (Function*) decl.get();
            CallStats* callStats;

            CalculateStackDepth( func );

            if ( func->IsLambda )
            {
                callStats = &mStats.Lambda;
            }
            else
            {
                callStats = &mStats.Static;

                if ( func->CallsIndirectly )
                    mStats.CallsIndirectly = true;
            }

            // Only count parameters of publics, if that's ever a distinction

            uint32_t stackUsage = func->TreeStackUsage + func->ParamCount;

            callStats->MaxCallDepth  = std::max( callStats->MaxCallDepth,  func->CallDepth );
            callStats->MaxStackUsage = std::max( callStats->MaxStackUsage, stackUsage );

            if ( func->IsRecursive )
                callStats->Recurses = true;
        }
    }
}

void Compiler::CalculateStackDepth( Function* func )
{
    if ( func->IsDepthKnown )
        return;

    if ( func->IsCalculating )
    {
        func->IsRecursive = true;
        return;
    }

    func->IsCalculating = true;
    func->IndividualStackUsage = FRAME_WORDS + func->LocalCount;

    uint32_t maxChildDepth = 0;
    uint32_t maxChildStackUsage = func->ExprDepth;

    for ( const auto& site : func->CalledFunctions )
    {
        SymTable* table = nullptr;

        if ( site.ModIndex == mModIndex )
        {
            table = &mGlobalTable;
        }
        else
        {
            if ( auto modIt = mModulesById.find( site.ModIndex );
                modIt != mModulesById.end() )
            {
                table = &modIt->second->Table;
            }
        }

        if ( table == nullptr )
            continue;

        if ( auto it = table->find( site.FunctionName );
            it != table->end() )
        {
            if ( it->second->Kind != DeclKind::Func )
                continue;

            auto childFunc = (Function*) it->second.get();

            CalculateStackDepth( childFunc );

            uint32_t childStackUsage = childFunc->TreeStackUsage + site.ExprDepth;

            maxChildDepth = std::max( maxChildDepth, childFunc->CallDepth );
            maxChildStackUsage = std::max( maxChildStackUsage, childStackUsage );

            if ( childFunc->CallsIndirectly )
                func->CallsIndirectly = true;

            if ( childFunc->IsRecursive )
            {
                func->IsRecursive = true;
                break;
            }
        }
    }

    func->IsCalculating = false;
    func->IsDepthKnown = true;
    func->CallDepth = 1 + maxChildDepth;
    func->TreeStackUsage = func->IndividualStackUsage + maxChildStackUsage;
}

size_t Compiler::ReserveProgram( size_t size )
{
    if ( size > (CodeSizeMax - mCodeBin.size()) )
        mRep.ThrowSemanticsError( NULL, "Generated code is too big. Max=%u", CodeSizeMax );

    size_t curIndex = mCodeBin.size();
    mCodeBin.resize( mCodeBin.size() + size );

    return curIndex;
}

size_t Compiler::ReserveCode( size_t size )
{
    assert( mInFunc );
    return ReserveProgram( size );
}

void Compiler::DeleteCode( size_t size )
{
    assert( mCodeBin.size() >= size );

    mCodeBin.resize( mCodeBin.size() - size );
}

void Compiler::DeleteCode( size_t start, size_t size )
{
    assert( start < mCodeBin.size() && size <= (mCodeBin.size() - start) );

    mCodeBin.erase( mCodeBin.begin() + start, mCodeBin.begin() + start + size );
}

void Compiler::EmitBranch( OpCode opcode, PatchChain* chain )
{
    PushPatch( chain );

    size_t curIndex = ReserveCode( BranchInst::Size );

    mCodeBin[curIndex] = opcode;

    DISASSEMBLE( &mCodeBin[curIndex], BranchInst::Size );
}

void Compiler::Emit( OpCode opcode )
{
    size_t curIndex = ReserveCode( 1 );

    mCodeBin[curIndex] = opcode;

    DISASSEMBLE( &mCodeBin[curIndex], 1 );
}

void Compiler::EmitU8( OpCode opcode, uint8_t operand )
{
    size_t curIndex = ReserveCode( 2 );

    mCodeBin[curIndex] = opcode;
    mCodeBin[curIndex + 1] = operand;

    DISASSEMBLE( &mCodeBin[curIndex], 2 );
}

void Compiler::EmitU16( OpCode opcode, uint16_t operand )
{
    size_t curIndex = ReserveCode( 3 );

    mCodeBin[curIndex] = opcode;

    StoreU16( &mCodeBin.at( curIndex + 1 ), operand );

    DISASSEMBLE( &mCodeBin[curIndex], 3 );
}

void Compiler::EmitU24( OpCode opcode, uint32_t operand )
{
    size_t curIndex = ReserveCode( 4 );

    mCodeBin[curIndex] = opcode;

    StoreU24( &mCodeBin.at( curIndex + 1 ), operand );

    DISASSEMBLE( &mCodeBin[curIndex], 4 );
}

void Compiler::EmitU32( OpCode opcode, uint32_t operand )
{
    size_t curIndex = ReserveCode( 5 );

    mCodeBin[curIndex] = opcode;

    StoreU32( &mCodeBin.at( curIndex + 1 ), operand );

    DISASSEMBLE( &mCodeBin[curIndex], 5 );
}

void Compiler::EmitOpenIndex( OpCode opcode, uint32_t stride, uint32_t bound )
{
    size_t curIndex = ReserveCode( 7 );

    mCodeBin[curIndex] = opcode;

    StoreU24( &mCodeBin.at( curIndex + 1 ), stride );
    StoreU24( &mCodeBin.at( curIndex + 4 ), bound );

    DISASSEMBLE( &mCodeBin[curIndex], 7 );
}

void Compiler::EmitModAccess( OpCode opcode, uint8_t mod, uint16_t addr )
{
    size_t curIndex = ReserveCode( 4 );

    mCodeBin[curIndex+0] = opcode;
    mCodeBin[curIndex+1] = mod;

    StoreU16( &mCodeBin[curIndex + 2], addr );

    DISASSEMBLE( &mCodeBin[curIndex], 4 );
}


//----------------------------------------------------------------------------
//  Disassembly
//----------------------------------------------------------------------------

#if ENABLE_DISASSEMBLY

void Disassemble( const uint8_t* program, int size )
{
    Disassembler disassembler( program );
    int totalBytesDisasm = 0;
    while ( totalBytesDisasm < size )
    {
        char disasm[128];
        int bytesDisasm = disassembler.Disassemble( disasm, std::size( disasm ) );
        if ( bytesDisasm <= 0 )
            break;
        totalBytesDisasm += bytesDisasm;
        printf( "%s\n", disasm );
    }
}

#endif

}
