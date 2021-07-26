// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "pch.h"
#include "FolderVisitor.h"
#include "LangCommon.h"
#include "Syntax.h"
#include "VmCommon.h"
#include <stdarg.h>


namespace Gemini
{

//----------------------------------------------------------------------------
//  Log
//----------------------------------------------------------------------------

void Log( ICompilerLog* log, LogCategory category, const char* fileName, int line, int col, const char* format, va_list args )
{
    if ( log != nullptr )
    {
        char msg[256] = "";
        vsnprintf( msg, sizeof msg, format, args );
        log->Add( category, fileName, line, col, msg );
    }
}


Reporter::Reporter( ICompilerLog* log ) :
    mLog( log )
{
    assert( log != nullptr );
}

ICompilerLog* Reporter::GetLog()
{
    return mLog;
}

void Reporter::ThrowError( CompilerErr exceptionCode, Syntax* elem, const char* format, va_list args )
{
    const char* fileName = nullptr;
    int line = 0;
    int column = 0;
    if ( elem != nullptr )
    {
        fileName = elem->FileName;
        line = elem->Line;
        column = elem->Column;
    }
    ThrowError( exceptionCode, fileName, line, column, format, args );
}

void Reporter::ThrowError( CompilerErr exceptionCode, const char* fileName, int line, int col, const char* format, va_list args )
{
    Log( LogCategory::ERROR, fileName, line, col, format, args );
    throw CompilerException( exceptionCode );
}

void Reporter::ThrowSemanticsError( Syntax* node, const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CompilerErr::SEMANTICS, node, format, args );
    // No need to run va_end( args ), since an exception was thrown
}

void Reporter::ThrowInternalError( const char* fileName, int line, const char* format, ... )
{
    va_list args;
    va_start( args, format );
    ThrowError( CompilerErr::INTERNAL, fileName, line, 1, format, args );
    // No need to run va_end( args ), since an exception was thrown
}

void Reporter::Log( LogCategory category, const char* fileName, int line, int col, const char* format, va_list args )
{
    Gemini::Log( mLog, category, fileName, line, col, format, args );
}

void Reporter::LogWarning( const char* fileName, int line, int col, const char* format, ... )
{
    va_list args;
    va_start( args, format );
    Log( LogCategory::WARNING, fileName, line, col, format, args );
    va_end( args );
}


//----------------------------------------------------------------------------
//  CompilerAttrs
//----------------------------------------------------------------------------

int32_t CompilerAttrs::AddFunctionByIndex( std::shared_ptr<Function> func )
{
    auto funcIt = mConstFuncIndexMap.find( func.get() );
    int32_t index;

    if ( funcIt == mConstFuncIndexMap.end() )
    {
        index = -static_cast<int32_t>(mConstFuncIndexMap.size()) - 1;

        mConstFuncIndexMap.insert( ConstFuncIndexMap::value_type( func.get(), index ) );
        mConstIndexFuncMap.insert( ConstIndexFuncMap::value_type( index, func ) );
    }
    else
    {
        index = funcIt->second;
    }

    return index;
}

void CompilerAttrs::AddFunctionByAddress( std::shared_ptr<Function> func )
{
    uint32_t address = CodeAddr::Build( func->Address, func->ModIndex );

    mAddressFuncMap.insert( AddressFuncMap::value_type( address, func ) );
}

std::shared_ptr<Function> CompilerAttrs::GetFunction( int32_t id ) const
{
    if ( id < 0 )
        return mConstIndexFuncMap.find( id )->second;
    else
        return mAddressFuncMap.find( id )->second;
}

void CompilerAttrs::AddModule( std::shared_ptr<ModuleAttrs> module )
{
    assert( module->GetIndex() < ModSizeMax );

    if ( module->GetIndex() >= mModules.size() )
    {
        mModules.resize( module->GetIndex() + 1 );
    }

    mModules[module->GetIndex()] = module;
}

std::shared_ptr<ModuleAttrs> CompilerAttrs::GetModule( int32_t index ) const
{
    return mModules[index];
}


//----------------------------------------------------------------------------
//  ModuleAttrs
//----------------------------------------------------------------------------

ModuleAttrs::ModuleAttrs( ModSize modIndex, CompilerAttrs& globalAttrs ) :
    mModIndex( modIndex ),
    mGlobalAttrs( globalAttrs )
{
}

ModSize ModuleAttrs::GetIndex()
{
    return mModIndex;
}

CompilerAttrs& ModuleAttrs::GetGlobalAttrs()
{
    return mGlobalAttrs;
}

std::vector<int32_t>& ModuleAttrs::GetConsts()
{
    return mConsts;
}

GlobalSize ModuleAttrs::GrowConsts( GlobalSize amount )
{
    auto oldSize = mConsts.size();

    assert( amount <= (GlobalSizeMax - oldSize) );

    mConsts.resize( oldSize + amount );

    return static_cast<GlobalSize>(oldSize);
}


//----------------------------------------------------------------------------
//  GlobalDataGenerator
//----------------------------------------------------------------------------

GlobalDataGenerator::GlobalDataGenerator(
    std::vector<int32_t>& globals,
    EmitFuncAddressFunctor emitFuncAddressFunctor,
    CopyAggregateFunctor copyAggregateFunctor,
    ModuleAttrs& mModuleAttrs,
    Reporter& reporter )
    :
    mGlobals( globals ),
    mEmitFuncAddressFunctor( emitFuncAddressFunctor ),
    mCopyAggregateFunctor( copyAggregateFunctor ),
    mModuleAttrs( mModuleAttrs ),
    mRep( reporter )
{
}

void GlobalDataGenerator::GenerateGlobalInit( GlobalSize offset, Syntax* initializer )
{
    if ( initializer == nullptr )
        return;

    auto type = initializer->Type.get();

    if ( IsScalarType( type->GetKind() ) )
    {
        EmitGlobalScalar( offset, initializer );
    }
    else if ( initializer->Kind == SyntaxKind::ArrayInitializer )
    {
        auto arrayType = (ArrayType*) type;

        EmitGlobalArrayInitializer( offset, (InitList*) initializer, arrayType->Count );
    }
    else if ( initializer->Kind == SyntaxKind::RecordInitializer )
    {
        EmitGlobalRecordInitializer( offset, (RecordInitializer*) initializer );
    }
    else
    {
        mCopyAggregateFunctor( offset, mGlobals.data(), initializer );
    }
}

void GlobalDataGenerator::EmitGlobalScalar( GlobalSize offset, Syntax* valueElem )
{
    FolderVisitor folder( mRep.GetLog() );

    auto optVal = folder.Evaluate( valueElem );

    std::optional<std::shared_ptr<Function>> optFunc;

    if ( optVal.has_value() )
    {
        if ( optVal.value().Is( ValueKind::Integer ) )
        {
            mGlobals[offset] = optVal.value().GetInteger();
            return;
        }
        else
        {
            assert( optVal.value().Is( ValueKind::Function ) );

            optFunc = optVal.value().GetFunction();
        }
    }

    mEmitFuncAddressFunctor( optFunc, offset, mGlobals.data(), valueElem );
}

void GlobalDataGenerator::EmitGlobalArrayInitializer( GlobalSize offset, InitList* initList, size_t size )
{
    GlobalSize  i = 0;
    GlobalSize  globalIndex = offset;

    if ( initList->Values.size() > size )
        mRep.ThrowSemanticsError( initList, "Array has too many initializers" );

    for ( auto& entry : initList->Values )
    {
        GenerateGlobalInit( globalIndex, entry.get() );
        i++;
        globalIndex += entry->Type->GetSize();
    }

    if ( initList->Fill == ArrayFill::Extrapolate && i >= 1 )
    {
        // Use unsigned values for well defined overflow

        uint32_t prevValue = mGlobals[offset + i - 1];
        uint32_t step = 0;

        if ( i >= 2 )
        {
            step = VmSub( prevValue, mGlobals[offset + i - 2] );
        }

        for ( ; i < size; i++ )
        {
            uint32_t newValue = VmAdd( prevValue, step );

            mGlobals[offset + i] = newValue;
            prevValue = newValue;
        }
    }
    else if ( initList->Fill == ArrayFill::Repeat && i >= 1 )
    {
        Syntax* lastNode = initList->Values.back().get();

        for ( ; i < size; i++ )
        {
            GenerateGlobalInit( globalIndex, lastNode );
            globalIndex += lastNode->Type->GetSize();
        }
    }
}

void GlobalDataGenerator::EmitGlobalRecordInitializer( GlobalSize offset, RecordInitializer* recordInit )
{
    for ( auto& fieldInit : recordInit->Fields )
    {
        auto fieldDecl = (FieldStorage*) fieldInit->GetDecl();

        GenerateGlobalInit( offset + fieldDecl->Offset, fieldInit->Initializer.get() );
    }
}

}
