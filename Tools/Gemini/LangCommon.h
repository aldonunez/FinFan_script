// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "Common.h"
#include <exception>
#include <functional>
#include <map>
#include <memory>
#include <optional>


namespace Gemini
{

class Syntax;


enum class CompilerErr
{
    NONE,
    OK,
    INTERNAL,
    UNSUPPORTED,
    SYNTAX,
    SEMANTICS,
};


enum class LogCategory
{
    ERROR,
    WARNING,
};


class ICompilerLog
{
public:
    virtual void Add( LogCategory category, const char* fileName, int line, int column, const char* message ) = 0;
};


class Reporter
{
    ICompilerLog* mLog;

public:
    Reporter( ICompilerLog* log );

    ICompilerLog* GetLog();

    void Log( LogCategory category, const char* fileName, int line, int col, const char* format, va_list args );
    void LogWarning( const char* fileName, int line, int col, const char* format, ... );

    [[noreturn]] void ThrowError( CompilerErr exceptionCode, Syntax* elem, const char* format, va_list args );
    [[noreturn]] void ThrowError( CompilerErr exceptionCode, const char* fileName, int line, int col, const char* format, va_list args );
    [[noreturn]] void ThrowSemanticsError( Syntax* node, const char* format, ... );
    [[noreturn]] void ThrowInternalError( const char* fileName, int line, const char* format, ... );
};


#define THROW_INTERNAL_ERROR( ... ) \
    do { assert( false ); mRep.ThrowInternalError( __FILE__, __LINE__, __VA_ARGS__ ); } while ( 0 )


class CompilerException : public std::exception
{
    CompilerErr     mError;

public:
    CompilerException( CompilerErr error )
        : mError( error )
    {
    }

    CompilerErr GetError() const
    {
        return mError;
    }
};


struct Function;
class ModuleAttrs;


class CompilerAttrs
{
    using ConstFuncIndexMap = std::map<Function*, int32_t>;
    using ConstIndexFuncMap = std::map<int32_t, std::shared_ptr<Function>>;
    using AddressFuncMap    = std::map<uint32_t, std::shared_ptr<Function>>;
    using ModuleVec         = std::vector<std::shared_ptr<ModuleAttrs>>;

    ConstFuncIndexMap   mConstFuncIndexMap;
    ConstIndexFuncMap   mConstIndexFuncMap;
    AddressFuncMap      mAddressFuncMap;
    ModuleVec           mModules;

public:
    int32_t AddFunctionByIndex( std::shared_ptr<Function> func );
    void AddFunctionByAddress( std::shared_ptr<Function> func );
    std::shared_ptr<Function> GetFunction( int32_t id ) const;

    void AddModule( std::shared_ptr<ModuleAttrs> module );
    std::shared_ptr<ModuleAttrs> GetModule( int32_t index ) const;
};


class ModuleAttrs
{
    using ConstVec = std::vector<int32_t>;

    ModSize             mModIndex;
    CompilerAttrs&      mGlobalAttrs;
    ConstVec            mConsts;

public:
    ModuleAttrs( ModSize modIndex, CompilerAttrs& globalAttrs );

    ModSize GetIndex();
    CompilerAttrs& GetGlobalAttrs();

    std::vector<int32_t>& GetConsts();
    GlobalSize GrowConsts( GlobalSize amount );
};


class InitList;
class RecordInitializer;

class GlobalDataGenerator
{
public:
    using EmitFuncAddressFunctor = std::function<void( std::optional<std::shared_ptr<Function>>, GlobalSize, int32_t*, Syntax* )>;
    using CopyAggregateFunctor = std::function<void( GlobalSize, int32_t*, Syntax* )>;

private:
    std::vector<int32_t>&   mGlobals;
    EmitFuncAddressFunctor  mEmitFuncAddressFunctor;
    CopyAggregateFunctor    mCopyAggregateFunctor;
    ModuleAttrs&            mModuleAttrs;
    Reporter&               mRep;

public:
    GlobalDataGenerator(
        std::vector<int32_t>& globals,
        EmitFuncAddressFunctor emitFuncAddressFunctor,
        CopyAggregateFunctor copyAggregateFunctor,
        ModuleAttrs& moduleAttrs,
        Reporter& reporter );

    void GenerateGlobalInit( GlobalSize offset, Syntax* initializer );

private:
    void EmitGlobalScalar( GlobalSize offset, Syntax* valueElem );
    void EmitGlobalArrayInitializer( GlobalSize offset, InitList* initList, size_t size );
    void EmitGlobalRecordInitializer( GlobalSize offset, RecordInitializer* recordInit );
};


enum class TypeKind;
class Type;

bool IsScalarType( TypeKind kind );
bool IsIntegralType( TypeKind kind );
bool IsClosedArrayType( Type& type );
bool IsOpenArrayType( Type& type );
bool IsPtrFuncType( Type& type );
bool IsSerializableConstType( Type& type );

}
