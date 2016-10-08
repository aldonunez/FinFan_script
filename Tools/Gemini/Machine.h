#pragma once

#include "GeminiCommon.h"


enum
{
    ERR_NONE,
    ERR_YIELDED,
    ERR_NOT_RUNING,
    ERR_BAD_ARG,
    ERR_BAD_OPCODE,
    ERR_STACK_OVERFLOW,
    ERR_BYTECODE_NOT_FOUND,
    ERR_NATIVECODE_NOT_FOUND,
    ERR_DIVIDE,
    ERR_NATIVE_ERROR,
};

struct Module
{
    const U8*       CodeBase;
};

struct ByteCode
{
    const Module*   Module;
    U16             Address;
};

class Machine;

typedef int (*NativeFunc)( Machine* machine, int argc, int* args, int& resultCount, int& result );
typedef int (*NativeContinuationFunc)( Machine* machine, int argc, int* args, int& resultCount, int& result, int context );

struct NativeCode
{
    NativeFunc  Proc;
};

struct StackFrame
{
    StackFrame*     Prev;
    const U8*       CodePtr;
    const Module*   Module;
    // TODO: try to get rid of this
    U8              ArgCount;
};

class IEnvironment
{
public:
    virtual bool FindByteCode( int id, ByteCode* byteCode ) = 0;
    virtual bool FindNativeCode( int id, NativeCode* nativeCode ) = 0;
};

class Machine
{
public:
    enum
    {
        MIN_STACK   = 16,
    };

private:
    enum
    {
        FRAME_WORDS = (sizeof( StackFrame ) + sizeof( int ) - 1) / sizeof( int ),
    };

    int*            mStack;
    int             mStackSize;
    int*            mGlobals;
    StackFrame*     mCurFrame;
    int*            mSP;
    IEnvironment*   mEnv;
    int             mScriptCtx;

    NativeContinuationFunc mNativeContinuation;
    int             mNativeContinuationContext;
    int             mNativeContinuationArgc;

public:
    Machine();
    void Init( int* globals, int* stack, int stackSize, IEnvironment* environment, int scriptCtx = 0 );
    bool IsRunning();
    int GetScriptContext();
    const StackFrame* GetCallerFrame();
    int* Start( const ByteCode* byteCode, int argCount );
    void Reset();
    int Run();
    int Yield( NativeContinuationFunc proc, int context );

private:
    int* Push( int count );
    StackFrame* PushFrame( const ByteCode* byteCode, int argCount );
    StackFrame* PopFrame( int words );
    int CallPrimitive( int func, int count );
};
