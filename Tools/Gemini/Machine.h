#pragma once

#include "GeminiCommon.h"


enum
{
    ERR_NONE,
    ERR_YIELDED,
    ERR_NOT_RUNING,
    ERR_BAD_ARG,
    ERR_BAD_OPCODE,
    ERR_BAD_ADDRESS,
    ERR_STACK_OVERFLOW,
    ERR_STACK_UNDERFLOW,
    ERR_BYTECODE_NOT_FOUND,
    ERR_NATIVECODE_NOT_FOUND,
    ERR_DIVIDE,
    ERR_NATIVE_ERROR,
};


typedef I32 CELL;
typedef uintptr_t UserContext;

struct Module
{
    const U8*       CodeBase;
};

struct ByteCode
{
    const Module*   Module;
    U32             Address;
};

class Machine;

typedef int (*NativeFunc)( Machine* machine, U8 argc, CELL* args, UserContext context );

struct NativeCode
{
    NativeFunc  Proc;
};

class IEnvironment
{
public:
    virtual bool FindNativeCode( U32 id, NativeCode* nativeCode ) = 0;
    virtual const Module* FindModule( U8 index ) = 0;
};

struct StackFrame
{
    U16             FrameAddr;
    U8              CallFlags;
    U32             RetAddrWord;
};

class Machine : private IEnvironment
{
private:
    enum
    {
        FRAME_WORDS = (sizeof( StackFrame ) + sizeof( CELL ) - 1) / sizeof( CELL ),
    };

public:
    enum
    {
        MIN_STACK = FRAME_WORDS * 4,
    };

private:
    CELL*           mGlobals;
    CELL*           mStack;
    U16             mStackSize;
    U16             mFramePtr;
    CELL*           mSP;
    IEnvironment*   mEnv;
    UserContext     mScriptCtx;

    NativeFunc      mNativeContinuation;
    UserContext     mNativeContinuationContext;
    U8              mNativeContinuationFlags;
    U8              mModIndex;
    U32             mPC;
    const Module*   mMod;

public:
    Machine();
    void Init( CELL* globals, CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx = 0 );
    void Init( CELL* globals, CELL* stack, U16 stackSize, int modIndex, const Module* module, UserContext scriptCtx = 0 );
    bool IsRunning();
    UserContext GetScriptContext();
    CELL* Start( U8 modIndex, U32 address, U8 argCount );
    void Reset();
    int Run();
    int Yield( NativeFunc proc, UserContext context );
    int PushCell( CELL value );

private:
    void Init( CELL* globals, CELL* stack, U16 stackSize, UserContext scriptCtx );
    StackFrame* PushFrame( const U8* curCodePtr, U8 argCount );
    int PopFrame();
    int CallPrimitive( U8 func );
    int CallNative( NativeFunc proc, U8 argCount, UserContext context );

    void Push( CELL word )
    {
        mSP--;
        *mSP = word;
    }

    CELL Pop()
    {
        return *mSP++;
    }

    const Module* GetModule( U8 index );

    virtual bool FindNativeCode( U32 id, NativeCode* nativeCode ) override;
    virtual const Module* FindModule( U8 index ) override;
};
