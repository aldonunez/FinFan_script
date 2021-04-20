#pragma once

#include "GeminiCommon.h"

#include <utility>


enum
{
    ERR_NONE,
    ERR_YIELDED,
    ERR_NOT_RUNING,
    ERR_BAD_ARG,
    ERR_BAD_OPCODE,
    ERR_BAD_ADDRESS,
    ERR_BAD_MODULE,
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
    CELL*           DataBase;
    U32             CodeSize;
    U16             DataSize;
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
    CELL*           mSP;
    CELL*           mStack;
    U16             mStackSize;
    U16             mFramePtr;
    IEnvironment*   mEnv;
    UserContext     mScriptCtx;

    NativeFunc      mNativeContinuation;
    UserContext     mNativeContinuationContext;
    U8              mNativeContinuationFlags;
    U8              mModIndex;
    U32             mPC;
    const Module*   mMod;
    Module          mStackMod;

public:
    Machine();
    void Init( CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx = 0 );
    void Init( CELL* stack, U16 stackSize, int modIndex, const Module* module, UserContext scriptCtx = 0 );
    bool IsRunning();
    UserContext GetScriptContext();
    CELL* Start( U8 modIndex, U32 address, U8 argCount );
    void Reset();
    int Run();
    int Yield( NativeFunc proc, UserContext context );
    int PushCell( CELL value );

private:
    void Init( CELL* stack, U16 stackSize, UserContext scriptCtx );
    StackFrame* PushFrame( const U8* curCodePtr, U8 argCount );
    int PopFrame();
    int CallPrimitive( U8 func );
    int CallNative( NativeFunc proc, U8 argCount, UserContext context );

    int SwitchModule( U8 newModIndex );

    void Push( CELL word );
    CELL Pop();

    bool WouldOverflow() const;
    bool WouldOverflow( U16 count ) const;
    bool WouldUnderflow() const;
    bool WouldUnderflow( U16 count ) const;

    bool IsCodeInBounds( U32 address ) const;

    std::pair<int, const Module*> GetDataModule( U8 index );

    const Module* GetModule( U8 index );

    virtual bool FindNativeCode( U32 id, NativeCode* nativeCode ) override;
    virtual const Module* FindModule( U8 index ) override;
};


int VerifyModule( const Module* mod );
