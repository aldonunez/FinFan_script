// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include <utility>


namespace Gemini
{

enum VmError
{
    ERR_NONE,
    ERR_YIELDED,
    ERR_SWITCH_TO_NATIVE,
    ERR_NOT_RUNING,
    ERR_BAD_ARG,
    ERR_BAD_OPCODE,
    ERR_BAD_ADDRESS,
    ERR_BAD_MODULE,
    ERR_BAD_STATE,
    ERR_STACK_OVERFLOW,
    ERR_STACK_UNDERFLOW,
    ERR_BYTECODE_NOT_FOUND,
    ERR_NATIVECODE_NOT_FOUND,
    ERR_DIVIDE,
    ERR_NATIVE_ERROR,
    ERR_BOUND,
};


typedef uint8_t     U8;
typedef uint16_t    U16;
typedef uint32_t    U32;
typedef uint64_t    U64;

typedef int8_t      I8;
typedef int16_t     I16;
typedef int32_t     I32;

typedef int32_t     CELL;
typedef uint32_t    UCELL;
typedef uintptr_t   UserContext;


struct Module
{
    const U8*       CodeBase;
    CELL*           DataBase;
    CELL*           ConstBase;
    U32             CodeSize;
    U16             DataSize;
    U16             ConstSize;
};

struct ByteCode
{
    const Gemini::Module*   Module;
    U32                     Address;
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
    struct ReadableDataModule
    {
        const CELL* Base;
        U16         Size;
    };

    struct WritableDataModule
    {
        CELL*       Base;
        U16         Size;
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
    U8              mNativeNestingLevel;
    U8              mModIndex;
    U32             mPC;
    const Module*   mMod;
    Module          mStackMod;

public:
    Machine();

    void Init( CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx = 0 );
    void Init( CELL* stack, U16 stackSize, U8 modIndex, const Module* module, UserContext scriptCtx = 0 );

    bool IsRunning() const;
    UserContext GetScriptContext() const;
    U8 GetModIndex() const;
    U32 GetPC() const;

    CELL* Start( U8 modIndex, U32 address, U8 argCount );
    CELL* Start( CELL addrWord, U8 argCount );
    void Reset();
    int Run();
    int Yield( NativeFunc proc, UserContext context );
    int PushCell( CELL value );
    int PopCell( CELL& value );

private:
    void Init( CELL* stack, U16 stackSize, UserContext scriptCtx );
    StackFrame* PushFrame( const U8* curCodePtr, U8 argCount );
    int PopFrame();
    int CallPrimitive( U8 func );
    int CallNative( NativeFunc proc, U8 argCount, UserContext context );

    int SwitchModule( U8 newModIndex );

    void DecrementSP( U16 count );

    void Push( CELL word );
    CELL Pop();

    bool WouldOverflow() const;
    bool WouldOverflow( U16 count ) const;
    bool WouldUnderflow() const;
    bool WouldUnderflow( U16 count ) const;

    bool IsCodeInBounds( U32 address ) const;

    std::pair<int, ReadableDataModule> GetReadableDataModule( U8 index, U32 addr, bool writable = false );
    std::pair<int, WritableDataModule> GetWritableDataModule( U8 index, U32 addr );

    std::pair<int, const CELL*> GetSizedReadableDataPtr( CELL addrWord, CELL size, bool writable = false );
    std::pair<int,       CELL*> GetSizedWritableDataPtr( CELL addrWord, CELL size );

    const Module* GetModule( U8 index );

    virtual bool FindNativeCode( U32 id, NativeCode* nativeCode ) override;
    virtual const Module* FindModule( U8 index ) override;
};


int VerifyModule( const Module* mod );

}
