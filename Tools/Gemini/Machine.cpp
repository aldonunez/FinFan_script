// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "pch.h"
#include "Machine.h"
#include "OpCodes.h"
#include "VmCommon.h"
#include <algorithm>


/*
dup
push <uint8>
pop
not

ldarg, starg <uint8>
ldloc, stloc <uint8>
ldloca <uint8>
ldmod, stmod <uint8> <uint16>
ldc <int32>
ldc.s <int8>
loadi
storei
prim <uint8>

b <int16>
bfalse, btrue <int16>

ret
call <uint8> <uint24>
calli <uint8>
callm <uint8> <uint8> <uint24>
*/


namespace Gemini
{

static_assert( FRAME_WORDS == (sizeof( StackFrame ) + sizeof( CELL ) - 1) / sizeof( CELL ) );


Machine::Machine() :
    mSP( nullptr ),
    mStack( nullptr ),
    mStackSize( 0 ),
    mFramePtr(),
    mEnv( nullptr ),
    mScriptCtx( 0 ),
    mNativeContinuation( nullptr ),
    mNativeContinuationContext( 0 ),
    mNativeContinuationFlags( 0 ),
    mNativeNestingLevel( 0 ),
    mModIndex(),
    mPC(),
    mMod(),
    mStackMod{}
{
}

void Machine::Init( CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx )
{
    Init( stack, stackSize, scriptCtx );
    mEnv = environment;
}

void Machine::Init( CELL* stack, U16 stackSize, U8 modIndex, const Module* module, UserContext scriptCtx )
{
    Init( stack, stackSize, scriptCtx );
    mEnv = this;
    mMod = module;
    mModIndex = modIndex;
}

void Machine::Init( CELL* stack, U16 stackSize, UserContext scriptCtx )
{
    mStack = stack;
    mStackSize = stackSize;
    mScriptCtx = scriptCtx;

    mStackMod = {};
    mStackMod.DataBase = stack;
    mStackMod.DataSize = stackSize;

    Reset();
}

bool Machine::IsRunning() const
{
    return mFramePtr < mStackSize;
}

UserContext Machine::GetScriptContext() const
{
    return mScriptCtx;
}

U8 Machine::GetModIndex() const
{
    return mModIndex;
}

U32 Machine::GetPC() const
{
    return mPC;
}

CELL* Machine::Start( U8 modIndex, U32 address, U8 argCount )
{
    const Module* module = GetModule( modIndex );

    if ( module == nullptr )
        return nullptr;

    if (   module->CodeBase == nullptr
        || module->CodeSize <= SENTINEL_SIZE
        || address >= module->CodeSize - SENTINEL_SIZE )
        return nullptr;

    if ( WouldOverflow( argCount ) )
        return nullptr;

    DecrementSP( argCount );

    CELL* args = mSP;

    mPC         = address;
    mMod        = module;
    mModIndex   = MODINDEX_NATIVE;

    U8 callFlags = CallFlags::Build( argCount, false );

    if ( PushFrame( mMod->CodeBase, callFlags ) == nullptr )
        return nullptr;

    mPC         = address;
    mMod        = module;
    mModIndex   = modIndex;

    return args;
}

CELL* Machine::Start( CELL addrWord, U8 argCount )
{
    U8 modIndex = CodeAddr::GetModule( addrWord );
    U32 address = CodeAddr::GetAddress( addrWord );

    return Start( modIndex, address, argCount );
}

void Machine::Reset()
{
    mSP = &mStack[mStackSize];
    mFramePtr = mStackSize;

    mNativeContinuation = nullptr;
    mNativeContinuationContext = 0;
    mNativeContinuationFlags = 0;
}

int Machine::CallNative( NativeFunc proc, U8 callFlags, UserContext context )
{
    U8    argCount = CallFlags::GetCount( callFlags );

    if ( WouldUnderflow( argCount ) )
        return ERR_STACK_UNDERFLOW;

    if ( WouldOverflow() )
        return ERR_STACK_OVERFLOW;

    CELL* args = mSP + FRAME_WORDS;
    CELL* oldSP = mSP;

    if ( mNativeNestingLevel == MAX_NATIVE_NESTING )
        return ERR_NATIVE_ERROR;

    mNativeNestingLevel++;

    int ret = proc( this, argCount, args, context );

    mNativeNestingLevel--;

    if ( ret == ERR_NONE )
    {
        CELL* newSP = mSP;
        ptrdiff_t resultCount = oldSP - newSP;

        if ( resultCount == 0 )
        {
            Push( 0 );
        }
        else if ( resultCount != 1 )
        {
            return ERR_NATIVE_ERROR;
        }
    }
    else if ( ret == ERR_YIELDED )
    {
        if ( mNativeContinuation == nullptr )
            return ERR_NATIVE_ERROR;

        mNativeContinuationFlags = callFlags;
    }

    return ret;
}

int Machine::Run()
{
    if ( mFramePtr >= mStackSize )
        return ERR_NOT_RUNING;

    if ( mNativeContinuation != nullptr )
    {
        NativeFunc continuation = mNativeContinuation;
        UserContext context = mNativeContinuationContext;

        mNativeContinuation = nullptr;
        mNativeContinuationContext = 0;

        int ret = CallNative( continuation, mNativeContinuationFlags, context );
        if ( ret != ERR_NONE )
            return ret;

        int err = PopFrame();
        if ( err != ERR_NONE )
            return err;

        if ( !IsCodeInBounds( mPC ) )
            return ERR_BAD_ADDRESS;
    }

    const U8* codePtr = mMod->CodeBase + mPC;

    while ( true )
    {
        mPC = static_cast<U32>( codePtr - mMod->CodeBase );

        const U8 op = *codePtr;
        codePtr++;

        switch ( op )
        {
        case OP_POP:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mSP++;
            }
            break;

        case OP_DUP:
            {
                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                DecrementSP( 1 );
                *mSP = mSP[1];
            }
            break;

        case OP_OVER:
            {
                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                DecrementSP( 1 );
                *mSP = mSP[2];
            }
            break;

        case OP_PUSH:
            {
                U8 count = ReadU8( codePtr );

                if ( WouldOverflow( count ) )
                    return ERR_STACK_OVERFLOW;

                DecrementSP( count );

                std::fill( mSP, mSP + count, 0 );
            }
            break;

        case OP_NOT:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mSP[0] = !mSP[0];
            }
            break;

        case OP_LDARGA:
            {
                int index = ReadU8( codePtr );
                long offset = mFramePtr + FRAME_WORDS + index;

                if ( offset >= mStackSize )
                    return ERR_BAD_ADDRESS;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                U32 addrWord = CodeAddr::Build( offset, MODINDEX_STACK );
                Push( addrWord );
            }
            break;

        case OP_LDARG:
            {
                int index = ReadU8( codePtr );
                long offset = mFramePtr + FRAME_WORDS + index;

                if ( offset >= mStackSize )
                    return ERR_BAD_ADDRESS;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                Push( mStack[offset] );
            }
            break;

        case OP_STARG:
            {
                int index = ReadU8( codePtr );
                long offset = mFramePtr + FRAME_WORDS + index;

                if ( offset >= mStackSize )
                    return ERR_BAD_ADDRESS;

                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mStack[offset] = Pop();
            }
            break;

        case OP_LDLOCA:
            {
                U8  index = ReadU8( codePtr );
                I32 offset = mFramePtr - 1 - index;

                if ( offset < 0 )
                    return ERR_BAD_ADDRESS;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                U32 addrWord = CodeAddr::Build( offset, MODINDEX_STACK );
                Push( addrWord );
            }
            break;

        case OP_LDLOC:
            {
                int index = ReadU8( codePtr );
                long offset = mFramePtr - 1 - index;

                if ( offset < 0 )
                    return ERR_BAD_ADDRESS;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                Push( mStack[offset] );
            }
            break;

        case OP_STLOC:
            {
                int index = ReadU8( codePtr );
                long offset = mFramePtr - 1 - index;

                if ( offset < 0 )
                    return ERR_BAD_ADDRESS;

                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mStack[offset] = Pop();
            }
            break;

        case OP_LDMOD:
            {
                U8  iMod = ReadU8( codePtr );
                U16 addr = ReadU16( codePtr );

                auto [err, mod] = GetReadableDataModule( iMod, addr );
                if ( err != ERR_NONE )
                    return err;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                Push( mod.Base[addr] );
            }
            break;

        case OP_STMOD:
            {
                U8  iMod = ReadU8( codePtr );
                U16 addr = ReadU16( codePtr );

                auto [err, mod] = GetWritableDataModule( iMod, addr );
                if ( err != ERR_NONE )
                    return err;

                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mod.Base[addr] = Pop();
            }
            break;

        case OP_LDC:
            {
                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                CELL word = ReadI32( codePtr );
                Push( word );
            }
            break;

        case OP_LDC_S:
            {
                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                CELL word = ReadI8( codePtr );
                Push( word );
            }
            break;

        case OP_LOADI:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                U32 addrWord = Pop();
                U8  iMod = CodeAddr::GetModule( addrWord );
                U32 addr = CodeAddr::GetAddress( addrWord );

                auto [err, mod] = GetReadableDataModule( iMod, addr );
                if ( err != ERR_NONE )
                    return err;

                Push( mod.Base[addr] );
            }
            break;

        case OP_STOREI:
            {
                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                U32 addrWord = Pop();
                U32 value = Pop();
                U8  iMod = CodeAddr::GetModule( addrWord );
                U32 addr = CodeAddr::GetAddress( addrWord );

                auto [err, mod] = GetWritableDataModule( iMod, addr );
                if ( err != ERR_NONE )
                    return err;

                mod.Base[addr] = value;
            }
            break;

        case OP_PRIM:
            {
                U8 func = ReadU8( codePtr );
                int err = CallPrimitive( func );
                if ( err != ERR_NONE )
                    return err;
            }
            break;

        case OP_B:
            {
                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                I32 addr = static_cast<I32>(codePtr - mMod->CodeBase) + offset;

                if ( !IsCodeInBounds( addr ) )
                    return ERR_BAD_ADDRESS;

                codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_BFALSE:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                I32 addr = static_cast<I32>(codePtr - mMod->CodeBase) + offset;

                if ( !IsCodeInBounds( addr ) )
                    return ERR_BAD_ADDRESS;

                CELL condition = Pop();
                if ( !condition )
                    codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_BTRUE:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                I32 addr = static_cast<I32>(codePtr - mMod->CodeBase) + offset;

                if ( !IsCodeInBounds( addr ) )
                    return ERR_BAD_ADDRESS;

                CELL condition = Pop();
                if ( condition )
                    codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_RET:
            {
                int err = PopFrame();
                if ( err == ERR_SWITCH_TO_NATIVE )
                    goto Done;

                if ( err != ERR_NONE )
                    return err;

                if ( !IsCodeInBounds( mPC ) )
                    return ERR_BAD_ADDRESS;

                codePtr = mMod->CodeBase + mPC;
            }
            break;

        case OP_CALL:
            {
                U8 callFlags = ReadU8( codePtr );
                U32 addr = ReadU24( codePtr );

                if ( PushFrame( codePtr, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                if ( !IsCodeInBounds( addr ) )
                    return ERR_BAD_ADDRESS;

                codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_CALLI:
        case OP_CALLM:
            {
                U8 callFlags = ReadU8( codePtr );
                U32 addrWord;

                if ( op == OP_CALLI )
                {
                    if ( WouldUnderflow() )
                        return ERR_STACK_UNDERFLOW;

                    addrWord = Pop();
                }
                else
                {
                    addrWord = ReadU32( codePtr );
                }

                if ( PushFrame( codePtr, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                U32 addr        = CodeAddr::GetAddress( addrWord );
                U8  newModIndex = CodeAddr::GetModule( addrWord );

                int err = SwitchModule( newModIndex );
                if ( err != ERR_NONE )
                    return err;

                if ( !IsCodeInBounds( addr ) )
                    return ERR_BAD_ADDRESS;

                codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_CALLNATIVE:
        case OP_CALLNATIVE_S:
            {
                U8 callFlags = ReadU8( codePtr );
                U32 id;

                if ( op == OP_CALLNATIVE )
                {
                    id = ReadU32( codePtr );
                }
                else
                {
                    id = ReadU8( codePtr );
                }

                if ( PushFrame( codePtr, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                NativeCode nativeCode;

                if ( !mEnv->FindNativeCode( id, &nativeCode ) )
                    return ERR_NATIVECODE_NOT_FOUND;

                int ret = CallNative( nativeCode.Proc, callFlags, 0 );
                if ( ret != ERR_NONE )
                    return ret;

                int err = PopFrame();
                if ( err != ERR_NONE )
                    return err;

                if ( !IsCodeInBounds( mPC ) )
                    return ERR_BAD_ADDRESS;

                codePtr = mMod->CodeBase + mPC;
            }
            break;

        case OP_COPYBLOCK:
            {
                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL source = mSP[1];
                CELL dest   = mSP[0];
                U32  size   = ReadU24( codePtr );

                mSP += 2;

                auto [err, pDst] = GetSizedWritableDataPtr( dest, size );
                if ( err != ERR_NONE )
                    return err;

                auto [err1, pSrc] = GetSizedReadableDataPtr( source, size );
                if ( err1 != ERR_NONE )
                    return err1;

                std::copy_n( pSrc, size, pDst );
            }
            break;

        case OP_COPYARRAY:
            {
                if ( WouldUnderflow( 4 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL srcCount = mSP[3];
                CELL srcAddr  = mSP[2];
                CELL dstCount = mSP[1];
                CELL dstAddr  = mSP[0];
                U32  elemSize = ReadU24( codePtr );

                mSP += 4;

                if ( srcCount < 0 || dstCount < srcCount )
                    return ERR_BOUND;

                U64 size64 = static_cast<U64>(srcCount) * elemSize;

                if ( size64 > MAX_MODULE_DATA_SIZE )
                    return ERR_BOUND;

                U32 size = static_cast<U32>(size64);

                auto [err, pDst] = GetSizedWritableDataPtr( dstAddr, size );
                if ( err != ERR_NONE )
                    return err;

                auto [err1, pSrc] = GetSizedReadableDataPtr( srcAddr, size );
                if ( err1 != ERR_NONE )
                    return err1;

                std::copy_n( pSrc, size, pDst );
            }
            break;

        case OP_INDEX:
            {
                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                U32  base   = mSP[1];
                CELL index  = mSP[0];
                U32  stride = ReadU24( codePtr );
                U32  bound  = ReadU24( codePtr );

                if ( index < 0 || static_cast<U32>(index) >= bound )
                    return ERR_BOUND;

                auto newAddr = base + (static_cast<U64>(index) * stride);

                if ( newAddr > CodeAddr::ToModuleMax( base ) )
                    return ERR_BAD_ADDRESS;

                mSP[1] = static_cast<CELL>(newAddr);
                mSP++;
            }
            break;

        case OP_INDEXOPEN:
            {
                if ( WouldUnderflow( 3 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL bound  = mSP[2];
                U32  base   = mSP[1];
                CELL index  = mSP[0];
                U32  stride = ReadU24( codePtr );

                if ( index < 0 || index >= bound || bound < 0 )
                    return ERR_BOUND;

                auto newAddr = base + (static_cast<U64>(index) * stride);

                if ( newAddr > CodeAddr::ToModuleMax( base ) )
                    return ERR_BAD_ADDRESS;

                mSP[2] = static_cast<CELL>(newAddr);
                mSP += 2;
            }
            break;

        case OP_RANGEOPEN:
            {
                if ( WouldUnderflow( 4 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL bound  = mSP[3];
                U32  base   = mSP[2];
                CELL index  = mSP[1];
                CELL end    = mSP[0];
                U32  stride = ReadU24( codePtr );

                if ( end == -1 )
                    end = bound;

                if ( index < 0 || index >= bound || bound < 0
                    || end <= index || end > bound )
                    return ERR_BOUND;

                auto newAddr = base + (static_cast<U64>(index) * stride);

                if ( newAddr > CodeAddr::ToModuleMax( base ) )
                    return ERR_BAD_ADDRESS;

                mSP[3] = end - index;
                mSP[2] = static_cast<CELL>(newAddr);
                mSP += 2;
            }
            break;

        case OP_RANGEOPENCLOSED:
            {
                if ( WouldUnderflow( 4 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL bound  = mSP[3];
                U32  base   = mSP[2];
                CELL index  = mSP[1];
                CELL end    = mSP[0];
                U32  stride = ReadU24( codePtr );

                if ( end == -1 )
                    end = bound;

                if ( index < 0 || index >= bound || bound < 0
                    || end <= index || end > bound )
                    return ERR_BOUND;

                auto newAddr = base + (static_cast<U64>(index) * stride);

                if ( newAddr > CodeAddr::ToModuleMax( base ) )
                    return ERR_BAD_ADDRESS;

                mSP[3] = static_cast<CELL>(newAddr);
                mSP += 3;
            }
            break;

        case OP_RANGE:
            {
                if ( WouldUnderflow( 3 ) )
                    return ERR_STACK_UNDERFLOW;

                U32  base   = mSP[2];
                CELL index  = mSP[1];
                CELL end    = mSP[0];
                U32  stride = ReadU24( codePtr );
                U32  bound  = ReadU24( codePtr );

                if ( end == -1 )
                    end = bound;

                if ( index < 0 || static_cast<U32>(index) >= bound
                    || end <= index || static_cast<U32>(end) > bound )
                    return ERR_BOUND;

                auto newAddr = base + (static_cast<U64>(index) * stride);

                if ( newAddr > CodeAddr::ToModuleMax( base ) )
                    return ERR_BAD_ADDRESS;

                mSP[2] = end - index;
                mSP[1] = static_cast<CELL>(newAddr);
                mSP++;
            }
            break;

        case OP_OFFSET:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                U32  base   = mSP[0];
                U32  offset = ReadU24( codePtr );

                auto newAddr = base + static_cast<U64>(offset);

                if ( newAddr > CodeAddr::ToModuleMax( base ) )
                    return ERR_BAD_ADDRESS;

                mSP[0] = static_cast<CELL>(newAddr);
            }
            break;

        case OP_YIELD:
            {
                if ( mNativeNestingLevel > 0 )
                    return ERR_BAD_STATE;

                mPC = static_cast<U32>(codePtr - mMod->CodeBase);

                return ERR_YIELDED;
            }
            break;

        default:
            return ERR_BAD_OPCODE;
        }
    }
Done:

    return ERR_NONE;
}

StackFrame* Machine::PushFrame( const U8* curCodePtr, U8 callFlags )
{
    if ( WouldOverflow( FRAME_WORDS ) )
        return nullptr;

    if ( WouldUnderflow( CallFlags::GetCount( callFlags ) ) )
        return nullptr;

    DecrementSP( FRAME_WORDS );

    auto frame = reinterpret_cast<StackFrame*>( mSP );
    U32 retAddr = static_cast<U32>( curCodePtr - mMod->CodeBase );

    frame->RetAddrWord = CodeAddr::Build( retAddr, mModIndex );
    frame->CallFlags = callFlags;
    frame->FrameAddr = mFramePtr;

    mFramePtr = static_cast<U16>( mSP - mStack );

    return frame;
}

int Machine::PopFrame()
{
    int err = ERR_NONE;

    if ( (mFramePtr + FRAME_WORDS) > mStackSize )
        return ERR_STACK_UNDERFLOW;

    auto  curFrame = reinterpret_cast<StackFrame*>( &mStack[mFramePtr] );
    bool  autoPop = CallFlags::GetAutoPop( curFrame->CallFlags );
    U8    argCount = CallFlags::GetCount( curFrame->CallFlags );

    // Subtract one to save room for the return value
    unsigned long newStackPtr = mFramePtr + FRAME_WORDS + argCount - 1;

    if ( newStackPtr >= mStackSize )
        return ERR_STACK_UNDERFLOW;

    U8 newModIndex = CodeAddr::GetModule( curFrame->RetAddrWord );

    err = SwitchModule( newModIndex );
    if ( err != ERR_NONE && err != ERR_SWITCH_TO_NATIVE )
        return err;

    CELL* oldSP = mSP;

    mPC = CodeAddr::GetAddress( curFrame->RetAddrWord );
    mSP = &mStack[newStackPtr];
    mFramePtr = curFrame->FrameAddr;

    mSP[0] = oldSP[0];

    if ( autoPop )
        mSP++;

    return err;
}

int Machine::PushCell( CELL value )
{
    if ( WouldOverflow() )
        return ERR_STACK_OVERFLOW;

    Push( value );
    return ERR_NONE;
}

int Machine::PopCell( CELL& value )
{
    if ( WouldUnderflow() )
        return ERR_STACK_UNDERFLOW;

    value = Pop();
    return ERR_NONE;
}

int Machine::Yield( NativeFunc proc, UserContext context )
{
    if ( proc == nullptr )
        return ERR_BAD_ARG;

    if ( mNativeNestingLevel != 1 )
        return ERR_NATIVE_ERROR;

    mNativeContinuation = proc;
    mNativeContinuationContext = context;
    return ERR_YIELDED;
}

int Machine::CallPrimitive( U8 func )
{
    if ( WouldUnderflow( 2 ) )
        return ERR_STACK_UNDERFLOW;

    CELL result;
    CELL a = mSP[1];
    CELL b = mSP[0];

    switch ( func )
    {
    case PRIM_ADD:
        {
            result = VmAdd( a, b );
        }
        break;

    case PRIM_SUB:
        {
            result = VmSub( a, b );
        }
        break;

    case PRIM_MUL:
        {
            result = VmMul( a, b );
        }
        break;

    case PRIM_DIV:
        {
            if ( b == 0 )
                return ERR_DIVIDE;

            result = VmDiv( a, b );
        }
        break;

    case PRIM_MOD:
        {
            if ( b == 0 )
                return ERR_DIVIDE;

            result = VmMod( a, b );
        }
        break;

    case PRIM_EQ:
        {
            result = a == b;
        }
        break;

    case PRIM_NE:
        {
            result = a != b;
        }
        break;

    case PRIM_LT:
        {
            result = a < b;
        }
        break;

    case PRIM_LE:
        {
            result = a <= b;
        }
        break;

    case PRIM_GT:
        {
            result = a > b;
        }
        break;

    case PRIM_GE:
        {
            result = a >= b;
        }
        break;

    default:
        return ERR_BAD_OPCODE;
    }

    mSP[1] = result;
    mSP++;

    return ERR_NONE;
}

int Machine::SwitchModule( U8 newModIndex )
{
    if ( newModIndex == MODINDEX_NATIVE )
        return ERR_SWITCH_TO_NATIVE;

    if ( newModIndex != mModIndex )
    {
        mMod = GetModule( newModIndex );
        if ( mMod == nullptr )
            return ERR_BYTECODE_NOT_FOUND;

        if (   mMod->CodeBase == nullptr
            || mMod->CodeSize <= SENTINEL_SIZE )
            return ERR_BAD_MODULE;

        mModIndex = newModIndex;
    }

    return ERR_NONE;
}

void Machine::DecrementSP( U16 count )
{
    assert( count <= (mSP - mStack) );

    mSP -= count;
}

void Machine::Push( CELL word )
{
    DecrementSP( 1 );
    *mSP = word;
}

CELL Machine::Pop()
{
    return *mSP++;
}

bool Machine::WouldOverflow() const
{
    return mSP == mStack;
}

bool Machine::WouldOverflow( U16 count ) const
{
    return count > (mSP - mStack);
}

bool Machine::WouldUnderflow() const
{
    return mSP == &mStack[mStackSize];
}

bool Machine::WouldUnderflow( U16 count ) const
{
    return count > (&mStack[mStackSize] - mSP);
}

bool Machine::IsCodeInBounds( U32 address ) const
{
    return address < (mMod->CodeSize - SENTINEL_SIZE);
}

std::pair<int, const CELL*> Machine::GetSizedReadableDataPtr( CELL addrWord, CELL size, bool writable )
{
    U8  iMod = CodeAddr::GetModule( addrWord );
    U32 offs = CodeAddr::GetAddress( addrWord );

    auto [err, mod] = GetReadableDataModule( iMod, offs, writable );
    if ( err != ERR_NONE )
        return std::pair( err, nullptr );

    if ( size < 0 || size > (mod.Size - static_cast<U16>(offs)) )
        return std::pair( ERR_BAD_ADDRESS, nullptr );

    return std::pair( ERR_NONE, mod.Base + offs );
}

std::pair<int, CELL*> Machine::GetSizedWritableDataPtr( CELL addrWord, CELL size )
{
    auto [err, ptr] = GetSizedReadableDataPtr( addrWord, size, true );
    if ( err != ERR_NONE )
        return std::pair( err, nullptr );

    return std::pair( ERR_NONE, const_cast<CELL*>(ptr) );
}

std::pair<int, Machine::ReadableDataModule> Machine::GetReadableDataModule( U8 index, U32 addr, bool writable )
{
    const Module* mod;

    if ( index == MODINDEX_STACK )
    {
        mod = &mStackMod;
    }
    else
    {
        U8 baseIndex = index & ~CONST_SECTION_MOD_INDEX_MASK;
        bool isConstSection = (baseIndex != index);

        if ( baseIndex == mModIndex )
        {
            mod = mMod;
        }
        else
        {
            mod = GetModule( baseIndex );
            if ( mod == nullptr )
                return std::pair( ERR_BYTECODE_NOT_FOUND, ReadableDataModule() );
        }

        if ( isConstSection )
        {
            if ( writable || addr >= mod->ConstSize )
                return std::pair( ERR_BAD_ADDRESS, ReadableDataModule() );

            assert( mod->ConstBase != nullptr );

            return std::pair( ERR_NONE, ReadableDataModule{ mod->ConstBase, mod->ConstSize } );
        }
    }

    if ( addr >= mod->DataSize )
        return std::pair( ERR_BAD_ADDRESS, ReadableDataModule() );

    assert( mod->DataBase != nullptr );

    return std::pair( ERR_NONE, ReadableDataModule{ mod->DataBase, mod->DataSize } );
}

std::pair<int, Machine::WritableDataModule> Machine::GetWritableDataModule( U8 index, U32 addr )
{
    auto [err, mod] = GetReadableDataModule( index, addr, true );
    if ( err != ERR_NONE )
        return std::pair( err, WritableDataModule() );

    return std::pair( ERR_NONE, WritableDataModule{ const_cast<CELL*>(mod.Base), mod.Size } );
}

const Module* Machine::GetModule( U8 index )
{
    return mEnv->FindModule( index );
}

bool Machine::FindNativeCode( U32 id, NativeCode* nativeCode )
{
    return false;
}

const Module* Machine::FindModule( U8 index )
{
    return index == mModIndex ? mMod : nullptr;
}

}
