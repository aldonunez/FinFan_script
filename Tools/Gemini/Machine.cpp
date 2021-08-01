// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "stdafx.h"
#include "Machine.h"
#include "OpCodes.h"


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


Machine::Machine() :
    mStack( nullptr ),
    mStackSize( 0 ),
    mFramePtr(),
    mSP( nullptr ),
    mEnv( nullptr ),
    mScriptCtx( 0 ),
    mNativeContinuation( nullptr ),
    mNativeContinuationContext( 0 ),
    mNativeContinuationFlags( 0 ),
    mMod(),
    mModIndex(),
    mPC()
{
}

void Machine::Init( CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx )
{
    Init( stack, stackSize, scriptCtx );
    mEnv = environment;
}

void Machine::Init( CELL* stack, U16 stackSize, int modIndex, const Module* module, UserContext scriptCtx )
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

bool Machine::IsRunning()
{
    return mFramePtr < mStackSize;
}

UserContext Machine::GetScriptContext()
{
    return mScriptCtx;
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

    if ( mSP - mStack < argCount )
        return nullptr;

    mSP -= argCount;

    CELL* args = mSP;

    mPC         = address;
    mMod        = module;
    mModIndex   = modIndex;

    U8 callFlags = CallFlags::Build( argCount, false );

    if ( PushFrame( mMod->CodeBase, callFlags ) == nullptr )
        return nullptr;

    return args;
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
    bool  autoPop = CallFlags::GetAutoPop( callFlags );
    U8    argCount = CallFlags::GetCount( callFlags );

    if ( WouldUnderflow( argCount ) )
        return ERR_STACK_UNDERFLOW;

    if ( WouldOverflow() )
        return ERR_STACK_OVERFLOW;

    CELL* args = mSP;
    CELL* oldSP = mSP;

    int ret = proc( this, argCount, args, context );

    if ( ret == ERR_NONE )
    {
        CELL* newSP = mSP;
        ptrdiff_t resultCount = oldSP - newSP;

        mSP += argCount;

        if ( resultCount == 0 )
        {
            Push( 0 );
        }
        else if ( resultCount == 1 )
        {
            mSP[0] = newSP[0];
        }
        else
        {
            return ERR_NATIVE_ERROR;
        }

        if ( autoPop )
            mSP++;
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
    }

    const U8* codePtr = mMod->CodeBase + mPC;

    while ( true )
    {
        const U8 op = *codePtr;
        codePtr++;

        switch ( op )
        {
        case OP_DUP:
            {
                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mSP--;
                *mSP = *(mSP + 1);
            }
            break;

        case OP_PUSH:
            {
                U8 count = ReadU8( codePtr );

                if ( WouldOverflow( count ) )
                    return ERR_STACK_OVERFLOW;

                mSP -= count;

                std::fill( mSP, mSP + count, 0 );
            }
            break;

        case OP_POP:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mSP++;
            }
            break;

        case OP_NOT:
            {
                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mSP[0] = !mSP[0];
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

        case OP_LDLOCA:
            {
                int index = ReadU8( codePtr );
                long offset = mFramePtr - 1 - index;

                if ( offset < 0 )
                    return ERR_BAD_ADDRESS;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                U32 addrWord = CodeAddr::Build( offset, MODINDEX_STACK );
                Push( addrWord );
            }
            break;

        case OP_LDMOD:
            {
                U8  iMod = ReadU8( codePtr );
                U16 addr = ReadU16( codePtr );

                auto [err, mod] = GetDataModule( iMod );
                if ( err != ERR_NONE )
                    return err;

                if ( addr >= mod->DataSize )
                    return ERR_BAD_ADDRESS;

                if ( WouldOverflow() )
                    return ERR_STACK_OVERFLOW;

                Push( mod->DataBase[addr] );
            }
            break;

        case OP_STMOD:
            {
                U8  iMod = ReadU8( codePtr );
                U16 addr = ReadU16( codePtr );

                auto [err, mod] = GetDataModule( iMod );
                if ( err != ERR_NONE )
                    return err;

                if ( addr >= mod->DataSize )
                    return ERR_BAD_ADDRESS;

                if ( WouldUnderflow() )
                    return ERR_STACK_UNDERFLOW;

                mod->DataBase[addr] = Pop();
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
                U16 addr = CodeAddr::GetAddress( addrWord );

                auto [err, mod] = GetDataModule( iMod );
                if ( err != ERR_NONE )
                    return err;

                if ( addr >= mod->DataSize )
                    return ERR_BAD_ADDRESS;

                Push( mod->DataBase[addr] );
            }
            break;

        case OP_STOREI:
            {
                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                U32 addrWord = Pop();
                U32 value = Pop();
                U8  iMod = CodeAddr::GetModule( addrWord );
                U16 addr = CodeAddr::GetAddress( addrWord );

                auto [err, mod] = GetDataModule( iMod );
                if ( err != ERR_NONE )
                    return err;

                if ( addr >= mod->DataSize )
                    return ERR_BAD_ADDRESS;

                mod->DataBase[addr] = value;
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
                I32 addr = (codePtr - mMod->CodeBase) + offset;

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
                I32 addr = (codePtr - mMod->CodeBase) + offset;

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
                I32 addr = (codePtr - mMod->CodeBase) + offset;

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
                if ( err != ERR_NONE )
                    return err;
                if ( mFramePtr >= mStackSize )
                    goto Done;

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

                // If the native call yields, then we have to remember where we were.
                mPC = codePtr - mMod->CodeBase;

                NativeCode nativeCode;

                if ( !mEnv->FindNativeCode( id, &nativeCode ) )
                    return ERR_NATIVECODE_NOT_FOUND;

                int ret = CallNative( nativeCode.Proc, callFlags, 0 );
                if ( ret != ERR_NONE )
                    return ret;
            }
            break;

        case OP_INDEX:
            {
                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL base = mSP[1];
                CELL index = mSP[0];
                U32  stride = ReadU32( codePtr );

                mSP[1] = base + (index * stride);
                mSP++;
            }
            break;

        case OP_INDEX_S:
            {
                if ( WouldUnderflow( 2 ) )
                    return ERR_STACK_UNDERFLOW;

                CELL base = mSP[1];
                CELL index = mSP[0];
                U8   stride = ReadU8( codePtr );

                mSP[1] = base + (index * stride);
                mSP++;
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
    if ( (mSP - mStack) < FRAME_WORDS )
        return nullptr;

    if ( WouldUnderflow( CallFlags::GetCount( callFlags ) ) )
        return nullptr;

    mSP -= FRAME_WORDS;
    auto frame = (StackFrame*) mSP;

    U32 retAddr = curCodePtr - mMod->CodeBase;

    frame->RetAddrWord = CodeAddr::Build( retAddr, mModIndex );
    frame->CallFlags = callFlags;
    frame->FrameAddr = mFramePtr;

    mFramePtr = (CELL*) frame - mStack;

    return frame;
}

int Machine::PopFrame()
{
    if ( (mFramePtr + FRAME_WORDS) > mStackSize )
        return ERR_STACK_UNDERFLOW;

    auto  curFrame = (StackFrame*) &mStack[mFramePtr];
    bool  autoPop = CallFlags::GetAutoPop( curFrame->CallFlags );
    U8    argCount = CallFlags::GetCount( curFrame->CallFlags );

    // Subtract one to save room for the return value
    unsigned long newStackPtr = mFramePtr + FRAME_WORDS + argCount - 1;

    if ( newStackPtr >= mStackSize )
        return ERR_STACK_UNDERFLOW;

    U8 newModIndex = CodeAddr::GetModule( curFrame->RetAddrWord );

    int err = SwitchModule( newModIndex );
    if ( err != ERR_NONE )
        return err;

    CELL* oldSP = mSP;

    mPC = CodeAddr::GetAddress( curFrame->RetAddrWord );
    mSP = &mStack[newStackPtr];
    mFramePtr = curFrame->FrameAddr;

    mSP[0] = oldSP[0];

    if ( autoPop )
        mSP++;

    return ERR_NONE;
}

int Machine::PushCell( CELL value )
{
    if ( WouldOverflow() )
        return ERR_STACK_OVERFLOW;

    Push( value );
    return ERR_NONE;
}

int Machine::Yield( NativeFunc proc, UserContext context )
{
    if ( proc == nullptr )
        return ERR_BAD_ARG;

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
            result = a + b;
        }
        break;

    case PRIM_SUB:
        {
            result = a - b;
        }
        break;

    case PRIM_MUL:
        {
            result = a * b;
        }
        break;

    case PRIM_DIV:
        {
            if ( b == 0 )
                return ERR_DIVIDE;
            result = a / b;
        }
        break;

    case PRIM_MOD:
        {
            if ( b == 0 )
                return ERR_DIVIDE;
            result = a % b;
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

void Machine::Push( CELL word )
{
    mSP--;
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

std::pair<int, const Module*> Machine::GetDataModule( U8 index )
{
    const Module* mod;

    if ( index == mModIndex )
    {
        mod = mMod;
    }
    else if ( index == MODINDEX_STACK )
    {
        mod = &mStackMod;
    }
    else
    {
        mod = GetModule( index );
        if ( mod == nullptr )
            return std::pair( ERR_BYTECODE_NOT_FOUND, nullptr );
    }

    if ( mod->DataBase == nullptr )
        return std::pair( ERR_BAD_ADDRESS, nullptr );

    return std::pair( ERR_NONE, mod );
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
