#include "stdafx.h"
#include "Machine.h"
#include "OpCodes.h"


/*
dup
push <int8>
pop

ldarg, starg <int8>		ldreg, streg <int8> or <int16>	
ldloc, stloc <int8>			
ldglo, stglo <int16>			
ldc.0			
ldc.1			
ldc <int32>			

ret <int8>			
call <int8> <int16>			ldrtn <int16>
callp <int8> <int8>			
calln <int8> <int32>			
			
b <int8>			
bfalse, btrue <int8>			
*/


Machine::Machine() :
    mGlobals( nullptr ),
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

void Machine::Init( CELL* globals, CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx )
{
    Init( globals, stack, stackSize, scriptCtx );
    mEnv = environment;
}

void Machine::Init( CELL* globals, CELL* stack, U16 stackSize, int modIndex, const Module* module, UserContext scriptCtx )
{
    Init( globals, stack, stackSize, scriptCtx );
    mEnv = this;
    mMod = module;
    mModIndex = modIndex;
}

void Machine::Init( CELL* globals, CELL* stack, U16 stackSize, UserContext scriptCtx )
{
    mGlobals = globals;
    mStack = stack;
    mStackSize = stackSize;
    mSP = &stack[stackSize];
    mScriptCtx = scriptCtx;
    mFramePtr = mStackSize;
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
}

int Machine::CallNative( NativeFunc proc, U8 callFlags, UserContext context )
{
    bool  autoPop = CallFlags::GetAutoPop( callFlags );
    U8    argCount = CallFlags::GetCount( callFlags );

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

        mNativeContinuationFlags = 0;
    }
    else if ( ret == ERR_YIELDED )
    {
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
        U8 count = CallFlags::GetCount( mNativeContinuationFlags );

        mNativeContinuation = nullptr;
        mNativeContinuationContext = 0;

        int ret = CallNative( continuation, count, context );
        if ( ret != ERR_NONE )
            return ret;
    }

    const U8* codePtr = mMod->CodeBase + mPC;

    for ( ; ; )
    {
        const U8 op = *codePtr;
        codePtr++;

        switch ( op )
        {
        case OP_DUP:
            {
                mSP--;
                *mSP = *(mSP + 1);
            }
            break;

        case OP_PUSH:
            {
                U8 count = *(U8*) codePtr;
                codePtr++;
                mSP -= count;
            }
            break;

        case OP_POP:
            {
                mSP++;
            }
            break;

        case OP_NOT:
            {
                mSP[0] = !mSP[0];
            }
            break;

        case OP_LDARG:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                CELL* args = &mStack[mFramePtr + FRAME_WORDS];
                Push( args[index] );
            }
            break;

        case OP_STARG:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                CELL* args = &mStack[mFramePtr + FRAME_WORDS];
                args[index] = Pop();
            }
            break;

        case OP_LDLOC:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                index = -index;
                CELL* localsTop = &mStack[mFramePtr - 1];
                Push( localsTop[index] );
            }
            break;

        case OP_STLOC:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                index = -index;
                CELL* localsTop = &mStack[mFramePtr - 1];
                localsTop[index] = Pop();
            }
            break;

        case OP_LDGLO:
            {
                U16 addr = ReadU16( codePtr );
                CELL word = mGlobals[addr];
                Push( word );
            }
            break;

        case OP_STGLO:
            {
                U16 addr = ReadU16( codePtr );
                mGlobals[addr] = Pop();
            }
            break;

        case OP_LDC:
            {
                CELL word = ReadI32( codePtr );
                Push( word );
            }
            break;

        case OP_LDC_S:
            {
                CELL word = *(I8*) codePtr;
                codePtr += 1;
                Push( word );
            }
            break;

        case OP_B:
            {
                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                codePtr += offset;
            }
            break;

        case OP_BFALSE:
            {
                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                CELL condition = Pop();
                if ( !condition )
                    codePtr += offset;
            }
            break;

        case OP_BTRUE:
            {
                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                CELL condition = Pop();
                if ( condition )
                    codePtr += offset;
            }
            break;

        case OP_RET:
            {
                int err = PopFrame();
                if ( err != ERR_NONE )
                    return err;
                if ( mFramePtr >= mStackSize )
                    goto Done;

                codePtr = mMod->CodeBase + mPC;
            }
            break;

        case OP_CALLP:
            {
                U8 func = *(U8*) codePtr;
                codePtr++;
                int err = CallPrimitive( func );
                if ( err != ERR_NONE )
                    return err;
            }
            break;

        case OP_CALL:
            {
                U8 callFlags = *(U8*) codePtr;
                codePtr++;
                U32 addr = ReadU24( codePtr );

                if ( PushFrame( codePtr, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_CALLI:
        case OP_CALLM:
            {
                U8 callFlags = *(U8*) codePtr;
                codePtr++;
                U32 addrWord;

                if ( op == OP_CALLI )
                {
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

                if ( newModIndex != mModIndex )
                {
                    mMod = GetModule( newModIndex );
                    if ( mMod == nullptr )
                        return ERR_BYTECODE_NOT_FOUND;

                    mModIndex = newModIndex;
                }

                codePtr = mMod->CodeBase + addr;
            }
            break;

        case OP_CALLNATIVE:
        case OP_CALLNATIVE_S:
            {
                U8 callFlags = *(U8*) codePtr;
                codePtr++;
                U32 id;

                if ( op == OP_CALLNATIVE )
                {
                    id = ReadU32( codePtr );
                }
                else
                {
                    id = *codePtr;
                    codePtr += 1;
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
    auto  curFrame = (StackFrame*) &mStack[mFramePtr];
    bool  autoPop = CallFlags::GetAutoPop( curFrame->CallFlags );
    U8    argCount = CallFlags::GetCount( curFrame->CallFlags );

    // Subtract one to save room for the return value
    unsigned long newStackPtr = mFramePtr + FRAME_WORDS + argCount - 1;

    if ( newStackPtr >= mStackSize )
        return ERR_STACK_UNDERFLOW;

    U8 newModIndex = CodeAddr::GetModule( curFrame->RetAddrWord );

    if ( newModIndex != mModIndex )
    {
        mMod = GetModule( newModIndex );
        if ( mMod == nullptr )
            return ERR_BYTECODE_NOT_FOUND;

        mModIndex = newModIndex;
    }

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
    CELL result;
    CELL a = *(mSP + 0);
    CELL b = *(mSP + 1);

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

    *(mSP + 1) = result;
    mSP++;

    return ERR_NONE;
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
