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


Machine::Machine()
    :   mCurFrame( nullptr ),
        mGlobals( nullptr ),
        mStack( nullptr ),
        mStackSize( 0 ),
        mSP( nullptr ),
        mEnv( nullptr ),
        mScriptCtx( 0 ),
        mNativeContinuation( nullptr ),
        mNativeContinuationContext( 0 ),
        mNativeContinuationFlags( 0 )
{
}

void Machine::Init( CELL* globals, CELL* stack, U16 stackSize, IEnvironment* environment, UserContext scriptCtx )
{
    mGlobals = globals;
    mStack = stack;
    mStackSize = stackSize;
    mSP = &stack[stackSize - 1];
    mEnv = environment;
    mScriptCtx = scriptCtx;
}

bool Machine::IsRunning()
{
    return mCurFrame != nullptr;
}

UserContext Machine::GetScriptContext()
{
    return mScriptCtx;
}

const StackFrame* Machine::GetCallerFrame()
{
    return mCurFrame;
}

CELL* Machine::Push( U8 count )
{
    mSP -= count;
    return mSP + 1;
}

CELL* Machine::Start( const ByteCode* byteCode, U8 argCount )
{
    U8 callFlags = CallFlags::Build( argCount, false );
    CELL* args = Push( argCount );
    if ( PushFrame( byteCode, callFlags ) == nullptr )
        return nullptr;
    return args;
}

void Machine::Reset()
{
    mSP = &mStack[mStackSize - 1];
    mCurFrame = nullptr;
}

int Machine::CallNative( NativeFunc proc, U8 callFlags, UserContext context )
{
    bool  autoPop = CallFlags::GetAutoPop( callFlags );
    U8    argCount = CallFlags::GetCount( callFlags );

    CELL* args = mSP + 1;
    CELL* oldSP = mSP;

    int ret = proc( this, argCount, args, context );

    if ( ret == ERR_NONE )
    {
        CELL* newSP = mSP;
        ptrdiff_t resultCount = oldSP - newSP;

        mSP += argCount;

        if ( resultCount == 0 )
        {
            *mSP = 0;
            mSP--;
        }
        else if ( resultCount == 1 )
        {
            mSP[1] = newSP[1];
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
    if ( mCurFrame == nullptr )
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

    const U8* codePtr = mCurFrame->CodePtr;

    for ( ; ; )
    {
        const U8 op = *codePtr;
        codePtr++;

        switch ( op )
        {
        case OP_DUP:
            {
                *mSP = *(mSP + 1);
                mSP--;
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
                mSP[1] = !mSP[1];
            }
            break;

        case OP_LDARG:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                CELL* args = (CELL*) mCurFrame + FRAME_WORDS;
                CELL word = args[index];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STARG:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                CELL* args = (CELL*) mCurFrame + FRAME_WORDS;
                mSP++;
                CELL word = *mSP;
                args[index] = word;
            }
            break;

        case OP_LDLOC:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                index = -index;
                CELL* localsTop = ((CELL*) mCurFrame) - 1;
                CELL word = localsTop[index];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STLOC:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                index = -index;
                CELL* localsTop = ((CELL*) mCurFrame) - 1;
                mSP++;
                CELL word = *mSP;
                localsTop[index] = word;
            }
            break;

        case OP_LDGLO:
            {
                U16 addr = ReadU16( codePtr );
                CELL word = mGlobals[addr];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STGLO:
            {
                U16 addr = ReadU16( codePtr );
                mSP++;
                CELL word = *mSP;
                mGlobals[addr] = word;
            }
            break;

        case OP_LDC:
            {
                CELL word = ReadI32( codePtr );
                *mSP = word;
                mSP--;
            }
            break;

        case OP_LDC_S:
            {
                CELL word = *(I8*) codePtr;
                codePtr += 1;
                *mSP = word;
                mSP--;
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
                mSP++;
                CELL condition = *mSP;
                if ( !condition )
                    codePtr += offset;
            }
            break;

        case OP_BTRUE:
            {
                BranchInst::TOffset offset = BranchInst::ReadOffset( codePtr );
                mSP++;
                CELL condition = *mSP;
                if ( condition )
                    codePtr += offset;
            }
            break;

        case OP_RET:
            {
                int err = PopFrame();
                if ( err != ERR_NONE )
                    return err;
                if ( mCurFrame == nullptr )
                    goto Done;
                codePtr = mCurFrame->CodePtr;
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

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;
                byteCode.Address = addr;
                byteCode.Module = mCurFrame->Module;

                if ( PushFrame( &byteCode, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLI:
            {
                U8 callFlags = *(U8*) codePtr;
                codePtr++;

                mSP++;
                U32 addr = *mSP;
                if ( addr > ADDRESS_MAX )
                    return ERR_BAD_ADDRESS;

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;
                byteCode.Address = addr;
                byteCode.Module = mCurFrame->Module;

                if ( PushFrame( &byteCode, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLN:
            {
                U8 callFlags = *(U8*) codePtr;
                codePtr++;
                U32 id = ReadU32( codePtr );

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;

                if ( !mEnv->FindByteCode( id, &byteCode ) )
                    return ERR_BYTECODE_NOT_FOUND;

                if ( PushFrame( &byteCode, callFlags ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
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

                mCurFrame->CodePtr = codePtr;

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

StackFrame* Machine::PushFrame( const ByteCode* byteCode, U8 callFlags )
{
    if ( (mSP - FRAME_WORDS) < mStack )
        return nullptr;

    mSP -= FRAME_WORDS;
    auto* frame = (StackFrame*) (mSP + 1);

    frame->CodePtr = byteCode->Module->CodeBase + byteCode->Address;
    frame->Module = byteCode->Module;
    frame->CallFlags = callFlags;
    frame->Prev = mCurFrame;

    mCurFrame = frame;

    return frame;
}

int Machine::PopFrame()
{
    bool  autoPop = CallFlags::GetAutoPop( mCurFrame->CallFlags );
    U8    argCount = CallFlags::GetCount( mCurFrame->CallFlags );
    CELL* oldSP = mSP;
    CELL* newSP = ((CELL*) mCurFrame) + FRAME_WORDS - 1 + argCount;

    if ( newSP >= &mStack[mStackSize] )
        return ERR_STACK_OVERFLOW;

    mSP = newSP;
    mCurFrame = mCurFrame->Prev;

    if ( (mSP - 1) < mStack )
        return ERR_STACK_OVERFLOW;

    mSP -= 1;
    mSP[1] = oldSP[1];

    if ( autoPop )
        mSP++;

    return ERR_NONE;
}

int Machine::PushCell( CELL value )
{
    *mSP = value;
    mSP--;
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
    CELL a = *(mSP + 1);
    CELL b = *(mSP + 2);

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

    *(mSP + 2) = result;
    mSP++;

    return ERR_NONE;
}
