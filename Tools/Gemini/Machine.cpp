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
        mNativeContinuationArgc( 0 )
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
    CELL* args = Push( argCount );
    if ( PushFrame( byteCode, argCount ) == nullptr )
        return nullptr;
    return args;
}

void Machine::Reset()
{
    mSP = &mStack[mStackSize - 1];
    mCurFrame = nullptr;
}

int Machine::CallNative( NativeFunc proc, U8 argCount, UserContext context )
{
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
        else
        {
            for ( int i = 1; i <= resultCount; i++ )
            {
                mSP[i] = newSP[i];
            }
        }

        mNativeContinuationArgc = 0;
    }
    else if ( ret == ERR_YIELDED )
    {
        mNativeContinuationArgc = argCount;
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
        U8 count = mNativeContinuationArgc;

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
                U16 addr = *(U16*) codePtr;
                codePtr += 2;
                CELL word = mGlobals[addr];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STGLO:
            {
                U16 addr = *(U16*) codePtr;
                codePtr += 2;
                mSP++;
                CELL word = *mSP;
                mGlobals[addr] = word;
            }
            break;

        case OP_LDC:
            {
                CELL word = *(CELL*) codePtr;
                codePtr += 4;
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
                I8 offset = *(I8*) codePtr;
                codePtr++;
                codePtr += offset;
            }
            break;

        case OP_BFALSE:
            {
                I8 offset = *(I8*) codePtr;
                codePtr++;
                mSP++;
                CELL condition = *mSP;
                if ( !condition )
                    codePtr += offset;
            }
            break;

        case OP_BTRUE:
            {
                I8 offset = *(I8*) codePtr;
                codePtr++;
                mSP++;
                CELL condition = *mSP;
                if ( condition )
                    codePtr += offset;
            }
            break;

        case OP_RET:
            {
                U8 words = *codePtr;
                codePtr++;
                int err = PopFrame( words );
                if ( err != ERR_NONE )
                    return err;
                if ( mCurFrame == nullptr )
                    goto Done;
                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLP:
            {
                U8 count = *(U8*) codePtr;
                codePtr++;
                U8 func = *(U8*) codePtr;
                codePtr++;
                int err = CallPrimitive( func, count );
                if ( err != ERR_NONE )
                    return err;
            }
            break;

        case OP_CALL:
            {
                U8 count = *(U8*) codePtr;
                codePtr++;
                U16 addr = *(U16*) codePtr;
                codePtr += 2;

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;
                byteCode.Address = addr;
                byteCode.Module = mCurFrame->Module;

                if ( PushFrame( &byteCode, count ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLI:
            {
                U8 count = *(U8*) codePtr;
                codePtr++;

                mSP++;
                U32 addr = *mSP;
                if ( addr > ADDRESS_MAX )
                    return ERR_BAD_ADDRESS;

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;
                byteCode.Address = (U16) addr;
                byteCode.Module = mCurFrame->Module;

                if ( PushFrame( &byteCode, count ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLN:
            {
                U8 count = *(U8*) codePtr;
                codePtr++;
                U32 id = *(U32*) codePtr;
                codePtr += 4;

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;

                if ( !mEnv->FindByteCode( id, &byteCode ) )
                    return ERR_BYTECODE_NOT_FOUND;

                if ( PushFrame( &byteCode, count ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLNATIVE:
            {
                U8 count = *(U8*) codePtr;
                codePtr++;
                U32 id = *(U32*) codePtr;
                codePtr += 4;

                mCurFrame->CodePtr = codePtr;

                NativeCode nativeCode;

                if ( !mEnv->FindNativeCode( id, &nativeCode ) )
                    return ERR_NATIVECODE_NOT_FOUND;

                int ret = CallNative( nativeCode.Proc, count, 0 );
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

StackFrame* Machine::PushFrame( const ByteCode* byteCode, U8 argCount )
{
    if ( (mSP - FRAME_WORDS) < mStack )
        return nullptr;

    mSP -= FRAME_WORDS;
    auto* frame = (StackFrame*) (mSP + 1);

    frame->CodePtr = byteCode->Module->CodeBase + byteCode->Address;
    frame->Module = byteCode->Module;
    frame->ArgCount = argCount;
    frame->Prev = mCurFrame;

    mCurFrame = frame;

    return frame;
}

int Machine::PopFrame( U8 words )
{
    CELL* oldSP = mSP;
    CELL* newSP = ((CELL*) mCurFrame) + FRAME_WORDS - 1 + mCurFrame->ArgCount;

    if ( newSP >= &mStack[mStackSize] )
        return ERR_STACK_OVERFLOW;

    mSP = newSP;
    mCurFrame = mCurFrame->Prev;

    if ( (mSP - words) < mStack )
        return ERR_STACK_OVERFLOW;

    mSP -= words;

    for ( int i = 1; i <= words; i++ )
    {
        mSP[i] = oldSP[i];
    }

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

int Machine::CallPrimitive( U8 func, U8 count )
{
    if ( (func == PRIM_NEG || func == PRIM_NOT) && count != 1 )
        return ERR_BAD_OPCODE;
    else if ( count != 2 )
        return ERR_BAD_OPCODE;

    switch ( func )
    {
    case PRIM_ADD:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a + b;
            mSP++;
        }
        break;

    case PRIM_SUB:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a - b;
            mSP++;
        }
        break;

    case PRIM_MULT:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a * b;
            mSP++;
        }
        break;

    case PRIM_DIV:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            if ( b == 0 )
                return ERR_DIVIDE;
            *(mSP + 2) = a / b;
            mSP++;
        }
        break;

    case PRIM_MOD:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            if ( b == 0 )
                return ERR_DIVIDE;
            *(mSP + 2) = a % b;
            mSP++;
        }
        break;

    case PRIM_NEG:
        {
            CELL a = *(mSP + 1);
            *(mSP + 1) = -a;
        }
        break;

    case PRIM_AND:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a && b;
            mSP++;
        }
        break;

    case PRIM_OR:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a || b;
            mSP++;
        }
        break;

    case PRIM_NOT:
        {
            CELL a = *(mSP + 1);
            *(mSP + 1) = !a;
        }
        break;

    case PRIM_EQ:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a == b;
            mSP++;
        }
        break;

    case PRIM_NE:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a != b;
            mSP++;
        }
        break;

    case PRIM_LT:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a < b;
            mSP++;
        }
        break;

    case PRIM_LE:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a <= b;
            mSP++;
        }
        break;

    case PRIM_GT:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a > b;
            mSP++;
        }
        break;

    case PRIM_GE:
        {
            CELL a = *(mSP + 1);
            CELL b = *(mSP + 2);
            *(mSP + 2) = a >= b;
            mSP++;
        }
        break;

    default:
        return ERR_BAD_OPCODE;
    }

    return ERR_NONE;
}
