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

void Machine::Init( int* globals, int* stack, int stackSize, IEnvironment* environment, int scriptCtx )
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

int Machine::GetScriptContext()
{
    return mScriptCtx;
}

const StackFrame* Machine::GetCallerFrame()
{
    return mCurFrame;
}

int* Machine::Push( int count )
{
    mSP -= count;
    return mSP + 1;
}

int* Machine::Start( const ByteCode* byteCode, int argCount )
{
    int* args = Push( argCount );
    PushFrame( byteCode, argCount );
    return args;
}

void Machine::Reset()
{
    mSP = &mStack[mStackSize - 1];
    mCurFrame = nullptr;
}

int Machine::Run()
{
    if ( mCurFrame == nullptr )
        return ERR_NOT_RUNING;

    const U8* codePtr = mCurFrame->CodePtr;

    if ( mNativeContinuation != nullptr )
    {
        NativeContinuationFunc continuation = mNativeContinuation;
        int context = mNativeContinuationContext;
        int count = mNativeContinuationArgc;

        mNativeContinuation = nullptr;
        mNativeContinuationContext = 0;

        int* args = mSP + 1;
        int resultCount = 0;
        int result = 0;

        int ret = continuation( this, count, args, resultCount, result, context );

        if ( ret == ERR_NONE )
        {
            mSP += count;
            if ( resultCount == 0 )
                result = 0;
            *mSP = result;
            mSP--;

            mNativeContinuationArgc = 0;
        }
        else if ( ret == ERR_YIELDED )
        {
            mCurFrame->CodePtr = codePtr;
            return ERR_YIELDED;
        }
        else
        {
            return ret;
        }
    }

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
                int count = *(U8*) codePtr;
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
                int* args = (int*) (mCurFrame + 1);
                int word = args[index];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STARG:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                int* args = (int*) (mCurFrame + 1);
                mSP++;
                int word = *mSP;
                args[index] = word;
            }
            break;

        case OP_LDLOC:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                index = -index;
                int* localsTop = ((int*) mCurFrame) - 1;
                int word = localsTop[index];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STLOC:
            {
                int index = *(U8*) codePtr;
                codePtr++;
                index = -index;
                int* localsTop = ((int*) mCurFrame) - 1;
                mSP++;
                int word = *mSP;
                localsTop[index] = word;
            }
            break;

        case OP_LDGLO:
            {
                int addr = *(U16*) codePtr;
                codePtr += 2;
                int word = mGlobals[addr];
                *mSP = word;
                mSP--;
            }
            break;

        case OP_STGLO:
            {
                int addr = *(U16*) codePtr;
                codePtr += 2;
                mSP++;
                int word = *mSP;
                mGlobals[addr] = word;
            }
            break;

        case OP_LDC:
            {
                int word = *(int*) codePtr;
                codePtr += 4;
                *mSP = word;
                mSP--;
            }
            break;

        case OP_LDC_S:
            {
                int word = *(I8*) codePtr;
                codePtr += 1;
                *mSP = word;
                mSP--;
            }
            break;

        case OP_B:
            {
                int offset = *(I8*) codePtr;
                codePtr++;
                codePtr += offset;
            }
            break;

        case OP_BFALSE:
            {
                int offset = *(I8*) codePtr;
                codePtr++;
                mSP++;
                int condition = *mSP;
                if ( !condition )
                    codePtr += offset;
            }
            break;

        case OP_BTRUE:
            {
                int offset = *(I8*) codePtr;
                codePtr++;
                mSP++;
                int condition = *mSP;
                if ( condition )
                    codePtr += offset;
            }
            break;

        case OP_RET:
            {
                int words = *codePtr;
                codePtr++;
                if ( PopFrame( words ) == nullptr )
                    goto Done;
                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLP:
            {
                int count = *(U8*) codePtr;
                codePtr++;
                int func = *(U8*) codePtr;
                codePtr++;
                int err = CallPrimitive( func, count );
                if ( err != ERR_NONE )
                    return err;
            }
            break;

        case OP_CALL:
            {
                int count = *(U8*) codePtr;
                codePtr++;
                int addr = *(U16*) codePtr;
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
                int count = *(U8*) codePtr;
                codePtr++;

                mSP++;
                int addr = *mSP;

                mCurFrame->CodePtr = codePtr;

                ByteCode byteCode;
                byteCode.Address = addr;
                byteCode.Module = mCurFrame->Module;

                if ( PushFrame( &byteCode, count ) == nullptr )
                    return ERR_STACK_OVERFLOW;

                codePtr = mCurFrame->CodePtr;
            }
            break;

        case OP_CALLN:
            {
                int count = *(U8*) codePtr;
                codePtr++;
                int id = *(U32*) codePtr;
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
                int count = *(U8*) codePtr;
                codePtr++;
                int id = *(U32*) codePtr;
                codePtr += 4;

                mCurFrame->CodePtr = codePtr;

                NativeCode nativeCode;

                if ( !mEnv->FindNativeCode( id, &nativeCode ) )
                    return ERR_NATIVECODE_NOT_FOUND;

                int* args = mSP + 1;
                int resultCount = 0;
                int result = 0;

                int ret = nativeCode.Proc( this, count, args, resultCount, result );

                if ( ret == ERR_NONE )
                {
                    mSP += count;
                    if ( resultCount == 0 )
                        result = 0;
                    *mSP = result;
                    mSP--;
                }
                else if ( ret == ERR_YIELDED )
                {
                    mCurFrame->CodePtr = codePtr;
                    mNativeContinuationArgc = count;
                    return ERR_YIELDED;
                }
                else
                {
                    return ret;
                }
            }
            break;

        default:
            return ERR_BAD_OPCODE;
        }
    }
Done:

    return ERR_NONE;
}

StackFrame* Machine::PushFrame( const ByteCode* byteCode, int argCount )
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

StackFrame* Machine::PopFrame( int words )
{
    assert( words >= 0 && words <= 1 );

    int* oldSP = mSP;
    mSP = ((int*) mCurFrame) + FRAME_WORDS - 1 + mCurFrame->ArgCount;
    mCurFrame = mCurFrame->Prev;

    if ( words == 1 )
    {
        *mSP = *(oldSP + 1);
        mSP--;
    }

    return mCurFrame;
}

int Machine::Yield( NativeContinuationFunc proc, int context )
{
    if ( proc == nullptr )
        return ERR_BAD_ARG;
    mNativeContinuation = proc;
    mNativeContinuationContext = context;
    return ERR_YIELDED;
}

int Machine::CallPrimitive( int func, int count )
{
    switch ( func )
    {
    case PRIM_ADD:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a + b;
            mSP++;
        }
        break;

    case PRIM_SUB:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a - b;
            mSP++;
        }
        break;

    case PRIM_MULT:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a * b;
            mSP++;
        }
        break;

    case PRIM_DIV:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            if ( b == 0 )
                return ERR_DIVIDE;
            *(mSP + 2) = a / b;
            mSP++;
        }
        break;

    case PRIM_MOD:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            if ( b == 0 )
                return ERR_DIVIDE;
            *(mSP + 2) = a % b;
            mSP++;
        }
        break;

    case PRIM_NEG:
        {
            int a = *(mSP + 1);
            *(mSP + 1) = -a;
        }
        break;

    case PRIM_AND:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a && b;
            mSP++;
        }
        break;

    case PRIM_OR:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a || b;
            mSP++;
        }
        break;

    case PRIM_NOT:
        {
            int a = *(mSP + 1);
            *(mSP + 1) = !a;
        }
        break;

    case PRIM_EQ:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a == b;
            mSP++;
        }
        break;

    case PRIM_NE:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a != b;
            mSP++;
        }
        break;

    case PRIM_LT:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a < b;
            mSP++;
        }
        break;

    case PRIM_LE:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a <= b;
            mSP++;
        }
        break;

    case PRIM_GT:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a > b;
            mSP++;
        }
        break;

    case PRIM_GE:
        {
            int a = *(mSP + 1);
            int b = *(mSP + 2);
            *(mSP + 2) = a >= b;
            mSP++;
        }
        break;
    }

    return ERR_NONE;
}
