#include "stdafx.h"
#include "Disassembler.h"
#include "OpCodes.h"


static const char* gOpCodes[] = 
{
    "DUP",
    "PUSH",
    "POP",
    "NOT",
    "LDARG",
    "STARG",
    "LDLOC",
    "STLOC",
    "LDMOD",
    "STMOD",
    "LDC",
    "LDC.S",
    "RET",
    "CALL",
    "CALLI",
    "CALLP",
    "CALLM",
    "CALLNATIVE",
    "CALLNATIVE.S",
    "B",
    "BFALSE",
    "BTRUE",
};

static const char* gPrimitives[] = 
{
    "ADD",
    "SUB",
    "MUL",
    "DIV",
    "MOD",
    "EQ",
    "NE",
    "LT",
    "LE",
    "GT",
    "GE",
};


Disassembler::Disassembler( const U8* code )
    :   mCodeBin( code ),
        mCodePtr( code )
{
}

int Disassembler::Disassemble( char* disassembly, size_t capacity )
{
    U32 addr = mCodePtr - mCodeBin;

    const U8* origCodePtr = mCodePtr;
    U8 op = *mCodePtr++;

    const char* opName = nullptr;
    int charsWritten = 0;
    int totalCharsWritten = 0;

    if ( op < OP_MAXOPCODE )
        opName = gOpCodes[op];
    else if ( op == OP_SENTINEL )
        opName = "SENTINEL";
    else
        opName = "*Invalid opcode";

    charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), "%06X:  %s", addr, opName );
    if ( charsWritten < 0 )
        return -1;

    disassembly += charsWritten;
    totalCharsWritten += charsWritten;

    charsWritten = 0;

    switch ( op )
    {
    case OP_DUP:
    case OP_POP:
    case OP_NOT:
    case OP_RET:
        break;

    case OP_PUSH:
    case OP_LDARG:
    case OP_STARG:
    case OP_LDLOC:
    case OP_STLOC:
        {
            int value = *(U8*) mCodePtr++;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " %d", value );
        }
        break;

    case OP_LDC_S:
        {
            int value = *(I8*) mCodePtr++;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " %d", value );
        }
        break;

    case OP_LDMOD:
    case OP_STMOD:
        {
            int iMod = ReadU8( mCodePtr );
            int addr = ReadU16( mCodePtr );
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " $%02X, $%04X", iMod, addr );
        }
        break;

    case OP_LDC:
        {
            int value = ReadI32( mCodePtr );
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " %d", value );
        }
        break;

    case OP_CALL:
        {
            U8 callFlags = *mCodePtr++;
            int addr = ReadU24( mCodePtr );
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                "%s(%d) $%06X",
                (CallFlags::GetAutoPop( callFlags ) ? ".POP" : ""),
                CallFlags::GetCount( callFlags ), addr );
        }
        break;

    case OP_CALLI:
        {
            U8 callFlags = *mCodePtr++;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                "%s(%d)",
                (CallFlags::GetAutoPop( callFlags ) ? ".POP" : ""),
                CallFlags::GetCount( callFlags ) );
    }
        break;

    case OP_CALLP:
        {
            int primitive = *(U8*) mCodePtr++;
            if ( primitive >= PRIM_MAXPRIMITIVE )
                return -1;
            const char* primitiveName = gPrimitives[primitive];
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                " %s",
                primitiveName );
        }
        break;

    case OP_CALLM:
    case OP_CALLNATIVE:
        {
            U8 callFlags = *mCodePtr++;
            int id = ReadU32( mCodePtr );
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                "%s(%d) $%08X",
                (CallFlags::GetAutoPop( callFlags ) ? ".POP" : ""),
                CallFlags::GetCount( callFlags ), id );
        }
        break;

    case OP_CALLNATIVE_S:
        {
            U8 callFlags = *mCodePtr++;
            int id = *mCodePtr;
            mCodePtr += 1;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                "%s(%d) $%02X",
                (CallFlags::GetAutoPop( callFlags ) ? ".POP" : ""),
                CallFlags::GetCount( callFlags ), id );
        }
        break;

    case OP_B:
    case OP_BFALSE:
    case OP_BTRUE:
        {
            int offset = BranchInst::ReadOffset( mCodePtr );
            int target = mCodePtr - mCodeBin + offset;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " $%06X", target );
        }
        break;

    default:
        {
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " $%02X", op );
        }
        break;
    }

    if ( charsWritten < 0 )
        return -1;

    disassembly += charsWritten;
    totalCharsWritten += charsWritten;

    return mCodePtr - origCodePtr;
}
