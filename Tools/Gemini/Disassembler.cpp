#include "stdafx.h"
#include "Disassembler.h"
#include "OpCodes.h"


static const char* gOpCodes[] = 
{
    "DUP",
    "PUSH",
    "POP",
    "LDARG",
    "STARG",
    "LDLOC",
    "STLOC",
    "LDGLO",
    "STGLO",
    "LDC",
    "LDC.S",
    "RET",
    "CALL",
    "CALLI",
    "CALLP",
    "CALLN",
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
    "MULT",
    "DIV",
    "MOD",
    "NEG",
    "AND",
    "OR",
    "NOT",
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
    U16 addr = mCodePtr - mCodeBin;

    const U8* origCodePtr = mCodePtr;
    U8 op = *mCodePtr++;

    if ( op >= OP_MAXOPCODE )
        return -1;

    const char* opName = gOpCodes[op];
    int charsWritten = 0;
    int totalCharsWritten = 0;

    charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), "%04X:  %s", addr, opName );
    if ( charsWritten < 0 )
        return -1;

    disassembly += charsWritten;
    totalCharsWritten += charsWritten;

    charsWritten = 0;

    switch ( op )
    {
    case OP_DUP:
    case OP_POP:
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

    case OP_LDGLO:
    case OP_STGLO:
        {
            int addr = *(U16*) mCodePtr;
            mCodePtr += 2;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " $%04X", addr );
        }
        break;

    case OP_LDC:
        {
            int value = *(U32*) mCodePtr;
            mCodePtr += 4;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " %d", value );
        }
        break;

    case OP_CALL:
        {
            U8 callFlags = *mCodePtr++;
            int addr = *(U16*) mCodePtr;
            mCodePtr += 2;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                "%s(%d) $%04X",
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
            U8 callFlags = *mCodePtr++;
            int primitive = *(U8*) mCodePtr++;
            if ( primitive >= PRIM_MAXPRIMITIVE )
                return -1;
            const char* primitiveName = gPrimitives[primitive];
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten),
                "%s(%d) %s",
                (CallFlags::GetAutoPop( callFlags ) ? ".POP" : ""),
                CallFlags::GetCount( callFlags ), primitiveName );
        }
        break;

    case OP_CALLN:
    case OP_CALLNATIVE:
        {
            U8 callFlags = *mCodePtr++;
            int id = *(U32*) mCodePtr;
            mCodePtr += 4;
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
            int offset = *(I8*) mCodePtr++;
            int target = mCodePtr - mCodeBin + offset;
            charsWritten = sprintf_s( disassembly, (capacity - totalCharsWritten), " $%04X", target );
        }
        break;
    }

    if ( charsWritten < 0 )
        return -1;

    disassembly += charsWritten;
    totalCharsWritten += charsWritten;

    return mCodePtr - origCodePtr;
}
