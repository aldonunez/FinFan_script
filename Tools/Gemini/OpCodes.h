#pragma once


enum
{
    OP_DUP,
    OP_PUSH,
    OP_POP,
    OP_NOT,
    OP_LDARG,
    OP_STARG,
    OP_LDLOC,
    OP_STLOC,
    OP_LDGLO,
    OP_STGLO,
    OP_LDC,
    OP_LDC_S,
    OP_RET,
    OP_CALL,
    OP_CALLI,
    OP_CALLP,
    OP_CALLN,
    OP_CALLNATIVE,
    OP_CALLNATIVE_S,
    OP_B,
    OP_BFALSE,
    OP_BTRUE,
    OP_MAXOPCODE
};

enum
{
    PRIM_ADD,
    PRIM_SUB,
    PRIM_MUL,
    PRIM_DIV,
    PRIM_MOD,
    PRIM_EQ,
    PRIM_NE,
    PRIM_LT,
    PRIM_LE,
    PRIM_GT,
    PRIM_GE,
    PRIM_MAXPRIMITIVE,
};


class CallFlags
{
    enum
    {
        AutoPop     = 0x80,
        CountMask   = 0x1F,
    };

public:
    static U8 Build( int count, bool autoPop )
    {
        assert( (count & CountMask) == count );

        U8 flags = count;

        if ( autoPop )
            flags |= AutoPop;

        return flags;
    }

    static bool GetAutoPop( U8 flags )
    {
        return flags & AutoPop;
    }

    static U8 GetCount( U8 flags )
    {
        return flags & CountMask;
    }
};
