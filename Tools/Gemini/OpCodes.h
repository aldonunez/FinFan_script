#pragma once


enum
{
    OP_DUP,
    OP_PUSH,
    OP_POP,
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
    OP_B,
    OP_BFALSE,
    OP_BTRUE,
    OP_MAXOPCODE
};

enum
{
    PRIM_ADD,
    PRIM_SUB,
    PRIM_MULT,
    PRIM_DIV,
    PRIM_MOD,
    PRIM_NEG,
    PRIM_AND,
    PRIM_OR,
    PRIM_NOT,
    PRIM_EQ,
    PRIM_NE,
    PRIM_LT,
    PRIM_LE,
    PRIM_GT,
    PRIM_GE,
    PRIM_MAXPRIMITIVE,
};
