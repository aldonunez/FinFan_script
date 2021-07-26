// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once


enum OpCode : uint8_t
{
    OP_POP,
    OP_DUP,
    OP_OVER,
    OP_PUSH,
    OP_NOT,
    OP_LDARGA,
    OP_LDARG,
    OP_STARG,
    OP_LDLOCA,
    OP_LDLOC,
    OP_STLOC,
    OP_LDMOD,
    OP_STMOD,
    OP_LDC,
    OP_LDC_S,
    OP_LOADI,
    OP_STOREI,
    OP_PRIM,
    OP_B,
    OP_BFALSE,
    OP_BTRUE,
    OP_RET,
    OP_CALL,
    OP_CALLI,
    OP_CALLM,
    OP_CALLNATIVE,
    OP_CALLNATIVE_S,
    OP_COPYBLOCK,
    OP_COPYARRAY,
    OP_INDEX,
    OP_INDEXOPEN,
    OP_RANGEOPEN,
    OP_RANGEOPENCLOSED,
    OP_RANGE,
    OP_OFFSET,
    OP_YIELD,
    OP_MAXOPCODE,

    // Having each module end with this unsupported opcode ensures that:
    // 1. an error will result if control flows into the end of a module,
    //    without needing to check each instruction's bounds.
    // 2. modules can be easily verified.
    //
    // Also, having one guaranteed instruction for this purpose allows the
    // rest of the instruction set to be extended.

    OP_SENTINEL = 0xFF
};


enum : uint8_t
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


enum : uint8_t
{
    MODINDEX_STACK  = 0xFE,
    MODINDEX_NATIVE = 0xFF,
};


constexpr uint8_t   CONST_SECTION_MOD_INDEX_MASK = 0x80;
