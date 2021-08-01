// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include <assert.h>


enum
{
    OP_DUP,
    OP_PUSH,
    OP_POP,     // TODO: Can this be removed?
    OP_NOT,
    OP_LDARG,
    OP_STARG,
    OP_LDLOC,
    OP_STLOC,
    OP_LDLOCA,
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
    OP_INDEX,
    OP_INDEX_S,
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


enum
{
    MODINDEX_STACK  = 0xFE,
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


struct BranchInst
{
    using TOffset = int16_t;

    static constexpr int Size = 1 + sizeof( TOffset );
    static constexpr int OffsetMin = INT16_MIN;
    static constexpr int OffsetMax = INT16_MAX;

    static void StoreOffset( uint8_t* p, TOffset offset )
    {
        StoreI16( p, offset );
    }

    static TOffset ReadOffset( const uint8_t*& p )
    {
        return ReadI16( p );
    }

    static void WriteOffset( uint8_t*& p, TOffset offset )
    {
        WriteI16( p, offset );
    }
};


struct CodeAddr
{
    static uint32_t Build( uint32_t address, uint8_t module )
    {
        return address | (module << 24);
    }

    static uint32_t GetAddress( uint32_t addrWord )
    {
        return addrWord & 0xFFFFFF;
    }

    static uint8_t GetModule( uint32_t addrWord )
    {
        return addrWord >> 24;
    }
};
