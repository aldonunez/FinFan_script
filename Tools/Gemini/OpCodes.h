#pragma once


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
    OP_LDGLO,
    OP_STGLO,
    OP_LDC,
    OP_LDC_S,
    OP_RET,
    OP_CALL,
    OP_CALLI,
    OP_CALLP,
    OP_CALLM,
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
