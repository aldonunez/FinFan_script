// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

#include "Common.h"
#include <assert.h>
#include <stdint.h>
#include <limits.h>


namespace Gemini
{

template <typename T, size_t Size = sizeof( T )>
T ReadPacked( const uint8_t*& p )
{
    static_assert(Size >= 1 && Size <= sizeof( T ), "Size must be 1 to sizeof(T)");

    T x = p[0]
        | ((Size > 1) ? (p[1] << 8)  : 0)
        | ((Size > 2) ? (p[2] << 16) : 0)
        | ((Size > 3) ? (p[3] << 24) : 0);

    constexpr unsigned int Shift = (sizeof( T ) - Size) * CHAR_BIT;

    p += Size;

    return (x << Shift) >> Shift;
}


template <typename T, size_t Size = sizeof( T ), typename S>
void StorePacked( uint8_t* p, S value )
{
    static_assert(Size >= 2 && Size <= sizeof( T ), "Size must be 2 to sizeof(T)");

    auto t = (T) value;

    p[0] = (uint8_t) t;
    p[1] = (uint8_t) (t >> 8);

    if constexpr ( Size > 2 )
        p[2] = (uint8_t) (t >> 16);

    if constexpr ( Size > 3 )
        p[3] = (uint8_t) (t >> 24);
}


template <typename T, size_t Size = sizeof( T ), typename S>
void WritePacked( uint8_t*& p, S value )
{
    StorePacked<T, Size, S>( p, value );

    p += Size;
}


#define StoreI16    StorePacked<int16_t>
#define StoreU16    StorePacked<uint16_t>
#define StoreU24    StorePacked<uint32_t, 3>
#define StoreU32    StorePacked<uint32_t>

#define ReadI8      ReadPacked<int8_t>
#define ReadU8      ReadPacked<uint8_t>
#define ReadI16     ReadPacked<int16_t>
#define ReadU16     ReadPacked<uint16_t>
#define ReadU24     ReadPacked<uint32_t, 3>
#define ReadI32     ReadPacked<int32_t>
#define ReadU32     ReadPacked<uint32_t>

#define WriteI16    WritePacked<int16_t>
#define WriteU16    WritePacked<uint16_t>
#define WriteU24    WritePacked<uint32_t, 3>
#define WriteI32    WritePacked<int32_t>
#define WriteU32    WritePacked<uint32_t>


struct VmDivModResult
{
    int32_t Quotient;
    int32_t Remainder;
};


constexpr int32_t Saturate( int64_t n )
{
    if ( n > INT32_MAX )
        n = INT32_MAX;
    else if ( n < INT32_MIN )
        n = INT32_MIN;

    return static_cast<int32_t>(n);
}

constexpr int32_t VmAdd( int32_t a, int32_t b )
{
    return Saturate( static_cast<int64_t>(a) + b );
}

constexpr int32_t VmSub( int32_t a, int32_t b )
{
    return Saturate( static_cast<int64_t>(a) - b );
}

constexpr int32_t VmMul( int32_t a, int32_t b )
{
    return Saturate( static_cast<int64_t>(a) * b );
}

constexpr VmDivModResult VmDivMod( int32_t a, int32_t b )
{
    int32_t q = 0;
    int32_t r = 0;

    if ( a == INT32_MIN && b == -1 )
    {
        q = INT32_MAX;
        r = 0;
    }
    else
    {
        q = a / b;
        r = a % b;

        // (r < 0 && b > 0) || (r > 0 && b < 0)

        if ( r != 0 && (r ^ b) < 0 )
        {
            q--;
            r += b;
        }
    }

    return { q, r };
}

constexpr int32_t VmDiv( int32_t a, int32_t b )
{
    return VmDivMod( a, b ).Quotient;
}

constexpr int32_t VmMod( int32_t a, int32_t b )
{
    return VmDivMod( a, b ).Remainder;
}


class CallFlags
{
    enum
    {
        AutoPop = 0x80,
        CountMask = 0x7F,
    };

public:
    static uint8_t Build( uint8_t count, bool autoPop )
    {
        assert( (count & CountMask) == count );

        uint8_t flags = count;

        if ( autoPop )
            flags |= AutoPop;

        return flags;
    }

    static bool GetAutoPop( uint8_t flags )
    {
        return flags & AutoPop;
    }

    static uint8_t GetCount( uint8_t flags )
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

    static uint32_t ToModuleMax( uint32_t addrWord )
    {
        return addrWord | 0xFFFFFF;
    }
};

}
