// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include "targetver.h"

#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>

#include <stdio.h>
#include <tchar.h>
#include <assert.h>

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>


template <typename T, size_t Size = sizeof( T )>
T ReadPacked( const uint8_t*& p )
{
    static_assert( Size >= 2 && Size <= sizeof( T ), "Size must be 2 to sizeof(T)" );

    T x = p[0]
        | (p[1] << 8)
        | ((Size > 2) ? (p[2] << 16) : 0)
        | ((Size > 3) ? (p[3] << 24) : 0);

    constexpr unsigned int Shift = (sizeof( T ) - Size) * 8;

    p += Size;

    return (x << Shift) >> Shift;
}


template <typename T, size_t Size = sizeof( T ), typename S>
void StorePacked( uint8_t* p, S value )
{
    static_assert( Size >= 2 && Size <= sizeof( T ), "Size must be 2 to sizeof(T)" );

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


#define StoreU32    StorePacked<uint32_t>

#define ReadU16     ReadPacked<uint16_t>
#define ReadI32     ReadPacked<int32_t>
#define ReadU32     ReadPacked<uint32_t>

#define WriteU16    WritePacked<uint16_t>
#define WriteI32    WritePacked<int32_t>
#define WriteU32    WritePacked<uint32_t>
