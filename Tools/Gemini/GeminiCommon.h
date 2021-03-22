#pragma once

#include <stdint.h>


typedef unsigned char U8;
typedef unsigned short U16;
typedef unsigned int U32;
typedef signed char I8;
typedef signed short I16;
typedef signed int I32;


constexpr uint8_t SENTINEL_SIZE = 6;


template <typename T, size_t Size = sizeof( T )>
T ReadPacked( const uint8_t*& p )
{
    static_assert(Size >= 1 && Size <= sizeof( T ), "Size must be 1 to sizeof(T)");

    T x = p[0]
        | ((Size > 1) ? (p[1] << 8)  : 0)
        | ((Size > 2) ? (p[2] << 16) : 0)
        | ((Size > 3) ? (p[3] << 24) : 0);

    constexpr unsigned int Shift = (sizeof( T ) - Size) * 8;

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
