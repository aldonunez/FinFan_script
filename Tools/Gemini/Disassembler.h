#pragma once

#include "GeminiCommon.h"


class Disassembler
{
    const U8*   mCodeBin;
    const U8*   mCodePtr;

public:
    Disassembler( const U8* code );

    int Disassemble( char* disassembly, size_t capacity );
};
