// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

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
