// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once


namespace Gemini
{

class Disassembler
{
public:
    enum ConstFormat : uint8_t
    {
        DecimalConst,
        HexConst,
    };

private:
    const uint8_t*  mCodeBin;
    const uint8_t*  mCodePtr;
    bool            mShowInstAddr;
    ConstFormat     mConstFormat;

public:
    Disassembler( const uint8_t* code, bool showInstAddr = true, ConstFormat constFormat = DecimalConst );

    int32_t Disassemble( char* disassembly, size_t capacity );
};

}
