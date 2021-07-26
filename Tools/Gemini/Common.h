// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#pragma once

namespace Gemini
{

using CodeSize      = uint_least32_t;
using GlobalSize    = uint_least16_t;
using LocalSize     = uint_least8_t;
using ParamSize     = uint_least8_t;
using DataSize      = GlobalSize;
using ModSize       = uint_least8_t;

constexpr CodeSize      CodeSizeMax = 16777215;
constexpr GlobalSize    GlobalSizeMax = 65535;
constexpr LocalSize     LocalSizeMax = 255;
constexpr ParamSize     ParamSizeMax = 127;
constexpr DataSize      DataSizeMax = GlobalSizeMax;
constexpr ModSize       ModSizeMax = 126;

constexpr uint8_t       SENTINEL_SIZE = 7;
constexpr uint8_t       MODULE_CODE_ALIGNMENT = 4;
constexpr uint8_t       FRAME_WORDS = 2;

constexpr uint32_t      MAX_MODULE_CODE_SIZE = (CodeSizeMax - MODULE_CODE_ALIGNMENT);
constexpr uint16_t      MAX_MODULE_DATA_SIZE = GlobalSizeMax;
constexpr uint8_t       MAX_NATIVE_NESTING = 32;

}
