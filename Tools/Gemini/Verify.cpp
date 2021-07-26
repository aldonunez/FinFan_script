// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "pch.h"
#include "Common.h"
#include "Machine.h"
#include "OpCodes.h"


namespace Gemini
{

int VerifyModule( const Module* mod )
{
    if ( mod == nullptr )
        return ERR_BAD_ARG;

    if ( mod->CodeBase == nullptr
        || mod->CodeSize > MAX_MODULE_CODE_SIZE
        || mod->CodeSize <= SENTINEL_SIZE
        || (mod->CodeSize % MODULE_CODE_ALIGNMENT) != 0 )
        return ERR_BAD_MODULE;

    if ( (mod->DataBase == nullptr && mod->DataSize > 0)
        || (mod->ConstBase == nullptr && mod->ConstSize > 0) )
        return ERR_BAD_MODULE;

    const U8* codeBase = mod->CodeBase;

    for ( U32 i = mod->CodeSize - SENTINEL_SIZE; i < mod->CodeSize; i++ )
    {
        if ( codeBase[i] != OP_SENTINEL )
            return ERR_BAD_MODULE;
    }

    return ERR_NONE;
}

}
