// Gemini Languages and Virtual Machine
// Copyright 2021 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

#include "stdafx.h"
#include "Machine.h"
#include "OpCodes.h"


int VerifyModule( const Module* mod )
{
    if ( mod == nullptr )
        return ERR_BAD_ARG;

    if ( mod->CodeBase == nullptr
        || mod->CodeSize <= SENTINEL_SIZE )
        return ERR_BAD_MODULE;

    const U8* codeBase = mod->CodeBase;

    for ( U32 i = mod->CodeSize - SENTINEL_SIZE; i < mod->CodeSize; i++ )
    {
        if ( codeBase[i] != OP_SENTINEL )
            return ERR_BAD_MODULE;
    }

    return ERR_NONE;
}
