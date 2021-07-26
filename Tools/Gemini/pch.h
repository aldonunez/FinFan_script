// Gemini Languages and Virtual Machine
// Copyright 2019 Aldo Jose Nunez
//
// Licensed under the Apache License, Version 2.0.
// See the LICENSE.txt file for details.

// pch.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <memory>
#include <string>
#include <vector>
#include <map>

#if defined( _WIN32 )

    #define _CRTDBG_MAP_ALLOC
    #include <crtdbg.h>

#endif
