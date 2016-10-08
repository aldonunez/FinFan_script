// GeminiC.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "..\Gemini\Compiler.h"
#include "..\Gemini\Machine.h"
#include "..\Gemini\Disassembler.h"
#include <vector>


class CompilerEnv : public ICompilerEnv
{
public:
    typedef std::map<std::string, ExternalFunc> FuncMap;

private:
    struct MachineFunc
    {
        ExternalKind    Kind;
        union
        {
            ByteCode    ByteCode;
            NativeCode  NativeCode;
        };
    };

    typedef std::map<int, MachineFunc> IdMap;
    typedef std::map<std::string, int> GlobalMap;

    GlobalMap   mGlobalMap;
    FuncMap     mFuncMap;
    IdMap       mIdMap;
    Module*     mCurMod;

public:
    CompilerEnv();

    bool AddExternal( const std::string& name, ExternalKind kind, int address );
    bool FindExternal( const std::string& name, ExternalFunc* func );

    void SetCurrentModule( Module* mod );
    bool AddNative( const std::string& name, NativeFunc func );

    bool AddGlobal( const std::string& name, int offset ) override;
    bool FindGlobal( const std::string& name, int& offset ) override;

    FuncMap::const_iterator BeginExternals() const;
    FuncMap::const_iterator EndExternals() const;
};

CompilerEnv::CompilerEnv()
    :   mCurMod( nullptr )
{
}

bool CompilerEnv::AddExternal( const std::string& name, ExternalKind kind, int address )
{
    auto it = mFuncMap.find( name );
    if ( it != mFuncMap.end() )
        return false;

    ExternalFunc func;
    func.Id = mFuncMap.size();
    func.Kind = kind;
    func.Address = address;
    mFuncMap.insert( FuncMap::value_type( name, func ) );

    // A real CompilerEnv is free to declare native procs here.
    //assert( kind == External_Bytecode );

    MachineFunc macFunc;
    macFunc.Kind = kind;
    macFunc.ByteCode.Address = address;
    macFunc.ByteCode.Module = mCurMod;
    mIdMap.insert( IdMap::value_type( func.Id, macFunc ) );
    return true;
}

bool CompilerEnv::FindExternal( const std::string& name, ExternalFunc* func )
{
    auto it = mFuncMap.find( name );
    if ( it == mFuncMap.end() )
        return false;

    *func = it->second;
    return true;
}

void CompilerEnv::SetCurrentModule( Module* mod )
{
    mCurMod = mod;
}

bool CompilerEnv::AddNative( const std::string& name, NativeFunc proc )
{
    auto it = mFuncMap.find( name );
    if ( it != mFuncMap.end() )
        return false;

    ExternalFunc func;
    func.Id = mFuncMap.size();
    func.Kind = External_Native;
    func.Address = 0;
    mFuncMap.insert( FuncMap::value_type( name, func ) );

    MachineFunc macFunc;
    macFunc.Kind = External_Native;
    macFunc.NativeCode.Proc = proc;
    mIdMap.insert( IdMap::value_type( func.Id, macFunc ) );
    return true;
}

bool CompilerEnv::AddGlobal( const std::string& name, int offset )
{
    auto it = mGlobalMap.find( name );
    if ( it != mGlobalMap.end() )
        return false;

    mGlobalMap.insert( GlobalMap::value_type( name, offset ) );
    return true;
}

bool CompilerEnv::FindGlobal( const std::string& name, int& offset )
{
    auto it = mGlobalMap.find( name );
    if ( it == mGlobalMap.end() )
        return false;

    offset = it->second;
    return true;
}

CompilerEnv::FuncMap::const_iterator CompilerEnv::BeginExternals() const
{
    return mFuncMap.begin();
}

CompilerEnv::FuncMap::const_iterator CompilerEnv::EndExternals() const
{
    return mFuncMap.end();
}


class CompilerLog : public ICompilerLog
{
public:
    virtual void Add( LogCategory category, int line, int column, const char* message )
    {
        printf( "<%d>  ", category );
        printf( "%4d %3d  ", line, column );
        printf( "%s\n", message );
    }
};


int main( int argc, char* argv[] )
{
    FILE* file = nullptr;
    FILE* outFile = nullptr;
    FILE* indexFile = nullptr;
    errno_t err = 0;
    bool disassemble = false;

    if ( argc < 4 )
    {
        printf( "Three files are needed: source, binary program, index\n" );
        return 1;
    }

    err = fopen_s( &file, argv[1], "rb" );
    if ( err != 0 )
    {
        return 1;
    }

    err = fopen_s( &outFile, argv[2], "wb" );
    if ( err != 0 )
    {
        return 1;
    }

    err = fopen_s( &indexFile, argv[3], "wb" );
    if ( err != 0 )
    {
        return 1;
    }

    if ( argc == 5 && strcmp( argv[4], "-disasm" ) == 0 )
        disassemble = true;

    fseek( file, 0, SEEK_END );
    long fileSize = ftell( file );
    fseek( file, 0, SEEK_SET );

    std::vector<char> codeText( fileSize );
    int codeTextLen = fileSize;
    U8 codeBin[0x10000];
    int codeBinLen = sizeof codeBin;

    codeText.resize( fileSize );
    fread( &codeText.at(0), 1, fileSize, file );

    CompilerEnv env;
    CompilerLog log;

    // TODO: pass these to the compiler
    env.AddExternal( "HasItem", External_Native, 0 );
    env.AddExternal( "AddItem", External_Native, 0 );
    env.AddExternal( "RemoveItem", External_Native, 0 );
    env.AddExternal( "ShowDialog", External_Native, 0 );
    env.AddExternal( "SetObjectVisible", External_Native, 0 );
    env.AddExternal( "IsObjectVisible", External_Native, 0 );
    env.AddExternal( "HasEventFlag", External_Native, 0 );
    env.AddExternal( "SetEventFlag", External_Native, 0 );
    env.AddExternal( "PlayFanfare", External_Native, 0 );
    env.AddExternal( "PlayGotItem", External_Native, 0 );
    env.AddExternal( "HasWorldEventFlag", External_Native, 0 );
    env.AddExternal( "SetWorldEventFlag", External_Native, 0 );
    env.AddExternal( "Fight", External_Native, 0 );
    env.AddExternal( "FadeOut", External_Native, 0 );
    env.AddExternal( "FadeIn", External_Native, 0 );
    env.AddExternal( "SwapMap", External_Native, 0 );
    env.AddExternal( "MakeAllObjects", External_Native, 0 );
    env.AddExternal( "PlayDefaultSong", External_Native, 0 );
    env.AddExternal( "UpgradeClass", External_Native, 0 );
    env.AddExternal( "StartTrack", External_Native, 0 );
    env.AddExternal( "Turn", External_Native, 0 );
    env.AddExternal( "Pause", External_Native, 0 );
    env.AddExternal( "Join", External_Native, 0 );
    env.AddExternal( "PushSong", External_Native, 0 );

    env.AddGlobal( "%0", 0 );
    env.AddGlobal( "%1", 1 );
    env.AddGlobal( "%2", 2 );
    env.AddGlobal( "%3", 3 );

    Compiler compiler( &codeText.front(), codeTextLen, codeBin, codeBinLen, &env, &log );
    CompilerErr error = compiler.Compile();

    if ( error != 0 )
    {
        return 2;
    }

    CompilerStats stats = { 0 };
    compiler.GetStats( stats );

    fwrite( codeBin, 1, stats.CodeBytesWritten, outFile );

    for ( CompilerEnv::FuncMap::const_iterator it = env.BeginExternals();
        it != env.EndExternals();
        it++ )
    {
        if ( it->second.Kind == External_Bytecode )
        {
            fwrite( &it->second.Address, 4, 1, indexFile );
            fwrite( it->first.c_str(), 1, it->first.size(), indexFile );
            for ( int j = 32 - it->first.size(); j > 0; j-- )
            {
                fputc( '\0', indexFile );
            }
        }
    }

    printf( "%d bytes written.\n", stats.CodeBytesWritten );
    printf( "\n" );

    if ( disassemble )
    {
        std::map<int, std::string> codeToFunc;

        for ( CompilerEnv::FuncMap::const_iterator it = env.BeginExternals();
            it != env.EndExternals();
            it++ )
        {
            if ( it->second.Kind != External_Native )
            {
                codeToFunc[it->second.Address] = it->first;
            }
        }

        Disassembler disassembler( codeBin );
        int totalBytesDisasm = 0;
        while ( totalBytesDisasm < stats.CodeBytesWritten )
        {
            int addr = totalBytesDisasm;
            auto it = codeToFunc.find( addr );
            if ( it != codeToFunc.end() )
            {
                printf( "%s:\n", it->second.c_str() );
            }

            char disasm[256];
            int bytesDisasm = disassembler.Disassemble( disasm, _countof( disasm ) );
            if ( bytesDisasm <= 0 )
                break;
            totalBytesDisasm += bytesDisasm;
            printf( "%s\n", disasm );
        }
    }

	return 0;
}
