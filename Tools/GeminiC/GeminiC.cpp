// GeminiC.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "..\Gemini\AlgolyParser.h"
#include "..\Gemini\LispyParser.h"
#include "..\Gemini\Compiler.h"
#include "..\Gemini\Machine.h"
#include "..\Gemini\Disassembler.h"
#include <vector>

using namespace Gemini;


constexpr char LispyExt[]   = ".geml";
constexpr char AlgolyExt[]  = ".gema";

constexpr char AlgolyNatives[] =
    "native HasItem( itemId )\n"
    "native AddItem( itemId, amount )\n"
    "native RemoveItem( itemId, amount )\n"
    "native ShowDialog( textId, reserved )\n"
    "native SetObjectVisible( objId, visible )\n"
    "native IsObjectVisible( objId )\n"
    "native HasEventFlag( eventFlag )\n"
    "native SetEventFlag( eventFlag, enabled )\n"
    "native PlayFanfare\n"
    "native PlayGotItem\n"
    "native HasWorldEventFlag( eventFlag )\n"
    "native SetWorldEventFlag( eventFlag, enabled )\n"
    "native Fight( formationId )\n"
    "native FadeOut( frames )\n"
    "native FadeIn( frames )\n"
    "native SwapMap( mapId, startCol, startRow, inRoomState )\n"
    "native MakeAllObjects\n"
    "native PlayDefaultSong\n"
    "native UpgradeClass\n"
    "native StartTrack( track, handler: &proc )\n"
    "native Turn( dir )\n"
    "native Pause( frames )\n"
    "native Join\n"
    "native PushSong( songId )\n"
    ;

constexpr char LispyNatives[] =
    "(defnative HasItem (itemId) )\n"
    "(defnative AddItem (itemId amount) )\n"
    "(defnative RemoveItem (itemId amount) )\n"
    "(defnative ShowDialog (textId reserved) )\n"
    "(defnative SetObjectVisible (objId visible) )\n"
    "(defnative IsObjectVisible (objId) )\n"
    "(defnative HasEventFlag (eventFlag) )\n"
    "(defnative SetEventFlag (eventFlag enabled) )\n"
    "(defnative PlayFanfare () )\n"
    "(defnative PlayGotItem () )\n"
    "(defnative HasWorldEventFlag (eventFlag) )\n"
    "(defnative SetWorldEventFlag (eventFlag enabled) )\n"
    "(defnative Fight (formationId) )\n"
    "(defnative FadeOut (frames) )\n"
    "(defnative FadeIn (frames) )\n"
    "(defnative SwapMap (mapId startCol startRow inRoomState) )\n"
    "(defnative MakeAllObjects () )\n"
    "(defnative PlayDefaultSong () )\n"
    "(defnative UpgradeClass () )\n"
    "(defnative StartTrack (track (handler : ->)) )\n"
    "(defnative Turn (dir) )\n"
    "(defnative Pause (frames) )\n"
    "(defnative Join () )\n"
    "(defnative PushSong (songId) )\n"
    ;


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
    func.Kind = ExternalKind::Native;
    func.Address = 0;
    mFuncMap.insert( FuncMap::value_type( name, func ) );

    MachineFunc macFunc;
    macFunc.Kind = ExternalKind::Native;
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
    virtual void Add( LogCategory category, const char* fileName, int line, int column, const char* message )
    {
        printf( "<%d>  ", category );
        printf( "%s %4d %3d  ", (fileName != nullptr ? fileName : ""), line, column );
        printf( "%s\n", message );
    }
};


int main( int argc, char* argv[] )
{
    const char* filePath = nullptr;

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

    filePath = argv[1];
    err = fopen_s( &file, filePath, "rb" );
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

    std::string codeText;
    int codeTextLen = fileSize;

    codeText.resize( fileSize );
    fread( &codeText.at(0), 1, fileSize, file );

    CompilerEnv env;
    CompilerLog log;

    env.AddGlobal( "_P0", 0 );
    env.AddGlobal( "_P1", 1 );
    env.AddGlobal( "_P2", 2 );
    env.AddGlobal( "_P3", 3 );

    Unique<Unit> progTree;
    Unique<Unit> nativeUnit;
    size_t filePathLen = strlen( filePath );

    if ( filePathLen > (sizeof LispyExt - 1)
        && 0 == _stricmp( LispyExt, &filePath[filePathLen - (sizeof LispyExt - 1)] ) )
    {
        LispyParser parser( &codeText.front(), codeText.size(), filePath, &log );
        progTree = parser.Parse();

        LispyParser nativeParser( LispyNatives, sizeof LispyNatives, "", &log );
        nativeUnit = nativeParser.Parse();
    }
    else if ( filePathLen > (sizeof AlgolyExt - 1)
        && 0 == _stricmp( AlgolyExt, &filePath[filePathLen - (sizeof AlgolyExt - 1)] ) )
    {
        AlgolyParser parser( &codeText.front(), codeText.size(), filePath, &log );
        progTree = parser.Parse();

        AlgolyParser nativeParser( AlgolyNatives, sizeof AlgolyNatives, "", &log );
        nativeUnit = nativeParser.Parse();
    }
    else
    {
        fprintf( stderr, "Unrecognized source file type\n" );
        return 1;
    }

    CompilerAttrs compilerAttrs;
    Compiler compiler( &env, &log, compilerAttrs );
    compiler.AddUnit( std::move( progTree ) );
    compiler.AddUnit( std::move( nativeUnit ) );
    CompilerErr error = compiler.Compile();

    if ( error != CompilerErr::OK )
    {
        return 2;
    }

    CompilerStats stats = { 0 };
    compiler.GetStats( stats );

    fwrite( compiler.GetCode(), 1, stats.CodeBytesWritten, outFile );

    for ( CompilerEnv::FuncMap::const_iterator it = env.BeginExternals();
        it != env.EndExternals();
        it++ )
    {
        if ( it->second.Kind == ExternalKind::Bytecode )
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
    printf( "Calls indirectly: %s\n", stats.CallsIndirectly ? "true" : "false" );
    printf( "Type     Depth   Cells   Recurses\n" );

    CallStats* callSets[] = { &stats.Static, &stats.Lambda };
    const char* callSetTypes[] = { "Static", "Lambda" };
    int i = 0;

    for ( auto set : callSets )
    {
        printf( "%6s   %05d   %05d   %s\n",
            callSetTypes[i], set->MaxCallDepth, set->MaxStackUsage, set->Recurses ? "true" : "false" );
        i++;
    }

    printf( "\n" );

    if ( disassemble )
    {
        std::map<int, std::string> codeToFunc;

        for ( CompilerEnv::FuncMap::const_iterator it = env.BeginExternals();
            it != env.EndExternals();
            it++ )
        {
            if ( it->second.Kind != ExternalKind::Native )
            {
                codeToFunc[it->second.Address] = it->first;
            }
        }

        Disassembler disassembler( compiler.GetCode() );
        CodeSize totalBytesDisasm = 0;
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
