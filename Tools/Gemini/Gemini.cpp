// Gemini.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "Machine.h"
#include "OpCodes.h"
#include "Compiler.h"
#include "Disassembler.h"


int NativeAdd( Machine* machine, int argc, int* args, int& resultCount, int& result )
{
    assert( argc == 2 );
    result = args[0] + args[1];
    resultCount = 1;
    return ERR_NONE;
}

int NativeLatent2( Machine* machine, U8 argc, CELL* args, UserContext context )
{
    if ( context == 1 )
        return machine->Yield( NativeLatent2, 2 );

    assert( argc == 2 );
    return machine->PushCell( args[0] + args[1] );
}

int NativeLatent1( Machine* machine, U8 argc, CELL* args, UserContext context )
{
    assert( argc == 2 );
    return machine->Yield( NativeLatent2, 1 );
}

class Env : public IEnvironment
{
    ByteCode*   mByteCodes;
    U32         mByteCodeCount;
    NativeCode* mNativeCodes;
    U32         mNativeCodeCount;

public:
    Env();

    void SetByteCodes( ByteCode* byteCodes, U32 count );
    void SetNativeCodes( NativeCode* natives, U32 count );

    bool FindByteCode( U32 id, ByteCode* byteCode );
    bool FindNativeCode( U32 id, NativeCode* nativeCode );
};

Env::Env()
    :   mByteCodeCount( 0 ),
        mByteCodes( nullptr ),
        mNativeCodeCount( 0 ),
        mNativeCodes( nullptr )
{
}

void Env::SetByteCodes( ByteCode* byteCodes, U32 count )
{
    mByteCodes = byteCodes;
    mByteCodeCount = count;
}

void Env::SetNativeCodes( NativeCode* nativeCodes, U32 count )
{
    mNativeCodes = nativeCodes;
    mNativeCodeCount = count;
}

bool Env::FindByteCode( U32 id, ByteCode* byteCode )
{
    if ( id >= mByteCodeCount )
        return false;

    *byteCode = mByteCodes[id];
    return true;
}

bool Env::FindNativeCode( U32 id, NativeCode* nativeCode )
{
    if ( id >= mNativeCodeCount )
        return false;

    *nativeCode = mNativeCodes[id];
    return true;
}

class CompilerEnv : public ICompilerEnv, public IEnvironment
{
    struct MachineFunc
    {
        ExternalKind    Kind;
        union
        {
            ByteCode    ByteCode;
            NativeCode  NativeCode;
        };
    };

    typedef std::map<std::string, ExternalFunc> FuncMap;
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

    bool FindByteCode( U32 id, ByteCode* byteCode );
    bool FindNativeCode( U32 id, NativeCode* nativeCode );

    bool AddGlobal( const std::string& name, int offset ) override;
    bool FindGlobal( const std::string& name, int& offset ) override;
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
    assert( kind == External_Bytecode );

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

bool CompilerEnv::FindByteCode( U32 id, ByteCode* byteCode )
{
    auto it = mIdMap.find( id );
    if ( it == mIdMap.end() )
        return false;

    if ( it->second.Kind != External_Bytecode )
        return false;

    *byteCode = it->second.ByteCode;
    return true;
}

bool CompilerEnv::FindNativeCode( U32 id, NativeCode* nativeCode )
{
    auto it = mIdMap.find( id );
    if ( it == mIdMap.end() )
        return false;

    if ( it->second.Kind != External_Native )
        return false;

    *nativeCode = it->second.NativeCode;
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

int _tmain(int argc, _TCHAR* argv[])
{
    _CrtSetDbgFlag( _crtDbgFlag | _CRTDBG_LEAK_CHECK_DF | _CRTDBG_ALLOC_MEM_DF );

#if 0
    {
        const char progStr1[] =
            "(1"
            //"(defun a ())\n(set a 1)"
            //"(defun a () (if 1 2 3) )"
            //"(defun a () (if (not (and (or (< 1 2) (< 3 4)) (or (< 5 6) (< 7 8)))) 8 9) )"
            //"(defun a () (b 5 6) (b 7 8) (c 9 10)) (defun b (x y) (* x y)) (defun c (x y) (b 11 12))"
            //""
            ;
        U8 bin1[512];
        CompilerEnv env;
        CompilerLog log;
        Compiler compiler1( progStr1, sizeof progStr1 - 1, bin1, sizeof bin1, &env, &log );
        Module mod1;
        mod1.CodeBase = bin1;
        env.SetCurrentModule( &mod1 );
        auto err = compiler1.Compile();
    }
    return 0;
#endif

    {
#if 0
        const char progStr1[] = 
            "(defun a () "
            //"  (set global1      (< 3 4))"
            //"  (set global1 (not (< 3 4)))"
            //"  (if      (< 3 4) 8 9)"
            "  (if (not (< 3 4)) 8 9)"
            //"  (if      (and (or (< 1 2) (< 3 4)) (or (< 5 6) (< 7 8))) 8 9)"
            //"  (if (not (and (or (< 1 2) (< 3 4)) (or (< 5 6) (< 7 8)))) 8 9)"
            //"  (if      (or (and (< 1 2) (< 3 4)) (and (< 5 6) (< 7 8))) 8 9)"
            //"  (if (not (or (and (< 1 2) (< 3 4)) (and (< 5 6) (< 7 8)))) 8 9)"
            ")";
#else
        const char progStr1[] =
            //"(defun a () (b 5 6) (b 7 8) (c 9 10)) (defun b (x y) (* x y)) (defun c (x y) (b 11 12))"

            //"(defun a () \n"
            //"(set global1 1)\n"
            //"(cond \n"
            //"  ((= global1 1) 11)\n"
            //"  ((= global1 2) 12)\n"
            //"  ((= global1 3) 13)\n"
            //"  )"
            //"(return 9)"
            //")"

            //"(defun a () (set global1 1) (return) (set global2 2))"

            //"(defun a () (progn (b 1) (b 2) (b 3))) (defun b (x) x)"
            //"(defun a () (progn (b 1) (b 2) (b 3))) (defun b (x) (b x))"

            //"(defun a (x) (not (set x 33)) (return x))"
            //"(defun a (x) (not (set x 33)))"
            //"(defun a () (+ (set global1 33) (set global2 66)) (return 9))"

            //"(defun a () (+ (+ (set global1 1) (set global1 2)) (+ (set global1 3) (set global1 4))) (return 9))"
            //"(defun a () (+ (+ (b) (b)) (+ (b) (b))) (return 9))"
            // " (defun b ())"

            //"(defun a () (and (and (set global1 1) (set global1 2)) (and (set global1 3) (set global1 4))) (return 9))"
            //"(defun a () (and (and (b) (b)) (and (b) (b))) (return 9))"
            // " (defun b ())"

            //"(defun a () (or (or (set global1 1) (set global1 2)) (or (set global1 3) (set global1 4))) (return 9))"
            //"(defun a () (or (or (b) (b)) (or (b) (b))) (return 9))"
            // " (defun b ())"
            //"(defun a () \n"
            //"  (cond \n"
            //"    ((+ global1 2))\n"
            //"    ((+ global1 3))\n"
            //"    )\n"
            //"  ;(return 9)\n"
            //")"
            "(defun a () (if 2 3))"
            ;
#endif
        U8 bin1[512];
        CompilerEnv env;
        CompilerLog log;
        Compiler compiler1( progStr1, sizeof progStr1 - 1, bin1, sizeof bin1, &env, &log );
        Module mod1;
        mod1.CodeBase = bin1;
        env.AddGlobal( "global1", 0 );
        env.AddGlobal( "global2", 1 );
        env.AddGlobal( "global3", 2 );
        env.SetCurrentModule( &mod1 );
        CompilerErr compilerErr = compiler1.Compile();
        CompilerStats stats = { 0 };
        compiler1.GetStats( stats );

        if ( compilerErr != CERR_OK )
            return 1;

        Disassembler disassembler( bin1 );
        int totalBytesDisasm = 0;
        while ( totalBytesDisasm < stats.CodeBytesWritten )
        {
            char disasm[256];
            int bytesDisasm = disassembler.Disassemble( disasm, _countof( disasm ) );
            if ( bytesDisasm <= 0 )
                break;
            totalBytesDisasm += bytesDisasm;
            printf( "%s\n", disasm );
        }

        CELL data[100];
        CELL stack[Machine::MIN_STACK];
        Machine machine;
        machine.Init( data, stack, _countof( stack ), &env );
        ExternalFunc external = { 0 };
        ByteCode byteCode = { 0 };
        bool b = false;

        b = env.FindExternal( "a", &external );
        b = env.FindByteCode( external.Id, &byteCode );
        CELL* args = machine.Start( &byteCode, 1 );
        args[0] = 65;

        int err = 0;
        do
        {
            err = machine.Run();
        } while ( err == ERR_YIELDED );
    }
    return 0;

    {
        //const char progStr1[] = "(defun c () (+ 1 2)) (defun b (x y) (let ((c x) (d y)) (- c d)))";
        const char progStr1[] = "(defun c () (+ 1 2)) (defun ;comment 1\r\nb (x y) (let ((c x) (d y)) (- c d)))";
        //const char progStr2[] = "(defun d () (+ 1 2)) (defun a () (b 5 (add 1 1)))";
        //const char progStr2[] = "(defun d () (+ 1 2)) (defun a () (b 5 (funcall (lambda (x y) (add x y)) 100 -98) ))";
        const char progStr2[] = "(defun d () (+ 1 2)) (defun;comment 2\r\n a () (b 5 (funcall (lambda (x y) (add x y)) 100 -98) ))";
        U8 bin1[512];
        U8 bin2[512];
        CompilerEnv env;
        Compiler compiler1( progStr1, sizeof progStr1 - 1, bin1, sizeof bin1, &env, nullptr );
        Compiler compiler2( progStr2, sizeof progStr2 - 1, bin2, sizeof bin2, &env, nullptr );

        bool b = false;
        b = env.AddNative( "add", NativeLatent1 );

        Module mod1;
        mod1.CodeBase = bin1;
        env.SetCurrentModule( &mod1 );
        compiler1.Compile();

        Module mod2;
        mod2.CodeBase = bin2;
        env.SetCurrentModule( &mod2 );
        compiler2.Compile();

        CELL data[100];
        CELL stack[Machine::MIN_STACK];
        Machine machine;
        machine.Init( data, stack, _countof( stack ), &env );

        ExternalFunc external = { 0 };
        ByteCode byteCode = { 0 };

        b = env.FindExternal( "a", &external );
        b = env.FindByteCode( external.Id, &byteCode );

        machine.Start( &byteCode, 0 );
        int err = 0;
        do
        {
            err = machine.Run();
        } while ( err == ERR_YIELDED );
    }
    return 0;

    {
        //const char progStr1[] = "(+ 123 456 7)(do (hello1 (3) () world))(if 1 xyz)";
        //const char progStr1[] = "(return (+ (+ 123 1) (+ 456 2)))";
        //const char progStr1[] = "(return (if (< 3 4) 15 16))";
        //const char progStr1[] = "(set global2 7)(set global3 8)(return (if (< 3 4) global2 global3))";
        //const char progStr1[] = "(defun b (x y) (- x y))(defun a () (b 5 (+ 1 1)))";
        const char progStr1[] = "(defun b (x y) (let ((c x) (d y)) (- c d))) (defun a () (b 5 (+ 1 1)))";
        U8 bin[1024];
        CompilerEnv env;
        Compiler compiler( progStr1, sizeof progStr1 - 1, bin, sizeof bin, &env, nullptr );
        compiler.Compile();
        CELL data[100];
        CELL stack[Machine::MIN_STACK];
        Machine machine;
        machine.Init( data, stack, _countof( stack ), nullptr );

        Module mod;
        mod.CodeBase = bin;

        ByteCode byteCode;
#if 0
        byteCode.Address = 0;
#elif 0
        byteCode.Address = 0x9;
#else
        byteCode.Address = 0x13;
#endif
        byteCode.Module = &mod;

        machine.Start( &byteCode, 0 );
        int err = machine.Run();
    }
    return 0;

    U8 program[] = 
    {
        OP_PUSH,
        2,
        OP_LDC,
        0x78,
        0x56,
        0x34,
        0x12,
        //OP_DUP,
        OP_LDC,
        1,
        0,
        0,
        0,
        OP_CALLP,
        2,
        1,
        OP_DUP,
        OP_STGLO,
        3,
        0,
        OP_LDGLO,
        3,
        0,
        OP_STLOC,
        1,

        OP_LDC,
        0x44,
        0x33,
        0x22,
        0x11,
        OP_POP,
        OP_LDLOC,
        1,

        OP_B,
        5,

        OP_LDC,
        0x88,
        0x77,
        0x66,
        0x55,

        OP_LDC,
        99,
        0,
        0,
        0,
        OP_LDC,
        99,
        0,
        0,
        0,
        OP_BTRUE,
        5,
        OP_LDC,
        0xF1,
        0,
        0,
        0,
        OP_BFALSE,
        5,
        OP_LDC,
        0xF2,
        0,
        0,
        0,

        OP_RET,
        1,
    };
    U8 program2[] = 
    {
        OP_LDC,
        4,
        0,
        0,
        0,
        OP_LDC,
        3,
        0,
        0,
        0,
#if 0
        OP_CALL,
        2,
        0x12,
        0,
        OP_RET,
        1,
        OP_RET,
        1,
#elif 0
        OP_CALLN,
        2,
        0,
        0,
        0,
        0,
        OP_RET,
        1,
#else
        OP_CALLNATIVE,
        2,
        0,
        0,
        0,
        0,
        OP_RET,
        1,
#endif

        OP_LDARG,
        1,
        OP_LDARG,
        0,
        OP_CALLP,
        2,
        0,
        OP_STARG,
        0,
        OP_LDC,
        0xff,
        0xff,
        0xff,
        0xff,
        OP_POP,
        OP_LDARG,
        0,
        OP_RET,
        1
    };
    CELL data[10];
    Env env;

    CELL stack[Machine::MIN_STACK];
    Machine machine;
    machine.Init( data, stack, _countof( stack ), &env );

    Module mod;
    mod.CodeBase = program2;

    ByteCode byteCode;
    byteCode.Address = 0;
    byteCode.Module = &mod;

    ByteCode proc2;
    proc2.Address = 0x12;
    proc2.Module = &mod;
    env.SetByteCodes( &proc2, 1 );

    NativeCode native1;
#if 0
    native1.Proc = NativeAdd;
#else
    native1.Proc = NativeLatent1;
#endif
    env.SetNativeCodes( &native1, 1 );

    machine.Start( &byteCode, 0 );
    int err = 0;

    do
    {
        err = machine.Run();
    } while ( err == ERR_YIELDED );

	return 0;
}
