#ifndef coderepr_h_
#define coderepr_h_

#include <ffi.h>

/*========== Arguments ========================================*/
typedef enum ArgumentKind {
    REGISTER_ARG,
    RETURNLABEL_ARG,
    RESULTREG_ARG,
    BLOB_ARG
    // Possibly also: vtable
} ArgumentKind;

typedef union Register {
    void* p;
    int i;
} Register;
typedef struct Instruction* Label;

typedef struct Blob {
    size_t size;
    char* data;
} Blob;

typedef union ArgumentValue {
    int regnr;
    Blob* blob;
    Label label;
} ArgumentValue;

typedef struct ArgumentDescr {
    ArgumentKind kind;
    ArgumentValue value;
} ArgumentDescr;

/*========== Function ========================================*/
typedef struct ArgumentType {
    ArgumentKind kind;
    // TODO: type information
} ArgumentType;

typedef enum FunctionKind {
    NATIVE_FUNC,
    BUILTIN_FUNC,
    COMPOSITE_FUNC // Perhaps not the best name, but it'll make do.
} FunctionKind;

typedef struct Instruction* (*BuiltinFunctionPointer)(Register*); // (args) -> return-label

typedef struct FunctionDescr {
    const char* name;
    int argcnt;
    FunctionKind kind;
    union {
        ffi_cif native;
        // TODO: How to support variable numbers of results from builtins?
        BuiltinFunctionPointer builtin;
        //TODO: Composite functions.
    } function;
    //struct ArgumentType arguments[];
    ArgumentKind arguments[];
} FunctionDescr;


/*========== Instructions ========================================*/
typedef struct Instruction {
    const FunctionDescr* function_descr;
    const ArgumentDescr arguments[];
} Instruction;

#endif
