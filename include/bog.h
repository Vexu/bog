#ifndef BOG_H
#define BOG_H

#include <stdbool.h>
#include <stdint.h>

typedef struct bog_Vm bog_Vm;
typedef struct bog_Value bog_Value;
typedef struct bog_Errors bog_Errors;
typedef struct bog_Tree bog_Tree;

typedef enum bog_Error {
    BOG_ERROR_NONE,
    BOG_ERROR_OUT_OF_MEMORY,
    BOG_ERROR_TOKENIZE,
    BOG_ERROR_PARSE,
    BOG_ERROR_COMPILE,
    BOG_ERROR_RUNTIME,
    BOG_ERROR_MALFORMED_BYTE_CODE,
    BOG_ERROR_NOT_A_MAP,
    BOG_ERROR_NO_SUCH_MEMBER,
    BOG_ERROR_NOT_A_FUNCTION,
    BOG_ERROR_INVALID_ARG_COUNT,
    BOG_ERROR_NATIVE_FUNCTIONS_UNSUPPORTED,
    BOG_ERROR_IO,
} bog_Error;

// possible errors: BOG_ERROR_OUT_OF_MEMORY
bog_Error bog_Vm_init(bog_Vm **vm, bool import_files);
void bog_Vm_deinit(bog_Vm *vm);

// possible errors: BOG_ERROR_OUT_OF_MEMORY
// build with NO_ADD_STD to exclude this and reduce binary size
bog_Error bog_Vm_addStd(bog_Vm *vm);
// build with NO_ADD_STD_NO_IO to exclude this and reduce binary size
bog_Error bog_Vm_addStdNoIo(bog_Vm *vm);

// possible errors: BOG_ERROR_OUT_OF_MEMORY, BOG_ERROR_TOKENIZE, BOG_ERROR_PARSE, BOG_ERROR_COMPILE,
//      BOG_ERROR_RUNTIME, BOG_ERROR_MALFORMED_BYTE_CODE
bog_Error bog_Vm_run(bog_Vm *vm, bog_Value **result, const char *source);

// possible errors: BOG_ERROR_OUT_OF_MEMORY, BOG_ERROR_RUNTIME, BOG_ERROR_MALFORMED_BYTE_CODE,
//      BOG_ERROR_NOT_A_MAP, BOG_ERROR_NO_SUCH_MEMBER, BOG_ERROR_NOT_A_FUNCTION,
//      BOG_ERROR_INVALID_ARG_COUNT, BOG_ERROR_NATIVE_FUNCTIONS_UNSUPPORTED
bog_Error bog_Vm_call(bog_Vm *vm, bog_Value **result, bog_Value *container, const char *func_name);

// possible errors: BOG_ERROR_IO
bog_Error bog_Vm_renderErrors(bog_Vm *vm, const char *source, FILE *out);

// possible errors: BOG_ERROR_OUT_OF_MEMORY
bog_Error bog_Errors_init(bog_Errors **errors);
void bog_Errors_deinit(bog_Errors *errors);

// possible errors: BOG_ERROR_IO
bog_Error bog_Errors_render(bog_Errors *errors, const char *source, FILE *out);

// possible errors: BOG_ERROR_OUT_OF_MEMORY, BOG_ERROR_TOKENIZE, BOG_ERROR_PARSE
bog_Error bog_parse(bog_Tree **tree, const char *source, bog_Errors *errors);
void bog_Tree_deinit(bog_Tree *tree);

// possible errors: BOG_ERROR_IO
bog_Error bog_Tree_render(bog_Tree *tree, FILE *out, bool* changed);

#endif  // BOG_H
