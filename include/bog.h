#ifndef BOG_H
#define BOG_H

#include <stdbool.h>
#include <stdint.h>

typedef struct bog_Vm bog_Vm;
typedef struct bog_Value bog_Value;

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
bog_Error bog_Vm_addStd(bog_Vm *vm);

// possible errors: BOG_ERROR_OUT_OF_MEMORY, BOG_ERROR_TOKENIZE, BOG_ERROR_PARSE, BOG_ERROR_COMPILE,
//      BOG_ERROR_RUNTIME, BOG_ERROR_MALFORMED_BYTE_CODE
bog_Error bog_Vm_run(bog_Vm *vm, bog_Value **result, const char *source);
// possible errors: BOG_ERROR_OUT_OF_MEMORY, BOG_ERROR_RUNTIME, BOG_ERROR_MALFORMED_BYTE_CODE,
//      BOG_ERROR_NOT_A_MAP, BOG_ERROR_NO_SUCH_MEMBER, BOG_ERROR_NOT_A_FUNCTION,
//      BOG_ERROR_INVALID_ARG_COUNT, BOG_ERROR_NATIVE_FUNCTIONS_UNSUPPORTED
bog_Error bog_Vm_call(bog_Vm *vm, bog_Value **result, bog_Value *container, const char *func_name);

// possible errors: BOG_ERROR_IO
bog_Error bog_Vm_renderErrors(bog_Vm *vm, const char *source);

#endif  // BOG_H
