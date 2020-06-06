#include <stdio.h>
#include <stdlib.h>

#include "bog.h"

char *read_file(char *name) {
    FILE *file = fopen(name, "r");
    if (file == NULL)
        return NULL;

    fseek(file, 0, SEEK_END);
    long bytes = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = (char *)malloc(bytes + 1);
    if (!buffer) {
        fclose(file);
        return NULL;
    }

    fread(buffer, sizeof(char), bytes, file);
    buffer[bytes] = '\0';
    fclose(file);
    return buffer;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("expected file name\n");
        return 1;
    }
    char *source = read_file(argv[1]);
    if (!source) {
        printf("cannot read file %s\n", argv[1]);
        return 1;
    }

    bog_Vm *vm;
    if (bog_Vm_init(&vm, true) != BOG_ERROR_NONE)
        return 1;

    if (bog_Vm_addStd(vm) != BOG_ERROR_NONE)
        goto error;

    bog_Value *res;
    if (bog_Vm_run(vm, &res, source) != BOG_ERROR_NONE) {
        bog_Vm_renderErrors(vm, source, stderr);
        goto error;
    }

    free(source);
    bog_Vm_deinit(vm);
    return 0;

error:
    free(source);
    bog_Vm_deinit(vm);
    return 1;
}