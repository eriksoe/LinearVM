#include "builtins.h"
#include "coderepr.h"
#include <assert.h>
#include <stdio.h>

Label builtin_int_add(Register* args) {
    int op1 = args[0].i;
    int op2 = args[1].i;
    printf("builtin_int_add: %d + %d\n", op1, op2);
    Label return_label = (Label)args[2].p;
    Register* dest = (Register*)args[3].p;
    *(int*)dest = (op1 + op2);
    return return_label;
}

Label builtin_int_sub(Register* args) {
    int op1 = args[0].i;
    int op2 = args[1].i;
    Label return_label = (Label)args[2].p;
    Register* dest = (Register*)args[3].p;
    *(int*)dest = (op1 - op2);
    return return_label;
}

Label builtin_if_int_eq(Register* args) {
    int op1 = args[0].i;
    int op2 = args[1].i;
    Label on_true  = (Label)args[2].p;
    Label on_false = (Label)args[3].p;
    return (op1==op2) ? on_true : on_false;
}

Label builtin_if_int_lt(Register* args) {
    int op1 = args[0].i;
    int op2 = args[1].i;
    Label on_true  = (Label)args[2].p;
    Label on_false = (Label)args[3].p;
    return (op1<op2) ? on_true : on_false;
}

Label builtin_int_constant(Register* args) {
    Blob* op = (Blob*)args[0].p;
    Label return_label = (Label)args[1].p;
    Register* dest = (Register*)args[2].p;
    assert(op->size == sizeof(int));
    printf("builtin_int_constant: %d\n", *((int*)op->data));
    *(int*)dest = *((int*)op->data);
    return return_label;
}

Label builtin_print_int(Register* args) {
    int op = args[0].i;
    Label return_label = (Label)args[1].p;
    printf("%d\n", op);
    return return_label;
}

const FunctionDescr INT_ADD = {
    .name = "add<int>",
    .argcnt = 4,
    .kind = BUILTIN_FUNC,
    .function = {.builtin = &builtin_int_add},
    .arguments = {REGISTER_ARG, REGISTER_ARG, RETURNLABEL_ARG, RESULTREG_ARG}
};

const FunctionDescr INT_SUB = {
    .name = "sub<int>",
    .argcnt = 4,
    .kind = BUILTIN_FUNC,
    .function = {.builtin = &builtin_int_sub},
    .arguments = {REGISTER_ARG, REGISTER_ARG, RETURNLABEL_ARG, RESULTREG_ARG}
};

const FunctionDescr IF_INT_EQ = {
    .name = "if_eq<int>",
    .argcnt = 4,
    .kind = BUILTIN_FUNC,
    .function = {.builtin = &builtin_if_int_eq},
    .arguments = {REGISTER_ARG, REGISTER_ARG, RETURNLABEL_ARG, RETURNLABEL_ARG}
};

const FunctionDescr IF_INT_LT = {
    .name = "if_lt<int>",
    .argcnt = 4,
    .kind = BUILTIN_FUNC,
    .function = {.builtin = &builtin_if_int_lt},
    .arguments = {REGISTER_ARG, REGISTER_ARG, RETURNLABEL_ARG, RETURNLABEL_ARG}
};

const FunctionDescr INT_CONSTANT = {
    .name = "constant<int>",
    .argcnt = 3,
    .kind = BUILTIN_FUNC,
    .function = {.builtin = &builtin_int_constant},
    .arguments = {BLOB_ARG, RETURNLABEL_ARG, RESULTREG_ARG}
};

const FunctionDescr PRINT_INT = {
    .name = "print<int>",
    .argcnt = 2,
    .kind = BUILTIN_FUNC,
    .function = {.builtin = &builtin_print_int},
    .arguments = {REGISTER_ARG, RETURNLABEL_ARG}
};
