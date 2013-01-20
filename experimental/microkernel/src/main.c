#include "coderepr.h"
#include "builtins.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

static int c2 = 2;
static Blob const_2 = {sizeof(int), (void*)&c2};

static Instruction i4 = {&PRINT_INT,
                         {{REGISTER_ARG, {.regnr=0}},
                          {RETURNLABEL_ARG, {.label=NULL}}}};
static Instruction i3 = {&INT_ADD,
                         {{REGISTER_ARG, {.regnr=0}},
                          {REGISTER_ARG, {.regnr=1}},
                          {RETURNLABEL_ARG, {.label=&i4}},
                          {RESULTREG_ARG, {.regnr=0}}}};
static Instruction i2 = {&INT_CONSTANT,
                         {{BLOB_ARG, {.blob=&const_2}},
                          {RETURNLABEL_ARG, {.label=&i3}},
                          {RESULTREG_ARG, {.regnr=1}}}};
static Instruction i1 = {&INT_CONSTANT,
                         {{BLOB_ARG, {.blob=&const_2}},
                          {RETURNLABEL_ARG, {.label=&i2}},
                          {RESULTREG_ARG, {.regnr=0}}}};

void run(Instruction* entry_point, Register* registers) {
    const Instruction* ip = entry_point;
    Register args[20];
    while (ip) {
        const FunctionDescr* func = ip->function_descr;
        printf("<%s>\n", func->name);
        int argcnt = func->argcnt;
        assert(argcnt <= 20);
        switch (func->kind) {
        case NATIVE_FUNC:
            printf("(native)\n");
            abort();
            break;
        case BUILTIN_FUNC:
            printf("(builtin)\n");
            for (int i=0; i<argcnt; i++) {
                const ArgumentDescr* arg = &ip->arguments[i];
                switch (ip->arguments[i].kind) {
                case REGISTER_ARG:    args[i] = registers[arg->value.regnr]; break;
                case RESULTREG_ARG:   args[i].p = &registers[arg->value.regnr]; break;
                case RETURNLABEL_ARG: args[i].p = arg->value.label;
                case BLOB_ARG:        args[i].p = arg->value.blob;
                }//switch
            }
            Label next = func->function.builtin(args);
            printf("(done)\n");
            ip = next;
            break;
        case COMPOSITE_FUNC:
            printf("(composite)\n");
            abort();
            break;
        }//switch
    }
}

int main(int argc, const char** argv) {
    Register reg[10];

    run(&i1, reg);
    return 0; //TODO
}
