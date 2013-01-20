-ifndef(linearvm_asmconstants_hrl).
-define(linearvm_asmconstants_hrl, 1).

%%%==========  Tags for function opcodes/arguments ====================
-define(ARG_TAG_REG,      0).
-define(ARG_TAG_CONSTANT, 1).
-define(ARG_TAG_FUN,      2).
-define(ARG_TAG_LABEL,    3).
-define(ARG_TAG_RETURN,   4).

%%%==========  Tags for type categories ==============================
%%% Group 'type variables'
-define(TYPE_TAG_VARIABLE,    0).
-define(TYPE_TAG_APPLICATION, 1).
-define(TYPE_TAG_EXISTENTIAL, 2).
-define(TYPE_TAG_LETREC,      3).

%%% Group 'regular types'
-define(TYPE_TAG_STRUCT,      4).
-define(TYPE_TAG_VARIANT,     5).
-define(TYPE_TAG_VTABLE,      6).

%%% Group 'special'
-define(TYPE_TAG_SPECIAL,     8). % e.g. 'bottom'
-define(TYPE_TAG_RESTRAIN,    9).

-endif.
