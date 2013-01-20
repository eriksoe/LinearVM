-ifndef(linearvm_compiler_hrl).
-define(linearvm_compiler_hrl, 1).

-record(numeric_program,
        {
          symbols,
          names,

          type_imports,
          type_defs,

          fun_sigs,
          fun_imports,
          fun_decls,
          fun_exports,

          constant_source,
          constant_decoders,
          constant_table,
          % vtable

          fun_code
        }).


-endif.
