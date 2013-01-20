-module(linearvm_assembler).
-export([assemble/1, assemble_file/1]).

-include("../include/linearvm_compiler.hrl").
-include("../include/linearvm_asmconstants.hrl").

assemble_file(Filename) ->
    io:format("cwd=~p, filename=~p\n", [file:get_cwd(), Filename]),
    {ok, [Prg]} = file:consult(Filename),
    assemble(Prg).

assemble(#numeric_program{symbols=Symbols,
                          names=Names,
                          type_imports=TypeImports,
                          type_defs=TypeDefs,
                          fun_imports=FunImports,
                          fun_decls=FunDecls,
                          fun_exports=FunExports,
                          constant_source=ConstantSource,
                          constant_decoders=ConstantDecoders,
                          constant_table=ConstantTable,
                          fun_code=FunCode
                         }=_Prg) ->
    Chunks = [encode_symbols_chunk(Symbols),
              encode_names_chunk(Names),
              encode_typeimports_chunk(TypeImports),
              encode_typedefs_chunk(TypeDefs),
              encode_funimports_chunk(FunImports),
              encode_fundecls_chunk(FunDecls),
              encode_funexports_chunk(FunExports),
              encode_constantdecoders_chunk(ConstantDecoders),
              encode_constantsource_chunk(ConstantSource),
              encode_constanttable_chunk(ConstantTable),
              encode_funcode_chunk(FunCode)
             ],
    Chunks.



%%%========== Chunk encoders ========================================
encode_symbols_chunk(Symbols) ->
    [encode_int(length(Symbols))] ++
    [[encode_int(length(S)) | S]
     || S <- Symbols].

encode_names_chunk(Names) ->
    [encode_int(length(Names))] ++
    [[encode_int(Parent), encode_int(Name)]
     || {Parent, Name} <- Names].

encode_typeimports_chunk(TypeImports) ->
    [encode_int(length(TypeImports))] ++
    [[encode_int(TypeNr), encode_int(Arity)]
     || {TypeNr, Arity} <- TypeImports].

encode_typedefs_chunk({BaseNr,TypeDefs}) ->
    [encode_int(BaseNr), encode_int(length(TypeDefs))] ++
    ['TODO'
     || Type <- TypeDefs].

encode_funimports_chunk(FunImports) ->
    [encode_int(length(FunImports))] ++
    [[encode_int(Name), encode_int(Sig)]
     || {Name,Sig} <- FunImports].

encode_fundecls_chunk({BaseNr, FunDecls}) ->
    [encode_int(BaseNr), encode_int(length(FunDecls))] ++
    [[encode_int(Name), encode_int(Sig), encode_int(Label)]
     || {Name,Sig,Label} <- FunDecls].

encode_funexports_chunk(FunExports) ->
%% TODO: Should we constrain the exported functions to be together in a range?
    [encode_int(length(FunExports))] ++
    [[encode_int(Fun)]
     || Fun <- FunExports].

encode_constantdecoders_chunk(Decoders) ->
    [encode_int(length(Decoders))] ++
    [[encode_int(ByteSize), encode_int(DecodeFun), encode_int(CopyFun)]
     || {ByteSize, DecodeFun, CopyFun} <- Decoders].

encode_constantsource_chunk(Blob) ->
    Blob.

encode_constanttable_chunk(Constants) ->
    [encode_int(length(Constants))] ++
    [case C of
         {Type,ByteSize} -> [encode_int(Type), encode_int(ByteSize)];
         {Type}          -> [encode_int(Type)]
     end
     || C <- Constants].

encode_funcode_chunk(Functions) ->
    [encode_int(length(Functions))] ++
        [encode_function_body(F) || F <- Functions].

encode_function_body({LabelCount, Insns}) ->
    Body = [encode_instruction(I) || I <- Insns],
    [encode_int(LabelCount), encode_int(iolist_size(Body)) | Body].

encode_instruction({f, Fun, InOuts,Ins,[NormalOut | OtherOuts]}) ->
    [encode_tagged(?ARG_TAG_FUN, Fun),
     [encode_tagged(?ARG_TAG_REG, X) || X <- InOuts],
     [encode_tagged(?ARG_TAG_REG, X) || X <- Ins],
     [encode_tagged(?ARG_TAG_REG, X) || X <- NormalOut],
     [encode_call_returnspec(X) || X <- OtherOuts]
    ];
encode_instruction({c, Constant, OutReg}) ->
    [encode_tagged(?ARG_TAG_CONSTANT, Constant),
     encode_tagged(?ARG_TAG_REG, OutReg)];
encode_instruction({l, Label}) ->
    [encode_tagged(?ARG_TAG_LABEL, Label)];
encode_instruction({r, ReturnLabel, Regs}) ->
    [encode_tagged(?ARG_TAG_RETURN, ReturnLabel),
     [encode_tagged(?ARG_TAG_REG, X) || X <- Regs]].

encode_call_returnspec({Label, Outs}) ->
    [encode_tagged(?ARG_TAG_LABEL, Label)] ++
    [encode_tagged(?ARG_TAG_REG, X) || X <- Outs].


%%%========== Low-level encoders ========================================
encode_tagged(Tag,N) when is_integer(N), N>=0,
                          is_integer(Tag), Tag>=0, Tag<16 ->
    encode_int((N bsl 4) + Tag).

encode_int(N) when is_integer(N), N>=0 ->
    if N < 128 ->
            [N];
       true ->
            [(N band 16#7F) bor 16#80
             | encode_int(N bsr 8)]
    end.
