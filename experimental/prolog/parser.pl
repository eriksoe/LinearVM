% -*- mode: prolog -*-

%% Compile with: gplc parser.pl --no-top-level

% Reset precedences.
:- op(1050,xfy,'->').

main :- compile_from_stream(user_input).

main(FileName) :-  %"../samples/hello.livm2"
    open(FileName, read, SrcStream),
    compile_from_stream(SrcStream).

compile_from_stream(SrcStream) :-
    parse(SrcStream, Prg),
    format("Got module: ~p\n", [Prg]),
    (desugar_program(Prg, DPrg) -> !;
     throw(desugaring_failed(Prg))),
    format("Desugared module: ~p\n", [DPrg]),
    typecheck(DPrg),
    format("Program is OK.\n", []).

%%%==================== Reading / parsing ====================
parse(Stream, Prg) :-
    set_precedences,
    read_module(Stream, Prg).

read_module(Stream, Prg) :-
    read(Stream, X), !,
    (X==end_of_file -> !, Prg=[];
     format("got: ~p\n", [X]),
     %write_canonical(X), nl,
     Prg=[X|RestPrg], read_module(Stream, RestPrg)).

set_precedences :-
    % Builtin: ':' at 600
    op(575, xfy, '->'),
    op(550, xfy, '=>'),
    op(525, xfy, '~>'),
    op(100, yfx, '::'),
    op(200, fx, '&'),
    true.

%%%==================== Desugaring ====================
%%% Desugars the following:
%%% - Import aliases: optional -> mandatory
%%% - Function signatures:
%%%   - ins/outs: optional list wrapping -> mandatory
%%%   - '~>' to lists.
%%% - Types:
%%%   - '{...}' to struct(Fields)

desugar_program(Prg, Prg2) :-
    map_all(desugar_declaration, Prg, Prg2).

%% Aliases:
desugar_declaration(import_type(TName/Arity), R) :-
    !, desugar_declaration(import_type(TName/Arity, TName), R).
desugar_declaration(import_function(FName : FSig), R) :-
    !, desugar_declaration(import_function(FName : FSig, FName), R).
%% Types:
desugar_declaration(typedef(TName, Type),
                    typedef(TName, DType)) :-
    !, desugar_type(Type,DType).
desugar_declaration(import_function(FName : FSig, Alias),
                    import_function(FName : DFSig, Alias)) :-
    !, desugar_signature(FSig, DFSig).
%% Default:
desugar_declaration(function(FName : FSig, Body),
                    function(FName : DFSig, DBody)) :-
    !, desugar_signature(FSig, DFSig), desugar_function_body(Body, DBody).
desugar_declaration(X,X).

desugar_signature('->'(Ins,Outs), '->'(DIns,DOuts)) :-
    desugar_type_list(Ins, DIns),
    desugar_returns_list(Outs, DOuts).

desugar_returns_list([], [[]]) :- !.
desugar_returns_list(Rs, DRs) :- desugar_returns_list2(Rs, DRs).

desugar_returns_list2('~>'(Ts,Rs), [DTs|DRs]) :-
    !, desugar_type_list(Ts, DTs),
    desugar_returns_list2(Rs, DRs).
desugar_returns_list2(Ts, [DTs]) :-
    !, desugar_type_list(Ts, DTs).

desugar_type_list(Ts, DTs) :- list(Ts), !, map_all(desugar_type, Ts, DTs).
desugar_type_list(T, DTs) :- desugar_type_list([T], DTs). % Auto-wrap case.

desugar_type('{}', struct([])) :- !.
desugar_type('{}'(FieldsCS), struct(Fields)) :-
    !, commasep_to_list(FieldsCS, Fields).
desugar_type('=>'(TV,TExp), '=>'(TV,DTExp)) :- !, desugar_type(TExp, DTExp).
desugar_type('&'(TExp), '&'(DTExp)) :- !, desugar_type(TExp, DTExp).
desugar_type(T,T).

desugar_function_body(Body, DBody) :-
    map_all(desugar_instruction, Body, DBody).

desugar_instruction(return(Vs), return(0, Vs)).
desugar_instruction(X, X).

commasep_to_list(','(H,CST), [H|LT]) :- !, commasep_to_list(CST,LT).
commasep_to_list(X, [X]).

%%%==================== Type checking ====================
typecheck(Prg) :- typecheck(Prg, []).

typecheck([], _Env).
typecheck([H|T], Env) :-
    (typecheck_item(H, Env, Env2) -> !, typecheck(T, Env2);
     throw(item_did_not_typecheck(H, Env))).

typecheck_item(module(_,_), E, E).
typecheck_item(typedef(TName,TExp), Env, [TName:_TypeDef|Env]) :-
    unused_name_or_fail(TName, Env), valid_type_exp(TExp, Env).
typecheck_item(import_type(TName/Arity, Alias), Env,
               [Alias:type(imported(Arity)) | Env]) :-
    unused_name_or_fail(Alias, Env).
typecheck_item(import_function(FName : FType, Alias), Env,
               [Alias:function(FType, imported)|Env]) :-
    unused_name_or_fail(Alias, Env),
    (valid_signature(FType, Env) -> !;
     throw(invalid_function_signature(FType))).
typecheck_item(def_const_type(CTName,InitFunName,CopyFunName), Env,
               [CTName:constant_type(TConst,InitFunName, CopyFunName) | Env]) :-
    (lookup(InitFunName, Env, function(InitFunType,_)) -> !;
     throw(bad_def_const_type(CTName, undefined_function(InitFunName, Env)))),
    (lookup(CopyFunName, Env, function(CopyFunType,_)) -> !;
     throw(bad_def_const_type(CTName, undefined_function(CopyFunName, Env)))),

    (is_const_init_signature(InitFunType, TPrepared),
     is_const_copy_signature(CopyFunType, TPrepared, TConst) -> !;
     throw(bad_def_const_type(CTName, bad_function_signatures(InitFunType,CopyFunType)))).
typecheck_item(function(FName : ('->'(Inputs,Outputs)), Body), Env,
               [FName:function(('->'(Inputs,Outputs)), Body)|Env]) :-
    check_formal_arg_list(Inputs, Env, Locals),
    check_return_type_list(Outputs, Env, Returns),
    %% TODO: Defer function body checking till all signatures have been collected.
    check_function_body(Body, Locals, Returns, Env).

%%%==== def_const_type helpers:
is_const_init_signature('->'(['&'(bytestring)], [[T], []]), T) :- !.
is_const_init_signature('->'(['&'(bytestring)], [[T]]), T) :- !.

is_const_copy_signature('->'(['&'(T)], [[TConst]]), T, TConst).

%%%==== function helpers:
check_formal_arg_list([], _Env, []).
check_formal_arg_list([V:T | VTs], Env, [V:T | RestLocals]) :-
    check_formal_arg_list(VTs, Env, RestLocals), !,
    unused_name_or_fail(V, RestLocals),
    valid_arg_type(T, Env).

check_return_type_list(Outputs, Env, Returns) :-
    check_return_type_list(Outputs, Env, 0, Returns).

check_return_type_list([], _Env, _Nr, []).
check_return_type_list([RT|RTs], Env, Nr, [Nr:RT|Returns]) :-
    !,
    valid_type_exp_list(RT, Env),
    Nr1 is Nr+1,
    check_return_type_list(RTs, Env, Nr1, Returns).

check_function_body(Body, Locals, Returns, Env) :-
    Labels=[],
    check_function_body(Body, Locals, Labels, Returns, Env).

check_function_body([], _Locals, _Labels, _Returns, _Env).
check_function_body([Insn|Insns], Locals, Labels, Returns, Env) :-
    check_instruction(Insn,
                      (Locals, Labels), Returns, Env,
                      (Locals2, Labels2)),
    check_function_body(Insns, Locals2, Labels2, Returns, Env).

check_instruction(Insn,
                  (Locals, Labels), Returns, Env,
                  (Locals2, Labels2)) :-
    format("DB| check_instruction: ~p\n", [Insn]),
    ((Insn = constant(ConstTypeName, _RawData, OutReg)) ->
         !, lookup(ConstTypeName, Env, constant_type(TConst,_,_)),
         Locals2 = [OutReg:TConst | Locals],
         Labels2 = Labels;
     Insn = call(FName, Args, Outs) ->
         !,
         (lookup(FName, Env, function('->'(Ins,Outss), _)) -> !;
          throw(undefined_function(FName, Env))),
         (read_locals(Args, Ins, Locals, LocalsAfterPass) -> !;
          throw(cannot_pass_arguments(Args, Ins, Locals))),
         %% TODO: Handle return values
         Locals2 = LocalsAfterPass,
         Labels2 = Labels;
     Insn = return(Nr, Args) ->
         !, lookup(Nr, Returns, RetTypes),
         read_locals(Args, RetTypes, Locals, LocalsAfterReturn),
         (locals_contains_only_refs(LocalsAfterReturn) -> !;
          throw(registers_are_alive_at_return(LocalsAfterReturn))),
         (Locals2, Labels2)=(unreachable, Labels); % TODO
     Insn = label(LabelName) ->
         !, (Locals2, Labels2)=(Locals, Labels); % TODO
     throw(unknown_instruction(Insn)));
    throw(instruction_did_not_typecheck(Insn, (Locals, Returns, Env))).

read_locals([], [], Locals, Locals) :- !.
read_locals([V|Vs], ['&'(T)|Ts], Locals, Locals2) :-
    %% Handle "T is ref type" case.
    !,
    (lookup(V, Locals, VType) -> !;
     throw(cannot_read_local(V, Locals))),
    ('&'(VType2)=VType, check_assignable(VType2, T) -> !; % "Ref-to-ref" case
     check_assignable(VType, T) -> !;                     % "Nonref-to-ref" case
     throw(type_mismatch(V, VType, T))),
    read_locals(Vs, Ts, Locals, Locals2).
read_locals([V|Vs], [T|Ts], Locals, Locals2) :-
    !,
    (lookup_and_remove(V, Locals, VType, LocalsTmp) -> !;
     throw(cannot_read_local(V, Locals))),
    (check_assignable(VType, T) -> !;
     throw(type_mismatch(V, VType, T))),
    read_locals(Vs, Ts, LocalsTmp, Locals2).
read_locals(Vs, Ts, _Locals, _) :-
    throw(type_number_mismatch(Vs, Ts)).

locals_contains_only_refs([]).
locals_contains_only_refs([_V:'&'(_) | VTs]) :- locals_contains_only_refs(VTs).

check_assignable(T,T).
check_assignable(FromT, ToT) :-
    format("DB| check_assignable fails for (~p,~p)\n", [FromT, ToT]), fail.

%%%==== Type syntax helpers:
valid_signature('->'(Args, Result), Env) :-
    valid_type_exp_withrefs_list(Args, Env),
    valid_return_type(Result, Env).

valid_return_type(Tss, Env) :- all_are_1(valid_type_exp_withrefs_list, Tss, Env).

valid_type_exp_withrefs_list(L, Env) :- all_are_1(valid_type_exp_withrefs, L, Env).
valid_type_exp_list(L, Env) :- all_are_1(valid_type_exp, L, Env).

valid_type_exp_withrefs('&'(T), Env) :- !, valid_type_exp(T, Env).
valid_type_exp_withrefs(T, Env) :- valid_type_exp(T, Env).

valid_type_exp('=>'(TV,TExp), Env) :- valid_type_exp(TExp, [TV:type(typevar)|Env]).
valid_type_exp(TV, Env) :- atom(TV), lookup(TV, Env, type(_)), !.
valid_type_exp(struct(Fields), Env) :- valid_fields(Fields, Env, []), !.
valid_type_exp(TExp, Env) :-
    format("Not valid_type_exp: ~p in ~p\n", [TExp, Env]), fail.

valid_arg_type('&'(T), Env) :- !, valid_type_exp(T, Env).
valid_arg_type(T, Env) :- valid_type_exp(T, Env).

valid_fields([], _Env, _UsedNames).
valid_fields([K:T | Rest], Env, UsedNames) :-
    unused_name(K, UsedNames),
    valid_type_exp(T, Env),
    valid_fields(Rest, Env, [K:T|UsedNames]).


unused_name_or_fail(Name, Env) :-
    unused_name(Name,Env) -> !;
    throw(duplicate_name(Name, Env)).

unused_name(_Name, []).
unused_name(Name, [K:_|Rest]) :- Name \== K, unused_name(Name, Rest).

lookup(Name, [K:V|_], V) :- Name == K, !.
lookup(Name, [_|Rest], V) :- lookup(Name, Rest, V).

lookup_and_remove(Name, [K:V|Rest], V, Rest) :- Name == K, !.
lookup_and_remove(Name, [KV|Rest], V, [KV|Rest2]) :-
    lookup_and_remove(Name, Rest, V, Rest2).

map_all(_F, [], []).
map_all(F, [H|T], [RH|RT]) :- call(F, H, RH), map_all(F, T, RT).

%all_are(_F, []).
%all_are(F, [H|T]) :- call(F,H), all_are(F,T).
all_are_1(_F, [], _Arg2).
all_are_1(F, [H|T], Arg2) :- call(F,H,Arg2), all_are_1(F,T,Arg2).

:- initialization(set_precedences).
:- initialization(main).

