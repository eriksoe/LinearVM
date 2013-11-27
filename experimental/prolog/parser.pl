% -*- mode: prolog -*-

%% Compile with: gplc parser.pl --no-top-level

main :-
    parse(user_input, Prg),
    format("Got module: ~p\n", [Prg]),
    typecheck(Prg),
    format("Program is OK.\n", []).

%%%==================== Reading / parsing ====================
parse(Stream, Prg) :-
    set_precedences,
    read_module(Stream, Prg).

read_module(Stream, Prg) :-
    read(Stream, X), !,
    (X==end_of_file -> Prg=[];
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

%%%==================== Type checking ====================
typecheck(Prg) :- typecheck(Prg, []).

typecheck([], _Env).
typecheck([H|T], Env) :-
    (typecheck_item(H, Env, Env2) -> typecheck(T, Env2);
     throw(item_did_not_typecheck(H, Env))).

typecheck_item(module(_,_), E, E).
typecheck_item(typedef(TName,TExp), Env, [TName:_TypeDef|Env]) :-
    unused_name_or_fail(TName, Env), valid_type_exp(TExp, Env).
typecheck_item(import_type(TName/Arity), Env, Env2) :-
    typecheck_item(import_type(TName/Arity, TName), Env, Env2).
typecheck_item(import_type(TName/Arity, Alias), Env,
               [Alias:type(imported(Arity)) | Env]) :-
    unused_name_or_fail(Alias, Env).
typecheck_item(import_function(FName : FType), Env, Env2) :-
    typecheck_item(import_function(FName : FType, FName), Env, Env2).
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
     is_const_copy_signature(CopyFunType, TPrepared, TConst);
     throw(bad_def_const_type(CTName, bad_function_signatures(InitFunType,CopyFunType)))).
typecheck_item(function(FName : ('->'(Inputs,Outputs)), Body), Env,
               [FName:function(('->'(Inputs,Outputs)), Body)|Env]) :-
    check_formal_arg_list(Inputs, Env, Locals),
    check_function_body(Body, Locals, Env),
    Env2 = Env. % TODO

%%%==== def_const_type helpers:
is_const_init_signature('->'(['&'(bytestring)], '~>'(T, [])), T) :- !.
is_const_init_signature('->'(['&'(bytestring)], T), T) :- !.

is_const_copy_signature('->'(['&'(T)], _), T, TConst).

%%%==== function helpers:
check_formal_arg_list([], _Env, []).
check_formal_arg_list([V:T | VTs], Env, [V:T | RestLocals]) :-
    check_formal_arg_list(VTs, Env, RestLocals), !,
    unused_name_or_fail(V, RestLocals),
    valid_arg_type(T, Env).

check_function_body(Body, Locals, Env) :-
    true. %TODO

%%%==== Type syntax helpers:
valid_signature('->'(Args, Result), Env) :-
    valid_type_exp_list(Args, Env),
    valid_return_type(Result, Env).

valid_return_type('~>'(T1,T2), Env) :-
    !, valid_type_exp(T1, Env), valid_return_type(T2, Env).
valid_return_type(TL, Env) :- list(TL), !, valid_type_exp_list(TL, Env).
valid_return_type(T, Env) :- valid_type_exp(T, Env).
valid_return_type(T, Env) :-
    (valid_type_exp(T, Env) -> !;
     throw(invalid_return_type(T))).

valid_type_exp_list(L, Env).% :- all_are(valid_type_exp, L, Env).

valid_type_exp('=>'(TV,TExp), Env) :- valid_type_exp(TExp, [TV:type(typevar)|Env]).
valid_type_exp(TV, Env) :- atom(TV), lookup(TV, Env, type(_)), !.
valid_type_exp('{}', _Env).
valid_type_exp('{}'(FieldsCS), Env) :-
    commasep_to_list(FieldsCS, Fields),
    valid_fields(Fields, Env, []), !.
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

commasep_to_list(','(H,CST), [H|LT]) :- !, commasep_to_list(CST,LT).
commasep_to_list(X, [X]).

%map_all(_F, [], []).
%map_all(F, [H|T], [RH|RT]) :- call(F, H, RH), map_all(F, T, RT).

%all_are(_F, []).
%all_are(F, [H|T]) :- call(F,H), all_are(F,T).
all_are_1(_F, [], _Arg2).
all_are_1(F, [H|T], Arg2) :- call(F,H,Arg2), all_are_1(F,T,Arg2).

:- initialization(set_precedences).
:- initialization(main).

