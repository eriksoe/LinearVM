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
     Prg=[X|RestPrg], read_module(Stream, RestPrg)).

set_precedences :-
    % Builtin: ':' at 600
    op(550, xfy, '=>'),
    op(575, xfy, '//'),
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
    (unused_name(TName, Env) -> valid_type_exp(TExp, Env);
     throw(duplicate_name(Tname))).
typecheck_item(import_type(TName/Arity), Env, Env2) :-
    typecheck_item(import_type(TName/Arity, TName), Env, Env2).
typecheck_item(import_type(TName/Arity, Alias), Env,
               [Alias:type(imported(Arity))]) :-
    (unused_name(Alias, Env);
     throw(duplicate_name(Tname))).
typecheck_item(import_function(FName : FType), Env, Env2) :-
    typecheck_item(import_function(FName : FType, FName), Env, Env2).
typecheck_item(import_function(FName : FType, Alias), Env, Env2) :-
    true, Env2=Env. % TODO
typecheck_item(def_const_type(_,_,_), Env, Env) :-
    true. % TODO.
typecheck_item(function(FName : ('=>'(Inputs,Outputs)), _Body), Env, Env) :-
    Env2 = Env. % TODO

valid_type_exp('=>'(TV,TExp), Env) :- valid_type_exp(TExp, [TV:type(typevar)|Env]).
%valid_type_exp(typevar(_), _Env) :- !.
valid_type_exp(TV, Env) :- atom(TV), lookup(TV, Env, type(_)), !.
valid_type_exp('{}', _Env).
valid_type_exp('{}'(FieldsCS), Env) :-
    commasep_to_list(FieldsCS, Fields),
    valid_fields(Fields, Env, []).
valid_type_exp(TExp, Env) :-
    format("Not valid_type_exp: ~p in ~p\n", [TExp, Env]), fail.

valid_fields([], _Env, _UsedNames).
valid_fields([K:T | Rest], Env, UsedNames) :-
    unused_name(K, UsedNames),
    valid_type_exp(T, Env),
    valid_fields(Rest, Env, [K:T|UsedNames]).

unused_name(_Name, []).
unused_name(Name, [K:_|Rest]) :- Name \== K, unused_name(Name, Rest).

lookup(Name, [K:V|_], V) :- Name == K, !.
lookup(Name, [_|Rest], V) :- lookup(Name, Rest, V).

commasep_to_list(','(H,CST), [H|LT]) :- !, commasep_to_list(CST,LT).
commasep_to_list(X, [X]).

:- initialization(set_precedences).
:- initialization(main).
