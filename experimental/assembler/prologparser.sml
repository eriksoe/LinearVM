local structure T=Tokenizer in
structure PrologStyleParser =
struct
datatype Fixity = PREFIX | INFIX_L | INFIX_R | POSTFIX
type Priority = int
type Synspec = (string * (Fixity * Priority)) list
type Position = T.position

(* Concrete syntax tree *)
(*
datatype CST = INT of int
             | STRING of string
             | NODE of string * CST list
             | LIST of CST list
 *)
datatype CST' = INT of int
             | STRING of string
             | NODE of string * CST list
             | LIST of CST list
withtype CST = Position * CST'

datatype ParserStackElem = PREFIX_ELEM of Position * string * Priority
                         | INFIX_ELEM of Position * string * Priority
                         | CTOR_AWAITING_ARGS of Position * string
                         | CTOR_COLLECTING_ARGS of Position * string * CST list
                         | START_PAREN of Position
                         | LIST_ACC of Position * CST list
                         | START_CURLY of Position
                         | SUBTREE of CST;

type ParserStack = ParserStackElem list
type ParserState = CST list * ParserStack

val infinitePrio = 1000000;
val commaPrio = 1000;

fun lookup key [] = NONE
  | lookup key ((k,v)::rest) =
    if k=key then SOME v else lookup key rest;

fun cst'2str(INT v) = "INT("^(Int.toString v)^")"
  | cst'2str(STRING v) = "STRING("^v^")"
  | cst'2str(NODE(tag,children)) = tag^"("^(String.concatWith ", " (map cst2str children))^")"
  | cst'2str(LIST vs) = "["^(String.concatWith "," (map cst2str vs))^"]"
and cst2str(pos,cst') = cst'2str(cst')

fun stack2str([]) = "Stack is empty.\n"
  | stack2str([SUBTREE v]) = "Stack has one element: "^cst2str v^"\n"
  | stack2str((CTOR_AWAITING_ARGS _)::_) = "Stack has CTOR_AWAITING_ARGS as top element.\n"
  | stack2str((CTOR_COLLECTING_ARGS _)::_) = "Stack has CTOR_COLLECTING_ARGS as top element.\n"
  | stack2str((PREFIX_ELEM _)::_) = "Stack has PREFIX_ELEM as top element.\n"
  | stack2str((INFIX_ELEM _)::rest) = "Stack has INFIX_ELEM as top element. Rest of the stack is: "^stack2str rest^"\n"
  | stack2str((SUBTREE v)::rest) = "Stack has SUBTREE as top element; value is "^cst2str v^". Rest of the stack is: "^stack2str rest^"\n"
  | stack2str((START_PAREN _)::rest) = "Stack has '(' as top element. Rest of the stack is: "^stack2str rest^"\n"
  | stack2str((LIST_ACC _)::rest) = "Stack has '[' as top element. Rest of the stack is: "^stack2str rest^"\n"
  | stack2str((START_CURLY _)::rest) = "Stack has '{' as top element. Rest of the stack is: "^stack2str rest^"\n";

fun print_stack s = print(stack2str s)
(* fun print_stack([]) = print "Stack is empty.\n" *)
(*   | print_stack([SUBTREE v]) = print("Stack has one element: "^cst2str v^"\n") *)
(*   | print_stack((CTOR_AWAITING_ARGS _)::_) = print "Stack has CTOR_AWAITING_ARGS as top element.\n" *)
(*   | print_stack((CTOR_COLLECTING_ARGS _)::_) = print "Stack has CTOR_COLLECTING_ARGS as top element.\n" *)
(*   | print_stack((PREFIX_ELEM _)::_) = print "Stack has PREFIX_ELEM as top element.\n" *)
(*   | print_stack((INFIX_ELEM _)::_) = print "Stack has INFIX_ELEM as top element.\n"; *)

fun is_prefix(PREFIX) = true
  | is_prefix _ = false
fun is_nonprefix fixity = not(is_prefix fixity)

fun make_simple_consumer synspec =
    let fun step(stack, token:T.postoken) = token::stack
        fun finalize(stack) =
            (List.app (fn (_,t)=>  print(T.tok2s t^"...\n")) stack; stack)
    in
        {initState=[],
         step=step,
         finalize=finalize}
    end


fun make_stackbased_consumer synspec : (ParserState, CST list) T.consumer =
    let
        val synspec_prefix = List.filter (fn (_,(fixity,_)) => is_prefix fixity) synspec;
        val synspec_inpostfix = List.filter (fn (_,(fixity,_)) => is_nonprefix fixity) synspec;

        fun reduce_for_priority(prio, stack) =
            case stack of
                (CTOR_AWAITING_ARGS(pos, v))::rest =>
                reduce_for_priority(prio,
                                    SUBTREE(pos, NODE(v,[]))::rest)
              | (SUBTREE y)::(INFIX_ELEM(pos,f,fprio))::(SUBTREE x)::rest =>
                if fprio<=prio  (* TODO: handle difference-of-1 case *)
                then reduce_for_priority(prio,
                                         (SUBTREE(pos,NODE(f,[x,y])))::rest)
                else stack
              | (SUBTREE x)::(PREFIX_ELEM(pos,f,fprio))::rest =>
                if fprio<=prio  (* TODO: handle difference-of-1 case *)
                then reduce_for_priority(prio,
                                         (SUBTREE(pos,NODE(f,[x])))::rest)
                else stack
              | _ => stack;

        fun handle_infix_or_postfix(operator,pos,synspec,stack) =
            (case lookup operator synspec_inpostfix of
                 SOME(INFIX_L, prio) =>
                 (INFIX_ELEM(pos,operator,prio))::reduce_for_priority(prio,stack)
               | SOME(INFIX_R, prio) =>
                 (INFIX_ELEM(pos,operator,prio))::reduce_for_priority(prio-1,stack)
               | SOME(POSTFIX, prio) =>
                 (case reduce_for_priority(prio-1,stack) of
                     (SUBTREE x)::stack2 =>
                     (SUBTREE(pos, NODE(operator, [x])))::stack2
                 )
               | _ =>                    (* Unknown or postfix *)
                 raise T.SyntaxError(pos, "Unexpected operator: "^operator^" ; context is "^stack2str stack) (* TODO: Provide context from stack *)
            )

        fun handle_prefix_or_nonop(stack, operator, pos) =
            (case lookup operator synspec_prefix of
                 SOME(PREFIX,prio) =>
                 (PREFIX_ELEM(pos,operator,prio))::stack (* TODO: check nonassoc clash? *)
               |  NONE => (CTOR_AWAITING_ARGS(pos, operator))::stack
            )
        fun handle_op(stack as (SUBTREE n)::_, operator, pos) =
            handle_infix_or_postfix(operator,pos,synspec,stack)
          | handle_op(stack, operator, pos) =
            handle_prefix_or_nonop(stack, operator, pos)

fun step((CTOR_AWAITING_ARGS(pos_a, tag))::stack, (pos, T.SPECIAL(#"("))) =
    CTOR_COLLECTING_ARGS(pos_a, tag,[])::stack
  | step((CTOR_AWAITING_ARGS(pos_a, tag))::stack, othertoken) =
    (* atom-without-args case. *)
    step(SUBTREE(pos_a, NODE(tag,[]))::stack, othertoken)
  | step((CTOR_COLLECTING_ARGS(pos_a,tag,[]))::stack, (pos_z, T.SPECIAL(#")"))) =
    (* Special case: 0-ary constructor used with parentheses *)
    SUBTREE(pos_a, NODE(tag,[]))::stack
  | step(stack, postoken as (pos,T.SPECIAL #")")) =
    (case reduce_for_priority(infinitePrio,stack) of
         (SUBTREE v)::(CTOR_COLLECTING_ARGS(pos_a, tag,rargs))::stack2 =>
         SUBTREE(pos_a, NODE(tag,rev(v::rargs)))::stack2
       | (SUBTREE v)::(START_PAREN _)::stack2 =>
         (SUBTREE v)::stack2
       | _ =>
         raise T.SyntaxError(pos, "Unexpected \")\" when stack is "^stack2str stack)
    )
  | step(stack, postoken as (pos,T.SPECIAL #"]")) =
    (case reduce_for_priority(infinitePrio,stack) of
         (LIST_ACC(pos_a,vs))::stack2 =>
         (SUBTREE(pos_a, LIST(rev vs)))::stack2
       | (SUBTREE v)::(LIST_ACC(pos_a, vs))::stack2 =>
         (SUBTREE(pos_a, LIST(rev(v::vs))))::stack2
       | _ =>
         raise T.SyntaxError(pos, "Unexpected \"]\" when stack is "^stack2str stack)
    )
  | step(stack, postoken as (pos_z,T.SPECIAL #"}")) =
    (case reduce_for_priority(infinitePrio,stack) of
         (START_CURLY pos_a)::stack2 =>
         (SUBTREE(pos_a,NODE("{}",[])))::stack2
       | (SUBTREE v)::(START_CURLY pos_a)::stack2 =>
         (SUBTREE(pos_a,NODE("{}",[v])))::stack2
       | _ =>
         raise T.SyntaxError(pos_z, "Unexpected \"}\" when stack is "^stack2str stack)
    )
  | step(stack, postoken as (pos,T.SPECIAL #",")) =
    (case reduce_for_priority(commaPrio,stack) of
         (SUBTREE v)::(CTOR_COLLECTING_ARGS(pos_a,tag,rargs))::stack2 =>
         CTOR_COLLECTING_ARGS(pos_a, tag, v::rargs)::stack2
       | (SUBTREE v)::(LIST_ACC(pos_a,vs))::stack2 =>
         LIST_ACC(pos_a, v::vs)::stack2
       | stack2 =>
         handle_infix_or_postfix(",",pos,synspec,stack2)
    )
  | step(stack, (pos, T.SPECIAL #"(")) = (START_PAREN pos)::stack
  | step(stack, (pos, T.SPECIAL #"[")) = (LIST_ACC(pos, []))::stack
  | step(stack, (pos, T.SPECIAL #"{")) = (START_CURLY pos)::stack
  | step(stack, (pos, T.INT v)) = (SUBTREE(pos,INT v))::stack
  | step(stack, (pos, T.STRINGLIT v)) = (SUBTREE(pos,STRING v))::stack
  | step(stack, (pos, T.WORD v)) = (CTOR_AWAITING_ARGS(pos,v))::stack
  | step(stack, (pos, T.OP v)) = handle_op(stack, v, pos)
  | step(stack, (pos, T.SPECIAL v)) =
        raise T.SyntaxError(pos, "Unexpected \""^Char.toString v^"\" when stack is "^stack2str stack)

fun step2((terms,stack), (pos,T.OP ".")) =
    (* Terminator symbol. *)
    (case reduce_for_priority(infinitePrio,stack) of
        [SUBTREE v] => (v::terms, [])
      | stack' =>
        raise T.SyntaxError(pos,"\".\" not expected ; context is "^stack2str stack) (* TODO: add description of open constructs *)
    )
  | step2((terms,stack), token) =
    (terms, step(stack, token))

fun finalize (terms,[]) = rev terms
  | finalize (terms,stack) =
    raise T.SyntaxError((~1,~1), "Unexpected EOF: "^stack2str stack)

    in
        {initState=([],[]),
         step=step2,
         finalize=finalize}
    end

fun parse_string synspec s =
    let val consumer : (ParserState, CST list) T.consumer =
            make_stackbased_consumer synspec
    in T.tokenize_string(s,consumer)
    end

fun parse_file synspec filename =
    let val consumer : (ParserState, CST list) T.consumer =
            make_stackbased_consumer synspec
            (* {initState=(), *)
            (*  step=(fn ((),(_,t)) => print(T.tok2s(t)^"\n")), *)
            (*  finalize=fn() => ()} *)
    in T.tokenize_file(filename,consumer)
    end
end;
end;
