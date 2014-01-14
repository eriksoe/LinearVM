local structure T=Tokenizer in
structure PrologStyleParser =
struct
datatype Fixity = PREFIX | INFIX_L | INFIX_R | POSTFIX
type Priority = int
type synspec = (string * (Fixity * Priority)) list

(* Concrete syntax tree *)
datatype CST = INT of int
             | STRING of string
             | NODE of string * CST list
datatype ParserStackElem = PREFIX_ELEM of string * Priority
                         | INFIX_ELEM of string * Priority
                         | CTOR_AWAITING_ARGS of string
                         | CTOR_COLLECTING_ARGS of string * CST list
                         | SUBTREE of CST;

type ParserStack = ParserStackElem list

fun lookup key [] = NONE
  | lookup key ((k,v)::rest) =
    if k=key then SOME v else lookup key rest;

fun cst2str(INT v) = "INT("^(Int.toString v)^")"
  | cst2str(STRING v) = "STRING("^v^")"
  | cst2str(NODE(tag,children)) = tag^"("^(String.concat (map cst2str children))^")"

fun stack2str([]) = "Stack is empty.\n"
  | stack2str([SUBTREE v]) = "Stack has one element: "^cst2str v^"\n"
  | stack2str((CTOR_AWAITING_ARGS _)::_) = "Stack has CTOR_AWAITING_ARGS as top element.\n"
  | stack2str((CTOR_COLLECTING_ARGS _)::_) = "Stack has CTOR_COLLECTING_ARGS as top element.\n"
  | stack2str((PREFIX_ELEM _)::_) = "Stack has PREFIX_ELEM as top element.\n"
  | stack2str((INFIX_ELEM _)::rest) = "Stack has INFIX_ELEM as top element. Rest of the stack is: "^stack2str rest^"\n"
  | stack2str((SUBTREE v)::rest) = "Stack has SUBTREE as top element; value is "^cst2str v^". Rest of the stack is: "^stack2str rest^"\n";

fun print_stack s = print(stack2str s)
(* fun print_stack([]) = print "Stack is empty.\n" *)
(*   | print_stack([SUBTREE v]) = print("Stack has one element: "^cst2str v^"\n") *)
(*   | print_stack((CTOR_AWAITING_ARGS _)::_) = print "Stack has CTOR_AWAITING_ARGS as top element.\n" *)
(*   | print_stack((CTOR_COLLECTING_ARGS _)::_) = print "Stack has CTOR_COLLECTING_ARGS as top element.\n" *)
(*   | print_stack((PREFIX_ELEM _)::_) = print "Stack has PREFIX_ELEM as top element.\n" *)
(*   | print_stack((INFIX_ELEM _)::_) = print "Stack has INFIX_ELEM as top element.\n"; *)

fun make_simple_consumer synspec =
    let fun step(stack, token:T.postoken) = token::stack
        fun finalize(stack) =
            (List.app (fn (_,t)=>  print(T.tok2s t^"...\n")) stack; stack)
    in
        {initState=[],
         step=step,
         finalize=finalize}
    end

val init : ParserStack = []


fun make_stackbased_consumer synspec : (ParserStack,CST list) T.consumer =
    let
        fun reduce_for_priority(prio, stack) =
            case stack of
                (* TODO: handle prefix/postfix/infix ops *)
                (CTOR_AWAITING_ARGS v)::rest =>
                reduce_for_priority(prio,
                                    SUBTREE(NODE(v,[]))::rest)
              | (SUBTREE y)::(INFIX_ELEM(f,fprio))::(SUBTREE x)::rest =>
                if fprio<=prio  (* TODO: handle difference-of-1 case *)
                then reduce_for_priority(prio,
                                         (SUBTREE(NODE(f,[x,y])))::rest)
                else stack
              | _ => stack;
fun step((CTOR_AWAITING_ARGS(tag))::stack, (pos, T.SPECIAL(#"("))) =
    CTOR_COLLECTING_ARGS(tag,[])::stack
  | step((CTOR_AWAITING_ARGS(tag))::stack, othertoken) =
    (* atom-without-args case. *)
    step(SUBTREE(NODE(tag,[]))::stack, othertoken)
  | step((SUBTREE v)::(CTOR_COLLECTING_ARGS(tag,rargs))::stack, (pos, T.SPECIAL(#","))) =
    CTOR_COLLECTING_ARGS(tag,v::rargs)::stack
  | step((SUBTREE v)::(CTOR_COLLECTING_ARGS(tag,rargs))::stack, (pos, T.SPECIAL(#")"))) =
    SUBTREE(NODE(tag,rev(v::rargs)))::stack
  | step((CTOR_COLLECTING_ARGS(tag,[]))::stack, (pos, T.SPECIAL(#")"))) =
    (* Special case: 0-ary constructor used with parentheses *)
    SUBTREE(NODE(tag,[]))::stack
  | step(stack, (pos, T.OP ".")) =
    (* Terminator symbol. *)
    (case reduce_for_priority(1000000,stack) of
        [SUBTREE v] => [SUBTREE v]
      | _ => raise T.SyntaxError(pos,"\".\" not expected ; context is "^stack2str stack) (* TODO: add description of open constructs *)
    )
  | step(stack as (SUBTREE a)::_, (pos, T.OP operator)) =
  (* Check for infix/postfix *)
    (case lookup operator synspec of
        SOME(INFIX_L, prio) =>
        (INFIX_ELEM(operator,prio))::reduce_for_priority(prio,stack)
      | SOME(INFIX_R, prio) =>
        (INFIX_ELEM(operator,prio))::reduce_for_priority(prio+1,stack)
      | _ =>                    (* Unknown or postfix *)
        raise T.SyntaxError(pos, "Unexpected operator: "^operator^" ; context is "^stack2str stack) (* TODO: Provide context from stack *)
    )
  | step(stack, postoken as (pos,token)) =
    case token of
        T.INT v  => (SUBTREE(INT v))::stack
      | T.WORD v => (CTOR_AWAITING_ARGS v)::stack
      | T.OP v   => (CTOR_AWAITING_ARGS v)::stack
      | T.SPECIAL v =>
        raise T.SyntaxError(pos, "Unexpected special character: \""^Char.toString v^"\" when stack is "^stack2str stack)
fun finalize([SUBTREE v]) = [v] (* TODO *)
  | finalize(stack) = raise T.SyntaxError((~1,~1), "Unexpected EOF: "^stack2str stack)
            (* (List.app (fn (_,t)=>  print(T.tok2s t^"...\n")) stack; stack) *)
    in
        {initState=[],
         step=step,
         finalize=finalize}
    end

fun parse_string synspec s =
    let val consumer : (ParserStack, CST list) T.consumer =
            make_stackbased_consumer synspec
    in T.tokenize_string(s,consumer)
    end

fun parse_file synspec filename =
    let val consumer : (ParserStack, CST list) T.consumer =
            make_stackbased_consumer synspec
            (* {initState=(), *)
            (*  step=(fn ((),(_,t)) => print(T.tok2s(t)^"\n")), *)
            (*  finalize=fn() => ()} *)
    in T.tokenize_file(filename,consumer)
    end
end;
end;

(*
fun test() =
    PrologStyleParser.print_stack(PrologStyleParser.parse_file [] "test2.txt")
    handle T.SyntaxError(pos as (line,col),msg) =>
           (print(":"^Int.toString line^":"^Int.toString col^": Syntax error: "^msg^"\n");
            raise Tokenizer.SyntaxError(pos,msg))

val _ = test()
 *)
