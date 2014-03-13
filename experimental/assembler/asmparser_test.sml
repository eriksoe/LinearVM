
(* Tests of tokenizer: *)
open Describe
open Assert
open Expect

structure T = Tokenizer
structure PP = PrologStyleParser
structure AP = AsmParser
structure A = AsmAST

fun tokenize s = tokenize_string(s,accumulator_consumer)

val () =
    suite(describe "asmparser" [
          (* Trivial *)
          should("handle typedef",
              fn()=> expect (AP.parse_string "typedef(t1, t=>t).")
(*
                            toEqual([NODE("typedef", [NODE("t1",[]),
                                                      NODE("=>", [NODE("t",[]),
                                                                  NODE("t",[])])])])
*)
                            toEqual([A.TYPEDEF("t1", A.TYABS("t",A.TYVAR("t")))])
                )
(* typedef(pair, (ta => tb => {a:ta, b:tb})). *)

         ])

    (*
     structure T=Tokenizer; structure AP=AsmParser;
AP.parse_string "typedef(t1, t=>t)." handle e as T.SyntaxError(_,s) => (print s;raise e);
AP.parse_string "typedef(pair, (ta => tb => {a:ta, b:tb}))." handle e as T.SyntaxError(_,s) => (print s;raise e);
 *)
