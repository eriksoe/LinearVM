
(* Tests of tokenizer: *)
open Describe
open Assert
open Expect

structure T = Tokenizer
structure PP = PrologStyleParser
structure AP = AsmParser

fun tokenize s = tokenize_string(s,accumulator_consumer)

val () =
    suite(describe "asmparser" [
          (* Trivial *)
          should("handle typedef",
              fn()=> expect (AP.parse_string "typedef(t1, t=>t).")
                            toEqual([NODE("typedef", [NODE("t1",[]),
                                                      NODE("=>", [NODE("t",[]),
                                                                  NODE("t",[])])])]))
         ])

    (*
     structure T=Tokenizer; structure AP=AsmParser;
AP.parse_string "typedef(t1, t=>t)." handle e as T.SyntaxError(_,s) => (print s;raise e);
 *)
