
(* Tests of tokenizer: *)
open Tokenizer
open Describe
open Assert

fun tokenize s = tokenize_string(s,accumulator_consumer)

val _ =
    suite(describe "tokenizer" [
          (* Trivial *)
          should("handle empty input",
              fn()=> assertEqual(tokenize "", [])),
          should("handle whitespace-only input",
              fn()=> assertEqual(tokenize " \t", [])),

          (* Integers *)
          should("handle small integer token",
              fn()=> assertEqual(tokenize "9", [((1,1),INT 9)])),
          should("handle large integer token",
              fn()=> assertEqual(tokenize "123789", [((1,1),INT 123789)])),
          (* Words *)
          should("handle short word token",
              fn()=> assertEqual(tokenize "a", [((1,1), WORD "a")])),
          should("handle long word token",
              fn()=> assertEqual(tokenize "thequickbrownfoxjumpsoverlazydogs", [((1,1), WORD "thequickbrownfoxjumpsoverlazydogs")])),
          should("handle upper-case word token",
              fn()=> assertEqual(tokenize "THEQUICKBROWNFOXJUMPSOVERLAZYDOGS", [((1,1), WORD "THEQUICKBROWNFOXJUMPSOVERLAZYDOGS")])),
          should("handle mixed-case word token",
              fn()=> assertEqual(tokenize "TheQuickBrownFoxJumpsOverLazyDogs", [((1,1), WORD "TheQuickBrownFoxJumpsOverLazyDogs")])),
          should("handle alphanumeric word token",
              fn()=> assertEqual(tokenize "a0_9", [((1,1), WORD "a0_9")])),

          (* Specials *)
          should("handle all specials",
              fn() => assertEqual([SPECIAL #"(", SPECIAL #",",
                                  SPECIAL #")", SPECIAL #"[",
                                  SPECIAL #"{", SPECIAL #"]",
                                  SPECIAL #"}"],
                                  map #2 (tokenize "(,)[{]}"))),

          (* Operators *)
          should("handle short operators",
              fn() => assertEqual([OP "!", OP "#", OP "$", OP "&",
                                   OP "*", OP "+", OP "-", OP "/",
                                   OP ":", OP ";", OP "<", OP "=",
                                   OP ">", OP "?", OP "@", OP "\\",
                                   OP "^", OP "`", OP "|", OP "~"],
                                  map #2 (tokenize "! # $ & * + - / : ; < = > ? @ \\ ^ ` | ~"))),
          should("handle long operators",
              fn() => assertEqual([OP "++",
                                   OP "->",
                                   OP "!#$%&*+",
                                   OP "-./:;<=>?@\\^`|~"],
                                  map #2 (tokenize "++ -> !#$%&*+ -./:;<=>?@\\^`|~"))),
          (* TODO: Non-alphanum words *)

          (* Strings *)
          should("handle the empty string literal",
                 fn()=> assertEqual(tokenize "\"\"", [((1,1),STRINGLIT "")])),
          should("handle a simple one-character string literal",
                 fn()=> assertEqual(tokenize "\"x\"", [((1,1),STRINGLIT "x")])),
          should("handle a simple multi-character string literal",
                 fn()=> assertEqual(tokenize "\"abc 123-xyz\"", [((1,1),STRINGLIT "abc 123-xyz")])),
          should("handle non-general escape sequences",
                 fn()=> assertEqual(tokenize "\"\\a \\b \\f \\n \\r \\t \\v \\\" \\'\"", [((1,1),STRINGLIT "\a \b \f \n \r \t \v \" '")])),

          (* Whitespace *)
          should("handle initial whitespace",
              fn()=> assertEqual(tokenize "  9", [((1,3),INT 9)])),
          should("handle terminating whitespace",
              fn()=> assertEqual(tokenize "9  ", [((1,1),INT 9)])),

          (* Line numbers and positions *)
          should("handle trivial line numbers",
              fn()=> assertEqual(tokenize "a\nb\nc",
                                 [((1,1),WORD "a"),((2,1),WORD "b"),((3,1),WORD "c")])),
          should("handle line numbers with plenty of whitespace",
              fn()=> assertEqual(tokenize "\nab\n  cd  \n\n\ne\n",
                                 [((2,1),WORD "ab"),((3,3),WORD "cd"),((6,1),WORD "e")])),
          should("handle compute columns correctly for several tokens (1)",
              fn() => assertEqual([((1,1), INT 12),
                                   ((1,4), WORD "abc"),
                                   ((1,8), SPECIAL #"("),
                                   ((1,10), INT 7890)],
                                  tokenize "12 abc ( 7890")),
          should("handle compute columns correctly for several tokens (2)",
              fn() => assertEqual([((1,1), INT 12),
                                   ((1,5), WORD "abc"),
                                   ((1,10), SPECIAL #"("),
                                   ((1,13), INT 7890)],
                                  tokenize "12  abc  (  7890")),

          (* Multi-token *)
          should("handle several tokens (1)",
              fn() => assertEqual([INT 1,INT 23, INT 456, INT 7890],
                                  map #2 (tokenize "1 23 456 7890"))),

          should("handle several tokens (mix)",
              fn() => assertEqual([INT 1, WORD "Aa", INT 456, WORD "zZ", WORD "a_z"],
                                  map #2 (tokenize "1 Aa 456 zZ a_z"))),

          should("not allow words to start with a digit",
              fn() => assertEqual([INT 1, WORD "ab9"],
                                  map #2 (tokenize "1ab9")))

         ])

open PrologStyleParser

val synspec0 = []
val synspec_arith = [("+", (INFIX_L, 500)),
                     ("-", (INFIX_L, 500)),
                     ("*", (INFIX_L, 400)),
                     ("::", (INFIX_R, 800)),
                     ("&", (PREFIX, 450)),
                     ("!", (POSTFIX, 450))
                    ]

val synspec_special = [(",", (INFIX_L, 1000))]

val _ =
    suite(describe "parser" [
          should("handle single integer token",
                fn() => assertEqual(parse_string synspec0 "123.", [INT 123])),
          should("handle single word token",
                fn() => assertEqual(parse_string synspec0 "word.", [NODE("word",[])])),

          (* Normal constructors: *)
          should("handle ctor with zero parameters",
                fn() => assertEqual(parse_string synspec0 "word().", [NODE("word",[])])),
          should("handle ctor with one parameter",
                fn() => assertEqual(parse_string synspec0 "word(x).", [NODE("word",[NODE("x",[])])])),
          should("handle ctor with multiple parameter",
                fn() => assertEqual(parse_string synspec0 "word(x,1234).", [NODE("word",[NODE("x",[]), INT 1234])])),

          (* Operators as constructors: *)
          should("handle operators as constructors",
                fn() => assertEqual(parse_string synspec0 "+(12,34).", [NODE("+",[INT 12, INT 34])])),
          should("handle operators as constructors (even if it has an infix definition)",
                fn() => assertEqual(parse_string synspec_arith "+(12,34).", [NODE("+",[INT 12, INT 34])])),

          (* Infix operators: *)
          should("handle simple infix use",
                fn() => assertEqual(parse_string synspec_arith "12 + 34.", [NODE("+",[INT 12, INT 34])])),

          should("handle infix priorities (leftassoc: ++)",
                fn() => assertEqual(parse_string synspec_arith "1 + 2 + 3.", [NODE("+",[NODE("+",[INT 1, INT 2]), INT 3])])),
          should("handle infix priorities (leftassoc: *+)",
                fn() => assertEqual(parse_string synspec_arith "1 * 2 + 3.", [NODE("+",[NODE("*",[INT 1, INT 2]), INT 3])])),
          should("handle infix priorities (leftassoc: +*)",
                fn() => assertEqual(parse_string synspec_arith "1 + 2 * 3.", [NODE("+",[INT 1, NODE("*",[INT 2, INT 3])])])),
          should("handle infix priorities (rightassoc)",
                fn() => assertEqual(parse_string synspec_arith "1::2::3.", [NODE("::",[INT 1, NODE("::",[INT 2, INT 3])])])),
          should("handle infix priorities (rightassoc) with atoms",
                fn() => assertEqual(parse_string synspec_arith "a::b::c.", [NODE("::",[NODE("a",[]), NODE("::",[NODE("b",[]), NODE("c",[])])])])),

         (* Special infix constructors: *)
          should("handle ',' as infix constructor",
                fn() => assertEqual(parse_string synspec_special "1,2,3.", [NODE(",",[NODE(",",[INT 1, INT 2]), INT 3])])),

          (* Prefix operators *)
          should("handle prefix operators",
                fn() => assertEqual(parse_string synspec_arith "&1.", [NODE("&",[INT 1])])),
          should("handle double prefix operators",
                fn() => assertEqual(parse_string synspec_arith "& &1.", [NODE("&",[NODE("&",[INT 1])])])),
          should("handle prefix priorities vs. infix (*&)",
                fn() => assertEqual(parse_string synspec_arith "3 * &1.", [NODE("*", [INT 3,NODE("&",[INT 1])])])),
          should("handle prefix priorities vs. infix (&+)",
                fn() => assertEqual(parse_string synspec_arith "&1 + 3.", [NODE("+", [NODE("&",[INT 1]), INT 3])])),
          should("handle prefix priorities vs. infix (&*)",
                fn() => assertEqual(parse_string synspec_arith "&1 * 3.", [NODE("&", [NODE("*",[INT 1, INT 3])])])),

          (* Postfix operators *)
          should("handle postfix operators",
                fn() => assertEqual(parse_string synspec_arith "1! .", [NODE("!",[INT 1])])),
          should("handle double postfix operators",
                fn() => assertEqual(parse_string synspec_arith "1! ! .", [NODE("!",[NODE("!",[INT 1])])])),
          should("handle postfix priorities vs. infix (!*)",
                fn() => assertEqual(parse_string synspec_arith "3! * 1.", [NODE("*", [NODE("!",[INT 3]), INT 1])])),
          should("handle postfix priorities vs. infix (+!)",
                fn() => assertEqual(parse_string synspec_arith "1 + 3! .", [NODE("+", [INT 1, NODE("!",[INT 3])])])),
          should("handle postfix priorities vs. infix (*!)",
                fn() => assertEqual(parse_string synspec_arith "1 * 3! .", [NODE("!", [NODE("*",[INT 1, INT 3])])])),

         (* Interactions. *)
          should("handle infix-in-explicit (1: right-paren)",
                fn() => assertEqual(parse_string synspec_arith "foo(1+2).",
                                    [NODE("foo",[NODE("+",[INT 1, INT 2])])])),
          should("handle infix-in-explicit (2: comma)",
                fn() => assertEqual(parse_string synspec_arith "foo(1+2,3).",
                                    [NODE("foo",[NODE("+",[INT 1, INT 2]), INT 3])])),
          should("handle explicit-in-infix",
                fn() => assertEqual(parse_string synspec_arith "1+foo(2)+3.",
                                    [NODE("+",[NODE("+",[INT 1, NODE("foo",[INT 2])]), INT 3])])),

         (* Parentheses *)
          should("handle simple parenthesis",
                fn() => assertEqual(parse_string synspec0 "(123).", [INT 123])),
          should("handle ctor in parenthesis",
                fn() => assertEqual(parse_string synspec0 "(a()).", [NODE("a",[])])),
          should("handle binop in parenthesis",
                fn() => assertEqual(parse_string synspec_arith "(12+3).", [NODE("+",[INT 12,INT 3])])),

         (* '{}' *)
          should("handle empty curly-group",
                fn() => assertEqual(parse_string synspec0 "{}.", [NODE("{}",[])])),
          should("handle simple curly-group",
                fn() => assertEqual(parse_string synspec0 "{123}.", [NODE("{}",[INT 123])])),
          should("handle comma in curly-group",
                fn() => assertEqual(parse_string synspec_special "{12,3}.", [NODE("{}",[NODE(",",[INT 12,INT 3])])])),

         (* Lists *)
          should("handle the empty list",
                fn() => assertEqual(parse_string synspec0 "[]. [ ].",
                                    [LIST [], LIST []])),

          should("handle singletons lists",
                fn() => assertEqual(parse_string synspec0 "[a]. [f()].",
                                    [LIST [NODE("a",[])], LIST [NODE("f",[])]])),
          should("handle multi-element lists",
                fn() => assertEqual(parse_string synspec0 "[a,2,c,4].",
                                    [LIST [NODE("a",[]), INT 2, NODE("c",[]), INT 4]])),

         (* TODO: "priority-difference of 1" case *)

         (* Multiple terms *)
          should("handle multiple terms",
                fn() => assertEqual(parse_string synspec0 "a. b. 12.",
                                    [NODE("a",[]), NODE("b",[]), INT 12]))

         ])

    (*
     open Tokenizer; open PrologStyleParser;
parse_string [] "12 + 34." handle e as SyntaxError(_,s) => (print s;raise e);
 *)
