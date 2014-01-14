
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
              fn() => assertEqual([OP "!", OP "#", OP "$", OP "%",
                                   OP "&", OP "*", OP "+", OP "-",
                                   OP ".", OP "/", OP ":", OP ";",
                                   OP "<", OP "=", OP ">", OP "?",
                                   OP "@", OP "\\", OP "^", OP "`",
                                   OP "|", OP "~"],
                                  map #2 (tokenize "! # $ % & * + - . / : ; < = > ? @ \\ ^ ` | ~"))),
          should("handle long operators",
              fn() => assertEqual([OP "++",
                                   OP "->",
                                   OP "!#$%&*+",
                                   OP "-./:;<=>?@\\^`|~"],
                                  map #2 (tokenize "++ -> !#$%&*+ -./:;<=>?@\\^`|~"))),
          (* TODO: Non-alphanum words *)
          (* TODO: Strings *)

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
val _ =
    suite(describe "parser" [
          should("handle single integer token",
                fn() => assertEqual(parse_string synspec0 "123.", [INT 123])),
          should("handle single word token",
                fn() => assertEqual(parse_string synspec0 "word.", [NODE("word",[])])),
          should("handle ctor with zero parameters",
                fn() => assertEqual(parse_string synspec0 "word().", [NODE("word",[])])),
          should("handle ctor with one parameter",
                fn() => assertEqual(parse_string synspec0 "word(x).", [NODE("word",[NODE("x",[])])])),
          should("handle ctor with multiple parameter",
                fn() => assertEqual(parse_string synspec0 "word(x,1234).", [NODE("word",[NODE("x",[]), INT 1234])]))
          ])
