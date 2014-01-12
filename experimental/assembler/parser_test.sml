
(* Tests of tokenizer: *)
open Tokenizer
open Describe
open Assert

val _ =
    suite(describe "parser" [
          (* Trivial *)
          should("handle empty input",
              fn()=> assertEqual(tokenize_line 1 "" , [])),
          should("handle whitespace-only input",
              fn()=> assertEqual(tokenize_line 1 " \t" , [])),

          (* Integers *)
          should("handle small integer token",
              fn()=> assertEqual(tokenize_line 22 "9" , [((22,1),INT 9)])),
          should("handle large integer token",
              fn()=> assertEqual(tokenize_line 2222 "123789" , [((2222,1),INT 123789)])),
          (* Words *)
          should("handle short word token",
              fn()=> assertEqual(tokenize_line 2 "a" , [((2,1), WORD "a")])),
          should("handle long word token",
              fn()=> assertEqual(tokenize_line 2 "thequickbrownfoxjumpsoverlazydogs" , [((2,1), WORD "thequickbrownfoxjumpsoverlazydogs")])),
          should("handle upper-case word token",
              fn()=> assertEqual(tokenize_line 2 "THEQUICKBROWNFOXJUMPSOVERLAZYDOGS" , [((2,1), WORD "THEQUICKBROWNFOXJUMPSOVERLAZYDOGS")])),
          should("handle mixed-case word token",
              fn()=> assertEqual(tokenize_line 2 "TheQuickBrownFoxJumpsOverLazyDogs" , [((2,1), WORD "TheQuickBrownFoxJumpsOverLazyDogs")])),
          should("handle alphanumeric word token",
              fn()=> assertEqual(tokenize_line 2 "a0_9" , [((2,1), WORD "a0_9")])),

          (* Specials *)
          should("handle all specials",
              fn() => assertEqual([SPECIAL #"(", SPECIAL #",",
                                  SPECIAL #")", SPECIAL #"[",
                                  SPECIAL #"{", SPECIAL #"]",
                                  SPECIAL #"}"],
                                  map #2 (tokenize_line 0 "(,)[{]}"))),

          (* Operators *)
          should("handle short operators",
              fn() => assertEqual([OP "!", OP "#", OP "$", OP "%",
                                   OP "&", OP "*", OP "+", OP "-",
                                   OP ".", OP "/", OP ":", OP ";",
                                   OP "<", OP "=", OP ">", OP "?",
                                   OP "@", OP "\\", OP "^", OP "`",
                                   OP "|", OP "~"],
                                  map #2 (tokenize_line 0 "! # $ % & * + - . / : ; < = > ? @ \\ ^ ` | ~"))),
          should("handle long operators",
              fn() => assertEqual([OP "++",
                                   OP "->",
                                   OP "!#$%&*+",
                                   OP "-./:;<=>?@\\^`|~"],
                                  map #2 (tokenize_line 0 "++ -> !#$%&*+ -./:;<=>?@\\^`|~"))),
          (* "!#$%&*+-./:;<=>?@\\^`|~" *)
          (* TODO: Non-alphanum words *)
          (* TODO: Strings *)

          (* Whitespace *)
          should("handle initial whitespace",
              fn()=> assertEqual(tokenize_line 22 "  9" , [((22,3),INT 9)])),
          should("handle terminating whitespace",
              fn()=> assertEqual(tokenize_line 22 "9  " , [((22,1),INT 9)])),

          (* Multi-token *)
          should("handle several tokens (1)",
              fn() => assertEqual([INT 1,INT 23, INT 456, INT 7890],
                                  map #2 (tokenize_line 0 "1 23 456 7890"))),

          should("handle several tokens (mix)",
              fn() => assertEqual([INT 1, WORD "Aa", INT 456, WORD "zZ", WORD "a_z"],
                                  map #2 (tokenize_line 0 "1 Aa 456 zZ a_z"))),

          should("not allow words to start with a digit",
              fn() => assertEqual([INT 1, WORD "ab9"],
                                  map #2 (tokenize_line 0 "1ab9")))

         ])

