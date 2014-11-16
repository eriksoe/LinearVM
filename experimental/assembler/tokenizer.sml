signature TOKENIZER =
sig
    datatype token
      = INT of int | OP of string | SPECIAL of char | WORD of string | STRINGLIT of string
    type position = int * int
    type postoken = position * token
    type ('a,'b) consumer =
      {finalize:'a -> 'b, initState:'a, step:'a * postoken -> 'a}
    exception SyntaxError of position * string
    val tok2s : token -> string
    (* val tokenize_line : int -> string -> postoken list *)
    val tokenize_stream : (TextIO.instream * ('a,'b) consumer) -> 'b
    val tokenize_string : string * ('a,'b) consumer -> 'b
    val tokenize_file   : string * ('a,'b) consumer -> 'b

    val accumulator_consumer : (postoken list, postoken list) consumer

    val pos2str : position -> string

end;

structure Tokenizer : TOKENIZER =
struct
  datatype token = INT of int
                 | WORD of string
                 | OP of string
                 | SPECIAL of char (* One of ",()[]{}" *)
                 | STRINGLIT of string;
  type position = int * int
  type postoken = position * token
  type ('s,'r) consumer = {
       initState: 's,
       step: ('s * postoken) -> 's,
       finalize: 's -> 'r
  }

  exception SyntaxError of position * string

  fun tok2s(INT v)  = "INT("^Int.toString v^")"
    | tok2s(WORD v) = "WORD("^v^")"
    | tok2s(OP v) = "OP("^v^")"
    | tok2s(SPECIAL v) = "SPECIAL(\""^implode [v]^"\")"
;

  fun takeWhile2 pred (l,already) =
      let fun loop([],acc,pos) = (rev acc, [],pos)
            | loop(all as (x::xs),acc,pos) =
              if pred x
              then loop(xs, x::acc, pos+1)
              else (rev acc, all, pos)
      in loop(l,already,0)
      end

  fun isAlphaNumOrUnderscore #"_" = true
    | isAlphaNumOrUnderscore c = Char.isAlphaNum c

  fun isOperatorChar c =
      List.exists (fn opchar => c=opchar) (explode "!\"#$%&'*+-./:;<=>?@\\^_`|~")

  fun tokenize_line lineNr (line:string) =
      let
          fun parse_string_literal([], acc, col) =
              raise SyntaxError((lineNr,col), "String literal still open at end of line")
            | parse_string_literal(#"\""::cs, acc, col) =
              (implode(rev acc), cs, col+1)
            | parse_string_literal(#"\\"::cs, acc, col) =
              (* TODO: Handle escape sequences. *)
              (case cs of
                   [] => raise SyntaxError((lineNr,col),
                                           "\"\\\" at end of line in string literal")
                 | #"a"::cs' => parse_string_literal(cs', #"\a"::acc, col+2)
                 | #"b"::cs' => parse_string_literal(cs', #"\b"::acc, col+2)
                 | #"f"::cs' => parse_string_literal(cs', #"\f"::acc, col+2)
                 | #"t"::cs' => parse_string_literal(cs', #"\t"::acc, col+2)
                 | #"n"::cs' => parse_string_literal(cs', #"\n"::acc, col+2)
                 | #"r"::cs' => parse_string_literal(cs', #"\r"::acc, col+2)
                 | #"v"::cs' => parse_string_literal(cs', #"\v"::acc, col+2)
                 | #"\""::cs' => parse_string_literal(cs', #"\""::acc, col+2)
                 | #"'"::cs' => parse_string_literal(cs', #"'"::acc, col+2)
                 | c::_ => raise SyntaxError((lineNr,col),
                                             "Bad escape sequence: \"\\"^Char.toString c^"\"")
              )

            | parse_string_literal(c::cs, acc, col) =
              if Char.isCntrl c
              then raise SyntaxError((lineNr,col), "Bad control character in string literal: Character code "^Int.toString(ord c))
              else parse_string_literal(cs, c::acc, col+1);

          fun loop([], acc, col) = rev acc
            | loop(#"%"::_, acc, col) = loop([], acc, col) (* Comment *)
            | loop(#"\""::cs, acc, col) =
              let val (s,cs',col') = parse_string_literal(cs,[],col)
              in loop(cs', ((lineNr,col),STRINGLIT s)::acc, col')
              end
            | loop(c::cs, acc, col) =
              if Char.isSpace c
              then loop(cs, acc, col+1)
              else let val (tok,rest,taken) =
                       if Char.isDigit c
                       then let val (tokstr,cs',taken0) = takeWhile2 Char.isDigit (cs,[c])
                            in (INT((Option.valOf o Int.fromString o implode) tokstr), cs', 1+taken0)
                            end
                       else if Char.isAlpha c
                       then let val (tokstr,cs',taken0) = takeWhile2 isAlphaNumOrUnderscore (cs,[c])
                            in (WORD(implode tokstr), cs', 1+taken0)
                            end
                       else if List.exists (fn special => c=special) (explode ",()[]{}")
                       then (SPECIAL c, cs, 1)
                       else if isOperatorChar c
                       then let val (tokstr,cs',taken0) = takeWhile2 isOperatorChar (cs,[c])
                            in (OP(implode tokstr), cs', 1+taken0)
                            end
                       else raise SyntaxError((lineNr,col), "Bad character: '"^implode[c]^"'")
                   in
                       loop(rest, ((lineNr,col), tok)::acc, col+taken)
                   end

      in loop(explode line, [], 1)
      end : postoken list
      (* ([((lineNr,0), ATOM "dummy")] : postoken list) *)

  fun tokenize_stream(stream, consumer: ('s,'r) consumer) =
      let val {initState=s0, step=consumerStep, finalize=consumerFinalize} =
              consumer
          fun line_loop(s, stream, lineNr) =
              (case TextIO.inputLine stream of
                   NONE => s
                 | SOME(line) =>
                   let
                       val tokens : postoken list = tokenize_line lineNr line;
                       val s2 = List.foldl
                                    (fn (t,ss)=>consumerStep(ss,t))
                                    s tokens
                   in
                       line_loop(s2, stream, lineNr+1)
                   end)
      in
          consumerFinalize(line_loop(s0, stream, 1))
      end

  fun tokenize_string(s, consumer: ('s,'r) consumer) =
      let val stream = TextIO.openString(s)
      in
          tokenize_stream(stream, consumer)
          before TextIO.closeIn(stream)
      end

  fun tokenize_file(filename, consumer: ('s,'r) consumer) =
      let val f = TextIO.openIn(filename)
      in
          tokenize_stream(f, consumer)
          before TextIO.closeIn(f)
      end

    val accumulator_consumer : (postoken list, postoken list) consumer
      = {initState=[],
         step=fn(s,t)=>t::s,
         finalize=rev}

    fun pos2str(line,col) =
        Int.toString line^":"^Int.toString col

end

(* val s = "Hello, World!" *)
(* val _ = print(s^"\n") *)
(* fun test() = *)
(*     Tokenizer.tokenize_file("test.txt", *)
(*                             {initState=(), *)
(*                              step=(fn ((),(_,t)) => print(Tokenizer.tok2s(t)^"\n")), *)
(*                              finalize=fn() => ()}) *)

(* val _ = test() *)
