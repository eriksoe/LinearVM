local
    structure T = Tokenizer;
    structure P = PrologStyleParser;
in
structure AsmParser =
struct
val synspec : P.Synspec =
    [("->", (P.INFIX_L,1050)), ("~>", (P.INFIX_L,1050)),
     (",",  (P.INFIX_L,1000)),
     ("=>", (P.INFIX_R,900)),
     (":",  (P.INFIX_R,600)),
     ("/",  (P.INFIX_L,400)),
     ("&",  (P.PREFIX, 200)),
     ("::", (P.INFIX_L,100))];

fun parse_string s =
    P.parse_string synspec s

end;
end;
