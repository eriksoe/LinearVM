local
    structure T = Tokenizer;
    structure P = PrologStyleParser;
    open AsmAST0
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

fun parse_type(P.NODE("=>", [P.NODE(tyname,[]), body])) =
    TYABS(tyname, parse_type body)
  | parse_type(P.NODE(tyvar,[])) =
    NAMED_TYPE([tyvar])

fun parse_form(P.NODE("typedef", [P.NODE(name,[]), def])) =
    TYPEDEF(name, parse_type def)

fun parse_string s =
    let
        val forms = P.parse_string synspec s
    in
        map parse_form forms
    end

end;
end;
