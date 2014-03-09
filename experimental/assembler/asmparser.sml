structure AsmAST =
struct
  type Name = string
  datatype Type = TYVAR of Name
                | TYABS of Name * Type
  datatype Decl = TYPEDEF of Name * Type
  type AST = Decl list
end

local
    structure T = Tokenizer;
    structure P = PrologStyleParser;
    open AsmAST
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
    TYVAR(tyvar)

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
