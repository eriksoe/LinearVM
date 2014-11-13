local
    structure T = Tokenizer;
    structure P = PrologStyleParser;
    structure A = AsmAST0
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

type 'a ParserTable = ((string * int) * (P.CST list -> 'a)) list;
fun nodeparse (tab: 'a ParserTable) (tree: P.CST) : 'a =
    let fun find_parser (na as (name_,arity_)) =
             Option.map #2 (List.find (fn (k,_) => k = na) tab)
    in case tree of
           (##,P.NODE(name,args)) =>
           (case find_parser(name, length args) of
                SOME parser => parser args
              | NONE => raise T.SyntaxError(##, "Unrecognized item: "^name^"/"^Int.toString(length args)))
    (* TODO: handle non-nodes? *)
    end;

fun parse_fullname tree =
    let fun aux ((_, P.NODE(sname,[])), acc) =
            rev(sname::acc)
          | aux ((_, P.NODE("::", [a,b])), acc) =
            (case b of
                 (_, P.NODE(sname,[])) => aux(a, sname::acc)
               | (##, _) => raise T.SyntaxError(##, "Error in qualified name(1)"))
          | aux((##, x), _) =
            raise T.SyntaxError(##, "Error in qualified name -- "^P.cst'2str x)
    in aux(tree,[])
    end;

fun parse_type(##, P.NODE("=>", [(_,P.NODE(tyname,[])), body])) =
    A.TYABS(tyname, parse_type body)
  | parse_type(##, P.NODE(tyvar,[])) =
    A.NAMED_TYPE([tyvar])

fun parse_form tree =
    nodeparse [(("module",1), fn [fullname0] =>
                                 A.MODULEDEF(parse_fullname fullname0)),
               (("typedef",2), fn [(_,P.NODE(name,[])), def] =>
                                  A.TYPEDEF(name, parse_type def) )
              ] tree


fun parse_string s =
    let
        val raw_forms = P.parse_string synspec s
        val forms = map parse_form raw_forms
    in
        forms
            (* parse_modules forms *)
    end

end;
end;
