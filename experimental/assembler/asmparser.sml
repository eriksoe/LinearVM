local
    structure T = Tokenizer;
    structure P = PrologStyleParser;
    structure A = AsmAST0
in

structure AsmParser =
struct
val synspec : P.Synspec =
    [
     (*
     ("->", (P.INFIX_L,1050)), ("~>", (P.INFIX_L,1050)),
     (",",  (P.INFIX_L,1000)),
     ("=>", (P.INFIX_R,900)),
     (":",  (P.INFIX_R,600)),
 *)

     (",",  (P.INFIX_R,1000)),
     (":",  (P.INFIX_R,800)),
     ("->", (P.INFIX_L,700)), ("~>", (P.INFIX_L,700)),
     ("=>", (P.INFIX_R,600)),

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
         | other as (##,_) =>
           raise T.SyntaxError(##, "Unrecognized item: "^P.cst2str(other))
    (* TODO: handle non-nodes? *)
    end;

fun flatten_by_delimiter delim tree =
    let
        fun aux(node as (_, P.NODE(ctor, [a,b])), acc) =
            if ctor = delim then
                aux(a, b::acc)
            else
                (node::acc)
          | aux(node,acc) = (node::acc)
    in aux(tree,[])
    end;

fun parse_simplename(_, P.NODE(sname,[])) = sname
  | parse_simplename(##, _) = raise T.SyntaxError(##, "Error in simple name")

fun parse_fullname tree =
    let fun aux ((_, P.NODE(sname,[])), acc) =
            sname::acc
          | aux ((_, P.NODE("::", [a,b])), acc) =
            (case b of
                 (_, P.NODE(sname,[])) => aux(a, sname::acc)
               | (##, _) => raise T.SyntaxError(##, "Error in qualified name(1)"))
          | aux((##, x), _) =
            raise T.SyntaxError(##, "Error in qualified name -- "^P.cst'2str x)
    in aux(tree,[])
    end;

fun parse_name_with_arity(_, P.NODE("/", [fullname, (_,P.INT arity)])) =
    (parse_fullname fullname, arity)

fun parse_type(##, P.NODE(tyvar,[])) =
    A.NAMED_TYPE([tyvar])
  | parse_type tree =
    nodeparse [(("=>",2), fn [(_,P.NODE(tyname,[])), body] =>
                             A.TYABS(tyname, parse_type body) ),
               (("{}",1), fn [tylist_tree] =>
                             A.RECORD(map parse_field (flatten_by_delimiter "," tylist_tree)) ),
               (("&",1), fn [typ] =>
                           A.BORROW(parse_type typ))
               ] tree
and parse_typelist(##,P.LIST(typenodes)) =
    map parse_type typenodes
  | parse_typelist(typenode) =
    [parse_type typenode]
and parse_field(##,P.NODE(":", [name0, type0])) =
    (parse_simplename name0, parse_type type0)
and parse_signature(node as (##,_)) =
    let val items = flatten_by_delimiter "~>" node
    in case items of
           (##,P.NODE("->", [ins,main_outs]))::nonmain_outs =>
           (parse_typelist ins,
            (NONE,parse_typelist main_outs)::(map (fn ts=>(NONE, parse_typelist ts)) nonmain_outs))
         | bad::_ => raise T.SyntaxError(##, "Signature lacks a '->'  (I see instead this: "^P.cst2str bad^")")
    end
(* | parse_signature(##,P.NODE("->", [ins, outs])) = *)
(*     ([], [])                    (* TODO *) *)
(* | parse_signature(other as (##,_)) = *)
(*   raise T.SyntaxError(##, "Error: not a signature: "^P.cst2str other) *)


fun parse_form tree =
    nodeparse [(("module",1), fn [fullname0] =>
                                 A.MODULEDEF(parse_fullname fullname0)),
               (("typedef",2), fn [(_,P.NODE(name,[])), def] =>
                                  A.TYPEDEF(name, parse_type def) ),
               (("import_type", 1), fn [fullname0] =>
                                       let val (name,arity) = parse_name_with_arity(fullname0)
                                       in A.TYPEIMPORT{name=name, arity=arity, alias=NONE, actual_type=NONE}
                                       end),
               (("import_type", 2), fn [fullname0, alias0] =>
                                       let val (name,arity) = parse_name_with_arity(fullname0)
                                       in A.TYPEIMPORT{name=name, arity=arity, alias=SOME(parse_simplename alias0), actual_type=NONE}
                                       end),
               (("import_function", 1), fn [name_and_signature] =>
                                           (case name_and_signature of
                                                (_,P.NODE(":", [name,signa])) =>
                                                A.FUNCIMPORT{name=parse_fullname name, signa=parse_signature signa}
                                              | (##,P.NODE(nodename, _)) =>
                                                raise T.SyntaxError(##, "Expected name:signature, not "^nodename)))
              ] tree


fun parse_string s =
    let
        val raw_forms = P.parse_string synspec s
        val forms = map parse_form raw_forms
    in
        forms
    end

end;
end;
