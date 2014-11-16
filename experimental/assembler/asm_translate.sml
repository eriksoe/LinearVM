signature ASM_TRANSLATE =
sig
    type TypeEnv = AsmAST0.Name list
    val convert_type : AsmAST0.Type * TypeEnv -> AsmAST1.Type
end

local
    structure A0 = AsmAST0
    structure A1 = AsmAST1
in


structure AsmTranslate : ASM_TRANSLATE =
struct
    (* datatype Binding = B_IMPORTEDTYPE of A1.Type *)
    (* type Env = (A0.FullName * Binding) list *)
    type TypeEnv = A0.Name list

    fun lookup_kvlist(key,[]) = NONE
      | lookup_kvlist(key, (k,v)::env) =
        if key=k then SOME v
        else lookup_kvlist(key, env);

    fun lookup(name : A0.Name, tyenv : TypeEnv) : int option =
        let
            (*
             fun lookup_globenv() =
                (case lookup_kvlist(name, glenv) of
                    NONE => raise Fail("Unknown type: "^name)
                  | SOME B_IMPORTEDTYPE => A1.NAMED_TYPE name)
             *)
            fun lookup_tyenv([],_) =
                NONE
                (* lookup_globenv(glenv,name) *)
              | lookup_tyenv(n::ns, idx) =
                if n=name
                then SOME idx
                else lookup_tyenv(ns,idx+1)
        in
            lookup_tyenv(tyenv,1)
        end

fun convert_tagelem env (A0.INT_TAG v) = A1.INT_TAG v
  | convert_tagelem env (A0.TYVAR_TAG varname) =
    (case lookup(varname, env) of
         NONE => raise Fail("tag type isn't a type variable: "^varname)
       | SOME idx => A1.TYVAR_TAG idx)

fun convert_tag env (tagname,tagparams) =
    (tagname, map (convert_tagelem env) tagparams)

fun convert_type(A0.NAMED_TYPE tname, env:TypeEnv) =
    (case tname of
        [simplename] =>
        (case lookup(simplename, env) of
             NONE => A1.NAMED_TYPE tname
           | SOME idx => A1.TYVAR idx)
      | _ => A1.NAMED_TYPE tname)
  | convert_type(A0.TYABS(targ,tbody), env) =
    A1.TYABS(convert_type(tbody, targ::env))
  | convert_type(A0.EXISTENTIAL(targ,tbody), env)=
    A1.EXISTENTIAL(convert_type(tbody, targ::env))
  | convert_type(A0.TYAPP(tabs,targ), env) =
    A1.TYAPP(convert_type(tabs,env), convert_type(targ,env))
  | convert_type(A0.RECORD fs, env) =
    A1.RECORD(map (fn (n,t) => (n,convert_type(t,env))) fs)
  | convert_type(A0.VARIANT fs, env) =
    A1.VARIANT(map (fn (n,t) => (n,convert_type(t,env))) fs)
  | convert_type(A0.BLESS(tag,body), env) =
    A1.BLESS(convert_tag env tag,
             convert_type(body,env))
  | convert_type(A0.CURSE(tag,body), env) =
    A1.CURSE(convert_tag env tag,
             convert_type(body,env))
  | convert_type(A0.POINTER tbody, env) =
    A1.POINTER(convert_type(tbody,env))
  | convert_type(A0.VTABLE fs, env) =
    A1.VTABLE(map (fn (n,s) => (n,convert_signature(s,env))) fs)
and convert_typelist(ts,env) = map (fn t => convert_type(t,env)) ts
and convert_signature((inputs, exits):A0.Signature, env:TypeEnv) =
    (convert_typelist(inputs, env),
     map (fn (exit_name,outputs) => (exit_name, convert_typelist(outputs,env)))
     exits)


end (* structure AsmTranslate *)

end (* local *)
