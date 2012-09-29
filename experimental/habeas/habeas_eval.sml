structure HabeasEval = struct
local open HabeasAst in

datatype Value = PRIMVAL of LiteralValue
               | TUPLEVAL of Value list
               | VARIANTVAL of Tag*Value
               | MENUVAL of (Tag*Env*Exp) list
withtype Env = (Var*Value) list;

fun lookup((k,v)::env, var) =
    if k=var then (v,env)
    else lookup(env,var)
  | lookup([],var) = raise Fail("Unknown variable: "^var)

fun eval env (VAR v) =
    let val (res,env') = lookup(env,v)
    in (res,env')
    end
  | eval env (LIT res) = (PRIMVAL res,env)
  | eval env (LET(v,exp,body)) =
    let val (value,env') = eval env exp
        val env'' = (v,value)::env'
    in eval env'' body
    end
  | eval env (TUPLE exps) =
    let val (values, env') = eval_list env exps
    in (TUPLEVAL values, env')
    end

and eval_list env exps =
    foldr (fn (exp,(acc,env')) => let val (value,env'')=eval env' exp
                                  in (value::acc, env'')
                                  end)
          ([],env) exps;

end;
end;
