structure HabeasTest = struct
local
    open HabeasAst;
    open HabeasEval;
in

val (r1:Value,[]) = eval [] (LIT(INTLIT 1234))
val true =  r1 = (PRIMVAL(INTLIT 1234))

val (r2:Value,[]) = eval [] (LIT(ATOMLIT "xyz"))
val true =  r2 = (PRIMVAL(ATOMLIT "xyz"))

val (r3:Value,[]) = eval [] (TUPLE([LIT(ATOMLIT "x"), LIT(INTLIT 12)]))
val true = r3 = TUPLEVAL([PRIMVAL(ATOMLIT "x"), PRIMVAL(INTLIT 12)])

val (r4:Value,[]) = eval [] (LET("v", LIT(ATOMLIT "X"), VAR "v"))
val true = r4 = PRIMVAL(ATOMLIT "X")

end;
end;
