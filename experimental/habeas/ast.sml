structure HabeasAst = struct
type Var = string
type Tag = string

datatype LiteralValue =
         INTLIT of int
       | ATOMLIT of string
;

datatype PrimType = INT_TYPE | ATOM_TYPE;

datatype Type = PRIM_TYPE of PrimType
              | TUPLE_TYPE of Type list
              | VARIANT_TYPE of (Tag*Type) list
              | MENU_TYPE of (Tag*Type) list
              | PAR_TYPE of Type list
(* TODO: PARTYPE *)
;

val UNIT_TYPE = TUPLE_TYPE[];

datatype Exp = VAR of Var
             | LIT of LiteralValue
             | LET of Var * Exp * Exp (* (var,bindexp,bodyexp) *)
             | CAST of Type * Exp

             (* Product type introduction & elimination *)
             | TUPLE of Exp list
             | UNTUPLE of Var list * Exp * Exp

             (* Sum type introduction & elimination *)
             (* If Γ⊢Δ,A,Θ or Γ⊢Δ,B,Θ, then Γ⊢Δ,A(+)B,Θ *)
             | INJECT of Tag * Exp
             (* If Γ,A,Δ⊢Θ and Γ,B,Δ⊢Θ, then Γ,A(+)B,Δ⊢Θ. *)
             | CASE of Exp * (Tag*Var*Exp) list

             (* 'With' type introduction & elimination *)
             (* If Γ⊢Δ,A,Θ and Γ⊢Δ,B,Θ, then Γ⊢Δ,A&B,Θ.  *)
             | MENU of (Tag*Exp) list
             (* If Γ,A,Δ⊢Θ or Γ,B,Δ⊢Θ, then Γ,A&B,Δ⊢Θ. *)
             | SELECT of Tag * Exp

             (* 'Par' type introduction & elimination *)
             (* If Γ⊢Δ,A,B,Θ, then Γ⊢Δ,A|B,Θ *)
             | PAR_INTRO of Exp * Exp (* Abstract/lambda *)
             (* If Γ,A⊢Δ and B,Θ⊢Λ, then Γ,A|B,Θ⊢Δ,Λ. *)
             | PAR_ELIM of Exp  (* Apply/fork *)

             (* Negation type introduction & elimination *)
             (* If Γ,A⊢Δ, then Γ⊢neg(A),Δ. *)
             | MAKE_FUTURE of Var * Exp
             (* If Γ⊢A,Δ, then Γ,neg(A)⊢Δ *)
             | FILL_FUTURE of Var * Exp
;

end;
