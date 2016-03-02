module BoolProofs

open System;


type VarID = string

type BoolExp =
  | CST of bool
  | VAR of VarID
  | NOT of BoolExp
  | AND of BoolExp * BoolExp
  | OR  of BoolExp * BoolExp
  | XOR of BoolExp * BoolExp
;;

type BoolPosProof =
  | CST_TRUE
  //| POS_ASSUM of VarID * BoolPosProof * BoolPosProof
  | POS_NOT of BoolNegProof
  | AND_PROOF of BoolPosProof * BoolPosProof
  | OR1_PROOF of BoolPosProof
  | OR2_PROOF of BoolPosProof

and BoolNegProof =
  | CST_FALSE
  //| NEG_ASSUM of VarID * BoolNegProof * BoolNegProof
  | NEG_NOT of BoolPosProof
  | OR_PROOF of BoolNegProof * BoolNegProof
  | AND1_PROOF of BoolNegProof
  | AND2_PROOF of BoolNegProof

type BoolProof =
  | POS of BoolPosProof
  | NEG of BoolNegProof
  | NEITHER

type 'a ProofAttempt =
  | PROOF of 'a
  | NO_PROOF of VarID list

/// Verify that `proof` is a proof of `exp`
let rec validate_pos(proof,exp) : bool =
  match (proof,exp) with
    | (CST_TRUE, CST true) -> true
    | (POS_NOT p, NOT e) -> validate_neg(p, e)
    | (AND_PROOF(p1, p2), AND(e1,e2)) ->
      validate_pos(p1,e1) && validate_pos(p2,e2)
    | (OR1_PROOF p1, OR(e1,e2)) -> validate_pos(p1,e1)
    | (OR2_PROOF p2, OR(e1,e2)) -> validate_pos(p2,e2)
    | _ -> false
and validate_neg(proof,exp) : bool =
  match (proof,exp) with
    | (CST_FALSE, CST false) -> true
    | (NEG_NOT p, NOT e) -> validate_pos(p, e)
    | (OR_PROOF(p1, p2), OR(e1,e2)) ->
      validate_neg(p1,e1) && validate_neg(p2,e2)
    | (AND1_PROOF p1, AND(e1,e2)) -> validate_neg(p1,e1)
    | (AND2_PROOF p2, AND(e1,e2)) -> validate_neg(p2,e2)
    | _ -> false

let rec prove_pos exp : BoolPosProof ProofAttempt =
  match exp with
  | CST true -> PROOF CST_TRUE
  | NOT e ->
    (match prove_neg e with
     | PROOF p -> PROOF(POS_NOT p)
     | NO_PROOF vs -> NO_PROOF vs)
  | AND(e1,e2) ->
    (match prove_pos e1 with PROOF p1 ->
     (match prove_pos e2 with PROOF p2 ->
      PROOF(AND_PROOF(p1,p2))
      | _ -> NO_PROOF [])
     | _ -> NO_PROOF [])
  | OR(e1,e2) ->
    (match prove_pos e1 with
     PROOF p1 -> PROOF(OR1_PROOF p1)
     | _ -> (match prove_pos e2 with
             PROOF p2 -> PROOF(OR2_PROOF p2)
             | _ -> NO_PROOF []))
  | _ -> NO_PROOF []

and prove_neg exp : BoolNegProof ProofAttempt =
  match exp with
  | CST false -> PROOF CST_FALSE
  | NOT e ->
    (match prove_pos e with
     | PROOF p -> PROOF(NEG_NOT p)
     | NO_PROOF vs -> NO_PROOF vs)
  | OR(e1,e2) ->
    (match prove_neg e1 with PROOF p1 ->
     (match prove_neg e2 with PROOF p2 ->
      PROOF(OR_PROOF(p1,p2))
      | _ -> NO_PROOF [])
     | _ -> NO_PROOF [])
  | AND(e1,e2) ->
    (match prove_neg e1 with
     PROOF p1 -> PROOF(AND1_PROOF p1)
     | _ -> (match prove_neg e2 with
             PROOF p2 -> PROOF(AND2_PROOF p2)
             | _ -> NO_PROOF []))
  | _ -> NO_PROOF []

let prove exp : BoolProof =
  match prove_pos exp with
  | PROOF p -> POS p
  | NO_PROOF branchvars_p ->
    match prove_neg exp with
      | PROOF p -> NEG p
      | NO_PROOF branchvars_n ->
        NEITHER

let show v = Console.WriteLine(string v)

let test exp =
  let proof = prove exp
  Console.WriteLine(string exp + ": "+string proof)
  let opt_verif = match proof with
    | POS p -> Some(validate_pos(p,exp))
    | NEG p -> Some(validate_neg(p,exp))
    | _ -> None
  match opt_verif with
    | Some v -> Console.WriteLine("  Verify: " + (string v))
    | None -> ()


test(CST true)
test(CST false)
test(NOT(CST true))
test(NOT(CST false))
test(AND(CST true, CST true))
test(AND(CST false, CST true))
test(OR(CST false, CST false))
test(OR(CST false, CST true))

(*   open BoolProofs;;
   prove (CST true);;
   show(prove (CST true))
*)
