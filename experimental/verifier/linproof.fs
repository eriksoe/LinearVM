module LinearProofs

open System;

type VarIndex = int
type Name = string
type Symbol = string

type Type =
  | Import of Name
  | Struct of (Symbol * Type) list
  | Variant of (Symbol * Type) list
  | TyVar of VarIndex
  | TyAbs of Type
  | TyApp of Type * Type


type AssignabilityProof =
| ASame                                 // Precisely the same type.
| AImport                               // Imported type.
| AStruct of AssignabilityProof list    // Covariance
| AVariant of AssignabilityProof list    // Covariance
//| AVariantExtend of int

let rec all3 f (xs, ys, zs) : bool =
  match (xs,ys,zs) with
    | ([], [], []) -> true
    | (x::xs', y::ys', z::zs') ->
      f(x,y,z) && all3 f (xs', ys', zs')
    | _ -> false

/// Check that "proof" is a proof that "a := b":
let rec checkAProof (proof,a,b) : bool =
  match (proof,a,b) with
    | (ASame, t1, t2) -> t1 = t2
    | (AImport, Import name, Import name') -> name = name'
    | (AStruct ps, Struct fs1, Struct fs2) ->
      all3 (fun(p, (f1,t1), (f2,t2)) ->
            (f1 = f2 &&
             checkAProof(p,t1,t2))) (ps, fs1, fs2)
    | (AVariant ps, Variant fs1, Variant fs2) ->
      all3 (fun(p, (f1,t1), (f2,t2)) ->
            (f1 = f2 &&
             checkAProof(p,t1,t2))) (ps, fs1, fs2)
    | _ -> false
;;

//==================== Test cases: ==============================
let test(exp1,exp2,proof) =
  let res = checkAProof(proof, exp1, exp2)
  Console.WriteLine(string exp1 + ":=? "+string exp2+" : "+string res)

// Positive:
test(Import "a", Import "a", AImport)
test(Struct[("x", Import "a"); ("y", Import "b")],
     Struct[("x", Import "a"); ("y", Import "b")],
     AStruct[AImport; AImport])
test(Variant[("x", Import "a"); ("y", Import "b")],
     Variant[("x", Import "a"); ("y", Import "b")],
     AVariant[AImport; AImport])
// Negative:
test(Struct[("x", Import "a"); ("y", Import "b")],
     Variant[("x", Import "a"); ("y", Import "b")],
     AVariant[AImport; AImport])
test(Variant[("x", Import "a"); ("y", Import "b")],
     Struct[("x", Import "a"); ("y", Import "b")],
     AStruct[AImport; AImport])

