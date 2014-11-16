structure AsmAST0 =
struct
  type Name = string
  type FullName = string list

  datatype TagElem = INT_TAG of int
                   | TYVAR_TAG of Name
  type Tag = FullName * TagElem list

  datatype Type = TYABS of Name * Type
                | EXISTENTIAL of Name * Type (* ? *)
                | TYAPP of Type * Type
                | NAMED_TYPE of FullName
                | RECORD of (Name * Type) list
                | VARIANT of (Name * Type) list
                | BLESS of Tag * Type
                | CURSE of Tag * Type
                | POINTER of Type
                | VTABLE of (Name * Signature) list (* TODO: Inheritance? *)
                | BORROW of Type                    (* at later levels, only allowed on top level. *)
       withtype Signature = Type list * (string option * Type list) list

  datatype Decl = MODULEDEF of FullName
                | TYPEDEF of Name * Type
                | TYPEIMPORT of {name:FullName, arity:int, alias: Name option, actual_type: Type option}
                | FUNCIMPORT of {name:FullName, signa:Signature}
  type AST = Decl list
end

structure AsmAST1 =
struct
  type Name = string
  type FullName = string list
  type Index = int

  datatype TagElem = INT_TAG of int
                   | TYVAR_TAG of int
  type Tag = FullName * TagElem list

  datatype Type = TYVAR of Index
                | TYABS of Type
                | TYAPP of Type * Type
                | EXISTENTIAL of Type
                | NAMED_TYPE of FullName
                | RECORD of (Name * Type) list
                | VARIANT of (Name * Type) list
                | BLESS of Tag * Type
                | CURSE of Tag * Type
                | POINTER of Type
                | VTABLE of (Name * Signature) list (* TODO: Inheritance? *)
(* TODO: mutually recursive types. *)
  withtype Signature = Type list * (string option * Type list) list
;

  datatype Decl = TYPEDEF of Name * Type
  type AST = Decl list
end
