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
                | VTABLE of (Name * Type) list (* TODO: Inheritance? *)
                            ;

  datatype Decl = MODULEDEF of FullName
                | TYPEDEF of Name * Type
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
                | VTABLE of (Name * Type) list (* TODO: Inheritance? *)
(* TODO: mutually recursive types. *)
;

  datatype Decl = TYPEDEF of Name * Type
  type AST = Decl list
end
