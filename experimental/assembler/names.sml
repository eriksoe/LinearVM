signature NAMES = sig
    type Names;

    (* sname means a simple name, e.g. "fred".
     * qname means a qualified name, such as "flintstones.fred".
     *)
    type SNameIndex;
    type QNameIndex;

    val create : unit -> Names;
    val root : QNameIndex;
    val sname2index : Names -> string -> SNameIndex;
    val qname2index : Names -> QNameIndex * string -> QNameIndex;
    val encode_sname : SNameIndex -> int;
    val encode_qname : QNameIndex -> int;
end;

signature CANONICAL_INDEX_TABLE = sig
    type ''a CanonTable;
    val create : (''a -> word) -> ''a CanonTable;
    val canon : ''a CanonTable * ''a -> int;
    val lookup : ''a CanonTable * int -> ''a option;
end

local
structure H = HashMap
structure EA = ExtensibleArray
in
structure CanonicalIndexTable : CANONICAL_INDEX_TABLE = struct
  type ''a CanonTable = {
       to_int : (''a,int) H.HashMap,
       from_int : ''a EA.ExtArray
  }

  fun create hash_fun = {to_int = H.create hash_fun 37,
                         from_int = EA.create()}

  fun canon({to_int,from_int} : ''a CanonTable, v) =
      case H.find to_int v of
          SOME idx => idx
        | NONE => let val idx = EA.add(from_int, v)
                      val () = H.insert to_int (v, idx)
                  in idx
                  end

    fun lookup ({from_int,...}: ''a CanonTable, idx) =
        EA.subscript(from_int,idx)
end
end

local
structure CT = CanonicalIndexTable
in
structure Names = struct
  type Names = {snames : string CT.CanonTable,
                qnames : (int * int) CT.CanonTable}
end;
end;
