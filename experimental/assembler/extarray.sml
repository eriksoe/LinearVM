signature EXTENSIBLE_ARRAY = sig
    type 'a ExtArray;
    val create : unit -> 'a ExtArray;
    val add : 'a ExtArray * 'a -> int;
    val subscript : 'a ExtArray * int -> 'a option;
end

structure ExtensibleArray : EXTENSIBLE_ARRAY = struct
  val CHUNK_SIZE = 32;
  datatype 'a ExtArrayNode = EA_BASE of 'a option array
                           | EA_NODE of 'a ExtArrayNode option array
  type LevelDescr = {divisor:int} list

  type 'a ExtArray = {size : int ref,
                      descr : LevelDescr ref,
                      data: 'a ExtArrayNode ref}

  fun empty_node []    = EA_BASE(Array.tabulate(CHUNK_SIZE, fn _ => NONE))
    | empty_node(_::_) = EA_NODE(Array.tabulate(CHUNK_SIZE, fn _ => NONE))

  fun create() = {size = ref 0,
                  descr = ref [],
                  data = ref (empty_node [])}

  fun deepen({size, descr, data=root} : 'a ExtArray) : unit =
      let val new_divisor = case !descr of
                                [] => CHUNK_SIZE
                              | {divisor}::_ => CHUNK_SIZE * divisor;
          val new_descr = {divisor=new_divisor}::(!descr)
          val new_root as EA_NODE(arr) = empty_node new_descr;
          val _ = Array.update(arr, 0, SOME(!root))
      in (descr := new_descr;
          root := new_root)
      end;

  (* fun find_pos(EA_LEVEL{depth, capacity, data, next, ...}, pos) : ('a option array * int) = *)
  (*     if pos < 0 orelse pos >= capacity then raise Subscript *)
  (*     else let fun aux(EA_LEVEL *)

  fun find_place(EA_BASE arr, pos, []) =
      if pos >= CHUNK_SIZE
      then NONE
      else SOME (arr,pos)
    | find_place(EA_NODE(children), pos, (descr::descrs) : LevelDescr) =
      let val the_divisor = (#divisor descr)
          val bucket = pos div the_divisor
          val sub_pos = Int.rem(pos, the_divisor)
      in if bucket >= CHUNK_SIZE
         then NONE
         else let val child = case Array.sub(children,bucket) of
                                  NONE => let val new_child = empty_node descrs;
                                              val _ = Array.update(children, bucket, SOME new_child)
                                          in new_child
                                          end
                                | SOME ch => ch
              in find_place(child, sub_pos, descrs)
              end
      end;

  fun add(ea as {size, descr, data}:'a ExtArray, item : 'a) =
      let val pos = ! size;
      in
          case find_place(!data, pos, !descr) of
              NONE => (deepen ea; add(ea, item))
            | SOME (arr,idx) =>
              (Array.update(arr, idx, SOME item);
               size := pos+1;
               pos)
      end

  fun subscript(ea as {size=_, descr, data} : 'a ExtArray, pos : int) =
          case find_place(!data, pos, !descr) of
              NONE => NONE
            | SOME (arr,idx) => Array.sub(arr,idx);
end
