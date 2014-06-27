local
    open Describe
    open Assert
    structure H = HashMap
in
fun str_hash s =
    if (String.size s) = 0 then 0wx0
    else Word.fromInt(Char.ord(String.sub(s,0)))
val h : (string,int) H.HashMap = H.create str_hash 17

val _ =
    suite(describe "hashmap" [
          (* Contains I *)
          should("answer contains I",
              fn()=> assertEqual(H.contains h "x", false)),
          should("answer find I",
              fn()=> assertEqual(H.find h "x", NONE)),
          should("support insertion",
              fn()=> assertEqual((H.insert h ("x",  123),
                                  H.insert h ("xx", 456),
                                  H.insert h ("xxx",456)),
                                 ((), (), ())
                )),
          should("answer contains II",
              fn()=> assertEqual((H.contains h "x",
                                  H.contains h "xx",
                                  H.contains h "xxx",
                                  H.contains h "y"),
                                 (true, true, true, false)
                )),
          should("answer find II",
              fn()=> assertEqual((H.find h "x",
                                  H.find h "xx",
                                  H.find h "xxx",
                                  H.find h "y"),
                                 (SOME 123,
                                  SOME 456,
                                  SOME 456,
                                  NONE)
                )),
          should("support deletion",
              fn()=> assertEqual(H.remove h "xx", () )),
          should("answer contains III",
              fn()=> assertEqual((H.contains h "x",
                                  H.contains h "xx",
                                  H.contains h "xxx",
                                  H.contains h "y"),
                                 (true, false, true, false)
                )),
          should("answer find III",
              fn()=> assertEqual((H.find h "x",
                                  H.find h "xx",
                                  H.find h "xxx",
                                  H.find h "y"),
                                 (SOME 123,
                                  NONE,
                                  SOME 456,
                                  NONE)
                ))
         ])


end
