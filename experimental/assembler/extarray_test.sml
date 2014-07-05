open Describe
open Assert

structure EA = ExtensibleArray

val _ =
    suite(describe "extensible array" [
          (* Trivial *)
          should("support creation",
              fn() => assertEqual((EA.create() : (string EA.ExtArray); ()), ())),
          should("support addition",
              fn() => let val ea = EA.create();
                      in assertEqual(EA.add(ea,"value"), 0)
                      end),

          should("support multiple additions",
              fn() => let val ea = EA.create()
                          val adds = (EA.add(ea,"value 1"),
                                      EA.add(ea,"value 2"),
                                      EA.add(ea,"value 3"))
                      in assertEqual(adds, (0,1,2))
                      end),

          should("support lookup",
              fn() => let val ea = EA.create()
                          val add_res = EA.add(ea,"value 1")
                          val look_res = EA.subscript(ea,0)
                      in assertEqual((add_res,look_res),
                                     (0, SOME "value 1"))
                      end),

          should("support lookup at nonexisting index",
              fn() => let val ea = EA.create()
                          val add_res = EA.add(ea,"value 1")
                          val look_res = EA.subscript(ea,1)
                      in assertEqual((add_res,look_res),
                                     (0, NONE))
                      end),

          should("yield the correct item at lookup",
              fn() => let val ea = EA.create()
                          val adds = (EA.add(ea,"value 1"),
                                      EA.add(ea,"value 2"),
                                      EA.add(ea,"value 3"))
                          val lookups = (EA.subscript(ea,0),
                                         EA.subscript(ea,2),
                                         EA.subscript(ea,1))
                      in assertEqual( (adds, lookups),
                                      ( (0,1,2),
                                        (SOME "value 1",
                                         SOME "value 3",
                                         SOME "value 2") ) )
                      end),

          should("expand as needed",
                 fn() =>
                   let val ea = EA.create()
                       val values = Vector.tabulate(1000000, fn x=>7*x)
                       val _ : int vector = values
                       val positions = Vector.map(fn v => EA.add(ea,v)) values
                       val _ : int vector = positions
                       val lookups = Vector.map(fn p=>Option.valOf(EA.subscript(ea,p))) positions
                       val _ : int vector = lookups
                   in assertEqual(values, lookups)
                   end)
         ])
