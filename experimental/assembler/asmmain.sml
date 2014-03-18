
fun pos2str(line,col) : string=
    Int.toString line^":"^Int.toString col

fun main(_,[asmfilename]) =
    let
        val _ = print("DB| Assembler called on "^asmfilename^"\n")
        val f = TextIO.openIn asmfilename
    (* TODO: Make stream-based. *)
        val file_as_text = TextIO.inputAll f
        val _ = TextIO.closeIn f
    in
        (AsmParser.parse_string file_as_text;
         print "Parsed.\n")
        handle Tokenizer.SyntaxError(pos,err) =>
               print ((pos2str pos)^": Syntax error: "^err^"\n")
    end
  | main(cmdname,_) =
    print("Usage: "^cmdname^" <infile.asm>\n")


val _ = main(CommandLine.name(), CommandLine.arguments())
