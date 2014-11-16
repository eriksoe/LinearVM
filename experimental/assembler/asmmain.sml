
fun read_file filename =
    let
        val f = TextIO.openIn filename
        (* TODO: Make stream-based? *)
        val file_as_text = TextIO.inputAll f
        val _ = TextIO.closeIn f
    in file_as_text
    end;

fun parse_file filename =
    AsmParser.parse_string (read_file filename);

fun main(_,[asmfilename]) =
    let
        val _ = print("DB| Assembler called on "^asmfilename^"\n")
    in
        (parse_file asmfilename;
         print "Parsed.\n")
        handle Tokenizer.SyntaxError(pos,err) =>
               print ((Tokenizer.pos2str pos)^": Syntax error: "^err^"\n")
    end
  | main(cmdname,_) =
    print("Usage: "^cmdname^" <infile.asm>\n")


val _ = main(CommandLine.name(), CommandLine.arguments())
