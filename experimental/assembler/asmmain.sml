
fun main(_,[infile]) =
    print("DB| Assembler called on "^infile^"\n")
  | main(cmdname,_) =
    print("Usage: "^cmdname^" <infile.asm>\n")


val _ = main(CommandLine.name(), CommandLine.arguments())
