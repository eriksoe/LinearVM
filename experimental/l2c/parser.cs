using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace LinearVM.L2C {
    public class FileParser {
        private readonly TokenParser tokenParser;
        private readonly ProgramContext context;

        private List<int> symbolIndexTable;

        public FileParser(StreamReader stream, ProgramContext context) {
            this.tokenParser = new TokenParser(stream);
            this.context = context;
        }

        public void ParseFile() {
            try {
                tokenParser.ExpectTag("LINEARVM");
                ReadModuleHeader();
                this.symbolIndexTable = ReadSymbolTable();
                ReadNameTable();
                ReadImportTable();

                tokenParser.ExpectEOF();
            } catch (FileFormatException e) {
                throw new FileFormatException(e.Message, tokenParser.StreamApproxPosition);
            }
        }

        public void ReadModuleHeader() {
            tokenParser.ExpectTag("MODULE");
            var moduleNameIndex = tokenParser.ReadInteger();
            Console.WriteLine("moduleNameIndex="+moduleNameIndex);
        }

        public List<int> ReadSymbolTable() {
            tokenParser.ExpectTag("SYMBOLS");
            var size = tokenParser.ReadInteger();

            var table = new List<int>();
            for (int i=1; i<=size; i++) {
                var symbol = tokenParser.ReadString();
                var canonIndex = context.SymbolTable.GetIndex(symbol);
                Console.WriteLine("Symbol #"+i+"="+symbol+" ~> "+canonIndex);
                table.Add(canonIndex);
            }
            return table;
        }

        public void ReadNameTable() {
            tokenParser.ExpectTag("NAMES");
            var size = tokenParser.ReadInteger();

            var table = new List<int>();
            for (int i=1; i<=size; i++) {
                var parentNameIndex = tokenParser.ReadInteger();
                var symbolIndex = tokenParser.ReadInteger();
                var parentCanonIndex = parentNameIndex==0 ? 0 : table[parentNameIndex-1];
                var symbolCanonIndex = symbolIndexTable[symbolIndex-1];
                var nameElement = new NameElement(parentCanonIndex, symbolCanonIndex);
                var symbol = context.SymbolTable.Lookup(symbolCanonIndex);

                var canonNameIndex = context.NameTable.GetIndex(nameElement);
                Console.WriteLine("Name #"+i+"="+parentNameIndex+"."+symbolIndex+"("+symbol+") ~> "+canonNameIndex);
                table.Add(canonNameIndex);
            }
        }

        public void ReadImportTable() {
            tokenParser.ExpectTag("IMPORTEDTYPES");
            var size = tokenParser.ReadInteger();
            for (int i=1; i<=size; i++) {
                var nameIndex = tokenParser.ReadInteger();
                var arity = tokenParser.ReadInteger();
                Console.WriteLine("Import #"+i+"="+nameIndex+"/"+arity);
            }
        }

    }

    public class TokenParser {
        private readonly StreamReader stream;

        public long StreamApproxPosition {
            get {
                return stream.BaseStream.Position;
            }
        }

        public TokenParser(StreamReader stream) {
            this.stream = stream;
        }

        public void ExpectTag(string expectedTag) {
            ExpectChar('$');
            var s = ReadIdentifier();
            if (s != expectedTag) throw new FileFormatException("Expected tag \"$"+expectedTag+"\"");
        }

        public void ExpectEOF() {
            var c = NextCharOrEOFSkippingWhitespace();
            if (c != -1) throw new FileFormatException("Expected EOF, found '"+(char)c+"'");
        }

        public int ReadInteger() {
            var c = NextCharSkippingWhitespace();
            if (!Char.IsDigit(c)) throw new FileFormatException("Expected an integer");
            int acc = c-'0';
            do {
                c = NextChar();
                if (Char.IsDigit(c))
                    acc = 10*acc + (c-'0');
                else
                    break;
            } while (true);
            // TODO: Overflow check.
            return acc;
        }

        public string ReadString() {
            {
                var c = NextCharSkippingWhitespace();
                if (c != '\"') throw new FileFormatException("Expected a string");
            }

            var buf = new StringBuilder();
            do {
                var c = NextChar();
                if (c == '\"') break;
                buf.Append(c);
            } while (true);

            return buf.ToString();
        }


        //========================================
        private string ReadIdentifier() {
            var buf = new StringBuilder();
            do {
                var c = NextChar();
                if (!Char.IsLetter(c)) break;
                buf.Append(c);
            } while (true);
            return buf.ToString();
        }

        private void ExpectChar(char expectedChar) {
            var c = NextCharSkippingWhitespace();
            if (c != expectedChar) throw new FileFormatException("Expected '"+expectedChar+"'");
        }

        //========================================

        private char NextCharSkippingWhitespace() {
            int c = NextCharOrEOFSkippingWhitespace();
            if (c == -1) throw new FileFormatException("EOF");
            return (char)c;
        }

        private int NextCharOrEOFSkippingWhitespace() {
            int c;
            do {
                c = NextCharInclEOF();
                if (c==-1) break;
                if (Char.IsWhiteSpace((char)c)) continue;
                if (c=='(') {
                    do {
                        c = NextChar();
                    } while (c != ')');
                    continue;
                }

                break;
            } while (true);
            return c;
        }

        private char NextChar() {
            int c = NextCharInclEOF();
            if (c == -1) throw new FileFormatException("EOF");
            return (char)c;
        }

        private int NextCharInclEOF() {
            int c = stream.Read();
            return c;
        }
    }

    public class FileFormatException : Exception {
        public FileFormatException(string reason) : base(reason) {}
        public FileFormatException(string reason, long offset)
            :
            base("File format error before offset "+offset+": "+reason) {}
    }
}