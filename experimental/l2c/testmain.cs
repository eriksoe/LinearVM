using System;
using System.IO;
using System.Text;

namespace LinearVM.L2C {
    public class TestMain {
        public static void Main(string[] args) {
            var filename = args[0];
            Console.WriteLine("Input file: "+filename);

            var context = new ProgramContext();
            var parser = new FileParser(new StreamReader(filename, Encoding.UTF8),
                                        context);
            parser.ParseFile();
            Console.WriteLine("OK");
        }
    }
}