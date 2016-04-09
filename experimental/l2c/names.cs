using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace LinearVM.L2C {
    public class ProgramContext {
        public readonly SymbolTable SymbolTable = new SymbolTable();
        public readonly NameTable NameTable = new NameTable();
    }


    public class CanonicalTable<T> where T: class {
        private readonly List<T> items = new List<T>();
        private readonly Dictionary<T,int> lookupTable = new Dictionary<T,int>();

        public int GetIndex(T value) {
            int index;
            if (!lookupTable.TryGetValue(value, out index)) {
                index = items.Count + 1;
                items.Add(value);
                lookupTable.Add(value, index);
            }
            return index;
        }

        public T Lookup(int index) {
            return items[index-1];
        }
    }

    public class SymbolTable : CanonicalTable<string> {
    }

    public class NameTable : CanonicalTable<NameElement> {
    }

    public class NameElement : IEquatable<NameElement> {
        public readonly int ParentIndex, SymbolIndex;
        public NameElement(int parentIndex, int symbolIndex) {
            this.ParentIndex = parentIndex;
            this.SymbolIndex = symbolIndex;
        }

        public override int GetHashCode() {
            return ParentIndex * 0xFFF + SymbolIndex;
        }

        bool IEquatable<NameElement>.Equals(NameElement other) {
            return (this.ParentIndex == other.ParentIndex &&
                    this.SymbolIndex == other.SymbolIndex);
        }
    }
}
