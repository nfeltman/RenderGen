using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    public class Environment<T>
    {
        public static readonly Environment<T> EMPTY = new Environment<T>();
        private string label;
        private T val;
        private Environment<T> next;

        private Environment()
        {
            label = null;
            val = default(T);
        }

        private Environment(String l, T v, Environment<T> e)
        {
            if (l == null)
                throw new ArgumentException("Environment label cannot be null.");
            label = l;
            val = v;
            next = e;
        }

        public Environment<T> Add(String l, T v)
        {
            return new Environment<T>(l, v, this);
        }

        public Erroring<T> LookUp(string l)
        {
            if (label == null) return new Erroring<T>("Label \'"+l+"\' not found.");
            if (label.Equals(l)) return new Erroring<T>(val);
            return next.LookUp(l);
        }

        public T AssertLookUp(string l)
        {
            if (label == null) throw new Exception("Label not found, but was asserted to exist.");
            if (label.Equals(l)) return val;
            return next.AssertLookUp(l);
        }
    }

    public static class UnitEnvironmentHelper
    {
        public static Environment<Unit> Add(this Environment<Unit> e, String l)
        {
            return e.Add(l, Unit.ONLY);
        }
    }
}
