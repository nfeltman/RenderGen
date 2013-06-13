using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    public struct Erroring<T>
    {
        private string message;
        private T value;

        public bool IsError { get { return message != null; } }
        
        public Erroring(T t)
        {
            message = null;
            value = t;
        }
        public Erroring(String m)
        {
            if (m == null)
                throw new ArgumentException("Message cannot be null.");
            message = m;
            value = default(T);
        }

        // monadic bind
        public Erroring<U> Bind<U>(Func<T,Erroring<U>> f)
        {
            return message == null ? f(value) : new Erroring<U>(message);
        }
        public Erroring<U> FMap<U>(Func<T, U> f)
        {
            return message == null ? new Erroring<U>(f(value)) : new Erroring<U>(message);
        }
    }

    public static class UnitErroringHelper
    {
        public static Erroring<U> Bind<U>(this Erroring<Unit> er, Func<Erroring<U>> r)
        {
            return er.Bind(_ => r());
        }
    }
}
