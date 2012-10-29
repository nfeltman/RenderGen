using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    public interface MbNode
    {
        T Accept<T>(MbNodeVisitor<T> v);
        T Accept<T, A>(MbNodeVisitor<T,A> v, A arg);
    }

    public class Compose : MbNode
    {
        public MbNode First { get; set; }
        public MbNode Second { get; set; }
        public T Accept<T>(MbNodeVisitor<T> v)
        {
            return v.ForCompose(this);
        }
        public T Accept<T, A>(MbNodeVisitor<T, A> v, A arg)
        {
            return v.ForCompose(this, arg);
        }
    }

    public class Fix : MbNode
    {
        public MbNode Child { get; set; }
        public String Label { get; set; }
        public MbType Type { get; set; }
        public T Accept<T>(MbNodeVisitor<T> v)
        {
            return v.ForFix(this);
        }
        public T Accept<T, A>(MbNodeVisitor<T, A> v, A arg)
        {
            return v.ForFix(this, arg);
        }
    }

    public class Call : MbNode
    {
        public String Label { get; set; }
        public T Accept<T>(MbNodeVisitor<T> v)
        {
            return v.ForCall(this);
        }
        public T Accept<T, A>(MbNodeVisitor<T, A> v, A arg)
        {
            return v.ForCall(this, arg);
        }
    }

    public class Branch : MbNode
    {
        public MbNode Left { get; set; }
        public MbNode Right { get; set; }
        public T Accept<T>(MbNodeVisitor<T> v)
        {
            return v.ForBranch(this);
        }
        public T Accept<T, A>(MbNodeVisitor<T, A> v, A arg)
        {
            return v.ForBranch(this, arg);
        }
    }

    public interface MbNodeVisitor<T>
    {
        T ForCompose(Compose b);
        T ForFix(Fix b);
        T ForCall(Call b);
        T ForBranch(Branch b);   
    }

    public interface MbNodeVisitor<T, A1>
    {
        T ForCompose(Compose b, A1 arg1);
        T ForFix(Fix b, A1 arg1);
        T ForCall(Call b, A1 arg1);
        T ForBranch(Branch b, A1 arg1);
    }
}
