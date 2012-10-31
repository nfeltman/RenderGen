using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGen.Compiler
{
    public enum PredicateOperator
    {
        Less, Greater, Equal,
        LessOrEqual, GreaterOrEqual,
        NotEqual
    }
    public interface IPredicate
    {
    }

    public interface IPredicate<T> : IPredicate
        where T : RenderItemType
    { }

    public class GeometryCountPredicate<TSample> : IPredicate<TupleItem<TSample, GeometrySet>>
        where TSample : RenderItemType, new()
    {
        public PredicateOperator Operator { get; set; }
        public int Threshold { get; set; }

        public GeometryCountPredicate(PredicateOperator op, int count)
        {
            this.Operator = op;
            this.Threshold = count;
        }
    }
}
