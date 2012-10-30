using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGenCompiler
{
    public class Operator<TIn, TOut>
    {
        internal Operator Op; 
        internal Operator(Operator op)
        {
            this.Op = op;
        }
    }

    public abstract class Operator
    {
        public static Operator<TIn, TOut> Kernel<TIn, TOut>(NativeKernel<TIn, TOut> kernel)
        {
            return new Operator<TIn, TOut>(new NativeOperator(kernel));
        }
    
    }

    public class NativeOperator : Operator
    {
        public NativeKernel Kernel;
        internal NativeOperator(NativeKernel kernel)
        {
            Kernel = kernel;
        }
    }

    public class SubPipeOperator : Operator
    {
        public ExpressionNode SubExpr;

        public SubPipeOperator(ExpressionNode subExpr)
        {
            this.SubExpr = subExpr;
        }
    }
}
