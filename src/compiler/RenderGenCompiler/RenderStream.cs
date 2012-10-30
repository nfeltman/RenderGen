using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public class RenderStream
    {
        private RenderStream()
        { 
        }
        public static RenderStream<TupleItem<Container<Ray>, Container<Geometry>>> CreateRayGeometryStream()
        {
            return new RenderStream<TupleItem<Container<Ray>, Container<Geometry>>>(new GeometryRayInputNode());
        }
    }

    public class RenderStream<T>
    {
        internal ExpressionNode Expression;

        internal RenderStream(ExpressionNode expr)
        {
            Expression = expr;
        }

        public RenderStream<TResult> PipeGS<TResult>(NativeKernel<T, TResult> kernel)
        {
            return new RenderStream<TResult>(new PipeNode(Expression, new NativeOperator(kernel)));
        }
        public RenderStream<TResult> PipeGS<TResult>(Func<RenderStream<T>, RenderStream<TResult>> subPipe)
        {
            var input = new RenderStream<T>(new GeometryRayInputNode());
            var output = subPipe(input);
            return new RenderStream<TResult>(new PipeNode(Expression, new SubPipeOperator(output.Expression)));
        }
    }
}
