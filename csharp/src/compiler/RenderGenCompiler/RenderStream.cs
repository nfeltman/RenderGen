using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    public class RenderStream
    {
        internal ExpressionNode Expression;

        internal RenderStream(ExpressionNode expr)
        {
            this.Expression = expr;
        }

        internal static RecurringRenderStream<TRecIn, TRecResult, TupleItem<RaySet, GeometrySet>> CreateRecurringRayGeometryStream<TRecIn, TRecResult>()
            where TRecIn : RenderItemType, new()
            where TRecResult : RenderItemType, new()
        {
            return new RecurringRenderStream<TRecIn, TRecResult, TupleItem<RaySet, GeometrySet>>(new InputNode(new TupleItem<RaySet, GeometrySet>()));
        }

        public static RenderStream<TupleItem<RaySet, GeometrySet>> CreateRayGeometryStream()
        {
            return new RenderStream<TupleItem<RaySet, GeometrySet>>(new InputNode(new TupleItem<RaySet, GeometrySet>()));
        }

        public CompileResult Compile()
        {
            return RenderStreamCompiler.Compile(this);
        }
    }

    public class StreamResult<T> : RenderStream
             where T : RenderItemType, new ()
    {
        internal StreamResult(ExpressionNode expr)
            : base(expr)
        {
        }
    }

    public class RenderStream<T> : StreamResult<T>
        where T : RenderItemType, new ()
    {
        internal RenderStream(ExpressionNode expr)
            : base(expr)
        {
        }

        public RenderStream<TResult> Recurring<TResult>(
           Func<RecurringRenderStream<T, TResult, T>, StreamResult<TResult>> subPipe)
            where TResult : RenderItemType, new()
        {
            var input = new RecurringRenderStream<T, TResult, T>(new InputNode(new T()));
            var expr = subPipe(input).Expression;
            return new RenderStream<TResult>(new RecurringNode(Expression, expr));
        }
    }

    public class RecurringRenderStream<TIn, TResult, T> : StreamResult<T>
        where T : RenderItemType, new()
        where TResult : RenderItemType, new()
        where TIn : RenderItemType, new()
    {
        internal RecurringRenderStream(ExpressionNode expr)
            : base(expr)
        {
        }
    }
}
