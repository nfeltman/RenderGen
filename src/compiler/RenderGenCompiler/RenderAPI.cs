using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    public static class RenderAPI
    {
        public static RenderStream<TResult> PipeGS<TResult>(this RenderStream<TupleItem<RaySet, GeometrySet>> stream, IKernel<TupleItem<RaySet, GeometrySet>, Container<TResult>> kernel)
            where TResult : RenderItemType, new()
        {
            return new RenderStream<TResult>(new PipeNode(stream.Expression, new KernelNode(kernel)));
        }

        public static RecurringRenderStream<TRecIn, TRecResult, TResult> PipeGS<TResult, TRecIn, TRecResult>(this RecurringRenderStream<TRecIn, TRecResult, TupleItem<RaySet, GeometrySet>> stream, IKernel<TupleItem<RaySet, GeometrySet>, Container<TResult>> kernel)
            where TResult : RenderItemType, new()
            where TRecIn : RenderItemType, new()
            where TRecResult : RenderItemType, new()
        {
            return new RecurringRenderStream<TRecIn, TRecResult, TResult>(new PipeNode(stream.Expression, new KernelNode(kernel)));
        }

        public static RenderStream<TResult> PipeGS<TResult>(this RenderStream<TupleItem<RaySet, GeometrySet>> stream, Func<RenderStream<TupleItem<RaySet, GeometrySet>>, StreamResult<Container<TResult>>> subPipe)
            where TResult : RenderItemType, new()
        {
            var input = RenderStream.CreateRayGeometryStream();
            var output = subPipe(input);
            return new RenderStream<TResult>(new PipeNode(stream.Expression, new SubPipeNode(output.Expression)));
        }

        public static RecurringRenderStream<TRecIn, TRecResult, TResult> PipeGS<TRecIn, TRecResult, TResult>(this RecurringRenderStream<TRecIn, TRecResult, TupleItem<RaySet, GeometrySet>> stream, Func<RecurringRenderStream<TRecIn, TRecResult, TupleItem<RaySet, GeometrySet>>, StreamResult<Container<TResult>>> subPipe)
            where TResult : RenderItemType, new()
            where TRecIn : RenderItemType, new()
            where TRecResult : RenderItemType, new()
        {
            var input = RenderStream.CreateRecurringRayGeometryStream<TRecIn, TRecResult>();
            var output = subPipe(input);
            return new RecurringRenderStream<TRecIn, TRecResult, TResult>(new PipeNode(stream.Expression, new SubPipeNode(output.Expression)));
        }

        public static RenderStream<TResult> Map<T, TResult>(this RenderStream<T> stream, IKernel<T, TResult> kernel)
            where TResult : RenderItemType, new()
            where T : RenderItemType, new()
        {
            return new RenderStream<TResult>(new MapNode(stream.Expression, new KernelNode(kernel)));
        }

        public static RecurringRenderStream<TRecIn, TRecResult, TResult> Map<TRecIn, TRecResult, T, TResult>(this RecurringRenderStream<TRecIn, TRecResult, T> stream, IKernel<T, TResult> kernel)
            where TResult : RenderItemType, new()
            where TRecIn : RenderItemType, new()
            where TRecResult : RenderItemType, new()
            where T : RenderItemType, new()
        {
            return new RecurringRenderStream<TRecIn, TRecResult, TResult>(new MapNode(stream.Expression, new KernelNode(kernel)));
        }

        public static RenderStream<TResult> Map<T, TResult>(this RenderStream<T> stream, Func<RenderStream<T>, StreamResult<TResult>> subPipe)
            where TResult : RenderItemType, new()
            where T : RenderItemType, new()
        {
            var input = new RenderStream<T>(new InputNode(new T()));
            var output = subPipe(input);
            return new RenderStream<TResult>(new PipeNode(stream.Expression, new SubPipeNode(output.Expression)));
        }

        public static RecurringRenderStream<TRecIn, TRecResult, TResult> Map<TRecIn, TRecResult, T, TResult>(this RecurringRenderStream<TRecIn, TRecResult, T> stream, Func<RecurringRenderStream<TRecIn, TRecResult, T>, StreamResult<TResult>> subPipe)
            where TResult : RenderItemType, new()
            where T : RenderItemType, new()
            where TRecIn : RenderItemType, new()
            where TRecResult : RenderItemType, new()
        {
            var input = new RecurringRenderStream<TRecIn, TRecResult, T>(new InputNode(new T()));
            var output = subPipe(input);
            return new RecurringRenderStream<TRecIn, TRecResult, TResult>(new PipeNode(stream.Expression, new SubPipeNode(output.Expression)));
        }

        public static RenderStream<TResult> Branch<T, TResult>(
            this RenderStream<T> stream,
            IPredicate<T> predicate,
            Func<RenderStream<T>, StreamResult<TResult>> trueBranch,
            Func<RenderStream<T>, StreamResult<TResult>> falseBranch)
            where TResult : RenderItemType, new()
            where T : RenderItemType, new()
        {
            var input1 = new RenderStream<T>(new InputNode(new T()));
            var trueExpr = trueBranch(input1).Expression;
            var input2 = new RenderStream<T>(new InputNode(new T()));
            var falseExpr = falseBranch(input2).Expression;
            return new RenderStream<TResult>(new BranchNode(stream.Expression, predicate, trueExpr, falseExpr));
        }

        public static RecurringRenderStream<TRecIn, TRecResult, TResult> Branch<TRecIn, TRecResult, T, TResult>(
            this RecurringRenderStream<TRecIn, TRecResult, T> stream,
            IPredicate<T> predicate,
            Func<RecurringRenderStream<TRecIn, TRecResult, T>, StreamResult<TResult>> trueBranch,
            Func<RecurringRenderStream<TRecIn, TRecResult, T>, StreamResult<TResult>> falseBranch)
            where TResult : RenderItemType, new()
            where T : RenderItemType, new()
            where TRecIn : RenderItemType, new()
            where TRecResult : RenderItemType, new()
        {
            var input1 = new RecurringRenderStream<TRecIn, TRecResult, T>(new InputNode(new T()));
            var trueExpr = trueBranch(input1).Expression;
            var input2 = new RecurringRenderStream<TRecIn, TRecResult, T>(new InputNode(new T()));
            var falseExpr = falseBranch(input2).Expression;
            return new RecurringRenderStream<TRecIn, TRecResult, TResult>(new BranchNode(stream.Expression, predicate, trueExpr, falseExpr));
        }

        public static StreamResult<TResult> PipeGS_Recur<TIn, TResult>(this RecurringRenderStream<TIn, TResult, TIn> stream)
            where TResult : RenderItemType, new()
            where TIn : RenderItemType, new()
        {
            return new StreamResult<TResult>(new PipeNode(stream.Expression, new RecurNode()));
        }

        public static StreamResult<TResult> MapRecur<TIn, TResult>(this RecurringRenderStream<TIn, TResult, TIn> stream)
            where TResult : RenderItemType, new()
            where TIn : RenderItemType, new()
        {
            return new StreamResult<TResult>(new MapNode(stream.Expression, new RecurNode()));
        }
    }
}
