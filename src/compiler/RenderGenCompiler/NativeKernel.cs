using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGen.Compiler
{
    public interface IKernel
    {
        string Name { get; }
    }

    public interface IKernel<TIn, TOut> : IKernel
    {
        RenderItemType ReturnType { get; }
        RenderItemType[] ArgumentTypes { get; }
    }
}
