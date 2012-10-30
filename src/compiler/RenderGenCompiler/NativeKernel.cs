using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGenCompiler
{
    public class NativeKernel
    {
        public string Name;
    }
    public class NativeKernel<TIn, TOut> : NativeKernel
    {
        public CodeType ReturnType;
        public CodeType[] ArgumentTypes;
    }
}
