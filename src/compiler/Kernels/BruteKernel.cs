using RenderGen.Compiler;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGen.Kernels
{
    class BruteKernel : IKernel<TupleItem<RaySet, GeometrySet>, Container<Fragment>>
    {
        public RenderItemType ReturnType
        {
            get { throw new NotImplementedException(); }
        }

        public RenderItemType[] ArgumentTypes
        {
            get { throw new NotImplementedException(); }
        }

        public string Name
        {
            get { throw new NotImplementedException(); }
        }
    }
}
