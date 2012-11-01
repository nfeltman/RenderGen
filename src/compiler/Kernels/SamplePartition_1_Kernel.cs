using RenderGen.Compiler;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGen.Kernels
{
    class SamplePartition_1_Kernel : IKernel<TupleItem<RaySet, GeometrySet>, Container<TupleItem<RaySet, GeometrySet>>>
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
            get { return "SamplePartition1"; }
        }
    }
}
