using RenderGen.Compiler;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGen.Kernels
{
    class ShadeKernel : IKernel<Fragment, Pixel>
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
