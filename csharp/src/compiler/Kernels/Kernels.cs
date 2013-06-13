using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using RenderGen.Compiler;
using RenderGen.Kernels;

namespace RenderGen.Client
{
    public static class Kernels
    {
        public static IKernel<TupleItem<RaySet, GeometrySet>, Container<TupleItem<RaySet, GeometrySet>>> SamplePartition_1 = new SamplePartition_1_Kernel();
        public static IKernel<TupleItem<RaySet, GeometrySet>, Container<TupleItem<RaySet, GeometrySet>>> BinaryGeometryPartition = new BinaryGeometryPartition_Kernel();
        public static IKernel<TupleItem<RaySet, GeometrySet>, Container<Fragment>> Brute = new BruteKernel();
        public static IKernel<Fragment, Pixel> Shade = new ShadeKernel();

    }
}
