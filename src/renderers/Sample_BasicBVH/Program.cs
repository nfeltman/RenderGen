using RenderGen.Compiler;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using RenderGen.Client;
using RenderGen.Compiler.Visualization;

namespace Sample_BasicBVH
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = RenderStream.CreateRayGeometryStream();
            var renderer = 
                input.PipeGS(Kernels.SamplePartition_1)
                     .Recurring<TupleItem<RaySet, GeometrySet>>(
                          stream => stream.Branch(
                              new GeometryCountPredicate<RaySet>(PredicateOperator.Less, 10),
                              x => x,
                              x => x.PipeGS(Kernels.BinaryGeometryPartition).PipeGS_Recur()))
                     .PipeGS(Kernels.Brute)
                     .Map(Kernels.Shade);


            var rs = renderer.Compile();
            PipeVisualizer.Visualize(renderer, "pipe.bmp");

            if (!rs.Success)
            {
                foreach (var err in rs.Errors)
                    Console.WriteLine(err);
            }
            else
            {
                File.WriteAllText("result.cpp", rs.Code);
            }
        }
    }
}
