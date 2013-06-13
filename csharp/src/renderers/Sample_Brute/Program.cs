using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using RenderGen.Compiler;
using System.IO;
using RenderGen.Client;

namespace Sample_Brute
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = RenderStream.CreateRayGeometryStream();
            var renderer =
                 input.PipeGS(Kernels.Brute)
                      .Map(Kernels.Shade);


            var rs = renderer.Compile();
            if (!rs.Success)
            {
                foreach (var err in rs.Errors)
                    Console.WriteLine(err);
            }
            else
            {
                File.WriteAllText("result.cpp", rs.Code);
            }

            //CppCompiler.FromSpec("msvc.txt").CreateDLL("void RenderMain(RenderGen::ImageRef img, RenderGen::Scene * scene){}");
        }
    }
}
