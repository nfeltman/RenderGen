using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using RenderGenCompiler;
using System.IO;

namespace Sample_Brute
{
    class Program
    {
        static void Main(string[] args)
        {
            var context = new Context();
            var type_Sample = new PlainCodeType("sample");
            var type_geom = new PlainCodeType("triangle");
            var type_SampleSet = new ContainerType(type_Sample);
            var type_GeomSet = new ContainerType(type_geom);

            context.AddFunction(new ExtFunctionDef("ray_tri_intersection", new ContainerType(SystemTypes.Intersection), type_geom, type_Sample));

            context.DeclareInput("samples", type_SampleSet);
            context.DeclareInput("geometry", type_GeomSet);

            var expr =
                Exprs.Select(
                    Exprs.Var("samples"),
                    Exprs.Lambda("s",
                        Exprs.Aggregate(
                            Aggregator.Min, "t",
                            Exprs.SelectMany(Exprs.Var("geometry"),
                                Exprs.Lambda("g",
                                    Exprs.CallExt("ray_tri_intersection", Exprs.Var("g"), Exprs.Var("s"))
                                )
                            )
                        )
                    )
                );
            CppCompiler.FromSpec("msvc.txt").CreateDLL("void RenderMain(RenderGen::ImageRef img, RenderGen::Scene * scene){}");
            var rs = expr.Compile(context);
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
