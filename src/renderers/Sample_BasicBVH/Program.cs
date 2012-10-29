using RenderGenCompiler;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Sample_BasicBVH
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
            var type_geomSamplePair = new PlainCodeType("sample_tri_pair");

            context.AddFunction(new ExtFunctionDef("SamplePartition_1", type_SampleSet, type_SampleSet));
            context.AddFunction(new ExtFunctionDef("leafCondition", PlainCodeType.BoolType, type_GeomSet));
            context.AddFunction(new ExtFunctionDef("createLeaf", new ContainerType(type_geomSamplePair), type_Sample, type_GeomSet));
            context.AddFunction(new ExtFunctionDef("partition_Geom", new ContainerType(type_GeomSet), type_GeomSet));

            context.DeclareInput("samples", type_SampleSet);
            context.DeclareInput("geometry", type_GeomSet);

            var expr =
                Exprs.SelectMany(
                    Exprs.Call("SamplePartition_1", Exprs.Var("samples")),
                    Exprs.Lambda("s",
                        Exprs.Apply(
                            Exprs.Fix("f", new ContainerType(type_geomSamplePair), "g",
                                Exprs.If(Exprs.CallExt("leafCondition", Exprs.Var("g")),
                                         Exprs.CallExt("createLeaf", Exprs.Var("s"), Exprs.Var("g")),
                                         Exprs.SelectMany(Exprs.CallExt("partition_Geom", Exprs.Var("g")),
                                             Exprs.Lambda("g1", Exprs.Call("f", Exprs.Var("g1")))
                                         )
                                )
                            ),
                            Exprs.Var("geometry")
                        )
                    )
                );

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
            //var printer = new PrintVisitor();
            //expr.Accept(printer);
            //File.WriteAllText("syntax.txt", printer.ToString());
        }
    }
}
