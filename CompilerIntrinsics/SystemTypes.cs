using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public static class SystemTypes
    {
        public static StructType Float2 = new StructType("Vec4") 
        {
            { "x", PlainCodeType.FloatType },
            { "y", PlainCodeType.FloatType }
        };
        public static StructType Float3 = new StructType("Vec3") 
        {
            { "x", PlainCodeType.FloatType },
            { "y", PlainCodeType.FloatType },
            { "z", PlainCodeType.FloatType }
        };
        public static StructType Float4 = new StructType("Vec4") 
        {
            { "x", PlainCodeType.FloatType },
            { "y", PlainCodeType.FloatType },
            { "z", PlainCodeType.FloatType },
            { "w", PlainCodeType.FloatType }
        };
        public static StructType Intersection = new StructType("Intersection") 
        {
            { "t", PlainCodeType.FloatType },
            { "Position", Float3 },
            { "Normal", Float3 },
            { "TexCoord", Float2 },
            { "MaterialId", PlainCodeType.IntType },
        };
    }
}
