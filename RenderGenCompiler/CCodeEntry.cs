using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public class CodeEntry
    {
        public string Code;
    }

    public abstract class CodeType : IEquatable<CodeType>
    {
        public abstract bool Equals(CodeType other);
    }

    public class PlainCodeType : CodeType
    {
        public string TypeName;

        public PlainCodeType()
        {}

        public PlainCodeType(string typeName)
        {
            this.TypeName = typeName;
        }

        public override bool Equals(CodeType other)
        {
            return other is PlainCodeType && (other as PlainCodeType).TypeName == this.TypeName;
        }

        public static PlainCodeType IntType = new PlainCodeType("int");
        public static PlainCodeType FloatType = new PlainCodeType("float");
        public static PlainCodeType BoolType = new PlainCodeType("bool");
    }

    public class ContainerType : CodeType
    {
        public CodeType ClientType;
        public ContainerType(CodeType clientType)
        {
            this.ClientType = clientType;
        }
        public override bool Equals(CodeType other)
        {
            return other is ContainerType && (other as ContainerType).ClientType.Equals(this.ClientType);
        }
    }

    public class FunctionType : CodeType
    {
        public CodeType ReturnType;
        public CodeType[] ParameterTypes;
        public override bool Equals(CodeType other)
        {
            return other is FunctionType && (other as FunctionType).ReturnType.Equals(this.ReturnType) &&
                (other as FunctionType).ParameterTypes.Aggregate(true, (x,t)=>x && t.Equals(this.ReturnType));
        }
    }

    class ErrorCodeType : CodeType
    {
        public override bool Equals(CodeType other)
        {
            return false;
        }
    }

    public class ObjectDef : CodeEntry
    {
        public PlainCodeType Type;
    }

    public class ExtFunctionDef : CodeEntry
    {
        public FunctionType Type;
        public string Name;
        public ExtFunctionDef(string name, CodeType returnType, params CodeType[] paramTypes)
        {
            this.Type = new FunctionType() { ReturnType = returnType, ParameterTypes = paramTypes };
            this.Name = name;
        }
    }
}
