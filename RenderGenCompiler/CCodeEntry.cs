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

    public class StructType : CodeType, ICollection<KeyValuePair<string, CodeType>>
    {
        public string TypeName;
        public Dictionary<string, CodeType> Fields = new Dictionary<string,CodeType>();
        public StructType()
        {}

        public StructType(string typeName)
        {
            this.TypeName = typeName;
        }

        public void Add(string fieldName, CodeType type)
        {
            Fields.Add(fieldName, type);
        }

        public override bool Equals(CodeType other)
        {
            return other is StructType && (other as StructType).TypeName == this.TypeName;
        }

        public void Add(KeyValuePair<string, CodeType> item)
        {
            Fields.Add(item.Key, item.Value);
        }

        public void Clear()
        {
            Fields.Clear();
        }

        public bool Contains(KeyValuePair<string, CodeType> item)
        {
            return Fields.Contains(item);
        }

        public void CopyTo(KeyValuePair<string, CodeType>[] array, int arrayIndex)
        {
            throw new NotImplementedException();
        }

        public int Count
        {
            get { return Fields.Count; }
        }

        public bool IsReadOnly
        {
            get { return false; }
        }

        public bool Remove(KeyValuePair<string, CodeType> item)
        {
            return Fields.Remove(item.Key);
        }

        public IEnumerator<KeyValuePair<string, CodeType>> GetEnumerator()
        {
            return Fields.GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return Fields.GetEnumerator();
        }
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
