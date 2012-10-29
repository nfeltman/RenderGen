using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public class Context
    {
        internal Dictionary<string, ObjectDef> ObjectDefs = new Dictionary<string,ObjectDef>();
        internal Dictionary<string, ExtFunctionDef> FunctionDefs = new Dictionary<string,ExtFunctionDef>();
        internal Dictionary<string, CodeType> Inputs = new Dictionary<string,CodeType>();
        public void AddObjectType(ObjectDef obj)
        {
            ObjectDefs[obj.Type.TypeName] = obj;
        }
        public void AddFunction(ExtFunctionDef f)
        {
            FunctionDefs[f.Name] = f;
        }
        public void DeclareInput(string name, CodeType type)
        {
            Inputs[name] = type;
        }

        internal Scope CreateScope()
        {
            Scope rs = new Scope();
            foreach (var p in Inputs)
                rs.Variables.Add(p.Key, p.Value);
            foreach (var p in FunctionDefs)
                rs.Variables.Add(p.Key, p.Value.Type);
            return rs;
        }
    }

    internal class Scope
    {
        internal Dictionary<string, CodeType> Variables = new Dictionary<string,CodeType>();
        internal Scope Parent = null;

        internal Scope CreateChild()
        {
            Scope rs = new Scope();
            rs.Parent = this;
            return rs;
        }

        internal CodeType Lookup(string name)
        {
            CodeType value;
            if (Variables.TryGetValue(name, out value))
                return value;
            else if (Parent != null)
                return Parent.Lookup(name);
            else
                return null;
        }
    }
}
