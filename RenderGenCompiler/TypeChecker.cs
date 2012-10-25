using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGenCompiler
{
    class TypeChecker : ExprVisitor
    {
        public List<String> Errors { get; private set; }

        private Context context;
        private Stack<Scope> scopeStack = new Stack<Scope>();

        public TypeChecker(Context context)
        {
            this.Errors = new List<string>();
            this.context = context;
            scopeStack.Push(context.CreateScope());
        }

        public override FunctionNode VisitFunctionNode(FunctionNode node)
        {
            // requires node.Type already exists ( parameter types already evaluated )
            var type = node.Type as FunctionType;
            var nScope = scopeStack.Peek().CreateChild();
            node.Scope = nScope;
            nScope.Variables.Add(node.FunctionName, node.Type);
            if (type == null)
            {
                Errors.Add("Cannot infer parameter types from given expression.");
                node.Type = new ErrorCodeType();
            }
            else
            {
                if (node.Variables.Count != type.ParameterTypes.Length)
                {
                    Errors.Add("Unexpected function type.");
                    node.Type = new ErrorCodeType();
                }
                else
                {
                    for (int i = 0; i < node.Variables.Count; i++)
                        nScope.Variables[node.Variables[i]] = type.ParameterTypes[i];
                    scopeStack.Push(nScope);
                    node.Body.Accept(this);
                    scopeStack.Pop();
                    if (!(type.ReturnType is ErrorCodeType) && type.ReturnType != null)
                    {
                        if (!type.ReturnType.Equals(node.Body.Type))
                            Errors.Add("Fix: actual return type does not match specified return type.");
                    }
                    else
                    {
                        type.ReturnType = node.Body.Type;
                    }
                }

            }

            return node;
        }
        public override VarRefNode VisitVarRefNode(VarRefNode node)
        {
            node.Scope = scopeStack.Peek();
            var type = scopeStack.Peek().Lookup(node.Variable);
            if (type == null)
            {
                Errors.Add("Undeclared variable: " + node.Variable);
                type = new ErrorCodeType();
            }
            node.Type = type;
            return node;
        }
        public override ApplyNode VisitApplyNode(ApplyNode node)
        {
            node.Scope = scopeStack.Peek();
            node.Arguments.ForEach(n => n.Accept(this));
            node.Function.Type = CreateFunctionType(node.Function.Type, node.Arguments.Select(n => n.Type).ToArray());
            node.Function.Accept(this);
            
            var ftype = node.Function.Type as FunctionType;
            if (ftype != null)
            {
                // verify argument types
                node.Type = (node.Function.Type as FunctionType).ReturnType;
                if (node.Arguments.Count != ftype.ParameterTypes.Length)
                    Errors.Add(string.Format("Function '{0}' does not accept {1} arguments", node.Function.FunctionName, node.Arguments.Count));
                else
                {
                    for (int i = 0; i<node.Arguments.Count; i++)
                        if (!node.Arguments[i].Type.Equals(ftype.ParameterTypes[i]))
                            Errors.Add(string.Format("Applying function '{0}': argument {1} type mismatch", node.Function.FunctionName, i));
                }
            }
            else
                node.Type = new ErrorCodeType(); 
            return node;
        }

        public override InvokeNode VisitInvokeNode(InvokeNode node)
        {
            node.Scope = scopeStack.Peek();
            var type = scopeStack.Peek().Lookup(node.FunctionName) as FunctionType;
            if (type == null)
            {
                Errors.Add("Undefined function " + node.FunctionName);
                node.Type = new ErrorCodeType();
            }
            else
            {
                if (type.ParameterTypes.Length != node.Arguments.Count)
                {
                    Errors.Add(string.Format("Function '{0}' does not accept {1} arguments", node.FunctionName, node.Arguments.Count));
                }
                else
                {
                    for (int i = 0; i < node.Arguments.Count; i++)
                    {
                        node.Arguments[i].Accept(this);
                        if (!node.Arguments[i].Type.Equals(type.ParameterTypes[i]))
                            Errors.Add(string.Format("Applying function '{0}': argument {1} type mismatch", node.FunctionName, i));
                    }
                }
                node.Type = type.ReturnType;
            }
            return node;
        }

        public override SelectNode VisitSelectNode(SelectNode node)
        {
            node.Scope = scopeStack.Peek();
            node.BaseNode.Accept(this);
            if (!(node.BaseNode.Type is ContainerType))
            {
                node.Type = new ErrorCodeType();
            }
            else
            {
                node.Selector.Type = CreateFunctionType(node.Selector.Type, (node.BaseNode.Type as ContainerType).ClientType);
                node.Selector.Accept(this);
                var ftype = (node.Selector.Type as FunctionType);
                if (ftype != null)
                    node.Type = new ContainerType(ftype.ReturnType);
                else
                    node.Type = new ErrorCodeType();
            }
            return node;
        }

        private FunctionType CreateFunctionType(CodeType ftype, params CodeType[] codeType)
        {
            FunctionType rs = new FunctionType();
            rs.ParameterTypes = codeType;
            if (ftype is FunctionType)
                rs.ReturnType = (ftype as FunctionType).ReturnType;
            else
                rs.ReturnType = new ErrorCodeType();
            return rs;
        }

        public override SelectManyNode VisitSelectManyNode(SelectManyNode node)
        {
            node.Scope = scopeStack.Peek();
            node.BaseNode.Accept(this);
            if (!(node.BaseNode.Type is ContainerType))
            {
                node.Type = new ErrorCodeType();
            }
            else
            {
                node.Selector.Type = CreateFunctionType(node.Selector.Type, (node.BaseNode.Type as ContainerType).ClientType);
                node.Selector.Accept(this);
                var ftype = (node.Selector.Type as FunctionType);
                if (ftype != null)
                {
                    if (ftype.ReturnType is ContainerType)
                        node.Type = ftype.ReturnType;
                    else
                    {
                        Errors.Add("SelectMany: the selector must return a collection.");
                        node.Type = new ErrorCodeType();
                    }
                }
                else
                    node.Type = new ErrorCodeType();
            }
            return node;
        }
        public override IfNode VisitIfNode(IfNode node)
        {
            node.Scope = scopeStack.Peek();
            node.Predicate.Accept(this);
            if (!PlainCodeType.IntType.Equals(node.Predicate.Type) &&
                !PlainCodeType.BoolType.Equals(node.Predicate.Type))
            {
                Errors.Add("if: predicate must evaluate to bool or int.");
            }
            node.TrueExpression.Accept(this);
            node.FalseExpression.Accept(this);
            node.Type = node.TrueExpression.Type;
            if (!node.TrueExpression.Type.Equals(node.FalseExpression.Type))
            {
                Errors.Add("if: branches must evaluate to same type.");
            }
            return node;
        }
    }
}
