using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public abstract class ExpressionNode
    {
        internal Scope Scope;
        public CodeType Type;
        public CompileResult Compile(Context context)
        {
            return Compiler.Compile(context, this);
        }
        public abstract ExpressionNode Accept(ExprVisitor visitor);
    }

    public class FunctionNode : ExpressionNode
    {
        
        public string FunctionName;
        public List<string> Variables;
        public ExpressionNode Body;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitFunctionNode(this);
        }
    }

    public class VarRefNode : ExpressionNode
    {
        public string Variable;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitVarRefNode(this);
        }
    }

    public class ApplyNode : ExpressionNode
    {
        public FunctionNode Function;
        public List<ExpressionNode> Arguments;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitApplyNode(this);
        }
    }

    public class InvokeNode : ExpressionNode
    {
        public string FunctionName;
        public List<ExpressionNode> Arguments;
        public bool IsExternal = false;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitInvokeNode(this);
        }
    }

    public class SelectNode : ExpressionNode
    {
        public ExpressionNode BaseNode;
        public FunctionNode Selector;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitSelectNode(this);
        }
    }

    public class SelectManyNode : ExpressionNode
    {
        public ExpressionNode BaseNode;
        public FunctionNode Selector;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitSelectManyNode(this);
        }
    }

    public class IfNode : ExpressionNode
    {
        public ExpressionNode Predicate;
        public ExpressionNode TrueExpression, FalseExpression;
        public override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitIfNode(this);
        }
    }

    public static class Exprs
    {
        public static ApplyNode Apply(FunctionNode func, params ExpressionNode[] args)
        {
            return new ApplyNode() { Function = func, Arguments = args.ToList() };
        }
        public static InvokeNode Call(string funcName, params ExpressionNode[] args)
        {
            return new InvokeNode() { FunctionName = funcName, Arguments = args.ToList(), IsExternal = false };
        }
        public static InvokeNode CallExt(string funcName, params ExpressionNode[] args)
        {
            return new InvokeNode() { FunctionName = funcName, Arguments = args.ToList(), IsExternal = true };
        }
        public static FunctionNode Lambda(string varName, ExpressionNode body)
        {
            return new FunctionNode() { Body = body, Variables = new List<string>() { varName }, FunctionName = "ignore_func_name" };
        }
        public static FunctionNode Lambda(string[] varNames, ExpressionNode body)
        {
            return new FunctionNode() { Body = body, Variables = varNames.ToList(), FunctionName = "ignore_func_name" };
        }
        public static FunctionNode Fix(string funName, CodeType returnType, string varName, ExpressionNode body)
        {
            return new FunctionNode() { Body = body, Variables = new List<string>() { varName }, FunctionName = funName, Type = new FunctionType() { ReturnType = returnType } };
        }
        public static VarRefNode Var(string varName)
        {
            return new VarRefNode() { Variable = varName };
        }
        public static SelectNode Select(ExpressionNode baseNode, FunctionNode fun)
        {
            return new SelectNode() { BaseNode = baseNode, Selector = fun };
        }
        public static SelectManyNode SelectMany(ExpressionNode baseNode, FunctionNode fun)
        {
            return new SelectManyNode() { BaseNode = baseNode, Selector = fun };
        }
        public static IfNode If(ExpressionNode predicate, ExpressionNode tExpr, ExpressionNode fExpr)
        {
            return new IfNode() { Predicate = predicate, TrueExpression = tExpr, FalseExpression = fExpr };
        }
    }
}
