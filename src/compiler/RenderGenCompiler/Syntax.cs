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

    public class GeometryRayInputNode : ExpressionNode
    { }

    public class PipeNode : ExpressionNode
    {
        public ExpressionNode BaseNode;
        public Operator Operator;
        public PipeNode(ExpressionNode baseExpr, Operator op)
        {
            this.BaseNode = baseExpr;
            this.Operator = op;
        }

    }

    public class MapNode : ExpressionNode
    {
        public ExpressionNode BaseNode;
        public Operator Operator;
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
        // geometry predicates.
        // predicate tree

        // partition node: 1s
        // geometry partition node: impl
        // fix
        // helper : shading.
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
        public static AggregateNode Aggregate(Aggregator aggregator, string aggregationField, ExpressionNode baseNode)
        {
            return new AggregateNode() { Aggregator = aggregator, AggregationField = aggregationField, BaseExpr = baseNode };
        }
    }
}
