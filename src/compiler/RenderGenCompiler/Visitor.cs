using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public class ExprVisitor
    {
        public virtual FunctionNode VisitFunctionNode(FunctionNode node)
        {
            node.Body.Accept(this);
            return node;
        }
        public virtual VarRefNode VisitVarRefNode(VarRefNode node)
        {
            return node;
        }
        public virtual AggregateNode VisitAggregateNode(AggregateNode node)
        {
            node.BaseExpr.Accept(this);
            return node;
        }
        public virtual ApplyNode VisitApplyNode(ApplyNode node)
        {
            node.Function.Accept(this);
            node.Arguments.ForEach(n => n.Accept(this));
            return node;
        }
        public virtual InvokeNode VisitInvokeNode(InvokeNode node)
        {
            node.Arguments.ForEach(n => n.Accept(this));
            return node;
        }
        public virtual SelectNode VisitSelectNode(SelectNode node)
        {
            node.BaseNode.Accept(this);
            node.Selector.Accept(this);
            return node;
        }
        public virtual SelectManyNode VisitSelectManyNode(SelectManyNode node)
        {
            node.BaseNode.Accept(this);
            node.Selector.Accept(this);
            return node;
        }
        public virtual IfNode VisitIfNode(IfNode node)
        {
            node.Predicate.Accept(this);
            node.TrueExpression.Accept(this);
            node.FalseExpression.Accept(this);
            return node;
        }
    }
}
