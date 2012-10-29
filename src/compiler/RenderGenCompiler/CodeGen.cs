using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    class CodeGen : ExprVisitor
    {
        public override FunctionNode VisitFunctionNode(FunctionNode node)
        {
            node.Body.Accept(this);
            return node;
        }
        public override VarRefNode VisitVarRefNode(VarRefNode node)
        {
            return node;
        }
        public override ApplyNode VisitApplyNode(ApplyNode node)
        {
            node.Function.Accept(this);
            node.Arguments.ForEach(n => n.Accept(this));
            return node;
        }
        public override InvokeNode VisitInvokeNode(InvokeNode node)
        {
            node.Arguments.ForEach(n => n.Accept(this));
            return node;
        }
        public override SelectNode VisitSelectNode(SelectNode node)
        {
            node.BaseNode.Accept(this);
            node.Selector.Accept(this);
            return node;
        }
        public override SelectManyNode VisitSelectManyNode(SelectManyNode node)
        {
            node.BaseNode.Accept(this);
            node.Selector.Accept(this);
            return node;
        }
        public override IfNode VisitIfNode(IfNode node)
        {
            node.Predicate.Accept(this);
            node.TrueExpression.Accept(this);
            node.FalseExpression.Accept(this);
            return node;
        }
    }
}
