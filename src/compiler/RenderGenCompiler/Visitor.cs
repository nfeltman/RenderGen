using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    internal class ExprVisitor
    {
        internal virtual ExpressionNode VisitPipeNode(PipeNode node)
        {
            node.BaseNode.Accept(this);
            return node;
        }

        internal virtual ExpressionNode VisitMapNode(MapNode node)
        {
            node.BaseNode.Accept(this);
            return node;
        }

        internal virtual ExpressionNode VisitBranchNode(BranchNode node)
        {
            node.BaseNode.Accept(this);
            node.TrueBranch.Accept(this);
            node.FalseBranch.Accept(this);
            return node;
        }

        internal virtual ExpressionNode VisitRecurringNode(RecurringNode node)
        {
            node.BaseNode.Accept(this);
            node.SubExpr.Accept(this);
            return node;
        }

        internal virtual ExpressionNode VisitKernelNode(KernelNode node)
        {
            return node;
        }

        internal virtual ExpressionNode VisitInputNode(InputNode node)
        {
            return node;
        }

        internal virtual ExpressionNode VisitSubPipeNode(SubPipeNode node)
        {
            node.SubPipe.Accept(this);
            return node;
        }

        internal virtual ExpressionNode VisitRecurNode(RecurNode node)
        {
            return node;
        }
    }
}
