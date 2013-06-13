using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    internal class PrintVisitor : ExprVisitor
    {
        internal override ExpressionNode VisitInputNode(InputNode node)
        {
            return base.VisitInputNode(node);
        }

        internal override ExpressionNode VisitKernelNode(KernelNode node)
        {
            return base.VisitKernelNode(node);
        }

        internal override ExpressionNode VisitMapNode(MapNode node)
        {
            return base.VisitMapNode(node);
        }

        internal override ExpressionNode VisitPipeNode(PipeNode node)
        {
            return base.VisitPipeNode(node);
        }

        internal override ExpressionNode VisitRecurNode(RecurNode node)
        {
            return base.VisitRecurNode(node);
        }

        internal override ExpressionNode VisitRecurringNode(RecurringNode node)
        {
            return base.VisitRecurringNode(node);
        }

        internal override ExpressionNode VisitSubPipeNode(SubPipeNode node)
        {
            return base.VisitSubPipeNode(node);
        }

        internal override ExpressionNode VisitBranchNode(BranchNode node)
        {
            return base.VisitBranchNode(node);
        }
    }
}
