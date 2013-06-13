using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    internal abstract class ExpressionNode
    {
        //internal Scope Scope;
        internal RenderItemType Type;
        internal abstract ExpressionNode Accept(ExprVisitor visitor);
    }

    internal class InputNode : ExpressionNode
    {
        internal InputNode(RenderItemType type)
        {
            Type = type;
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitInputNode(this);
        }
    }

    internal abstract class OperatorNode : ExpressionNode
    {}

    internal class KernelNode : OperatorNode
    {
        internal IKernel Kernel;
        internal KernelNode(IKernel kernel)
        {
            this.Kernel = kernel;
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitKernelNode(this);
        }
    }

    internal class SubPipeNode : OperatorNode
    {
        internal ExpressionNode SubPipe;
        internal SubPipeNode(ExpressionNode subPipe)
        {
            this.SubPipe = subPipe;
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitSubPipeNode(this);
        }
    }

    internal class RecurNode : OperatorNode
    {
        internal RecurNode()
        {
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitRecurNode(this);
        }
    }

    internal class PipeNode : ExpressionNode
    {
        internal ExpressionNode BaseNode;
        internal OperatorNode Operator;
        internal PipeNode(ExpressionNode baseExpr, OperatorNode op)
        {
            this.BaseNode = baseExpr;
            this.Operator = op;
        }


        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitPipeNode(this);
        }
    }

    internal class MapNode : ExpressionNode
    {
        internal ExpressionNode BaseNode;
        internal OperatorNode Operator;
        internal MapNode(ExpressionNode baseExpr, OperatorNode op)
        {
            this.BaseNode = baseExpr;
            this.Operator = op;
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitMapNode(this);
        }
    }

    internal class BranchNode : ExpressionNode
    {
        internal ExpressionNode BaseNode;
        internal ExpressionNode TrueBranch, FalseBranch;
        internal IPredicate Predicate;
        internal BranchNode(ExpressionNode baseNode, IPredicate predicate, ExpressionNode trueExpr, ExpressionNode falseExpr)
        {
            this.BaseNode = baseNode;
            this.Predicate = predicate;
            this.TrueBranch = trueExpr;
            this.FalseBranch = falseExpr;
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitBranchNode(this);
        }
    }

    internal class RecurringNode : ExpressionNode
    {
        internal ExpressionNode BaseNode;
        internal ExpressionNode SubExpr;
        internal RecurringNode(ExpressionNode baseNode, ExpressionNode subExpr)
        {
            this.BaseNode = baseNode;
            this.SubExpr = subExpr;
        }

        internal override ExpressionNode Accept(ExprVisitor visitor)
        {
            return visitor.VisitRecurringNode(this);
        }
    }
}
