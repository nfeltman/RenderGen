using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    public class PrintVisitor : ExprVisitor
    {
        StringBuilder sb = new StringBuilder();
        int indent = 0;
        private void WriteLine(string value)
        {
            sb.AppendLine(value);
            for (int i = 0; i < indent; i++)
                sb.Append("  ");
        }
        private void WriteList(List<ExpressionNode> list)
        {
            for (int i = 0; i < list.Count; i++)
            {
                list[i].Accept(this);
                if (i != list.Count - 1)
                    sb.Append(", ");
            }
        }
        private void WriteList(List<string> list)
        {
            for (int i = 0; i < list.Count; i++)
            {
                sb.Append(list[i]);
                if (i != list.Count - 1)
                    sb.Append(", ");
            }
        }
        public override string ToString()
        {
            return sb.ToString();
        }
        public override FunctionNode VisitFunctionNode(FunctionNode node)
        {
            sb.AppendFormat("Fun {0}(", node.FunctionName);
            WriteList(node.Variables);
            WriteLine(")");
            sb.Append("{");
            indent++;
            WriteLine("");
            node.Body.Accept(this);
            indent--;
            WriteLine("");
            sb.Append("}");
            return node;
        }
        public override VarRefNode VisitVarRefNode(VarRefNode node)
        {
            sb.Append(node.Variable);
            return node;
        }
        public override ApplyNode VisitApplyNode(ApplyNode node)
        {
            node.Function.Accept(this);
            sb.Append("(");
            WriteList(node.Arguments);
            sb.Append(")");
            return node;
        }
        public override InvokeNode VisitInvokeNode(InvokeNode node)
        {
            sb.Append(node.FunctionName);
            sb.Append("(");
            WriteList(node.Arguments);
            sb.Append(")");
            return node;
        }
        public override SelectNode VisitSelectNode(SelectNode node)
        {
            sb.Append("Select(");
            node.BaseNode.Accept(this);
            sb.Append(", ");
            node.Selector.Accept(this);
            sb.Append(")");
            return node;
        }
        public override SelectManyNode VisitSelectManyNode(SelectManyNode node)
        {
            sb.Append("SelectMany(");
            node.BaseNode.Accept(this);
            sb.Append(", ");
            node.Selector.Accept(this);
            sb.Append(")");
            return node;
        }
        public override IfNode VisitIfNode(IfNode node)
        {
            sb.Append("if (");
            node.Predicate.Accept(this);
            sb.Append("; ");
            node.TrueExpression.Accept(this);
            sb.Append(" else ");
            node.FalseExpression.Accept(this);
            sb.Append(")");
            return node;
        }
    }
}
