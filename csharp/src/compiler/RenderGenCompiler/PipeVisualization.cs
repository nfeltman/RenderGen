using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace RenderGen.Compiler.Visualization
{
    class DrawingStyle : IDisposable
    {
        public Font TextFont;
        public int ConnectorSize;
        public int ConnectorMargin;
        public int VertexMargin;
        public int SubGraphHeadPadding;
        public int BackloopSpacing;
        public int InputDiamondSize;
        public int RecurCircleSize;
        public Pen PipeGS_Pen;
        public Pen MapPen;
        public Pen BackloopPen;
        public DrawingStyle()
        {
            TextFont = new Font(FontFamily.GenericSansSerif, 16.0f);
            ConnectorSize = 24;
            InputDiamondSize = 16;
            RecurCircleSize = 20;
            ConnectorMargin = 4;
            VertexMargin = 6;
            SubGraphHeadPadding = 4;
            BackloopSpacing = 24;

            PipeGS_Pen = new Pen(Color.Black);
            PipeGS_Pen.CompoundArray = new float[] { 0.0f, 0.25f, 0.75f, 1.0f };
            PipeGS_Pen.Width = 8;

            MapPen = new Pen(Color.Black);
            MapPen.Width = 4;
            MapPen.EndCap = LineCap.Custom;

            BackloopPen = new Pen(Color.DarkGray);
            BackloopPen.DashStyle = DashStyle.Dash;
            BackloopPen.Width = 2;
            BackloopPen.EndCap = LineCap.Custom;

            using (GraphicsPath hPath = new GraphicsPath())
            {
                hPath.AddLine(new PointF(1.8f, -3), new PointF(0, 0));
                hPath.AddLine(new PointF(0, 0), new PointF(-1.8f, -3));
                var lc = new CustomLineCap(null, hPath);
                MapPen.CustomEndCap = lc;
                BackloopPen.CustomEndCap = lc;

            }
        }

        public void Dispose()
        {
            if (TextFont != null)
                TextFont.Dispose();
            if (PipeGS_Pen != null)
                PipeGS_Pen.Dispose();
            if (MapPen != null)
                MapPen.Dispose();
            if (BackloopPen != null)
                BackloopPen.Dispose();
        }
    }

    abstract class GraphicsObject
    {
        public Point Position;
        public Size Bounds;
        public abstract Size Layout(DrawingStyle ds, Graphics g, int ox, int oy);
        public abstract void Draw(DrawingStyle ds, Graphics g);
    }

    abstract class PipeVertex : GraphicsObject
    {
    }

    class InputVertex : PipeVertex
    {
        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            Bounds.Width = (int)(ds.InputDiamondSize * 1.414f);
            Bounds.Height = Bounds.Width;
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            g.TranslateTransform(Position.X + Bounds.Width / 2, Position.Y + Bounds.Height / 2);
            g.RotateTransform(45.0f);
            g.FillRectangle(Brushes.DarkGray, -ds.InputDiamondSize / 2, -ds.InputDiamondSize / 2, ds.InputDiamondSize, ds.InputDiamondSize);
            g.ResetTransform();
        }
    }

    class KernelVertex : PipeVertex
    {
        public string Text;
        public KernelVertex(string text)
        {
            this.Text = text;
        }

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            var tsize = g.MeasureString(Text, ds.TextFont).ToSize();
            Bounds.Width = ds.VertexMargin * 2 + tsize.Width;
            Bounds.Height = ds.VertexMargin * 2 + tsize.Height;
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            g.DrawString(Text, ds.TextFont, Brushes.Black, Position.X + ds.VertexMargin, Position.Y + ds.VertexMargin);
        }
    }

    class BranchVertex : PipeVertex
    {
        public string Predicate;
        public PipeGraph FirstBranch, SecondBranch;

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            var textSize = g.MeasureString(Predicate, ds.TextFont).ToSize();
            int y = oy + ds.VertexMargin + textSize.Height + ds.SubGraphHeadPadding;
            var branch1 = FirstBranch.Layout(ds, g, ox + ds.VertexMargin, y);
            y += branch1.Height + ds.SubGraphHeadPadding;
            var branch2 = SecondBranch.Layout(ds, g, ox + ds.VertexMargin, y);
            Bounds.Height = y - oy + branch2.Height + ds.VertexMargin;
            Bounds.Width = Math.Max(branch1.Width, Math.Max(branch2.Width, textSize.Width)) + ds.VertexMargin * 2;
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            g.DrawRectangle(Pens.Gray, new Rectangle(Position, Bounds));
            g.DrawString(Predicate, ds.TextFont, Brushes.Black, new Point(Position.X + ds.VertexMargin, Position.Y + ds.VertexMargin));
            FirstBranch.Draw(ds, g);
            SecondBranch.Draw(ds, g);
        }
    }

    class RecurringVertex : PipeVertex
    {
        public PipeGraph SubGraph;

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            var textSize = g.MeasureString("Recurring", ds.TextFont).ToSize();
            int y = oy + ds.VertexMargin + textSize.Height + ds.SubGraphHeadPadding;
            var subgraph = SubGraph.Layout(ds, g, ox + ds.VertexMargin, y);
            Bounds.Height = y - oy + subgraph.Height + ds.VertexMargin + ds.BackloopSpacing;
            Bounds.Width = Math.Max(subgraph.Width, textSize.Width) + ds.VertexMargin * 2;
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            g.DrawRectangle(Pens.Gray, new Rectangle(Position.X, Position.Y, Bounds.Width, Bounds.Height - ds.BackloopSpacing));
            g.DrawString("Recurring", ds.TextFont, Brushes.Black, new Point(Position.X + ds.VertexMargin, Position.Y + ds.VertexMargin));
            SubGraph.Draw(ds, g);
        }
    }

    class RecurVertex : PipeVertex
    {
        public RecurringVertex HeadNode;

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            Bounds = new Size(ds.RecurCircleSize, ds.RecurCircleSize);
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            // Draw backloop
            g.DrawLines(ds.BackloopPen, new Point[] 
            {
                new Point(Position.X + Bounds.Width/2, Position.Y + Bounds.Height/2),
                new Point(Position.X + Bounds.Width/2, HeadNode.Position.Y + HeadNode.Bounds.Height - ds.BackloopSpacing/2),
                new Point(HeadNode.Position.X-ds.ConnectorSize, HeadNode.Position.Y + HeadNode.Bounds.Height - ds.BackloopSpacing/2),
                new Point(HeadNode.Position.X-ds.ConnectorSize, HeadNode.Position.Y + HeadNode.Bounds.Height/2 + ds.ConnectorSize),
                new Point(HeadNode.Position.X, HeadNode.Position.Y + HeadNode.Position.Y + HeadNode.Bounds.Height/2 + ds.ConnectorSize)
            });
            g.FillEllipse(Brushes.Black, new Rectangle(Position, Bounds));
        }
    }

    class SubGraphVertex : PipeVertex
    {
        public PipeGraph SubGraph;

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            Bounds = SubGraph.Layout(ds, g, ox, oy);
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            SubGraph.Draw(ds, g);
        }
    }

    abstract class Connector : GraphicsObject
    {
    }

    class PipeGSConnector : Connector
    {

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            Bounds = new Size(ds.ConnectorSize, ds.ConnectorSize);
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            int ex = Position.X + Bounds.Width;
            int ey = Position.Y + Bounds.Height / 2;
            g.DrawLine(ds.PipeGS_Pen, Position.X, ey, ex - 4.5f, ey);
            using (var p = new Pen(Brushes.Black))
            {
                p.Width = 2;
                g.DrawLine(p, ex - 12, ey - 8, ex, ey);
                g.DrawLine(p, ex - 12, ey + 8, ex, ey);
            }
        }
    }

    class MapConnector : Connector
    {

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            Position = new Point(ox, oy);
            Bounds = new Size(ds.ConnectorSize, ds.ConnectorSize);
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            g.DrawLine(ds.MapPen, Position.X, Position.Y + Bounds.Height / 2, Position.X + Bounds.Width - ds.MapPen.Width, Position.Y + Bounds.Height / 2);
        }
    }

    class PipeGraph : GraphicsObject
    {
        public List<PipeVertex> Nodes = new List<PipeVertex>();
        public List<Connector> Connectors = new List<Connector>();

        public override Size Layout(DrawingStyle ds, Graphics g, int ox, int oy)
        {
            List<Size> sizes = new List<Size>();
            List<Size> connectorSizes = new List<Size>();
            foreach (var node in Nodes)
                sizes.Add(node.Layout(ds, g, ox, oy));
            foreach (var c in Connectors)
                connectorSizes.Add(c.Layout(ds, g, ox, oy));
            var height = sizes.Max(s => s.Height);
            int x = ox;
            for (int i = 0; i < Nodes.Count; i++)
            {
                int y = oy + (height - sizes[i].Height) / 2;
                Nodes[i].Layout(ds, g, x, y);
                x += Nodes[i].Bounds.Width;
                if (i < connectorSizes.Count)
                {
                    x += ds.ConnectorMargin;
                    Connectors[i].Layout(ds, g, x, oy + (height - connectorSizes[i].Height) / 2);
                    x += Connectors[i].Bounds.Width + ds.ConnectorMargin;
                }
            }
            Bounds.Width = x - ox;
            Bounds.Height = height;
            return Bounds;
        }

        public override void Draw(DrawingStyle ds, Graphics g)
        {
            foreach (var node in Nodes)
                node.Draw(ds, g);
            foreach (var c in Connectors)
                c.Draw(ds, g);
        }
    }

    class GraphGeneratorVisitor : ExprVisitor
    {
        Stack<PipeGraph> graphStack = new Stack<PipeGraph>() { };
        Stack<PipeVertex> curVertexStack = new Stack<PipeVertex>();
        Stack<RecurringVertex> recurringVertex = new Stack<RecurringVertex>();

        PipeGraph graph { get { return graphStack.Peek(); } }
        PipeVertex curVertex { get { return curVertexStack.Peek(); } set { curVertexStack.Pop(); curVertexStack.Push(value); } }

        public PipeGraph Graph { get { return graph; } }

        public GraphGeneratorVisitor()
        {
            graphStack.Push(new PipeGraph());
            curVertexStack.Push(null);
        }

        internal override ExpressionNode VisitInputNode(InputNode node)
        {
            var v = new InputVertex();
            curVertex = v;
            graph.Nodes.Add(v);
            return node;
        }

        internal override ExpressionNode VisitKernelNode(KernelNode node)
        {
            var v = new KernelVertex(node.Kernel.Name);
            graph.Nodes.Add(v);
            curVertex = v;
            return node;
        }

        internal override ExpressionNode VisitMapNode(MapNode node)
        {
            node.BaseNode.Accept(this);
            graph.Connectors.Add(new MapConnector());
            node.Operator.Accept(this);
            return node;
        }

        internal override ExpressionNode VisitPipeNode(PipeNode node)
        {
            node.BaseNode.Accept(this);
            graph.Connectors.Add(new PipeGSConnector());
            node.Operator.Accept(this);
            return node;
        }

        internal override ExpressionNode VisitRecurNode(RecurNode node)
        {
            var v = new RecurVertex();
            v.HeadNode = recurringVertex.Peek();
            graph.Nodes.Add(v);
            curVertex = v;
            return node;
        }

        internal override ExpressionNode VisitRecurringNode(RecurringNode node)
        {
            node.BaseNode.Accept(this);
            var v = new RecurringVertex();
            graph.Nodes.Add(v);
            graph.Connectors.Add(new MapConnector());
            v.SubGraph = new PipeGraph();
            recurringVertex.Push(v);
            graphStack.Push(v.SubGraph);
            curVertexStack.Push(null);
            node.SubExpr.Accept(this);
            curVertexStack.Pop();
            graphStack.Pop();
            recurringVertex.Pop();
            curVertex = v;
            return node;
        }

        internal override ExpressionNode VisitSubPipeNode(SubPipeNode node)
        {
            var v = new SubGraphVertex();
            graph.Nodes.Add(v);
            v.SubGraph = new PipeGraph();
            graphStack.Push(v.SubGraph);
            curVertexStack.Push(null);
            node.SubPipe.Accept(this);
            curVertexStack.Pop();
            graphStack.Pop();
            curVertex = v;
            return node;
        }

        internal override ExpressionNode VisitBranchNode(BranchNode node)
        {
            node.BaseNode.Accept(this);
            graph.Connectors.Add(new MapConnector());
            var v = new BranchVertex();
            v.Predicate = node.Predicate.ToString();

            graph.Nodes.Add(v);

            v.FirstBranch = new PipeGraph();
            graphStack.Push(v.FirstBranch);
            curVertexStack.Push(null);
            node.TrueBranch.Accept(this);
            curVertexStack.Pop();
            graphStack.Pop();

            v.SecondBranch = new PipeGraph();
            graphStack.Push(v.SecondBranch);
            curVertexStack.Push(null);
            node.FalseBranch.Accept(this);
            curVertexStack.Pop();
            graphStack.Pop();

            curVertex = v;
            return node;
        }
    }

    public static class PipeVisualizer
    {
        public static Bitmap Visualize(RenderStream stream)
        {
            Visualization.GraphGeneratorVisitor graphGen = new Visualization.GraphGeneratorVisitor();
            stream.Expression.Accept(graphGen);
            var g = graphGen.Graph;
            using (var ds = new Visualization.DrawingStyle())
            {
                using (var bmp = new Bitmap(2, 2))
                using (var graphics = Graphics.FromImage(bmp))
                    g.Layout(ds, graphics, 0, 0);
                var image = new Bitmap(g.Bounds.Width + 1, g.Bounds.Height + 3, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
                using (var graphics = Graphics.FromImage(image))
                {
                    graphics.Clear(Color.White);
                    graphics.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAlias;
                    graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;
                    g.Draw(ds, graphics);
                    return image;
                }
            }
        }
        public static void Visualize(RenderStream stream, string fileName) // save pipeline graph to file
        {
            using (var bmp = Visualize(stream))
                bmp.Save(fileName);
        }
    }
}
