using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    using GeometryList = Container<Geometry>;
    using RayList = Container<Ray>;
    using RayGeometryListTuple = TupleItem<Container<Ray>, Container<Geometry>>;

    public abstract class RenderItemType
    {
        public abstract string CodeName { get; }
    }

    public class Container<T> : RenderItemType
        where T : RenderItemType, new()
    {
        public override string CodeName
        {
            get { return string.Format("List<{0}>", new T().CodeName); }
        }
    }

    public class TupleItem<T1, T2> : RenderItemType
        where T1 : RenderItemType, new()
        where T2 : RenderItemType, new()
    {
        public override string CodeName
        {
            get { return string.Format("Tuple<{0}, {1}>", new T1().CodeName, new T2().CodeName); }
        }
    }

    public class Geometry : RenderItemType
    {
        public override string CodeName
        {
            get { return "Triangle"; }
        }
    }

    public class Fragment : RenderItemType
    {
        public override string CodeName
        {
            get { return "Fragment"; }
        }
    }

    public class Pixel : RenderItemType
    {
        public override string CodeName
        {
            get { return "Pixel"; }
        }
    }

    public class Ray : RenderItemType
    {
        public override string CodeName
        {
            get { return "Ray"; }
        }
    }

    public class GeometrySet : RenderItemType
    {
        public override string CodeName
        {
            get { return "TriangleSet"; }
        }
    }

    public class RaySet : RenderItemType
    {
        public override string CodeName
        {
            get { return "RaySet"; }
        }
    }

}
