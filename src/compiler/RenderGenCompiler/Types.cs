using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    using GeometryList = Container<Geometry>;
    using RayList = Container<Ray>;
    using RayGeometryListTuple = TupleItem<Container<Ray>, Container<Geometry>>;

    public class RenderItemType
    {
        public string CodeName;
    }

    public class Container<T> : RenderItemType
    { }

    public class TupleItem<T1, T2> : RenderItemType
    { 
    }

    public class Geometry : RenderItemType
    { }

    public class Ray : RenderItemType
    { }

}
