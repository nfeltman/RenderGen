using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mockingbird
{
    class IntermediateRep
    {
        Dictionary<string, IRExpr> context;
    }

    enum IRHalfType { SetToSet, SetToSingle, SingleToSingle }

    struct IRType
    {
        IRHalfType G { get; set; }
        IRHalfType S { get; set; }
    }

    class IRExpr
    {
        IRType Type { get; set; }
    }

    class IRCompose : IRExpr
    {
        IRExpr First { get; set; }
        IRExpr Second { get; set; }
    }

    class InSizeBranch : IRExpr
    {
        IRExpr First { get; set; }
        IRExpr Second { get; set; }
    }

    class InGeneralBranch : IRExpr
    {
        IRExpr First { get; set; }
        IRExpr Second { get; set; }
    }

    class InReference : IRExpr
    {
        string RefName { set; get; }
    }
}
