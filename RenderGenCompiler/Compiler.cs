using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGenCompiler
{
    static class Compiler
    {
        internal static CompileResult Compile(Context context, ExpressionNode exprNode)
        {
            CompileResult rs = new CompileResult();
            rs.Success = true;
            var typeChecker = new TypeChecker(context);
            exprNode.Accept(typeChecker);
            if (typeChecker.Errors.Count > 0)
            {
                rs.Errors.AddRange(typeChecker.Errors);
                rs.Success = false;
                return rs;
            }

            return rs;
        }
    }
}
