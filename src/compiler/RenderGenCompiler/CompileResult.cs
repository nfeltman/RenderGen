using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGenCompiler
{
    public class CompileResult
    {
        public bool Success;
        public List<string> Errors = new List<string>();
        public string Code;
    }
}
