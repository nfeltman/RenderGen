using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RenderGen.Compiler
{
    public class CompileResult
    {
        public bool Success;
        public List<string> Errors = new List<string>();
        public string Code;
    }
}
