using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CppIncludeGen
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("inclgen -fileName");
                return;
            }
            var fileName = args[0];
            var dirName = Path.GetDirectoryName(fileName);
            CodeBuilder cb = new CodeBuilder();
            cb.SearchDirs.Add(dirName);
            for (int i = 1; i < args.Length; i++)
                cb.SearchDirs.Add(args[i]);
            cb.ParseFile(dirName, Path.GetFileName(fileName));
            File.WriteAllText("sys.inc", cb.Code);
        }
    }

    class CodeBuilder
    {
        private StringBuilder sb = new StringBuilder();
        public List<String> SearchDirs = new List<string>();
        private HashSet<string> processedFiles = new HashSet<string>();
        public string Code { get { return sb.ToString(); } }
        public void ParseText(string dirName, string[] lines)
        {
            foreach (var l in lines)
            {
                if (l.StartsWith("#include"))
                {
                    var words = l.Split(' ');
                    var file = words[1].Substring(1, words[1].Length - 2);
                    if (ParseFile(dirName, file))
                        continue;
                    else
                        Console.WriteLine("Ignored file " + file);
                }
                sb.AppendLine(l);
            }
        }
        public bool ParseFile(string dirName, string fileName)
        {
            if (processedFiles.Contains(Path.GetFileName(fileName).ToUpper()))
                return true;
            string fname = Path.Combine(dirName, fileName);
            if (!File.Exists(fname))
            {
                foreach (var d in SearchDirs)
                {
                    fname = Path.Combine(d, fileName);
                    if (File.Exists(fname))
                        break;
                }
            }
            if (File.Exists(fname))
            {
                var txt = File.ReadAllLines(fname);
                processedFiles.Add(Path.GetFileName(fileName).ToUpper());
                ParseText(Path.GetDirectoryName(fname), txt);
                
                return true;
            }
            else
                return false;
        }
    }
}
