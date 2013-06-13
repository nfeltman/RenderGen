using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace RenderGen.Compiler
{
    public abstract class CppCompiler
    {
        protected CppCompiler()
        { }

        protected void Error(string errMsg)
        {
            Console.WriteLine(errMsg);
        }

        public static CppCompiler FromSpec(string specName)
        {
            using (var reader = new StreamReader(specName))
            {
                var line = reader.ReadLine();
                if (line == "msvc")
                {
                    return new MSVCCompiler(reader);
                }
                else // gcc
                {
                    throw new NotImplementedException("gcc compiler call not implemented");
                }
            }

        }

        protected abstract bool Compile(string fileName, CppCompilerOptions options);

        public void CreateDLL(string code)
        {
            string moduleDef = "LIBRARY\nEXPORTS\nRenderMain";
            StringBuilder cb = new StringBuilder();
            using (var stream = typeof(CppCompiler).Assembly.GetManifestResourceStream("RenderGenCompiler.sys.inc"))
            using (var txtReader = new StreamReader(stream))
            {
                cb.Append(txtReader.ReadToEnd());
                cb.AppendLine();
                cb.Append(code);
            }
            if (!Directory.Exists("build"))
                Directory.CreateDirectory("build");
            File.WriteAllText("build\\renderer.cpp", cb.ToString());
            File.WriteAllText("build\\Export.def", moduleDef);
            Compile(Path.GetFullPath("build\\renderer.cpp"), new CppCompilerOptions() { Debug = false });
            
        }
    }

    public class CppCompilerOptions
    {
        public bool Debug;
        public CppCompilerOptions()
        {
            Debug = false;
        }
    }

    class MSVCCompiler : CppCompiler
    {
        private string compilerDir;
        private string sdkDir;
        public MSVCCompiler(StreamReader specReader)
        {
            compilerDir = specReader.ReadLine();//C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC
            sdkDir = specReader.ReadLine(); //C:\Program Files (x86)\Windows Kits\8.0\Lib\win8\um\x86
        }

        protected override bool Compile(string fileName, CppCompilerOptions options)
        {
            var idePath = Path.Combine(compilerDir, "..\\Common7\\IDE");
            var startInfo = new ProcessStartInfo(Path.Combine(compilerDir, "bin\\cl.exe"));
            var dllName = Path.GetFileNameWithoutExtension(fileName);
            startInfo.UseShellExecute = false;
            startInfo.RedirectStandardOutput = true;
            startInfo.RedirectStandardError = true;
            startInfo.WorkingDirectory = Path.GetDirectoryName(Path.GetFullPath(fileName));
            startInfo.Arguments = @"/GS /GL /analyze- /W3 /Gy /Zc:wchar_t /Zi /Gm- /O2 /fp:precise /D""WIN32"" /D""NDEBUG"" /D""_WINDOWS"" /D""_USRDLL""/D""_WINDLL"" /D""_UNICODE"" /D""UNICODE"" /errorReport:prompt /WX- /Zc:forScope /Gd /Oy- /Oi /MT /EHsc /nologo """ + fileName + @""" ""kernel32.lib"" ""user32.lib"" ""gdi32.lib"" ""winspool.lib"" ""comdlg32.lib"" /link /OUT:""" + dllName + @".dll"" /DEF:""Export.def"" /DLL /nologo";
            var sdk = Environment.GetEnvironmentVariable("WindowsSdkDir");
            startInfo.EnvironmentVariables.Add("INCLUDE", Path.Combine(compilerDir, "include"));
            startInfo.EnvironmentVariables.Add("LIB", Path.Combine(compilerDir, "lib")  + ";" + sdkDir);
            startInfo.EnvironmentVariables.Add("LIBPATH", Path.Combine(compilerDir, "lib") + ";" + sdkDir);
            startInfo.EnvironmentVariables["PATH"] = Environment.GetEnvironmentVariable("PATH") + ";" + idePath;
            var proc = Process.Start(startInfo);
            var output = proc.StandardOutput.ReadToEnd();
            proc.WaitForExit();

            Console.WriteLine(output);
            if (proc.ExitCode != 0)
                return false;
            else
                return true;
        }
    }
}
