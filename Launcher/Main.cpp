#include "CoreLib/Basic.h"
#include "CoreLib/LibIO.h"
#include "Runtime/VectorMath.h"
#include "Runtime/ObjModel.h"
#include "Runtime/Common.h"
#include "CoreLib/Parser.h"
#include "CoreLib/DLL.h"

using namespace CoreLib::Basic;
using namespace CoreLib::IO;
using namespace VectorMath;

namespace RenderGen
{
	class StartupArguments
	{
	public:
		String FileName;
		String SceneFileName;
	public:
		static StartupArguments Parse(int argc, wchar_t *argv[])
		{
			StartupArguments rs;
			if (argc != 3)
				throw Exception(L"Expected command line: launcher [renderer_filename] [scene_filename]");
			rs.FileName = argv[1];
			rs.SceneFileName = argv[2];
			return rs;
		}
	};

	class SceneProfile
	{
	public:
		String FileName;
		float FOV, ZMin, ZMax;
		int Width, Height;
		Matrix4 Transform;

		SceneProfile()
		{
			FOV = 75.0f;
			ZMin = 1.0f;
			ZMax = 10000.0f;
			Width = 512;
			Height = 512;
			Matrix4::CreateIdentityMatrix(Transform);
		}
	public:
		static SceneProfile FromFile(String fileName)
		{
			SceneProfile rs;
			CoreLib::Text::Parser parser(File::ReadAllText(fileName));
			while (!parser.IsEnd())
			{
				auto tk = parser.ReadWord();
				if (tk == L"file")
					rs.FileName = parser.ReadStringLiteral();
				else if (tk == L"width")
					rs.Width = parser.ReadInt();
				else if (tk == L"height")
					rs.Height = parser.ReadInt();
				else if (tk == L"fov")
					rs.FOV = (float)parser.ReadDouble();
				else if (tk == L"zmin" || tk == L"znear")
					rs.ZMin = (float)parser.ReadDouble();
				else if (tk == L"zmax" || tk == L"zfar")
					rs.ZMax = (float)parser.ReadDouble();
				else if (tk == L"transform")
				{
					for (int i = 0; i<4; i++)
						for (int j = 0; j<4; j++)
							rs.Transform.m[j][i] = (float)parser.ReadDouble();
				}
				else
					parser.ReadToken();
			}
			return rs;
		}
	};

	int Main(int argc, wchar_t *argv[])
	{
		try
		{
			auto args = StartupArguments::Parse(argc, argv);

			auto sceneProfile = SceneProfile::FromFile(args.SceneFileName);
			ObjModel obj;
			if (!LoadObj(obj, Path::Combine(Path::GetDirectoryName(args.SceneFileName), sceneProfile.FileName).ToMultiByteString()))
				throw Exception(L"Failed to load \'" + sceneProfile.FileName + L"\'");

			printf("Mesh loaded. %d polygons.\n", obj.Faces.Count());

			// Load renderer library
			CoreLib::System::DynamicLibrary renderLib(args.FileName);
			RenderFunction render = renderLib.GetProc<RenderFunction>(L"RenderMain");

			if (render == 0)
				throw Exception(L"Failed to load renderer library.");
			

			// Create triangles buffer
			Scene scene;
			scene.ZMin = sceneProfile.ZMin;
			scene.ZMax = sceneProfile.ZMax;
			scene.FOV = sceneProfile.FOV;

			for (int i = 0; i<obj.Faces.Count(); i++)
			{
				Triangle tri;
				tri.Normal = obj.Normals[obj.Faces[i].NormalIds[0]];
				scene.Triangles.Add(tri);
			}

			printf("Render started...\n");

			Image image(sceneProfile.Width, sceneProfile.Height);
			render(image.CreateRef(), &scene);

			// Save image
			image.Save(Path::Combine(Path::GetDirectoryName(args.SceneFileName), "render_result.bmp"));

			return 0;
		}
		catch (Exception e)
		{
			wprintf_s(L"%s\n", e.Message.Buffer());
			return 1;
		}
		_CrtDumpMemoryLeaks();
	}
}

int wmain(int argc, wchar_t *argv[])
{
	return RenderGen::Main(argc, argv);
}