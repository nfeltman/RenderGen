#include "CoreLib/Basic.h"
#include "CoreLib/LibIO.h"
#include "Runtime/VectorMath.h"
#include "Runtime/ObjModel.h"
#include "Runtime/Common.h"

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
			StreamReader reader(fileName);
			rs.FileName = reader.ReadLine();
			rs.Width = StringToInt(reader.ReadLine());
			rs.Height = StringToInt(reader.ReadLine());
			rs.FOV = (float)StringToDouble(reader.ReadLine());
			rs.ZMin = (float)StringToDouble(reader.ReadLine());
			rs.ZMax = (float)StringToDouble(reader.ReadLine());
			for (int i = 0; i<4; i++)
			{
				auto line = reader.ReadLine();
				sscanf_s(line.ToMultiByteString(), "%f %f %f %f", rs.Transform.m[0][i], rs.Transform.m[1][i], rs.Transform.m[2][i], rs.Transform.m[3][i]);
			}
			return rs;
		}
	};

	int Main(int argc, wchar_t *argv[])
	{
		try
		{
			auto args = StartupArguments::Parse(argc, argv);
			// Load renderer library
			auto lib = LoadLibraryW(args.FileName.Buffer());
			RenderFunction render = (RenderFunction)GetProcAddress(lib, "RenderMain");
			if (render == 0)
				throw Exception(L"Failed to load renderer library.");
			
			auto sceneProfile = SceneProfile::FromFile(args.SceneFileName);
			ObjModel obj;
			if (!LoadObj(obj, sceneProfile.FileName.ToMultiByteString()))
				throw Exception(L"Failed to load obj model.");;
			
			// Create triangles buffer
			Scene scene;

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
	}
}

int wmain(int argc, wchar_t *argv[])
{
	return RenderGen::Main(argc, argv);
}