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

	void AddFace(Scene & scene, const Vec3 verts[3], int norm[3], int tex[3], short materialId)
	{
		/*BBox box;
		box.Max = box.Min = verts[0];
		box.Union(verts[1]);
		box.Union(verts[2]);
		faceBBoxes.Add(box);
		Bounds.Union(box);*/
		MeshFace f;
		f.Normal1 = norm[0]; f.Normal2 = norm[1]; f.Normal3 = norm[2];
		f.TexCoord1 = tex[0]; f.TexCoord2 = tex[1]; f.TexCoord3 = tex[2];
		Vec3 n, c, b, a;
		Vec3::Subtract(c, verts[1], verts[0]);
		Vec3::Subtract(b, verts[2], verts[0]);
		Vec3::Subtract(a, verts[2], verts[1]);
		Vec3::Cross(n, c, b);
		Vec3::Normalize(n, n);
		int k;
		Vec3 absN = Vec3(abs(n.x), abs(n.y), abs(n.z));
		if (absN.x > absN.y && absN.x > absN.z)
			k = 0;
		else if (absN.y > absN.x && absN.y > absN.z)
			k = 1;
		else
			k = 2;
		int mod3[5] = {0, 1, 2, 0, 1};
		int u = mod3[k + 1];
		int v = mod3[k + 2];
		f.ProjectionAxis = k;
		float invNk = 1.0f / n[k];
		Vec3::Scale(n, n, invNk);
		f.PlaneU = n[u];
		f.PlaneV = n[v];
		f.PlaneD = -Vec3::Dot(n, verts[0]); 
		float divisor = 1.0f / (b[u]*c[v] - b[v]*c[u]);
		f.K_beta_u = -b[v] * divisor;
		f.K_beta_v = b[u] * divisor;
		const Vec3 & A = verts[0];
		f.K_beta_d = (b[v] * A[u] - b[u] * A[v]) * divisor;
		f.K_gamma_u = c[v] * divisor;
		f.K_gamma_v = -c[u] * divisor;
		f.K_gamma_d = (c[u] * A[v] - c[v] * A[u]) * divisor;
		const Vec3 & B = verts[1];
		f.K_alpha_u = a[v] * divisor;
		f.K_alpha_v = -a[u] * divisor;
		//f.K_alpha_d = (a[u] * B[v] - a[v]*B[u]) * divisor;
		f.MaterialId = materialId;
		scene.Triangles.Add(f);
	}

	void CreateSceneGeometry(Scene & scene, ObjModel & obj, Matrix4 & transform)
	{
		RecomputeNormals(obj);
		scene.Normals.AddRange(obj.Normals);
		scene.TexCoords.AddRange(obj.TexCoords);
		Matrix4 normalTransform;
		transform.Inverse(normalTransform);
		for (auto & v : scene.Normals)
		{
			auto vin = v;
			normalTransform.TransposeTransform(v, vin);
		}
		for (auto & face : obj.Faces)
		{
			Vec3 verts[3];
			transform.Transform(verts[0], obj.Vertices[face.VertexIds[0]]);
			transform.Transform(verts[1], obj.Vertices[face.VertexIds[1]]);
			transform.Transform(verts[2], obj.Vertices[face.VertexIds[2]]);
			AddFace(scene, verts, face.NormalIds, face.TexCoordIds, face.MaterialId);
		}
	}

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
			CreateSceneGeometry(scene, obj, sceneProfile.Transform);

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