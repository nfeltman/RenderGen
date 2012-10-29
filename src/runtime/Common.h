#ifndef RENDER_GEN_RUNTIME_COMMON_H
#define RENDER_GEN_RUNTIME_COMMON_H

#include "CoreLib/Basic.h"
#include "VectorMath.h"

namespace RenderGen
{
	using namespace CoreLib::Basic;
	using namespace VectorMath;

	class ImageRef
	{
	public:
		Vec4 * Pixels;
		int Width, Height;
	};

	class Image
	{
	public:
		List<Vec4> Pixels;
		int Width, Height;

		Image()
		{
			Width = Height = 0;
		}

		Image(int w, int h)
		{
			Pixels.SetSize(w * h);
			Width = w;
			Height = h;
		}

		ImageRef CreateRef()
		{
			ImageRef rs;
			rs.Pixels = Pixels.Buffer();
			rs.Width = Width;
			rs.Height = Height;
			return rs;
		}
		void Save(String fileName); // Save the image in pfm or bmp format
	};

	struct MeshFace
	{
		float PlaneU, PlaneV, PlaneD;
		short MaterialId;
		short ProjectionAxis;
		float K_beta_u, K_beta_v, K_beta_d, K_alpha_u;
		float K_gamma_u, K_gamma_v, K_gamma_d, K_alpha_v;
		union
		{
			struct
			{
				int Normal1, Normal2, Normal3;
			};
			int Normals[3];
		};
		union
		{
			struct
			{
				int TexCoord1, TexCoord2, TexCoord3;
			};
			int TexCoords[3];
		};
	};

	class Scene
	{
	public:
		float ZMin, ZMax;
		float FOV;
		List<MeshFace> Triangles;
		List<Vec2> TexCoords;
		List<Vec3> Normals;
	};

	class Ray
	{
	public:
		Vec3 Origin;
		Vec3 Direction;
		Vec3 ReciprocalDirection;
		float tMin, tMax;
	};

	typedef void (*RenderFunction)(ImageRef image, Scene * scene);

}

#endif