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

		void Save(String fileName);
	};

	class Triangle
	{};

	class Scene
	{
	public:

	};

	typedef void (*RenderFunction)(ImageRef image, Scene * scene);

}

#endif