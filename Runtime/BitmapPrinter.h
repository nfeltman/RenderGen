#ifndef BITMAP_PRINTER_H
#define BITMAP_PRINTER_H

#include "CoreLib/Basic.h"
#include "Gdiplus.h"

namespace RenderGen
{
	using namespace Gdiplus;
	using namespace CoreLib::Basic;

	class Color
	{
	public:
		unsigned char B,G,R,A;
		Color(int R, int G, int B);
		Color();
	};
	class BitmapPrinter : public Object
	{
	private:
		int width, height;
		Bitmap * bitmap;
		List<List<Color>> pixels;
	public:
		void SetPixel(int i, int j, Color c);
		Color GetPixel(int i, int j);
		void SetResolution(int x, int y);
		int GetWidth();
		int GetHeight();
		Bitmap * Flush();
		Bitmap * FlushLine(int line);
		void DrawFocusRect(int x1, int x2, int y1, int y2);
		Bitmap * GetBitmap();
		BitmapPrinter();
		~BitmapPrinter();
	};
}

#endif