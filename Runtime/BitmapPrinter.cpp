#include "BitmapPrinter.h"


namespace RenderGen
{
	Color::Color(int r, int g, int b)
	{
		R = (unsigned char)r;
		G = (unsigned char)g;
		B = (unsigned char)b;
		A = 255;
	}
	Color::Color()
	{
		R=G=B=A=255;
	}
	BitmapPrinter::BitmapPrinter()
	{
		width = height = 0;
		bitmap = 0;
	}
	BitmapPrinter::~BitmapPrinter()
	{
		delete bitmap;
	}
	void BitmapPrinter::SetResolution(int x, int y)
	{
		if (bitmap)
			delete bitmap;
		bitmap = new Bitmap(x,y);
		width = x;
		height = y;
		pixels.SetSize(y);
		for (int i=0; i<y; i++)
			pixels[i].SetSize(x);
	}
	int BitmapPrinter::GetWidth()
	{
		return width;
	}
	int BitmapPrinter::GetHeight()
	{
		return height;
	}
	void BitmapPrinter::SetPixel(int i, int j, Color c)
	{
		pixels[j][i] = c;
	}

	void BitmapPrinter::DrawFocusRect(int x1, int x2, int y1, int y2)
	{
		int lineLen = 5;
		for (int i=x1; i<x2; i++)
		{
			if (i-x1 < lineLen || x2-i < lineLen)
			{
				pixels[y1][i] = Color(255,255,255);
				pixels[y2][i] = Color(255,255,255);
			}
		}
		for (int j=y1; j<y2; j++)
		{
			if (j-y1 < lineLen || y2-j < lineLen)
			{
				pixels[j][x1] = Color(255,255,255);
				pixels[j][x2] = Color(255,255,255);
			}
		}
	}

	Color BitmapPrinter::GetPixel(int i, int j)
	{
		return pixels[j][i];
	}

	Bitmap * BitmapPrinter::GetBitmap()
	{
		return bitmap;
	}

	Bitmap * BitmapPrinter::Flush()
	{
		Rect lr;
		lr.X = lr.Y = 0;
		lr.Height = height;
		lr.Width = width;
		BitmapData data;
		bitmap->LockBits(&lr, ImageLockModeWrite, PixelFormat32bppARGB, &data);
		for (int i=0; i<height; i++)
		{
			memcpy((char *)data.Scan0+data.Stride*i, pixels[i].Buffer(), sizeof(Color)*width);
		}
		bitmap->UnlockBits(&data);
		return bitmap;
	}

	Bitmap* BitmapPrinter::FlushLine(int line)
	{
		Rect lr;
		lr.X = 0;
		lr.Y = line;
		lr.Height = 1;
		lr.Width = width;
		BitmapData data;
		bitmap->LockBits(&lr, ImageLockModeWrite, PixelFormat32bppARGB, &data);
		memcpy((char *)data.Scan0, pixels[line].Buffer(), sizeof(Color)*width);
		bitmap->UnlockBits(&data);
		return bitmap;
	}

}