#include "Common.h"
#include "CoreLib/LibIO.h"

using namespace CoreLib::IO;

namespace RenderGen
{
	void SaveBmpImage(String fileName, ImageRef image)
	{
		FILE *f = 0;
		int filesize = 54 + 3*image.Width*image.Height;  //w is your image width, h is image height, both int
		List<unsigned char> img;
		img.SetSize(3*image.Width*image.Height);
		memset(img.Buffer(),0,3*image.Width*image.Height);

		for(int i=0; i<image.Width; i++)
		{
			for(int j=0; j<image.Height; j++)
			{
				int x=i; 
				int y=(image.Height-1)-j;
				int r = (int)(image.Pixels[j*image.Width+i].x*255);
				int g = (int)(image.Pixels[j*image.Width+i].y*255);
				int b = (int)(image.Pixels[j*image.Width+i].z*255);
				if (r > 255) r=255;
				if (g > 255) g=255;
				if (b > 255) b=255;
				img[(x+y*image.Width)*3+2] = (unsigned char)(r);
				img[(x+y*image.Width)*3+1] = (unsigned char)(g);
				img[(x+y*image.Width)*3+0] = (unsigned char)(b);
			}
		}

		unsigned char bmpfileheader[14] = {'B','M', 0,0,0,0, 0,0, 0,0, 54,0,0,0};
		unsigned char bmpinfoheader[40] = {40,0,0,0, 0,0,0,0, 0,0,0,0, 1,0, 24,0};
		unsigned char bmppad[3] = {0,0,0};

		bmpfileheader[ 2] = (unsigned char)(filesize    );
		bmpfileheader[ 3] = (unsigned char)(filesize>> 8);
		bmpfileheader[ 4] = (unsigned char)(filesize>>16);
		bmpfileheader[ 5] = (unsigned char)(filesize>>24);

		bmpinfoheader[ 4] = (unsigned char)(       image.Width    );
		bmpinfoheader[ 5] = (unsigned char)(       image.Width>> 8);
		bmpinfoheader[ 6] = (unsigned char)(       image.Width>>16);
		bmpinfoheader[ 7] = (unsigned char)(       image.Width>>24);
		bmpinfoheader[ 8] = (unsigned char)(       image.Height    );
		bmpinfoheader[ 9] = (unsigned char)(       image.Height>> 8);
		bmpinfoheader[10] = (unsigned char)(       image.Height>>16);
		bmpinfoheader[11] = (unsigned char)(       image.Height>>24);

		fopen_s(&f, fileName.ToMultiByteString(), "wb");
		if (f)
		{
			fwrite(bmpfileheader,1,14,f);
			fwrite(bmpinfoheader,1,40,f);
			for(int i=0; i<image.Height; i++)
			{
				fwrite(img.Buffer()+(image.Width*i*3),3,image.Width,f);
				fwrite(bmppad,1,(4-(image.Width*3)%4)%4,f);
			}
			fclose(f);
		}
	}

	void Image::Save(String fileName)
	{
		if (fileName.ToLower().EndsWith(L"bmp"))
		{
			SaveBmpImage(fileName, CreateRef());
			return;
		}
		else // save pfm
		{
			float * pixels = new float[Width*Height*3];
			for (int i=0; i<Pixels.Count(); i++)
			{
				pixels[i*3] = Pixels[i].x;
				pixels[i*3+1] = Pixels[i].y;
				pixels[i*3+2] = Pixels[i].z;
			}
			try
			{
				FileStream stream(fileName, FileMode::Create);
				stream.Write("PF\n", 3);
				String s(Width);
				stream.Write(s.ToMultiByteString(), s.Length());
				stream.Write(" ", 1);
				s = String(Height);
				stream.Write(s.ToMultiByteString(), s.Length());
				stream.Write("\n-1.000000\n", 11);
				for (int h = Height-1; h>=0; h--)
				{
					stream.Write(&pixels[h*Width*3], Width*3);
				}
				delete [] pixels;
			}
			catch(Exception e)
			{
				delete [] pixels;
				throw e;
			}
		}
	}
}