#include "Common.h"
#include "CoreLib/LibIO.h"

using namespace CoreLib::IO;

namespace RenderGen
{
	// save a pfm image.
	void Image::Save(String fileName)
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