#include "Common.h"

namespace RenderGen
{
	//int GetEncoderClsid(const WCHAR* format, CLSID* pClsid)
	//{
	//	UINT  num = 0;          // number of image encoders
	//	UINT  size = 0;         // size of the image encoder array in bytes

	//	ImageCodecInfo* pImageCodecInfo = NULL;

	//	GetImageEncodersSize(&num, &size);
	//	if(size == 0)
	//		return -1;  // Failure

	//	pImageCodecInfo = (ImageCodecInfo*)(malloc(size));
	//	if(pImageCodecInfo == NULL)
	//		return -1;  // Failure

	//	GetImageEncoders(num, size, pImageCodecInfo);

	//	for(UINT j = 0; j < num; ++j)
	//	{
	//		if( wcscmp(pImageCodecInfo[j].MimeType, format) == 0 )
	//		{
	//			*pClsid = pImageCodecInfo[j].Clsid;
	//			free(pImageCodecInfo);
	//			return j;  // Success
	//		}    
	//	}

	//	free(pImageCodecInfo);
	//	return -1;  // Failure
	//}

	void Image::Save(String fileName)
	{
		/*BitmapPrinter printer;
		printer.SetResolution(Width, Height);
		for (int i = 0; i<Height; i++)
			for (int j = 0; j<Width; j++)
				printer.SetPixel(j, i, Color((int)(Pixels[i*Width+j].x*255),
				                             (int)(Pixels[i*Width+j].y*255),
											 (int)(Pixels[i*Width+j].z*255)));
		CLSID clsid;
		GetEncoderClsid(L"image/bmp", &clsid);
		printer.GetBitmap()->Save(fileName.Buffer(), &clsid); */
	}
}