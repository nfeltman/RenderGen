#include "Runtime/Common.h"

using namespace RenderGen;
using namespace CoreLib::Basic;

void RenderMain(ImageRef image, Scene * scene)
{
	for (int i = 0; i<image.Width*image.Height; i++)
		image.Pixels[i] = Vec4((i/image.Width)/(float)image.Height, 0.0f, 0.0f, 1.0f);
}