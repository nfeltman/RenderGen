#ifndef RENDER_GEN_OBJ_MODEL_H
#define RENDER_GEN_OBJ_MODEL_H

#include "CoreLib/Basic.h"
#include "VectorMath.h"

namespace RenderGen
{
	using namespace CoreLib::Basic;
	using namespace VectorMath;

	struct ObjFace
	{
		int VertexIds[3];
		int NormalIds[3];
		int TexCoordIds[3];
		unsigned int SmoothGroup;
		int MaterialId;
	};
	struct ObjMaterial
	{
		float SpecularRate;
		Vec3 Diffuse, Specular;
		String DiffuseMap, BumpMap;
	};
	struct ObjModel
	{
		List<RefPtr<ObjMaterial>> Materials;
		List<Vec3> Vertices, Normals;
		List<Vec2> TexCoords;
		List<ObjFace> Faces;
	};
	bool LoadObj(ObjModel & mdl, const char * fileName);
	void RecomputeNormals(ObjModel & mdl);
}

#endif