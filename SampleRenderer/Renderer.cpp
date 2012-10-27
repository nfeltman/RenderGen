#include "Runtime/Common.h"
#include <ppl.h>

using namespace RenderGen;
using namespace CoreLib::Basic;

Scene * scene;

class DifferentialGeometry
{
public:
	Vec3 Position;
	Vec3 Normal;
	Vec2 UV;
	int MaterialId;
	float dUdx, dUdy, dVdx, dVdy;
	Vec3 dNdx, dNdy; 
};

struct Fragment
{
	Ray * Ray;
	DifferentialGeometry dGeom;
};

template<bool pred>
inline bool Trace(Scene & scene, DifferentialGeometry & inter, const MeshFace & face, const Ray & ray, float & t)
{
	const int mod3[] = {0,1,2,0,1};
	int u = mod3[face.ProjectionAxis + 1];
	int v = mod3[face.ProjectionAxis + 2];
	float invNdotD = 1.0f / (face.PlaneU * ray.Direction[u] + face.PlaneV * ray.Direction[v] + ray.Direction[face.ProjectionAxis]);
	float tplane = -(face.PlaneU * ray.Origin[u] + face.PlaneV * ray.Origin[v] + ray.Origin[face.ProjectionAxis] + face.PlaneD) * invNdotD;
	if (tplane < ray.tMin/*-Epsilon*/ || tplane > ray.tMax/*+Epsilon*/)
		return false;
	float hitU = ray.Origin[u] + ray.Direction[u] * tplane;
	float hitV = ray.Origin[v] + ray.Direction[v] * tplane;
			
	float beta = face.K_beta_u * hitU + face.K_beta_v * hitV + face.K_beta_d;
	if (beta < -Epsilon)
		return false;
	float gamma = face.K_gamma_u * hitU + face.K_gamma_v * hitV + face.K_gamma_d;
	if (gamma < -Epsilon)
		return false;
	if (beta + gamma > 1.0f + Epsilon)
		return false;
	if (pred)
		return true;
//	inter.Init();
	inter.Position[u] = hitU;
	inter.Position[v] = hitV;
	inter.Position[face.ProjectionAxis] = ray.Origin[face.ProjectionAxis] + ray.Direction[face.ProjectionAxis] * tplane;
	float alpha = 1.0f - beta - gamma;
	Vec3 tmp;
	Vec3::Scale(inter.Normal, scene.Normals[face.Normal1], alpha);
	Vec3::Scale(tmp, scene.Normals[face.Normal2], beta);
	inter.Normal += tmp;
	Vec3::Scale(tmp, scene.Normals[face.Normal3], gamma);
	inter.Normal += tmp;

	Vec2 tmpVec2;
	if (face.TexCoord1 != -1)
	{
		inter.UV.x = scene.TexCoords[face.TexCoord1].x * alpha;
		inter.UV.y = scene.TexCoords[face.TexCoord1].y * alpha;
		inter.UV.x += scene.TexCoords[face.TexCoord2].x * beta;
		inter.UV.y += scene.TexCoords[face.TexCoord2].y * beta;
		inter.UV.x += scene.TexCoords[face.TexCoord3].x * gamma;
		inter.UV.y += scene.TexCoords[face.TexCoord3].y * gamma;
	}
	t = tplane;
	inter.MaterialId = face.MaterialId;
	return true;
}

inline Vec4 Shade(DifferentialGeometry & inter)
{
	Vec4 rs;
	Vec3 L;
	Vec3::Subtract(L, Vec3(0.0f, 0.0f, 1.0f), inter.Position);
	Vec3::Normalize(L, L);
	rs = Math::Min(0.999f, Math::Max(0.0001f, Vec3::Dot(inter.Normal, L)));
	rs.w = 1.0f;
	return rs;
}

class CameraProjection
{
private:
	Vec3 right, up;
	int centerX, centerY;
	int screenWidth, screenHeight;
		
	float screenZ;
public:
	//Matrix4 CameraTransform;
	CameraProjection()
	{
		SetPerspective(640, 480, 45.0f);
	}
	inline int GetScreenWidth()
	{
		return screenWidth;
	}
	inline int GetScreenHeight()
	{
		return screenHeight;
	}
		
	inline void SetPerspective(int width, int height, float fovy)
	{
		screenWidth = width;
		screenHeight = height;
		screenZ = -(height>>1)/tanf(fovy*(0.5f*PI/180.0f));
		right.SetZero();
		up.SetZero();
		right.x = 1.0f;
		up.y = right.x;
		centerX = (width >> 1);
		centerY = (height >> 1);
	}
	__forceinline void GenerateRay(Ray & r, float x, float y)
	{
		Vec3 tmpVec;
		// Compute camera ray
		// dir = || right * px + up * py + screenZ ||
		r.tMin = Epsilon;
		r.tMax = FLT_MAX;
		Vec3::Scale(r.Direction, right, (float)(x - centerX));
		Vec3::Scale(tmpVec, up, (float)(centerY - y));
		r.Direction += tmpVec;
		r.Direction.z = screenZ;
		Vec3 d = r.Direction;
		float dirDOTdir = Vec3::Dot(r.Direction, r.Direction);
		float dirLength = sqrt(dirDOTdir);
		float invDirLength = 1.0f/dirLength;
		r.Direction *= invDirLength;
		r.Origin.SetZero();
		/*Vec3 tmp;
		CameraTransform.Transform(tmp, r.Origin); r.Origin = tmp;
		CameraTransform.TransformNormal(tmp, r.Direction); r.Direction = tmp;*/
		r.ReciprocalDirection.x = 1.0f / r.Direction.x;
		r.ReciprocalDirection.y = 1.0f / r.Direction.y;
		r.ReciprocalDirection.z = 1.0f / r.Direction.z;
		// Compute initial ray differential
		/*rayDiff.dPdx.SetZero();
		rayDiff.dPdy.SetZero();
		
		float dirDOTdir3o2 = 1.0f / sqrt(dirDOTdir * dirDOTdir * dirDOTdir);

		Vec3::Scale(rayDiff.dDdx, right, dirDOTdir);
		Vec3::Scale(tmpVec, right, Vec3::Dot(d, right));
		rayDiff.dDdx -= tmpVec;
		rayDiff.dDdx *= dirDOTdir3o2;

		Vec3::Scale(rayDiff.dDdy, up, dirDOTdir);
		Vec3::Scale(tmpVec, up, Vec3::Dot(d, up));
		rayDiff.dDdy -= tmpVec;
		rayDiff.dDdy *= dirDOTdir3o2;*/
	}
};

List<Ray> GenerateSamples(ImageRef image, Scene * scene)
{
	List<Ray> rs;
	CameraProjection proj;
	rs.SetSize(image.Width*image.Height);
	proj.SetPerspective(image.Width, image.Height, scene->FOV);
	for (int i = 0; i<image.Height; i++)
		for (int j = 0; j<image.Width; j++)
			proj.GenerateRay(rs[i*image.Width+j], (float)j, (float)i);
	return rs;
}

List<Vec4> Render(List<Ray> & samples, List<MeshFace> & geoms)
{
	List<Vec4> rs;
	rs.SetSize(samples.Count());
	concurrency::parallel_for(0, samples.Count(), [&](int i)
	{
		DifferentialGeometry inter;
		memset(&inter, 0, sizeof(DifferentialGeometry));
		inter.MaterialId = -1;
		float t = FLT_MAX;
		for (int j = 0; j<geoms.Count(); j++)
		{
			float tt = FLT_MAX;
			DifferentialGeometry tinter;
			if (Trace<false>(*scene, tinter, geoms[j], samples[i], tt) && tt < t)
			{
				inter = tinter;
				t = tt;
			}
		}
		rs[i] = Shade(inter);
	});
	return rs;
}

void RenderMain(ImageRef image, Scene * scene)
{
	::scene = scene;
	List<Ray> samples = GenerateSamples(image, scene);
	auto rs = Render(samples, scene->Triangles);
	for (int i = 0; i<image.Width*image.Height; i++)
		image.Pixels[i] = rs[i];
}