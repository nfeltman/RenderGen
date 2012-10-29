#include "ObjModel.h"
#include "CoreLib/LibIO.h"

#include <ppl.h>
using namespace Concurrency;
using namespace CoreLib::IO;

namespace RenderGen
{
	struct FaceVertex
	{
		int vid, nid, tid;
	};

	struct Vec3_Less
	{
		bool operator()(const Vec3 & v1, const Vec3 & v2) const
		{
			if (v1.x < v2.x)
				return true;
			else if (v1.x > v2.x)
				return false;
			else
			{
				if (v1.y < v2.y)
					return true;
				else if (v1.y > v2.y)
					return false;
				else
					return v1.z < v2.z;
			}
		}
	};

	void LoadObjMaterialLib(ObjModel & mdl, String & filename, Dictionary<String, int> & matLookup);
	void RemoveLineBreakAndQuote(String & name)
	{
		StringBuilder sb(name.Length());
		for (int i = 0; i<name.Length(); i++)
		{
			if (name[i] != L'\t' && name[i] != L' ' && name[i] != L'\n' && name[i] != L'\r' && name[i] != L'\''
				&& name[i] != L'\"')
				sb.Append(name[i]);
		}
		name = sb.ProduceString();
	}
	bool LoadObj(ObjModel & mdl, const char * fileName)
	{
		FILE * f = 0;
		fopen_s(&f, fileName, "rt");
		if (f == 0)
			return false;
		String matFileName;
		char buf[200];
		int smoothGroup = 0;
		int matId = -1;
		List<FaceVertex> vertices;
		Dictionary<String, int> matLookup;
		while (!feof(f))
		{
			fscanf_s(f, "%s", buf, 200);
			if (_stricmp(buf, "v") == 0)
			{
				Vec3 v;
				fscanf_s(f, "%f %f %f", &v.x, &v.y, &v.z);
				mdl.Vertices.Add(v);
			}
			else if (_stricmp(buf, "vn") == 0)
			{
				Vec3 v;
				fscanf_s(f, "%f %f %f", &v.x, &v.y, &v.z);
				mdl.Normals.Add(v);
			}
			else if (_stricmp(buf, "vt") == 0)
			{
				Vec2 v;
				fscanf_s(f, "%f %f", &v.x, &v.y);
				v.y = -v.y;
				mdl.TexCoords.Add(v);
			}
			else if (_stricmp(buf, "f") == 0)
			{
				fgets(buf, 200, f);
				int len = strlen(buf);
				int startPos = 0;
				vertices.Clear();
				for (int i = 0; i<len; i++)
				{
					if (buf[i] != ' ' && buf[i] != '\t' && buf[i] != '\n')
					{
						continue;
					}
					else if (i == startPos)
					{
						startPos++;
						continue;
					}
					char str[50];
					memset(str, 0, 50);
					memcpy(str, buf+startPos, i-startPos);
					if (strstr(str, "//"))
					{
						int vid, nid;
						sscanf_s(str, "%d//%d", &vid, &nid);
						FaceVertex vtx;
						vtx.vid = vid - 1;
						vtx.nid = nid - 1;
						vtx.tid = -1;
						vertices.Add(vtx);
					}
					else
					{
						int slashCount = 0;
						for (int j = 0; j<i-startPos; j++)
						{
							if (str[j] == '/') slashCount ++;
						}
						if (slashCount == 0)
						{
							FaceVertex vtx;
							vtx.nid = vtx.tid = -1;
							sscanf_s(str, "%d", &vtx.vid);
							vtx.vid --;
							vertices.Add(vtx);
						}
						else if (slashCount == 3)
						{
							FaceVertex vtx;
							vtx.nid = -1;
							sscanf_s(str, "%d/%d", &vtx.vid, &vtx.tid);
							vtx.vid --;
							vtx.tid --;
							vertices.Add(vtx);
						}
						else
						{
							FaceVertex vtx;
							sscanf_s(str, "%d/%d/%d", &vtx.vid, &vtx.tid, &vtx.nid);
							vtx.vid --;
							vtx.tid --;
							vtx.nid --;
							vertices.Add(vtx);
						}
					}
					startPos = i+1;
				}
				// simple triangulation
				for (int i = 2; i<vertices.Count(); i++)
				{
					ObjFace face;
					face.VertexIds[0] = vertices[0].vid;
					face.VertexIds[1] = vertices[i-1].vid;
					face.VertexIds[2] = vertices[i].vid;
					face.NormalIds[0] = vertices[0].nid;
					face.NormalIds[1] = vertices[i-1].nid;
					face.NormalIds[2] = vertices[i].nid;
					face.TexCoordIds[0] = vertices[0].tid;
					face.TexCoordIds[1] = vertices[i-1].tid;
					face.TexCoordIds[2] = vertices[i].tid;
					face.SmoothGroup = smoothGroup;
					face.MaterialId = matId;
					mdl.Faces.Add(face);
				}
			}
			else if (_stricmp(buf, "usemtl") == 0)
			{
				String mtlName;
				fgets(buf, 199, f);
				mtlName = buf;
				RemoveLineBreakAndQuote(mtlName);
				matLookup.TryGetValue(mtlName, matId);
			}
			else if (_stricmp(buf, "s") == 0)
			{
				fscanf_s(f, "%s", buf, 200);
				if (buf[0] >= '0' && buf[0] <= '9')
				{
					sscanf_s(buf, "%d", &smoothGroup);
				}
				else
					smoothGroup = 0;
			}
			else if (_stricmp(buf, "mtllib") == 0)
			{
				fgets(buf, 199, f);
				matFileName = buf;
				RemoveLineBreakAndQuote(matFileName);
				String path = Path::GetDirectoryName(fileName);
				LoadObjMaterialLib(mdl, Path::Combine(path, matFileName), matLookup);
			}
			else
			{
				while (!feof(f) && fgetc(f) != '\n');
			}
		}
		fclose(f);
		return true;
	}

	void LoadObjMaterialLib(ObjModel & mdl, String & filename, Dictionary<String, int> & matLookup)
	{
		FILE * f = 0;
		fopen_s(&f, filename.ToMultiByteString(), "rt");
		if (!f)
		{
			printf("Error loading obj material library \'%s\'", filename.ToMultiByteString());
			return;
		}
		char buf[200];
		ObjMaterial* curMat = 0;
		while (!feof(f))
		{
			fscanf_s(f, "%s", buf, 200);
			if (_stricmp(buf, "newmtl") == 0)
			{
				curMat = new ObjMaterial();
				mdl.Materials.Add(curMat);
				fgets(buf, 199, f);
				String matName = buf;
				RemoveLineBreakAndQuote(matName);
				matLookup[matName] = mdl.Materials.Count()-1;
			}
			else if (_stricmp(buf, "kd") == 0)
			{
				Vec3 v;
				fscanf_s(f, "%f %f %f", &v.x, &v.y, &v.z);
				if (curMat)
					curMat->Diffuse = v;
			}
			else if (_stricmp(buf, "ks") == 0)
			{
				Vec3 v;
				fscanf_s(f, "%f %f %f", &v.x, &v.y, &v.z);
				if (curMat)
					curMat->Specular = v;
			}
			else if (_stricmp(buf, "ns") == 0)
			{
				float s;
				fscanf_s(f, "%f", &s);
				if (curMat)
					curMat->SpecularRate = s;
			}
			else if (_stricmp(buf, "map_kd") == 0)
			{
				fgets(buf, 199, f);
				String name = buf;
				RemoveLineBreakAndQuote(name);
				if (curMat)
					curMat->DiffuseMap = name;
			}
			else if (_stricmp(buf, "map_bump") == 0)
			{
				fgets(buf, 199, f);
				String name = buf;
				RemoveLineBreakAndQuote(name);
				if (curMat)
					curMat->BumpMap = name;
			}
			else
			{
				while (!feof(f) && fgetc(f) != '\n');
			}
		}
		fclose(f);
	}

	void RecomputeNormals(ObjModel & mdl)
	{
		mdl.Normals.Clear();
		Dictionary<Vec3, int> normalMap;
		List<Vec3> faceNormals;
		faceNormals.SetSize(mdl.Faces.Count());
		parallel_for (0, mdl.Faces.Count(), [&](int i)
		{
			Vec3 v1 = mdl.Vertices[mdl.Faces[i].VertexIds[0]];
			Vec3 v2 = mdl.Vertices[mdl.Faces[i].VertexIds[1]];
			Vec3 v3 = mdl.Vertices[mdl.Faces[i].VertexIds[2]];
			Vec3 ab, ac;
			Vec3::Subtract(ab, v2, v1);
			Vec3::Subtract(ac, v3, v1);
			Vec3 n;
			Vec3::Cross(n, ab, ac);
			Vec3::Normalize(n, n);
			faceNormals[i] = n;
		});
		List<int> vertShare;
		List<int> vertFaces;
		vertShare.SetSize(mdl.Vertices.Count());
		memset(vertShare.Buffer(), 0, vertShare.Count()*sizeof(int));
		for (int i = 0; i<mdl.Faces.Count(); i++)
		{
			vertShare[mdl.Faces[i].VertexIds[0]]++;
			vertShare[mdl.Faces[i].VertexIds[1]]++;
			vertShare[mdl.Faces[i].VertexIds[2]]++;
		}
		int scan = 0;
		for (int i = 0; i<vertShare.Count(); i++)
		{
			int s = vertShare[i];
			vertShare[i] = scan;
			scan += s;
		}
		vertFaces.SetSize(scan);
		for (int i = 0; i<mdl.Faces.Count(); i++)
		{
			vertFaces[vertShare[mdl.Faces[i].VertexIds[0]]++] = i;
			vertFaces[vertShare[mdl.Faces[i].VertexIds[1]]++] = i;
			vertFaces[vertShare[mdl.Faces[i].VertexIds[2]]++] = i;
		}
		int start = 0;
		for (int i = 0; i<mdl.Faces.Count(); i++)
		{
			ObjFace & face = mdl.Faces[i];
			for (int j = 0; j < 3; j++)
			{
				int vid = face.VertexIds[j];
				if (vid == 0)
					start = 0;
				else
					start = vertShare[vid-1];
				int count = 0;
				Vec3 n;
				n.SetZero();
				for (int k = start; k < vertShare[vid]; k++)
				{
					int fid = vertFaces[k];
					if (mdl.Faces[fid].SmoothGroup & face.SmoothGroup)
					{
						Vec3::Add(n, faceNormals[fid], n);
						count ++;
					}
				}
				if (count == 0)
					n = faceNormals[i];
				else
					Vec3::Scale(n, n, 1.0f/count);
				if (!normalMap.TryGetValue(n, face.NormalIds[j]))
				{
					mdl.Normals.Add(n);
					face.NormalIds[j] = mdl.Normals.Count()-1;
					normalMap[n] = mdl.Normals.Count()-1;
				}
			}
		}
	}
}