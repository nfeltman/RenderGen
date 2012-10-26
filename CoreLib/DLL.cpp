#include "DLL.h"

#ifdef WIN32
#include <Windows.h>
#else
#include <dlfcn.h>
#endif

using namespace CoreLib::Basic;

namespace CoreLib
{
	namespace System
	{
		DynamicLibrary::DynamicLibrary(const String & fileName)
		{
#ifdef WIN32
			handle = LoadLibraryW(fileName.Buffer());

#else
			handle = dlopen(fileName.ToMultiByteString(), 2);
#endif
			if (handle == 0)
				throw InvalidLibraryException(L"Cannot load library \'" + fileName + L"\'");
		}

		void DynamicLibrary::Free()
		{
#ifdef WIN32
			FreeLibrary((HMODULE)handle);
#else
			dlclose(handle);
#endif
		}

		void * DynamicLibrary::GetProc(const String & procName)
		{
#ifdef WIN32
			return GetProcAddress((HMODULE)handle, procName.ToMultiByteString());
#else
			return dlsym(handle, procName.ToMultiByteString());
#endif
		}
	}
}