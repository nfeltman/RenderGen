#ifndef CORELIB_DLL_H
#define CORELIB_DLL_H

#include "LibString.h"
#include "Exception.h"

namespace CoreLib
{
	namespace System
	{
		class DynamicLibrary
		{
		private:
			void * handle;
		public:
			DynamicLibrary(const CoreLib::Basic::String & path);
			void Free();
			void * GetProc(const CoreLib::Basic::String & procName);
			template<typename T>
			T GetProc(const CoreLib::Basic::String & procName)
			{
				return (T)GetProc(procName);
			}
		};

		class InvalidLibraryException : public CoreLib::Basic::Exception
		{
		public:
			InvalidLibraryException(CoreLib::Basic::String message)
				: CoreLib::Basic::Exception(message)
			{}
		};
	}
}

#endif