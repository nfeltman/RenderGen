#ifndef CORE_LIB_COMMON_H
#define CORE_LIB_COMMON_H

namespace CoreLib
{
	namespace Basic
	{
		class Object
		{
		public:
			virtual ~Object()
			{}
		};

		template <typename T>
		inline T&& _Move(T & obj)
		{
			return static_cast<T&&>(obj);
		}

		inline int FloatAsInt(float val)
		{
			return *(int*)&val;
		}
	}
}

#endif