#ifndef RENDER_GEN_RUNTIME_COMMON_H
#define RENDER_GEN_RUNTIME_COMMON_H

#ifndef CORE_LIB_BASIC_H
#define CORE_LIB_BASIC_H

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
#ifndef CORE_LIB_MATH_H
#define CORE_LIB_MATH_H

namespace CoreLib
{
	namespace Basic
	{
		class Math
		{
		public:
			template<typename T>
			static T Min(const T& v1, const T&v2)
			{
				return v1<v2?v1:v2;
			}
			template<typename T>
			static T Max(const T& v1, const T&v2)
			{
				return v1>v2?v1:v2;
			}
			template<typename T>
			static T Clamp(const T& val, const T& vmin, const T&vmax)
			{
				if (val < vmin) return vmin;
				else if (val > vmax) return vmax;
				else return val;
			}
		};
	}
}

#endif 
#ifndef FUNDAMENTAL_LIB_STRING_H
#define FUNDAMENTAL_LIB_STRING_H
#include <string.h>
#include <tchar.h>
#include <cstdlib>
#include <stdio.h>
#ifndef WIDE_CHAR_H
#define WIDE_CHAR_H

void MByteToWideChar(wchar_t * buffer, int bufferSize, const char * str, int length);
char * WideCharToMByte(const wchar_t * buffer, int length);
wchar_t * MByteToWideChar(const char * buffer, int length);

#endif
#ifndef FUNDAMENTAL_LIB_SMART_POINTER_H
#define FUNDAMENTAL_LIB_SMART_POINTER_H

namespace CoreLib
{
	namespace Basic
	{
		class RefPtrDefaultDestructor
		{
		public:
			template<typename T>
			void operator ()(T * ptr)
			{
				delete ptr;
			}
		};

		class RefPtrArrayDestructor
		{
		public:
			template<typename T>
			void operator() (T * ptr)
			{
				delete [] ptr;
			}
		};

		template<typename T, typename Destructor = RefPtrDefaultDestructor>
		class RefPtr
		{
			template<typename T1, typename Destructor1>
			friend class RefPtr;
		private:
			T * pointer;
			int * refCount;
			
		public:
			RefPtr()
			{
				pointer = 0;
				refCount = 0;
			}
			RefPtr(T * ptr)
				: pointer(0), refCount(0)
			{
				this->operator=(ptr);
			}
			template<typename T1>
			RefPtr(T1 * ptr)
				: pointer(0), refCount(0)
			{
				this->operator=(ptr);
			}
			RefPtr(const RefPtr<T, Destructor> & ptr)
				: pointer(0), refCount(0)
			{
				this->operator=(ptr);
			}
			RefPtr(RefPtr<T, Destructor> && str)
				: pointer(0), refCount(0)
			{
				this->operator=(static_cast<RefPtr<T, Destructor> &&>(str));
			}
			RefPtr<T,Destructor>& operator=(T * ptr)
			{
				Dereferance();

				pointer = ptr;
				if(ptr)
				{
					refCount = new int;
					(*refCount) = 1;
				}
				else
					refCount = 0;
				return *this;
			}
			template<typename T1>
			RefPtr<T,Destructor>& operator=(T1 * ptr)
			{
				Dereferance();

				pointer = dynamic_cast<T*>(ptr);
				if(ptr)
				{
					refCount = new int;
					(*refCount) = 1;
				}
				else
					refCount = 0;
				return *this;
			}
			RefPtr<T,Destructor>& operator=(const RefPtr<T, Destructor> & ptr)
			{
				if(ptr.pointer != pointer)
				{
					Dereferance();
					pointer = ptr.pointer;
					refCount = ptr.refCount;
					(*refCount)++;
				}
				return *this;
			}

			template<typename T1>
			RefPtr(const RefPtr<T1> & ptr)
				: pointer(0), refCount(0)
			{
				this->operator=(ptr);
			}
			template<typename T1>
			RefPtr<T,Destructor> & operator = (const RefPtr<T1, Destructor> & ptr)
			{
				if(ptr.pointer != pointer)
				{
					Dereferance();
					pointer = dynamic_cast<T*>(ptr.pointer);
					refCount = ptr.refCount;
					(*refCount)++;
				}
				return *this;
			}
			bool operator == (const T * ptr) const
			{
				return pointer == ptr;
			}
			bool operator != (const T * ptr) const
			{
				return pointer != ptr;
			}
			bool operator == (const RefPtr<T> & ptr) const
			{
				return pointer == ptr.pointer;
			}
			bool operator != (const RefPtr<T> & ptr) const
			{
				return pointer != ptr.pointer;
			}

			T* operator +(int offset) const
			{
				return pointer+offset;
			}
			T& operator [](int idx) const
			{
				return *(pointer + idx);
			}
			RefPtr<T,Destructor>& operator=(RefPtr<T, Destructor> && ptr)
			{
				if(ptr.pointer != pointer)
				{
					Dereferance();
					pointer = ptr.pointer;
					refCount = ptr.refCount;
					ptr.pointer = 0;
					ptr.refCount = 0;
				}
				return *this;
			}
			T* Release()
			{
				if(pointer)
				{
					if((*refCount) > 1)
					{
						(*refCount)--;
					}
					else
					{
						delete refCount;
					}
				}
				auto rs = pointer;
				refCount = 0;
				pointer = 0;
				return rs;
			}
			~RefPtr()
			{
				Dereferance();
			}

			void Dereferance()
			{
				if(pointer)
				{
					if((*refCount) > 1)
					{
						(*refCount)--;
					}
					else
					{
						Destructor destructor;
						destructor(pointer);
						delete refCount;
					}
				}
			}
			T & operator *() const
			{
				return *pointer;
			}
			T * operator->() const
			{
				return pointer;
			}
			T * Ptr() const
			{
				return pointer;
			}
		private:
			class _BoolConversionClass
			{
			public:
				int Dummy;
			};
		public:
			operator void*() const 
			{
				if (pointer)
					return (void*)(pointer);
				else
					return 0;
			}
		};
	}
}

#endif

namespace CoreLib
{
	namespace Basic
	{
		class _EndLine
		{};
		extern _EndLine EndLine;
		class String
		{
			friend class StringBuilder;
		private:
			RefPtr<wchar_t, RefPtrArrayDestructor> buffer;
			char * multiByteBuffer;
			int length;
			void Free()
			{
				if (buffer)
					buffer = 0;
				if (multiByteBuffer)
					delete [] multiByteBuffer;
				buffer = 0;
				multiByteBuffer = 0;
				length = 0;
			}
		public:
			static String FromBuffer(RefPtr<wchar_t, RefPtrArrayDestructor> buffer, int len)
			{
				String rs;
				rs.buffer = buffer;
				rs.length = len;
				return rs;
			}
			String()
				:buffer(0), multiByteBuffer(0), length(0)
			{
			}
			String(const wchar_t * str)
				:buffer(0), multiByteBuffer(0), length(0)
			{
				this->operator=(str);
			}
			String(const wchar_t ch)
				:buffer(0), multiByteBuffer(0), length(0)
			{
				wchar_t arr[] = {ch, 0};
				*this = String(arr);
			}
			String(int val, int radix = 10)
				:buffer(0), multiByteBuffer(0), length(0)
			{
				buffer = new wchar_t[33];
				_itow_s(val, buffer.Ptr(), 33, radix);
				length = wcsnlen_s(buffer.Ptr(), 33);
			}
			String(float val, wchar_t * format = L"%e")
				:buffer(0), multiByteBuffer(0), length(0)
			{
				buffer = new wchar_t[48];
				swprintf_s(buffer.Ptr(),48, format, val);
				length = wcsnlen_s(buffer.Ptr(), 48);
			}
			String(double val, wchar_t * format = L"%e")
				:buffer(0), multiByteBuffer(0), length(0)
			{
				buffer = new wchar_t[48];
				swprintf_s(buffer.Ptr(),48, format, val);
				length = wcsnlen_s(buffer.Ptr(), 48);
			}
			String(const char * str)
				:buffer(0), multiByteBuffer(0), length(0)
			{
				buffer = MByteToWideChar(str, strlen(str));
				length = wcslen(buffer.Ptr());
			}
			String(const String & str)
				:buffer(0), multiByteBuffer(0), length(0)
			{				
				this->operator=(str);
			}
			String(String&& other)
				:buffer(0), multiByteBuffer(0), length(0)
			{
				this->operator=(static_cast<String&&>(other));
			}
			~String()
			{
				Free();
			}
			String & operator=(const wchar_t * str)
			{
				Free();
				length = wcslen(str);
				buffer = new wchar_t[length + 1];
				wcscpy_s(buffer.Ptr(), length + 1, str);
				return *this;
			}
			String & operator=(const String & str)
			{
				if (str.buffer == buffer)
					return *this;
				Free();
				if (str.buffer)
				{
					length = str.length;
					buffer = str.buffer;
					multiByteBuffer = 0;
				}
				return *this;
			}
			String & operator=(String&& other)
			{
				if (this != &other)
				{
					Free();
					buffer = _Move(other.buffer);
					length = other.length;
					multiByteBuffer = other.multiByteBuffer;
					other.buffer = 0;
					other.length = 0;
					other.multiByteBuffer = 0;
				}
				return *this;
			}
			wchar_t operator[](int id) const
			{
#if _DEBUG
				if (id < 0 || id >= length)
					throw "Operator[]: index out of range.";
#endif
				return buffer.Ptr()[id];
			}

			friend String StringConcat(const wchar_t * lhs, int leftLen, const wchar_t * rhs, int rightLen);
			friend String operator+(const wchar_t*op1, const String & op2);
			friend String operator+(const String & op1, const wchar_t * op2);
			friend String operator+(const String & op1, const String & op2);

			String TrimStart() const
			{
				if(!buffer)
					return *this;
				int startIndex = 0;
				while (startIndex < length && 
					(buffer[startIndex] == L' ' || buffer[startIndex] == L'\t'))
						startIndex++;
				return String(buffer + startIndex);
			}

			String TrimEnd() const
			{
				if(!buffer)
					return *this;

				int endIndex = length - 1;
				while (endIndex >= 0 &&
					(buffer[endIndex] == L' ' || buffer[endIndex] == L'\t'))
					endIndex--;
				String res;
				res.length = endIndex + 1;
				res.buffer = new wchar_t[endIndex + 2];
				wcsncpy_s(res.buffer.Ptr(), endIndex + 2, buffer.Ptr(), endIndex + 1);
				return res;
			}

			String Trim() const
			{
				if(!buffer)
					return *this;

				int startIndex = 0;
				while (startIndex < length && 
					(buffer[startIndex] == L' ' || buffer[startIndex] == L'\t'))
						startIndex++;
				int endIndex = length - 1;
				while (endIndex >= startIndex &&
					(buffer[endIndex] == L' ' || buffer[endIndex] == L'\t'))
					endIndex--;

				String res;
				res.length = endIndex - startIndex + 1;
				res.buffer = new wchar_t[res.length + 1];
				memcpy(res.buffer.Ptr(), buffer + startIndex, sizeof(wchar_t) * res.length);
				res.buffer[res.length] = L'\0';
				return res;
			}

			String SubString(int id, int len) const
			{
#if _DEBUG
				if (id < 0 || id >= length || (id + len) >= length)
					throw "SubString: index out of range.";
				if (len < 0)
					throw "SubString: length less than zero.";
#endif
				String res;
				res.buffer = new wchar_t[len + 1];
				res.length = len;
				wcsncpy_s(res.buffer.Ptr(), len + 1, buffer + id, len);
				return res;
			}

			wchar_t * Buffer() const
			{
				if (buffer)
					return buffer.Ptr();
				else
					return L"";
			}

			char * ToMultiByteString(int * len = 0) const
			{
				if (!buffer)
					return "";
				else
				{
					if (multiByteBuffer)
						return multiByteBuffer;
					((String*)this)->multiByteBuffer = WideCharToMByte(buffer.Ptr(), length);
					if (len)
						*len = strnlen_s(multiByteBuffer, length*2);
					return multiByteBuffer;
					/*if (multiByteBuffer)
						return multiByteBuffer;
					size_t requiredBufferSize;
					requiredBufferSize = WideCharToMultiByte(CP_OEMCP, NULL, buffer.Ptr(), length, 0, 0, NULL, NULL)+1;
					if (len)
						*len = requiredBufferSize-1;
					if (requiredBufferSize)
					{
						multiByteBuffer = new char[requiredBufferSize];
						WideCharToMultiByte(CP_OEMCP, NULL, buffer.Ptr(), length, multiByteBuffer, requiredBufferSize, NULL, NULL);
						multiByteBuffer[requiredBufferSize-1] = 0;
						return multiByteBuffer;
					}
					else
						return "";*/
				}
			}

			bool operator==(const String & str) const
			{
				if (!buffer)
					return (str.buffer == 0);
				return (wcscmp(buffer.Ptr(), str.buffer.Ptr()) == 0);
			}
			bool operator!=(const String & str) const
			{
				if (!buffer)
					return (str.buffer != 0);
				return (wcscmp(buffer.Ptr(), str.buffer.Ptr()) != 0);
			}
			bool operator>(const String & str) const
			{
				if (!buffer)
					return false;
				return (wcscmp(buffer.Ptr(), str.buffer.Ptr()) > 0);
			}
			bool operator<(const String & str) const
			{
				if (!buffer)
					return (str.buffer != 0);
				return (wcscmp(buffer.Ptr(), str.buffer.Ptr()) < 0);
			}
			bool operator>=(const String & str) const
			{
				if (!buffer)
					return (str.buffer == 0);
				int res = wcscmp(buffer.Ptr(), str.buffer.Ptr());
				return (res > 0 || res == 0);
			}
			bool operator<=(const String & str) const
			{
				if (!buffer)
					return true;
				int res = wcscmp(buffer.Ptr(), str.buffer.Ptr());
				return (res < 0 || res == 0);
			}

			String ToUpper() const
			{
				if(!buffer)
					return *this;
				String res;
				res.length = length;
				res.buffer = new wchar_t[length + 1];
				for (int i = 0; i <= length; i++)
					res.buffer[i] = (buffer[i] >= L'a' && buffer[i] <= L'z')? 
									(buffer[i] - L'a' + L'A') : buffer[i];
				return res;
			}

			String ToLower() const
			{
				if(!buffer)
					return *this;
				String res;
				res.length = length;
				res.buffer = new wchar_t[length + 1];
				for (int i = 0; i <= length; i++)
					res.buffer[i] = (buffer[i] >= L'A' && buffer[i] <= L'Z')? 
									(buffer[i] - L'A' + L'a') : buffer[i];
				return res;
			}
			
			int Length() const
			{
				return length;
			}

			int IndexOf(wchar_t * str, int id) const // String str
			{
#if _DEBUG
				if (id < 0 || id >= length)
					throw "SubString: index out of range.";
#endif
				if(!buffer)
					return -1;
				int res = wcsstr(buffer + id, str) - buffer.Ptr();
				if(res >= 0)
					return res;
				else
					 return -1;
			}
			
			int IndexOf(const String & str, int id) const
			{
				return IndexOf(str.buffer.Ptr(), id);
			}

			int IndexOf(wchar_t * str) const
			{
				return IndexOf(str, 0);
			}

			int IndexOf(const String & str) const
			{
				return IndexOf(str.buffer.Ptr(), 0);
			}

			int IndexOf(wchar_t ch, int id) const
			{
#if _DEBUG
				if (id < 0 || id >= length)
					throw "SubString: index out of range.";
#endif
				if(!buffer)
					return -1;
				int res = wcschr(buffer + id, ch) - buffer.Ptr();
				if(res >= 0)
					return res;
				else
					return -1;
			}

			int IndexOf(wchar_t ch) const
			{
				return IndexOf(ch, 0);
			}

			int LastIndexOf(wchar_t ch) const
			{
				for (int i = length-1; i>=0; i--)
					if (buffer[i] == ch)
						return i;
				return -1;
			}

			bool StartsWith(wchar_t * str) const // String str
			{
				if(!buffer)
					return false;
				int strLen = wcslen(str);
				if (strLen > length)
					return false;
				for (int i = 0; i < strLen; i++)
					if (str[i] != buffer[i])
						return false;
				return true;
			}

			bool StartWith(const String & str) const
			{
				return StartWith(str.buffer.Ptr());
			}

			bool EndsWith(wchar_t * str)  const // String str
			{
				if(!buffer)
					return false;
				int strLen = wcslen(str);
				if (strLen > length)
					return false;
				for (int i = strLen - 1; i >= 0; i--)
					if (str[i] != buffer[length - strLen + i])
						return false;
				return true;
			}

			bool EndsWith(const String & str) const
			{
				return EndsWith(str.buffer.Ptr());
			}

			bool Contains(wchar_t * str) const // String str
			{
				if(!buffer)
					return false;
				return (IndexOf(str) >= 0)? true : false;
			}

			bool Contains(const String & str) const
			{
				return Contains(str.buffer.Ptr());
			}

			int GetHashCode() const
			{
				if (!buffer)
					return 0;
				int hash = 0;
				int c;
				wchar_t * str = buffer.Ptr();
				while (c = *str++)
					hash = c + (hash << 6) + (hash << 16) - hash;
				return hash;
			}
		};

		class StringBuilder
		{
		private:
			wchar_t * buffer;
			int length;
			int bufferSize;
			static const int InitialSize = 512;
		public:
			StringBuilder(int bufferSize = 1024)
				:buffer(0), length(0), bufferSize(0)
			{
				buffer = new wchar_t[InitialSize]; // new a larger buffer 
				buffer[0] = L'\0';
				length = 0;
				bufferSize = InitialSize;
			}
			~StringBuilder()
			{
				if(buffer)
					delete [] buffer;
			}
			void EnsureCapacity(int size)
			{
				if(bufferSize < size)
				{
					wchar_t * newBuffer = new wchar_t[size + 1];
					if(buffer)
					{
						wcscpy_s(newBuffer, size + 1, buffer);
						delete [] buffer;
					}
					buffer = newBuffer;
					bufferSize = size;
				}
			}

			//void Append(wchar_t * str)
			//{
			//	length += wcslen(str);
			//	if(bufferSize < length + 1)
			//	{
			//		int newBufferSize = InitialSize;
			//		while(newBufferSize < length + 1)
			//			newBufferSize <<= 1;
			//		wchar_t * newBuffer = new wchar_t[newBufferSize];
			//		if (buffer)
			//		{
			//			wcscpy_s(newBuffer, newBufferSize, buffer);
			//			delete [] buffer;
			//		}
			//		wcscat_s(newBuffer, newBufferSize, str); // use memcpy, manually deal with zero terminator
			//		buffer = newBuffer;
			//		bufferSize = newBufferSize;
			//	}
			//	else
			//	{
			//		wcscat_s(buffer, bufferSize, str); // use memcpy, manually deal with zero terminator
			//	}
			//}
			StringBuilder & operator << (const wchar_t * str)
			{
				Append(str, wcslen(str));
				return *this;
			}
			StringBuilder & operator << (const String & str)
			{
				Append(str);
				return *this;
			}
			StringBuilder & operator << (const _EndLine & endl)
			{
				Append(L'\n');
				return *this;
			}
			void Append(wchar_t ch)
			{
				Append(&ch, 1);
			}
			void Append(int value, int radix = 10)
			{
				wchar_t buffer[33];
				_itow_s(value, buffer, radix);
				Append(buffer);
			}
			void Append(const String & str)
			{
				Append(str.Buffer(), str.Length());
			}
			void Append(const wchar_t * str)
			{
				Append(str, wcslen(str));
			}
			void Append(const wchar_t * str, int strLen)
			{
				int newLength = length + strLen;
				if(bufferSize < newLength + 1)
				{
					int newBufferSize = InitialSize;
					while(newBufferSize < newLength + 1)
						newBufferSize <<= 1;
					wchar_t * newBuffer = new wchar_t[newBufferSize];
					if (buffer)
					{
						//wcscpy_s(newBuffer, newBufferSize, buffer);
						memcpy(newBuffer, buffer, sizeof(wchar_t) * length);
						delete [] buffer;
					}
					//wcscat_s(newBuffer, newBufferSize, str);
					memcpy(newBuffer + length, str, sizeof(wchar_t) * strLen);
					newBuffer[newLength] = L'\0';
					buffer = newBuffer;
					bufferSize = newBufferSize;
				}
				else
				{
					memcpy(buffer + length, str, sizeof(wchar_t) * strLen);
					buffer[newLength] = L'\0';
					//wcscat_s(buffer, bufferSize, str); // use memcpy, manually deal with zero terminator
				}
				length = newLength;
			}

			int Capacity()
			{
				return bufferSize;
			}

			wchar_t * Buffer()
			{
				return buffer;
			}

			int Length()
			{
				return length;
			}

			String ToString()
			{
				return String(buffer);
			}

			String ProduceString()
			{
				String rs;
				rs.buffer = buffer;
				rs.length = length;
				buffer = 0;
				length = 0;
				return rs;

			}

			void Remove(int id, int len)
			{
#if _DEBUG
				if (id >= length || id < 0)
					throw "Remove: Index out of range.";
				if(len < 0)
					throw "Remove: remove length smaller than zero.";
#endif
				int actualDelLength = ((id + len) >= length)? (length - id) : len;
				for (int i = id + actualDelLength; i <= length; i++)
					buffer[i - actualDelLength] = buffer[i];
				length -= actualDelLength;
			}

			void Clear()
			{
				length = 0;
				if (buffer)
					buffer[0] = 0;
			}
		};

		int StringToInt(const String & str);
		double StringToDouble(const String & str);
	}
}

#endif
#ifndef FUNDAMENTAL_LIB_LIST_H
#define FUNDAMENTAL_LIB_LIST_H
#include <type_traits>

const int MIN_QSORT_SIZE = 32;

namespace CoreLib
{
	namespace Basic
	{
		template<typename T>
		class List
		{
		private:
			static const int InitialSize = 16;
		private:
			T * buffer;
			int _count;
			int bufferSize;

			void Free()
			{
				if (buffer)
					delete [] buffer;
				buffer = 0;
				_count = bufferSize = 0;
			}
		public:
			T* begin() const
			{
				return buffer;
			}
			T* end() const
			{
				return buffer+_count;
			}
		public:
			List()
				: buffer(0), _count(0), bufferSize(0)
			{
			}
			List(const List<T> & list)
				: buffer(0), _count(0), bufferSize(0)
			{
				this->operator=(list);
			}
			List(List<T> && list)
				: buffer(0), _count(0), bufferSize(0)
			{
				//int t = static_cast<int>(1.0f); reinterpret_cast<double*>(&t), dynamic_cast<> 
				this->operator=(static_cast<List<T>&&>(list));
			}
			~List()
			{
				Free();
			}
			List<T> & operator=(const List<T> & list)
			{
				Free();
				AddRange(list);

				return *this;
			}

			List<T> & operator=(List<T> && list)
			{
				Free();
				_count = list._count;
				bufferSize = list.bufferSize;
				buffer = list.buffer;

				list.buffer = 0;
				list._count = 0;
				list.bufferSize = 0;
				return *this;
			}

			T & First() const
			{
#ifdef _DEBUG
				if (_count == 0)
					throw "Index out of range.";
#endif
				return buffer[0];
			}

			T & Last() const
			{
#ifdef _DEBUG
				if (_count == 0)
					throw "Index out of range.";
#endif
				return buffer[_count-1];
			}

			void Add(T && obj)
			{
				if (bufferSize < _count + 1)
				{
					int newBufferSize = InitialSize;
					if (bufferSize)
						newBufferSize = (bufferSize << 1);

					Reserve(newBufferSize);
				}
				buffer[_count++] = static_cast<T&&>(obj);
			}

			void Add(const T & obj)
			{
				if (bufferSize < _count + 1)
				{
					int newBufferSize = InitialSize;
					if (bufferSize)
						newBufferSize = (bufferSize << 1);

					Reserve(newBufferSize);
				}
				buffer[_count++] = obj;

			}

			int Count() const
			{
				return _count;
			}

			T * Buffer() const
			{
				return buffer;
			}

			int Capacity() const
			{
				return bufferSize;
			}

			void Insert(int id, const T & val)
			{
				InsertRange(id, &val, 1);
			}

			void InsertRange(int id, const T * vals, int n)
			{
				if (bufferSize < _count + n)
				{
					int newBufferSize = InitialSize;
					while (newBufferSize < _count + n)
						newBufferSize = newBufferSize << 1;

					T * newBuffer = new T[newBufferSize];
					if (bufferSize)
					{
						if (std::has_trivial_assign<T>::value ||
							std::has_trivial_copy<T>::value)
						{
							memcpy(newBuffer, buffer, sizeof(T) * id);
							memcpy(newBuffer + id + n, buffer + id, sizeof(T) * (_count - id));
						}
						else
						{
							for (int i = 0; i < id; i++)
								newBuffer[i] = buffer[i];
							for (int i = id; i < _count; i++)
								newBuffer[i + n] = T(static_cast<T&&>(buffer[i]));
						}
						delete [] buffer;
					}
					buffer = newBuffer;
					bufferSize = newBufferSize;
				}
				else
				{
					if (std::has_trivial_assign<T>::value ||
						std::has_trivial_copy<T>::value)
						memmove(buffer + id + n, buffer + id, sizeof(T) * (_count - id));
					else
					{
						for (int i = _count - 1; i >= id; i--)
							buffer[i + n] = static_cast<T&&>(buffer[i]);
					}
				}
				if (std::has_trivial_assign<T>::value ||
					std::has_trivial_copy<T>::value)
					memcpy(buffer + id, vals, sizeof(T) * n);
				else
					for (int i = 0; i < n; i++)
						buffer[id + i] = vals[i];

				_count += n;
			}

			//slower than original edition
			//void Add(const T & val)
			//{
			//	InsertRange(_count, &val, 1);
			//}

			void InsertRange(int id, const List<T> & list)
			{
				InsertRange(id, list.buffer, list._count);
			}

			void AddRange(const T * vals, int n)
			{
				InsertRange(_count, vals, n);
			}

			void AddRange(const List<T> & list)
			{
				InsertRange(_count, list.buffer, list._count);
			}

			void RemoveRange(int id, int deleteCount)
			{
#if _DEBUG
				if (id >= _count || id < 0)
					throw "Remove: Index out of range.";
				if(deleteCount < 0)
					throw "Remove: deleteCount smaller than zero.";
#endif
				int actualDeleteCount = ((id + deleteCount) >= _count)? (_count - id) : deleteCount;
				for (int i = id + actualDeleteCount; i < _count; i++)
					buffer[i - actualDeleteCount] = static_cast<T&&>(buffer[i]);
				_count -= actualDeleteCount;
			}

			void RemoveAt(int id)
			{
				RemoveRange(id, 1);
			}

			void Remove(const T & val)
			{
				int idx = IndexOf(val);
				if (idx != -1)
					RemoveAt(idx);
			}

			void FastRemove(const T & val)
			{
				int idx = IndexOf(val);
				if (idx != -1 && _count-1 != idx)
				{
					buffer[idx] = _Move(buffer[_count-1]);
				}
				_count--;
			}

			void Clear()
			{
				_count = 0;
			}

			void Reserve(int size)
			{
				if(size > bufferSize)
				{
					T * newBuffer = new T[size];
					if (bufferSize)
					{
						if (std::has_trivial_assign<T>::value ||
							std::has_trivial_copy<T>::value)
							memcpy(newBuffer, buffer, _count * sizeof(T));
						else
						{
							for (int i = 0; i < _count; i++)
								newBuffer[i] = static_cast<T&&>(buffer[i]);
						}
						delete [] buffer;
					}
					buffer = newBuffer;
					bufferSize = size;
				}
			}

			void SetSize(int size)
			{
				Reserve(size);
				_count = size;
			}

			void Compress()
			{
				if (bufferSize > _count && _count > 0)
				{
					T * newBuffer = new T[_count];
					for (int i = 0; i < _count; i++)
						newBuffer[i] = static_cast<T&&>(buffer[i]);
					delete [] buffer;
					buffer = newBuffer;
					bufferSize = _count;
				}
			}

			T & operator [](int id) const
			{
#if _DEBUG
				if(id >= _count || id < 0)
					throw IndexOutofRangeException(L"Operator[]: Index out of Range.");
#endif
				return buffer[id];
			}

			template<typename T2>
			int IndexOf(const T2 & val) const
			{
				for (int i = 0; i < _count; i++)
				{
					if (buffer[i] == val)
						return i;
				}
				return -1;
			}

			template<typename T2>
			int LastIndexOf(const T2 & val) const
			{
				for (int i = _count - 1; i >= 0; i--)
				{
					if(buffer[i] == val)
						return i;
				}
				return -1;
			}

			void Sort()
			{
				Sort([](T& t1, T& t2){return t1<t2;});
			}

			bool Contains(const T & val)
			{
				for (int i = 0; i<_count; i++)
					if (buffer[i] == val)
						return true;
				return false;
			}

			template<typename Comparer>
			void Sort(Comparer compare)
			{
				//InsertionSort(buffer, 0, _count - 1);
				QuickSort(buffer, 0, _count - 1, compare);
			}

			template <typename IterateFunc>
			void ForEach(IterateFunc f) const
			{
				for (int i = 0; i<_count; i++)
					f(buffer[i]);
			}

			template<typename Comparer>
			void QuickSort(T * vals, int startIndex, int endIndex, Comparer comparer)
			{
				if(startIndex < endIndex)
				{
					if (endIndex - startIndex < MIN_QSORT_SIZE)
						InsertionSort(vals, startIndex, endIndex, comparer);
					else
					{
						int pivotIndex = (startIndex + endIndex) >> 1;
						int pivotNewIndex = Partition(vals, startIndex, endIndex, pivotIndex, comparer);
						QuickSort(vals, startIndex, pivotNewIndex - 1, comparer);
						QuickSort(vals, pivotNewIndex + 1, endIndex, comparer);
					}
				}

			}
			template<typename Comparer>
			int Partition(T * vals, int left, int right, int pivotIndex, Comparer comparer)
			{
				T pivotValue = vals[pivotIndex];
				Swap(vals, right, pivotIndex);
				int storeIndex = left;
				for (int i = left; i < right; i++)
				{
					if (comparer(vals[i], pivotValue))
					{
						Swap(vals, i, storeIndex);
						storeIndex++;
					}
				}
				Swap(vals, storeIndex, right);
				return storeIndex;
			}
			template<typename Comparer>
			void InsertionSort(T * vals, int startIndex, int endIndex, Comparer comparer)
			{
				for (int i = startIndex  + 1; i <= endIndex; i++)
				{
					T insertValue = static_cast<T&&>(vals[i]);
					int insertIndex = i - 1;
					while (insertIndex >= startIndex && comparer(insertValue, vals[insertIndex]))
					{
						vals[insertIndex + 1] = static_cast<T&&>(vals[insertIndex]);
						insertIndex--;
					}
					vals[insertIndex + 1] = static_cast<T&&>(insertValue);
				}
			}

			inline void Swap(T * vals, int index1, int index2)
			{
				if (index1 != index2)
				{
					T tmp = static_cast<T&&>(vals[index1]);
					vals[index1] = static_cast<T&&>(vals[index2]);
					vals[index2] = static_cast<T&&>(tmp);
				}
			}

			template<typename T2, typename Comparer>
			int BinarySearch(const T2 & obj, Comparer comparer)
			{
				int imin = 0, imax = _count - 1;
				while (imax >= imin)
				{
					int imid = (imin + imax) >> 1;
					int compareResult = comparer(buffer[imid], obj);
					if (compareResult == 0)
						return imid;
					else if (compareResult < 0)
						imin = imid + 1;
					else
						imax = imid - 1;
				}
				return -1;
			}

			template<typename T2>
			int BinarySearch(const T2 & obj)
			{
				return BinarySearch(obj, 
					[](T & curObj, const T2 & thatObj)->int
					{
						if (curObj < thatObj)
							return -1;
						else if (curObj == thatObj)
							return 0;
						else
							return 1;
					});
			}
		};

		template<typename T>
		T Min(const List<T> & list)
		{
			T minVal = list.First();
			for (int i = 1; i<list.Count(); i++)
				if (list[i] < minVal)
					minVal = list[i];
			return minVal;
		}

		template<typename T>
		T Max(const List<T> & list)
		{
			T maxVal = list.First();
			for (int i = 1; i<list.Count(); i++)
				if (list[i] > maxVal)
					maxVal = list[i];
			return maxVal;
		}
	}
}

#endif
#ifndef CORE_LIB_LINK_H
#define CORE_LIB_LINK_H

#ifndef CORE_LIB_EXCEPTION_H
#define CORE_LIB_EXCEPTION_H


namespace CoreLib
{
	namespace Basic
	{
		class Exception : public Object
		{
		public:
			String Message;
			Exception()
			{}
			Exception(const String & message)
				: Message(message)
			{
			}
		};

		class IndexOutofRangeException : public Exception
		{
		public:
			IndexOutofRangeException()
			{}
			IndexOutofRangeException(const String & message)
				: Exception(message)
			{
			}

		};

		class InvalidOperationException : public Exception
		{
		public:
			InvalidOperationException()
			{}
			InvalidOperationException(const String & message)
				: Exception(message)
			{
			}

		};
		
		class ArgumentException : public Exception
		{
		public:
			ArgumentException()
			{}
			ArgumentException(const String & message)
				: Exception(message)
			{
			}

		};

		class KeyNotFoundException : public Exception
		{
		public:
			KeyNotFoundException()
			{}
			KeyNotFoundException(const String & message)
				: Exception(message)
			{
			}
		};
		class KeyExistsException : public Exception
		{
		public:
			KeyExistsException()
			{}
			KeyExistsException(const String & message)
				: Exception(message)
			{
			}
		};

		class NotSupportedException : public Exception
		{
		public:
			NotSupportedException()
			{}
			NotSupportedException(const String & message)
				: Exception(message)
			{
			}
		};

		class NotImplementedException : public Exception
		{
		public:
			NotImplementedException()
			{}
			NotImplementedException(const String & message)
				: Exception(message)
			{
			}
		};
	}
}

#endif

namespace CoreLib
{
	namespace Basic
	{
		template<typename T>
		class LinkedNode
		{
			template<typename T1>
			friend class LinkedList;
		private:
			LinkedNode<T> *pPrev, *pNext;
			LinkedList<T> * FLink;
		public:
			T Value;
			LinkedNode (LinkedList<T> * lnk):FLink(lnk)
			{
				pPrev = pNext = 0;
			};
			LinkedNode<T> * GetPrevious()
			{
				return pPrev;
			};
			LinkedNode<T> * GetNext()
			{
				return pNext;
			};
			LinkedNode<T> * InsertAfter(const T & nData)
			{
				LinkedNode<T> * n = new LinkedNode<T>(FLink);
				n->Value = nData;
				n->pPrev = this;
				n->pNext = this->pNext;
				LinkedNode<T> *npp = n->pNext;
				if (npp)
				{
					npp->pPrev = n;
				}
				pNext = n;
				if (!n->pNext)
					FLink->FTail = n;
				FLink->FCount ++;
				return n;
			};
			LinkedNode<T> * InsertBefore(const T & nData)
			{
				LinkedNode<T> * n = new LinkedNode<T>(FLink);
				n->Value = nData;
				n->pPrev = pPrev;
				n->pNext = this;
				pPrev = n;
				LinkedNode<T> *npp = n->pPrev;
				if (npp)
					npp->pNext = n;
				if (!n->pPrev)
					FLink->FHead = n;
				FLink->FCount ++;
				return n;
			};
			void Delete()
			{
				if (pPrev)
					pPrev->pNext = pNext;
				if (pNext)
					pNext->pPrev = pPrev;
				FLink->FCount --;
				if (FLink->FHead == this)
				{
					FLink->FHead = pNext;
				}
				if (FLink->FTail == this)
				{
					FLink->FTail = pPrev;
				}
				delete this;
			}
		};
		template<typename T>
		class LinkedList
		{
			template<typename T1>
			friend class LinkedNode;
		private:
			LinkedNode<T> * FHead, *FTail;
			int FCount;
		public:
			class Iterator
			{
			public:
				LinkedNode<T> * Current, *Next;
				void SetCurrent(LinkedNode<T> * cur)
				{
					Current = cur;
					if (Current)
						Next = Current->GetNext();
					else
						Next = 0;
				}
				Iterator(LinkedNode<T> * cur)
				{
					SetCurrent(cur);
				}
				T & operator *() const
				{
					return Current->Value;
				}
				Iterator& operator ++()
				{
					SetCurrent(Next);
					return *this;
				}
				Iterator operator ++(int)
				{
					Iterator rs = *this;
					SetCurrent(Next);
					return rs;
				}
				bool operator != (const Iterator & iter) const
				{
					return Current != iter.Current;
				}
				bool operator == (const Iterator & iter) const
				{
					return Current == iter.Current;
				}
			};
			Iterator begin() const
			{
				return Iterator(FHead);
			}
			Iterator end() const
			{
				return Iterator(0);
			}
		public:
			LinkedList() : FHead(0), FTail(0), FCount(0)
			{
			}
			~LinkedList()
			{
				Clear();
			}
			LinkedList(const LinkedList<T> & link) : FHead(0), FTail(0), FCount(0)
			{
				this->operator=(link);
			}
			LinkedList(LinkedList<T> && link) : FHead(0), FTail(0), FCount(0)
			{
				this->operator=(_Move(link));
			}
			LinkedList<T> & operator = (LinkedList<T> && link)
			{
				if (FHead != 0)
					Clear();
				FHead = link.FHead;
				FTail = link.FTail;
				FCount = link.FCount;
				link.FHead = 0;
				link.FTail = 0;
				link.FCount = 0;
				return *this;
			}
			LinkedList<T> & operator = (const LinkedList<T> & link)
			{
				if (FHead != 0)
					Clear();
				auto p = link.FHead;
				while (p)
				{
					AddLast(p->Value);
					p = p->GetNext();
				}
				return *this;
			}
			template<typename IteratorFunc>
			void ForEach(const IteratorFunc & f)
			{
				auto p = FHead;
				while (p)
				{
					f(p->Value);
					p = p->GetNext();
				}
			}
			LinkedNode<T> * GetNode(int x)
			{
				LinkedNode<T> *pCur = FHead;
				for (int i=0;i<x;i++)
				{
					if (pCur)
						pCur = pCur->pNext;
					else
						throw "Index out of range";
				}
				return pCur;
			};
			LinkedNode<T> * Find(const T& fData)
			{
				for (LinkedNode<T> * pCur = FHead; pCur; pCur = pCur->pNext)
				{
					if (pCur->Value == fData)
						return pCur;
				}
				return 0;
			};
			LinkedNode<T> * FirstNode()
			{
				return FHead;
			};
			T & First()
			{
				if (!FHead)
					throw IndexOutofRangeException("LinkedList: index out of range.");
				return FHead->Value;
			}
			T & Last()
			{
				if (!FTail)
					throw IndexOutofRangeException("LinkedList: index out of range.");
				return FTail->Value;
			}
			LinkedNode<T> * LastNode()
			{
				return FTail;
			};
			LinkedNode<T> * AddLast(const T & nData)
			{
				LinkedNode<T> * n = new LinkedNode<T>(this);
				n->Value = nData;
				n->pPrev = FTail;
				if (FTail)
					FTail->pNext = n;
				n->pNext = 0;
				FTail = n;
				if (!FHead)
					FHead = n;
				FCount ++;
				return n;
			};
			// Insert a blank node
			LinkedNode<T> * AddLast()
			{
				LinkedNode<T> * n = new LinkedNode<T>(this);
				n->pPrev = FTail;
				if (FTail)
					FTail->pNext = n;
				n->pNext = 0;
				FTail = n;
				if (!FHead)
					FHead = n;
				FCount ++;
				return n;
			};
			LinkedNode<T> * AddFirst(const T& nData)
			{
				LinkedNode<T> *n = new LinkedNode<T>(this);
				n->Value = nData;
				n->pPrev = 0;
				n->pNext = FHead;
				if (FHead)
					FHead->pPrev = n;
				FHead = n;
				if (!FTail)
					FTail = n;
				FCount ++;
				return n;
			};
			void Delete(LinkedNode<T>*n, int Count = 1)
			{
				LinkedNode<T> *n1,*n2, *tn;
				n1 = n->pPrev;
				tn = n;
				int numDeleted = 0;
				for (int i=0; i<Count; i++)
				{
					n2 = tn->pNext;
					delete tn;
					tn = n2;
					numDeleted++;
					if (tn == 0)
						break;
				}
				if (n1)
					n1->pNext = n2;
				else
					FHead = n2;
				if (n2)
					n2->pPrev = n1;
				else
					FTail = n1;
				FCount -= numDeleted;
			}
			void Clear()
			{
				for (LinkedNode<T> *n = FHead; n; )
				{
					LinkedNode<T> * tmp = n->pNext;
					delete n;
					n = tmp;
				}
				FHead = 0;
				FTail = 0;
				FCount = 0;
			}
			List<T> ToList() const
			{
				List<T> rs;
				rs.Reserve(FCount);
				for (auto & item : *this)
				{
					rs.Add(item);
				}
				return rs;
			}
			int Count()
			{
				return FCount;
			}
		};
	}
}
#endif
#ifndef CORE_LIB_DICTIONARY_H
#define CORE_LIB_DICTIONARY_H
#ifndef BIT_VECTOR_INT_SET_H
#define BIT_VECTOR_INT_SET_H

#include <memory.h>

namespace CoreLib
{
	namespace Basic
	{
		class IntSet
		{
		private:
			List<int> buffer;
		public:
			IntSet()
			{}
			IntSet(const IntSet & other)
			{
				buffer = other.buffer;
			}
			IntSet(IntSet && other)
			{
				*this = (_Move(other));
			}
			IntSet & operator = (IntSet && other)
			{
				buffer = _Move(other.buffer);
				return *this;
			}
			IntSet & operator = (const IntSet & other)
			{
				buffer = other.buffer;
				return *this;
			}
			IntSet(int maxVal)
			{
				SetMax(maxVal);
			}
			int Size() const
			{
				return buffer.Count()*32;
			}
			void SetMax(int val)
			{
				Resize(val);
				Clear();
			}
			void Resize(int size)
			{
				int oldBufferSize = buffer.Count();
				buffer.SetSize((size+31)>>5);
				if (buffer.Count() > oldBufferSize)
					memset(buffer.Buffer()+oldBufferSize, 0, (buffer.Count()-oldBufferSize) * sizeof(int));
			}
			void Clear()
			{
				memset(buffer.Buffer(), 0, sizeof(int)*buffer.Count());
			}
			void Add(int val)
			{
				int id = val>>5;
				if (id < buffer.Count())
					buffer[id] |= (1<<(val&31));
				else
				{
					int oldSize = buffer.Count();
					buffer.SetSize(id+1);
					memset(buffer.Buffer() + oldSize, 0, (buffer.Count()-oldSize)*sizeof(int));
					buffer[id] |= (1<<(val&31));
				}
			}
			void Remove(int val)
			{
				if ((val>>5) < buffer.Count())
					buffer[(val>>5)] &= ~(1<<(val&31));
			}
			bool Contains(int val) const
			{
				if ((val>>5) >= buffer.Count())
					return false;
				return (buffer[(val>>5)] & (1<<(val&31))) != 0;
			}
			void UnionWith(const IntSet & set)
			{
				for (int i = 0; i<Math::Min(set.buffer.Count(), buffer.Count()); i++)
				{
					buffer[i] |= set.buffer[i];
				}
				if (set.buffer.Count() > buffer.Count())
					buffer.AddRange(set.buffer.Buffer()+buffer.Count(), set.buffer.Count()-buffer.Count());
			}
			bool operator == (const IntSet & set)
			{
				if (buffer.Count() != set.buffer.Count())
					return false;
				for (int i = 0; i<buffer.Count(); i++)
					if (buffer[i] != set.buffer[i])
						return false;
				return true;
			}
			bool operator != (const IntSet & set)
			{
				return !(*this == set);
			}
			void IntersectWith(const IntSet & set)
			{
				if (set.buffer.Count() < buffer.Count())
					memset(buffer.Buffer() + set.buffer.Count(), 0, (buffer.Count()-set.buffer.Count())*sizeof(int));
				for (int i = 0; i<Math::Min(set.buffer.Count(), buffer.Count()); i++)
				{
					buffer[i] &= set.buffer[i];
				}
			}
			static void Union(IntSet & rs, const IntSet & set1, const IntSet & set2)
			{
				rs.buffer.SetSize(Math::Max(set1.buffer.Count(), set2.buffer.Count()));
				rs.Clear();
				for (int i = 0; i<set1.buffer.Count(); i++)
					rs.buffer[i] |= set1.buffer[i];
				for (int i = 0; i<set2.buffer.Count(); i++)
					rs.buffer[i] |= set2.buffer[i];
			}
			static void Intersect(IntSet & rs, const IntSet & set1, const IntSet & set2)
			{
				rs.buffer.SetSize(Math::Min(set1.buffer.Count(), set2.buffer.Count()));
				for (int i = 0; i<rs.buffer.Count(); i++)
					rs.buffer[i] = set1.buffer[i] & set2.buffer[i];
			}
			static void Subtract(IntSet & rs, const IntSet & set1, const IntSet & set2)
			{
				rs.buffer.SetSize(set1.buffer.Count());
				for (int i = 0; i<Math::Min(set1.buffer.Count(), set2.buffer.Count()); i++)
					rs.buffer[i] = set1.buffer[i] & (~set2.buffer[i]);
			}
			static bool HasIntersection(const IntSet & set1, const IntSet & set2)
			{
				for (int i = 0; i<Math::Min(set1.buffer.Count(), set2.buffer.Count()); i++)
				{
					if (set1.buffer[i] & set2.buffer[i])
						return true;
				}
				return false;
			}
		};
	}
}

#endif

namespace CoreLib
{
	namespace Basic
	{
		template<int IsInt>
		class Hash
		{
		public:
		};
		template<>
		class Hash<1>
		{
		public:
			template<typename TKey>
			static int GetHashCode(TKey & key)
			{
				return (int)key;
			}
		};
		template<>
		class Hash<0>
		{
		public:
			template<typename TKey>
			static int GetHashCode(TKey & key)
			{
				return key.GetHashCode();
			}
		};
		template<int IsPointer>
		class PointerHash
		{};
		template<>
		class PointerHash<1>
		{
		public:
			template<typename TKey>
			static int GetHashCode(TKey & key)
			{
				return ((int)key)/sizeof(typename std::remove_pointer<TKey>::type);
			}
		};
		template<>
		class PointerHash<0>
		{
		public:
			template<typename TKey>
			static int GetHashCode(TKey & key)
			{
				return Hash<std::is_integral<TKey>::value || std::is_enum<TKey>::value>::GetHashCode(key);
			}
		};

		template<typename TKey>
		int GetHashCode(const TKey & key)
		{
			return PointerHash<std::is_pointer<TKey>::value>::GetHashCode(key);
		}

		template<typename TKey>
		int GetHashCode(TKey & key)
		{
			return PointerHash<std::is_pointer<TKey>::value>::GetHashCode(key);
		}
		
		
		inline int GetHashCode(double key)
		{
			return FloatAsInt((float)key);
		}
		inline int GetHashCode(float key)
		{
			return FloatAsInt(key);
		}

		template<typename TKey, typename TValue>
		class KeyValuePair
		{
		public:
			TKey Key;
			TValue Value;
			KeyValuePair()
			{}
			KeyValuePair(const TKey & key, const TValue & value)
			{
				Key = key;
				Value = value;
			}
			KeyValuePair(TKey && key, TValue && value)
			{
				Key = _Move(key);
				Value = _Move(value);
			}
			KeyValuePair(TKey && key, const TValue & value)
			{
				Key = _Move(key);
				Value = value;
			}
			KeyValuePair(const KeyValuePair<TKey, TValue> & _that)
			{
				Key = _that.Key;
				Value = _that.Value;
			}
			KeyValuePair(KeyValuePair<TKey, TValue> && _that)
			{
				operator=(_Move(_that));
			}
			KeyValuePair & operator=(KeyValuePair<TKey, TValue> && that)
			{
				Key = _Move(that.Key);
				Value = _Move(that.Value);
				return *this;
			}
			int GetHashCode()
			{
				return Hash<TKey>::GetHashCode(Key);
			}
		};

		const float MaxLoadFactor = 0.7f;

		template<typename TKey, typename TValue>
		class Dictionary
		{
			friend class Iterator;
			friend class ItemProxy;
		private:
			inline int GetProbeOffset(int probeId) const
			{
				// quadratic probing
				return probeId * probeId;
				//return 1;
			}
		private:
			int bucketSizeMinusOne;
			int _count;
			IntSet marks;
			KeyValuePair<TKey, TValue>* hashMap;
			void Free()
			{
				if (hashMap)
					delete [] hashMap;
				hashMap = 0;
			}
			inline bool IsDeleted(int pos) const
			{
				return marks.Contains((pos<<1) + 1);
			}
			inline bool IsEmpty(int pos) const
			{
				return !marks.Contains((pos<<1));
			}
			inline void SetDeleted(int pos, bool val)
			{
				if (val)
					marks.Add((pos<<1)+1);
				else
					marks.Remove((pos<<1)+1);
			}
			inline void SetEmpty(int pos, bool val)
			{
				if (val)
					marks.Remove((pos<<1));
				else
					marks.Add((pos<<1));
			}
			struct FindPositionResult
			{
				int ObjectPosition;
				int InsertionPosition;
				FindPositionResult()
				{
					ObjectPosition = -1;
					InsertionPosition = -1;
				}
				FindPositionResult(int objPos, int insertPos)
				{
					ObjectPosition = objPos;
					InsertionPosition = insertPos;
				}

			};
			inline int GetHashPos(TKey & key) const
			{
				return GetHashCode(key) & bucketSizeMinusOne;
			}
			FindPositionResult FindPosition(const TKey & key) const
			{
				int hashPos = GetHashPos((TKey&)key);
				int insertPos = -1;
				int numProbes = 0;
				while (numProbes <= bucketSizeMinusOne)
				{
					if (IsEmpty(hashPos))
					{
						if (insertPos == -1)
							return FindPositionResult(-1, hashPos);
						else
							return FindPositionResult(-1, insertPos);
					}
					else if (IsDeleted(hashPos))
					{
						if (insertPos == -1)
							insertPos = hashPos;
					}
					else if (hashMap[hashPos].Key == key)
					{
						return FindPositionResult(hashPos, -1);
					}
					numProbes++;
					hashPos = (hashPos+GetProbeOffset(numProbes)) & bucketSizeMinusOne;
				}
				throw InvalidOperationException(L"Hash map is full. This indicates an error in Key::Equal or Key::GetHashCode.");
			}
			TValue & _Insert(KeyValuePair<TKey, TValue> && kvPair, int pos)
			{
				hashMap[pos] = _Move(kvPair);
				SetEmpty(pos, false);
				SetDeleted(pos, false);
				return hashMap[pos].Value;
			}
			void Rehash()
			{
				if (bucketSizeMinusOne == -1 || _count/(float)bucketSizeMinusOne >= MaxLoadFactor)
				{
					int newSize = (bucketSizeMinusOne+1) * 2;
					if (newSize == 0)
						newSize = 16;
					Dictionary<TKey, TValue> newDict;
					newDict.bucketSizeMinusOne = newSize - 1;
					newDict.hashMap = new KeyValuePair<TKey, TValue>[newSize];
					newDict.marks.SetMax(newSize*2);
					if (hashMap)
					{
						for (auto & kvPair : *this)
						{
							newDict.Add(_Move(kvPair));
						}
					}
					*this = _Move(newDict);
				}
			}
			
			bool AddIfNotExists(KeyValuePair<TKey, TValue> && kvPair)
			{
				Rehash();
				auto pos = FindPosition(kvPair.Key);
				if (pos.ObjectPosition != -1)
					return false;
				else if (pos.InsertionPosition != -1)
				{
					_count++;
					_Insert(_Move(kvPair), pos.InsertionPosition);
					return true;
				}
				else
					throw InvalidOperationException(L"Inconsistent find result returned. This is a bug in Dictionary implementation.");
			}
			void Add(KeyValuePair<TKey, TValue> && kvPair)
			{
				if (!AddIfNotExists(_Move(kvPair)))
					throw KeyExistsException(L"The key already exists in Dictionary.");
			}
			TValue & Set(KeyValuePair<TKey, TValue> && kvPair)
			{
				Rehash();
				auto pos = FindPosition(kvPair.Key);
				if (pos.ObjectPosition != -1)
					return _Insert(_Move(kvPair), pos.ObjectPosition);
				else if (pos.InsertionPosition != -1)
				{
					_count++;
					return _Insert(_Move(kvPair), pos.InsertionPosition);
				}
				else
					throw InvalidOperationException(L"Inconsistent find result returned. This is a bug in Dictionary implementation.");
			}
		public:
			class Iterator
			{
			private:
				const Dictionary<TKey, TValue> * dict;
				int pos;
			public:
				KeyValuePair<TKey, TValue> & operator *()
				{
					return dict->hashMap[pos];
				}
				KeyValuePair<TKey, TValue> * operator ->()
				{
					return dict->hashMap + pos;
				}
				Iterator & operator ++()
				{
					if (pos > dict->bucketSizeMinusOne)
						return *this;
					pos++;
					while (pos <= dict->bucketSizeMinusOne && (dict->IsDeleted(pos) || dict->IsEmpty(pos)))
					{
						pos++;
					}
					return *this;
				}
				Iterator operator ++(int)
				{
					Iterator rs = * this;
					operator++( );
					return rs;
				}
				bool operator != (const Iterator & _that)
				{
					return pos != _that.pos || dict != _that.dict;
				}
				bool operator == (const Iterator & _that)
				{
					return pos == _that.pos && dict == _that.dict;
				}
				Iterator(const Dictionary<TKey, TValue> * _dict, int _pos)
				{
					this->dict = _dict;
					this->pos = _pos;
				}
				Iterator()
				{
					this->dict = 0;
					this->pos = 0;
				}
			};

			Iterator begin() const
			{
				int pos = 0;
				while (pos < bucketSizeMinusOne + 1)
				{
					if (IsEmpty(pos) || IsDeleted(pos))
						pos++;
					else
						break;
				}
				return Iterator(this, pos);
			}
			Iterator end() const
			{
				return Iterator(this, bucketSizeMinusOne + 1);
			}
		public:
			void Add(const TKey & key, const TValue & value)
			{
				Add(KeyValuePair<TKey, TValue>(key, value));
			}
			void Add(TKey && key, TValue && value)
			{
				Add(KeyValuePair<TKey, TValue>(_Move(key), _Move(value)));
			}
			bool AddIfNotExists(const TKey & key, const TValue & value)
			{
				return AddIfNotExists(KeyValuePair<TKey, TValue>(key, value));
			}
			bool AddIfNotExists(TKey && key, TValue && value)
			{
				return AddIfNotExists(KeyValuePair<TKey, TValue>(_Move(key), _Move(value)));
			}
			void Remove(const TKey & key)
			{
				auto pos = FindPosition(key);
				if (pos.ObjectPosition != -1)
				{
					SetDeleted(pos.ObjectPosition, true);
					_count--;
				}
			}
			void Clear()
			{
				_count = 0;
				marks.Clear();
			}
			bool ContainsKey(const TKey & key) const
			{
				if (bucketSizeMinusOne == -1)
					return false;
				auto pos = FindPosition(key);
				return pos.ObjectPosition != -1;
			}
			bool TryGetValue(const TKey & key, TValue & value) const
			{
				if (bucketSizeMinusOne == -1)
					return false;
				auto pos = FindPosition(key);
				if (pos.ObjectPosition != -1)
				{
					value = hashMap[pos.ObjectPosition].Value;
					return true;
				}
				return false;
			}
			class ItemProxy
			{
			private:
				const Dictionary<TKey, TValue> * dict;
				TKey key;
			public:
				ItemProxy(const TKey & _key, const Dictionary<TKey, TValue> * _dict)
				{
					this->dict = _dict;
					this->key = _key;
				}
				ItemProxy(TKey && _key, const Dictionary<TKey, TValue> * _dict)
				{
					this->dict = _dict;
					this->key = _Move(_key);
				}
				TValue & GetValue() const
				{
					auto pos = dict->FindPosition(key);
					if (pos.ObjectPosition != -1)
					{
						return dict->hashMap[pos.ObjectPosition].Value;
					}
					else
						throw KeyNotFoundException(L"The key does not exists in dictionary.");
				}
				operator TValue&() const
				{
					return GetValue();
				}
				TValue & operator = (const TValue & val)
				{
					return ((Dictionary<TKey,TValue>*)dict)->Set(KeyValuePair<TKey, TValue>(_Move(key), val));
				}
				TValue & operator = (TValue && val)
				{
					return ((Dictionary<TKey,TValue>*)dict)->Set(KeyValuePair<TKey, TValue>(_Move(key), _Move(val)));
				}
			};
			ItemProxy operator [](const TKey & key) const
			{
				return ItemProxy(key, this);
			}
			ItemProxy operator [](TKey && key) const
			{
				return ItemProxy(_Move(key), this);
			}
			int Count() const
			{
				return _count;
			}
		public:
			Dictionary()
			{
				bucketSizeMinusOne = -1;
				_count = 0;
				hashMap = 0;
			}
			Dictionary(const Dictionary<TKey, TValue> & other)
				: hashMap(0), _count(0), bucketSizeMinusOne(-1)
			{
				*this = other;
			}
			Dictionary(Dictionary<TKey, TValue> && other)
				: hashMap(0), _count(0), bucketSizeMinusOne(-1)
			{
				*this = (_Move(other));
			}
			Dictionary<TKey, TValue> & operator = (const Dictionary<TKey, TValue> & other)
			{
				Free();
				bucketSizeMinusOne = other.bucketSizeMinusOne;
				_count = other._count;
				hashMap = new KeyValuePair<TKey, TValue>[other.bucketSizeMinusOne+1];
				marks = other.marks;
				for (int i = 0; i<= bucketSizeMinusOne; i++)
					hashMap[i] = other.hashMap[i];
				return *this;
			}
			Dictionary<TKey, TValue> & operator = (Dictionary<TKey, TValue> && other)
			{
				Free();
				bucketSizeMinusOne = other.bucketSizeMinusOne;
				_count = other._count;
				hashMap = other.hashMap;
				marks = _Move(other.marks);
				other.hashMap = 0;
				other._count = 0;
				other.bucketSizeMinusOne = -1;
				return *this;
			}
			~Dictionary()
			{
				Free();
			}
		};

		

		template<typename T>
		class HashSet
		{
		private:
			class _DummyClass
			{};
			Dictionary<T, _DummyClass> dict;
		public:
			HashSet()
			{}
			HashSet(const HashSet & set)
			{
				operator=(set);
			}
			HashSet(HashSet && set)
			{
				operator=(_Move(set));
			}
			HashSet & operator = (const HashSet & set)
			{
				dict = set;
				return *this;
			}
			HashSet & operator = (HashSet && set)
			{
				dict = _Move(set.dict);
				return *this;
			}
		public:
			class Iterator
			{
			private:
				typename Dictionary<T, _DummyClass>::Iterator iter;
			public:
				T & operator *()
				{
					return (*iter).Key;
				}
				T * operator ->()
				{
					return &(*iter).Key;
				}
				Iterator & operator ++()
				{
					++iter;
					return *this;
				}
				Iterator operator ++(int)
				{
					Iterator rs = * this;
					operator++( );
					return rs;
				}
				bool operator != (const Iterator & _that)
				{
					return iter != _that.iter;
				}
				bool operator == (const Iterator & _that)
				{
					return iter == _that.iter;
				}
				Iterator(const typename Dictionary<T, _DummyClass>::Iterator & _iter)
				{
					this->iter = _iter;
				}
			};
			Iterator begin()
			{
				return Iterator(dict.begin());
			}
			Iterator end()
			{
				return Iterator(dict.end());
			}
		public:
			int Count() const
			{
				return dict.Count();
			}
			void Clear()
			{
				dict.Clear();
			}
			bool Add(const T& obj)
			{
				return dict.AddIfNotExists(obj, _DummyClass());
			}
			bool Add(T && obj)
			{
				return dict.AddIfNotExists(_Move(obj), _DummyClass());
			}
			void Remove(const T & obj)
			{
				dict.Remove(obj);
			}
			bool Contains(const T & obj)
			{
				return dict.ContainsKey(obj);
			}
		};
	}
}

#endif

#endif
#ifndef MATH_H
#define MATH_H
#include <memory.h>
#include <random>
#include <math.h>
#include <xmmintrin.h>

namespace VectorMath
{
	const float PI = 3.1415926535f;
	const float Epsilon = 1e-4f;
	const int DefaultFloatUlps = 1024;
	inline int FloatAsInt(float val);
	inline float IntAsFloat(int val);
	inline float Clamp(float val, float vmin, float vmax)
	{
		return val>vmax?vmax:val<vmin?vmin:val;
	}
	inline bool FloatEquals(float A, float B, int maxUlps = DefaultFloatUlps)
	{
		int aInt = *(int*)&A;
		// Make aInt lexicographically ordered as a twos-complement int
		if (aInt < 0)
			aInt = 0x80000000 - aInt;
		// Make bInt lexicographically ordered as a twos-complement int
		int bInt = *(int*)&B;
		if (bInt < 0)
			bInt = 0x80000000 - bInt;
		int intDiff = abs(aInt - bInt);
		if (intDiff <= maxUlps)
			return true;
		return false;
	}
	inline bool FloatLarger(float A, float B, int maxUlps = DefaultFloatUlps)
	{
		return A>B && !FloatEquals(A,B,maxUlps);
	}
	inline bool FloatSmaller(float A, float B, int maxUlps = DefaultFloatUlps)
	{
		return A<B && !FloatEquals(A,B,maxUlps);
	}
	inline bool FloatSmallerOrEquals(float A, float B, int maxUlps = DefaultFloatUlps)
	{
		return A<B || FloatEquals(A, B, maxUlps);
	}
	inline bool FloatLargerOrEquals(float A, float B, int maxUlps = DefaultFloatUlps)
	{
		return A>B || FloatEquals(A, B, maxUlps);
	}

	template<typename T>
	inline T Max(T v1, T v2)
	{
		if (v1>v2) return v1; else return v2;
	}
	template<typename T>
	inline T Min(T v1, T v2)
	{
		if (v1<v2) return v1; else return v2;
	}

	class Vec4;
	class Vec2
	{
	public:
		float x, y;
		Vec2()
		{}
		Vec2(float vx, float vy)
		{
			x = vx; y = vy;
		}
		Vec2(const Vec2 & v)
		{
			x = v.x;
			y = v.y;
		}
		inline void SetZero()
		{
			x = y = 0.0f;
		}
		inline float operator [] (int i)
		{
			return ((float*)this)[i];
		}
	};
	class Vec3
	{
	public:
		float x,y,z;
		Vec3()
		{}
		Vec3(float f)
		{
			x = y = z = f;
		}
		Vec3(float vx, float vy, float vz)
		{
			x = vx;	y = vy;	z = vz;
		}
		Vec3(const Vec3 & v)
		{
			x = v.x;
			y = v.y;
			z = v.z;
		}
		inline void SetZero()
		{
			x = y = z = 0.0f;
		}
		inline float& operator [] (int i) const
		{
			return ((float*)this)[i];
		}
		inline Vec3 & operator += (const Vec3 & vin)
		{
			x += vin.x; y += vin.y; z += vin.z;
			return *this;
		}
		inline Vec3 & operator -= (const Vec3 & vin)
		{
			x -= vin.x; y -= vin.y; z -= vin.z; 
			return *this;
		}
		inline Vec3 & operator *= (const Vec3 & vin)
		{
			x *= vin.x; y *= vin.y; z *= vin.z;
			return *this;
		}
		inline Vec3 & operator *= (float s)
		{
			x *= s; y *= s; z *= s;
			return *this;
		}
		inline Vec3 & operator /= (const Vec3 & vin)
		{
			x /= vin.x; y /= vin.y; z /= vin.z;
			return *this;
		}
		inline Vec3 & operator /= (float s)
		{
			float inv = 1.0f/s;
			return (*this)*=inv;
		}
		inline bool operator == (const Vec3 & vin)
		{
			return fabs(vin.x - x) < fabs(x)*Epsilon && 
				fabs(vin.y - y) < fabs(y)*Epsilon &&
				fabs(vin.z - z) < fabs(z)*Epsilon;
		}
		inline bool operator != (const Vec3 & vin)
		{
			return fabs(vin.x - x) > fabs(x)*Epsilon ||
				fabs(vin.y - y) > fabs(y)*Epsilon ||
				fabs(vin.z - z) > fabs(z)*Epsilon;
		}
		inline int GetHashCode()
		{
			return FloatAsInt(x) ^ FloatAsInt(y) ^ FloatAsInt(z);
		}
		inline static float Dot(const Vec3 & v1, const Vec3 & v2)
		{
			return v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
		}
		inline static void Cross(Vec3 & rs_d, const Vec3 & v1, const Vec3 & v2)
		{
			rs_d.x = v1.y*v2.z - v1.z * v2.y;
			rs_d.y = v1.z*v2.x - v1.x * v2.z;
			rs_d.z = v1.x*v2.y - v1.y * v2.x;
		}
		inline static void Scale(Vec3 & rs, const Vec3 & v1, float s)
		{
			rs.x = v1.x*s;	rs.y = v1.y*s;	rs.z = v1.z*s;
		}
		inline static void Add(Vec3 & rs, const Vec3 & v1, const Vec3 & v2)
		{
			rs.x = v1.x + v2.x;
			rs.y = v1.y + v2.y;
			rs.z = v1.z + v2.z;
		}
		inline static void Subtract(Vec3 & rs, const Vec3 & v1, const Vec3 & v2)
		{
			rs.x = v1.x - v2.x;
			rs.y = v1.y - v2.y;
			rs.z = v1.z - v2.z;
		}
		inline static void Multiply(Vec3 & rs, const Vec3 & v1, const Vec3 & v2)
		{
			rs.x = v1.x * v2.x;
			rs.y = v1.y * v2.y;
			rs.z = v1.z * v2.z;
		}
		inline float LengthFPU() const
		{
			return sqrt(x*x + y*y + z*z);
		}
		static inline void NormalizeFPU(Vec3 & rs, const Vec3 & vin)
		{
			float invLen = 1.0f/vin.LengthFPU();
			Scale(rs, vin, invLen);
		}
		inline float Length() const;
		static inline void Normalize(Vec3 & rs, const Vec3 & vin);
	};
	class Vec4
	{
	public:
		float x,y,z,w;
		Vec4()
		{}
		Vec4(float f)
		{
			x = y = z = w = f;
		}
		Vec4(float vx, float vy, float vz, float vw)
		{
			x = vx;	y = vy;	z = vz;	w = vw;
		}
		Vec4(const Vec3 & v)
		{
			x = v.x; y = v.y; z = v.z; w = 0.0f;
		}
		Vec4(const Vec3 & v, float vw)
		{
			x = v.x; y = v.y; z = v.z; w = vw;
		}
		Vec4(const Vec4 & v)
		{
			x = v.x; y = v.y; z = v.z; w = v.w;
		}
		inline void SetZero()
		{
			x = y = z = w = 0.0f;
		}
		inline void xyz(Vec3 & v) const
		{
			v.x = x;
			v.y = y;
			v.z = z;
		}
		inline float& operator [] (int i)
		{
			return ((float*)this)[i];
		}
		inline Vec4 & operator += (const Vec4 & vin)
		{
			x += vin.x; y += vin.y; z += vin.z; w += vin.w;
			return *this;
		}
		inline Vec4 & operator -= (const Vec4 & vin)
		{
			x -= vin.x; y -= vin.y; z -= vin.z; w -= vin.w;
			return *this;
		}
		inline Vec4 & operator *= (const Vec4 & vin)
		{
			x *= vin.x; y *= vin.y; z *= vin.z; w *= vin.w;
			return *this;
		}
		inline Vec4 & operator *= (float s)
		{
			x *= s; y *= s; z *= s; w *= s;
			return *this;
		}
		inline Vec4 & operator /= (const Vec4 & vin)
		{
			x /= vin.x; y /= vin.y; z /= vin.z; w /= vin.w;
			return *this;
		}
		inline Vec4 & operator /= (float s)
		{
			float inv = 1.0f/s;
			return (*this)*=inv;
		}
		inline bool operator == (const Vec4 & vin)
		{
			return fabs(vin.x - x) < fabs(x)*Epsilon && 
				fabs(vin.y - y) < fabs(y)*Epsilon &&
				fabs(vin.z - z) < fabs(z)*Epsilon;
		}
		inline bool operator != (const Vec4 & vin)
		{
			return fabs(vin.x - x) > fabs(x)*Epsilon ||
				fabs(vin.y - y) > fabs(y)*Epsilon ||
				fabs(vin.z - z) > fabs(z)*Epsilon;
		}
		inline int GetHashCode()
		{
			return FloatAsInt(x) ^ FloatAsInt(y) ^ FloatAsInt(z) ^ FloatAsInt(w);
		}
		static inline void Add(Vec4 & rs, const Vec4 & v1, const Vec4 & v2);
		static inline void Subtract(Vec4 & rs, const Vec4 & v1, const Vec4 & v2);
		static inline void Multiply(Vec4 & rs, const Vec4 & v1, const Vec4 & v2);
		static inline void MultiplyScale(Vec4 & rs, const Vec4 & v1, const Vec4 & v2);
		static inline void Scale(Vec4 & rs, const Vec4 & v1, float s);
		static inline float Dot(const Vec4 & v1, const Vec4 & v2);
		static inline void Cross(Vec4 & rs_d, const Vec4 & v1, const Vec4 & v2);
		inline float LengthFPU() const;
		inline float Length() const;
		static inline void NormalizeFPU(Vec4& vout, const Vec4& vin);
		static inline void Normalize(Vec4 &vout, const Vec4 &vin);
	};

	class Vec4_M128
	{
	public:
		__m128 vec;
		Vec4_M128()
		{}
		Vec4_M128(__m128 v)
		{
			vec = v;
		}
		Vec4_M128(float a, float b, float c, float d)
		{
			vec = _mm_set_ps(a, b, c, d);
		}
		Vec4_M128(const Vec4 & v)
		{
			vec = _mm_load_ps((const float*)&v);
		}
		inline void Zero()
		{
			vec = _mm_setzero_ps();
		}
		inline void ToVec4(Vec4 & v) const
		{
			_mm_store_ps((float*)&v, vec);
		}
	};

	class Matrix4
	{
	public:
		union
		{
			float values[16];
			float m[4][4];
			struct
			{
				float _11,_12,_13,_14,
				  _21,_22,_23,_24,
				  _31,_32,_33,_34,
				  _41,_42,_43,_44;
			} mi;
			struct
			{
				float _11,_12,_13,_14,
				  _21,_22,_23,_24,
				  _31,_32,_33,_34,
				  _41,_42,_43,_44;
			} mr;
		};
		Matrix4()
		{}
		Matrix4(float v)
		{
			for (int i = 0; i<16; i++)
				values[i] = v;
		}
		Matrix4(const Vec4 & c1, const Vec4 & c2, const Vec4 & c3, const Vec4 &c4)
		{
			memcpy(m[0], &c1, sizeof(Vec4));
			memcpy(m[1], &c2, sizeof(Vec4));
			memcpy(m[2], &c3, sizeof(Vec4));
			memcpy(m[3], &c4, sizeof(Vec4));
		}
		inline Matrix4 & operator *= (const Matrix4 & m)
		{
			Multiply(*this, *this, m);
			return *this;
		}
		inline Matrix4 & LeftMultiply(const Matrix4 & m)
		{
			Multiply(*this, m, *this);
			return *this;
		}
		static inline void CreateIdentityMatrix(Matrix4 & mOut);
		static inline void CreateRandomMatrix(Matrix4 & mOut);
		static void LookAt(Matrix4 & rs, const Vec3 & pos, const Vec3 & center, const Vec3 & up);
		static inline void RotationX(Matrix4 & rs, float angle);
		static inline void RotationY(Matrix4 & rs, float angle);
		static inline void RotationZ(Matrix4 & rs, float angle);
		static void Rotation(Matrix4 & rs, const Vec3 & axis, float angle);
		static void Rotation(Matrix4 & rs, float yaw, float pitch, float roll);
		static inline void Scale(Matrix4 & rs, float sx, float sy, float sz);
		static inline void Translation(Matrix4 & rs, float tx, float ty, float tz);
		inline void Transform(Vec3 & rs_d, const Vec3& vIn) const;
		inline void Transform(Vec4 & rs_d, const Vec4& vIn) const;
		inline void Matrix4::TransformNormal(Vec3 & rs, const Vec3& vIn) const;
		inline void TransposeTransformNormal(Vec3 & rs, const Vec3 & vIn) const;
		inline void TransposeTransform(Vec3 & rs, const Vec3 & vIn) const;
		inline void TransposeTransform(Vec4 & rs_d, const Vec4& vIn) const;
		static inline void MultiplyFPU(Matrix4 &mOut, const Matrix4& M1, const Matrix4& M2);
		static inline void Multiply(Matrix4 &mOut, const Matrix4& M1, const Matrix4& M2);
		float Inverse3D(Matrix4 & mOut_d) const;
		float InverseFPU(Matrix4 &mOut_d) const;
		inline float Inverse(Matrix4 &mOut_d) const;
	};

	__declspec(align(16))
	class Matrix4_M128
	{
	private:
		static const __m128 VecOne;
	public:
		__m128 C1,C2,C3,C4;
		Matrix4_M128()
		{}
		Matrix4_M128(const Matrix4 & m)
		{
			C1 = _mm_loadu_ps(m.values);
			C2 = _mm_loadu_ps(m.values+4);
			C3 = _mm_loadu_ps(m.values+8);
			C4 = _mm_loadu_ps(m.values+12);
		}
		inline void ToMatrix4(Matrix4 & mOut) const;
		inline void Transform(Vec4_M128 & rs, const Vec4& vIn) const;
		inline void Transform(Vec4 & rs, const Vec4& vIn) const;
		inline void Transform(Vec4_M128 & rs, const Vec3& vIn) const;
		inline void Transform(Vec3 & rs, const Vec3& vIn) const;
		inline void Transform(Vec4_M128 & rs, const Vec4_M128& vIn) const;
		inline void TransformNormal(Vec4_M128 & rs, const Vec4& vIn) const;
		inline void TransformNormal(Vec4 & rs, const Vec4& vIn) const;
		inline void TransformNormal(Vec4_M128 & rs, const Vec3& vIn) const;
		inline void TransformNormal(Vec3 & rs, const Vec3& vIn) const;
		inline void Multiply(Matrix4_M128 & rs, const Matrix4 & mB) const;
		inline void Multiply(Matrix4_M128 & rs, const Matrix4_M128 & mB) const;
		float Inverse(Matrix4_M128 &mOut) const;
	};

	//***********************************************************************
	/**************************** Implementation ****************************/
	//***********************************************************************
	inline int FloatAsInt(float val)
	{
		union InterCast
		{
			float fvalue;
			int ivalue;
		} cast;
		cast.fvalue = val;
		return cast.ivalue;
	}
	inline float IntAsFloat(int val)
	{
		union InterCast
		{
			float fvalue;
			int ivalue;
		} cast;
		cast.ivalue = val;
		return cast.fvalue;
	}
	// Vec3
	inline float Vec3::Length() const
	{
		return Vec4(*this).Length();
	}
	inline void Vec3::Normalize(Vec3 & rs, const Vec3 & vin)
	{
		Vec3::NormalizeFPU(rs, vin);
	}
	// Vec4
	inline void Vec4::Add(Vec4 & rs, const Vec4 & v1, const Vec4 & v2)
	{
		rs.x = v1.x + v2.x;
		rs.y = v1.y + v2.y;
		rs.z = v1.z + v2.z;
		rs.w = v1.w + v2.w;
	}
	inline void Vec4::Subtract(Vec4 & rs, const Vec4 & v1, const Vec4 & v2)
	{
		rs.x = v1.x - v2.x;
		rs.y = v1.y - v2.y;
		rs.z = v1.z - v2.z;
		rs.w = v1.w - v2.w;
	}
	inline void Vec4::Multiply(Vec4 & rs, const Vec4 & v1, const Vec4 & v2)
	{
		rs.x = v1.x * v2.x;
		rs.y = v1.y * v2.y;
		rs.z = v1.z * v2.z;
		rs.w = v1.w * v2.w;
	}
	inline void Vec4::MultiplyScale(Vec4 & rs, const Vec4 & v1, const Vec4 & v2)
	{
		rs.x = v1.x * v2.x;
		rs.y = v1.y * v2.y;
		rs.z = v1.z * v2.z;
		rs.w = v1.w * v2.w;
	}
	inline void Vec4::Scale(Vec4 & rs, const Vec4 & v1, float s)
	{
		rs.x = v1.x * s;
		rs.y = v1.y * s;
		rs.z = v1.z * s;
		rs.w = v1.w * s;
	}
	inline float Vec4::Dot(const Vec4 & v1, const Vec4 & v2)
	{
		return v1.x*v2.x + v1.y*v2.y + v1.z*v2.z + v1.w*v2.w;
	}
	inline void Vec4::Cross(Vec4 & rs_d, const Vec4 & v1, const Vec4 & v2)
	{
		rs_d.x = v1.y*v2.z - v1.z * v2.y;
		rs_d.y = v1.z*v2.x - v1.x * v2.z;
		rs_d.z = v1.x*v2.y - v1.y * v2.x;
		rs_d.w = 0.0f;
	}
	inline float Vec4::LengthFPU() const
	{
		return sqrt(Dot(*this, *this));
	}
	inline float Vec4::Length() const
	{
		float f;
		_asm
		{
			lea	ecx, f;
			mov	eax, this;

			movups	xmm0, [eax];
			mulps	xmm0, xmm0;
			movaps	xmm1, xmm0;
			shufps	xmm1, xmm1, 4Eh;
			addps	xmm0, xmm1;
			movaps	xmm1, xmm0;
			shufps	xmm1, xmm1, 11h;
			addss	xmm0, xmm1;

			sqrtss	xmm0, xmm0;
			movss	dword ptr [ecx], xmm0;
		}
		return f;
	}
	inline void Vec4::NormalizeFPU(Vec4& vout, const Vec4& vin)
	{
		float len = 1.0f/vin.Length();
		Scale(vout, vin, len);
	}
	/*inline void Vec4::Normalize(Vec4 &vout, const Vec4 &vin)
	{
		vout = vin;
		void * v_ptr = &vout;
		const void * v_ptr_in = &vin;
		_asm
		{
			movups	xmm0, [v_ptr_in];
			movaps	xmm2, xmm0;
			mulps	xmm0, xmm0;
			movaps	xmm1, xmm0;
			shufps	xmm1, xmm1, 4Eh;
			addps	xmm0, xmm1;
			movaps	xmm1, xmm0;
			shufps	xmm1, xmm1, 11h;
			addps	xmm0, xmm1;

			rsqrtps	xmm0, xmm0;
			mulps	xmm2, xmm0;
			movups	[v_ptr], xmm2;
		}
	}*/

	// Matrix4
	inline void Matrix4::CreateIdentityMatrix(Matrix4 & mOut)
	{
		memset(&mOut, 0, sizeof(Matrix4));
		mOut.m[0][0] = mOut.m[1][1] = mOut.m[2][2] = mOut.m[3][3] = 1.0f;
	}
	inline void Matrix4::CreateRandomMatrix(Matrix4 & mOut)
	{
		for (int i = 0; i<16; i++)
		{
			mOut.values[i] = rand()/(float)RAND_MAX;
		}
	}
	inline void Matrix4::RotationX(Matrix4 & rs, float angle)
	{
		float c = cosf(angle);
		float s = sinf(angle);

		Matrix4::CreateIdentityMatrix(rs);
		rs.m[1][1] = c;
		rs.m[2][1] = s;
		rs.m[1][2] = -s;
		rs.m[2][2] = c;
	}
	inline void Matrix4::RotationY(Matrix4 & rs, float angle)
	{
		float c = cosf(angle);
		float s = sinf(angle);

		Matrix4::CreateIdentityMatrix(rs);
		rs.m[0][0] = c;
		rs.m[2][0] = s;
		rs.m[0][2] = -s;
		rs.m[2][2] = c;
	}
	inline void Matrix4::RotationZ(Matrix4 & rs, float angle)
	{
		float c = cosf(angle);
		float s = sinf(angle);

		Matrix4::CreateIdentityMatrix(rs);
		rs.m[0][0] = c;
		rs.m[1][0] = s;
		rs.m[0][1] = -s;
		rs.m[1][1] = c;
	}

	inline void Matrix4::Scale(Matrix4 & rs, float sx, float sy, float sz)
	{
		Matrix4::CreateIdentityMatrix(rs);
		rs.m[0][0] = sx;
		rs.m[1][1] = sy;
		rs.m[2][2] = sz;
	}
	inline void Matrix4::Translation(Matrix4 & rs, float tx, float ty, float tz)
	{
		Matrix4::CreateIdentityMatrix(rs);
		rs.values[12] = tx;
		rs.values[13] = ty;
		rs.values[14] = tz;
	}
	inline void Matrix4::TransposeTransformNormal(Vec3 & rs, const Vec3 & vIn) const
	{
		rs.x = m[0][0]*vIn.x + m[0][1]*vIn.y + m[0][2]*vIn.z;
		rs.y = m[1][0]*vIn.x + m[1][1]*vIn.y + m[1][2]*vIn.z;
		rs.z = m[2][0]*vIn.x + m[2][1]*vIn.y + m[2][2]*vIn.z;
	}
	inline void Matrix4::TransposeTransform(Vec3 & rs, const Vec3 & vIn) const
	{
		rs.x = m[0][0]*vIn.x + m[0][1]*vIn.y + m[0][2]*vIn.z + m[0][3];
		rs.y = m[1][0]*vIn.x + m[1][1]*vIn.y + m[1][2]*vIn.z + m[1][3];
		rs.z = m[2][0]*vIn.x + m[2][1]*vIn.y + m[2][2]*vIn.z + m[2][3];
	}
	inline void Matrix4::TransposeTransform(Vec4 & rs, const Vec4 & vIn) const
	{
		rs.x = m[0][0]*vIn.x + m[0][1]*vIn.y + m[0][2]*vIn.z + m[0][3]*vIn.w;
		rs.y = m[1][0]*vIn.x + m[1][1]*vIn.y + m[1][2]*vIn.z + m[1][3]*vIn.w;
		rs.z = m[2][0]*vIn.x + m[2][1]*vIn.y + m[2][2]*vIn.z + m[2][3]*vIn.w;
		rs.w = m[3][0]*vIn.x + m[3][1]*vIn.y + m[3][2]*vIn.z + m[3][3]*vIn.w;
	}
	inline void Matrix4::Transform(Vec3 & rs, const Vec3& vIn) const
	{
		rs.x = m[0][0]*vIn.x + m[1][0]*vIn.y + m[2][0]*vIn.z + m[3][0];
		rs.y = m[0][1]*vIn.x + m[1][1]*vIn.y + m[2][1]*vIn.z + m[3][1];
		rs.z = m[0][2]*vIn.x + m[1][2]*vIn.y + m[2][2]*vIn.z + m[3][2];
	}
	inline void Matrix4::TransformNormal(Vec3 & rs, const Vec3& vIn) const
	{
		rs.x = m[0][0]*vIn.x + m[1][0]*vIn.y + m[2][0]*vIn.z;
		rs.y = m[0][1]*vIn.x + m[1][1]*vIn.y + m[2][1]*vIn.z;
		rs.z = m[0][2]*vIn.x + m[1][2]*vIn.y + m[2][2]*vIn.z;
	}
	inline void Matrix4::Transform(Vec4 & rs, const Vec4& vIn) const
	{
		rs.x = m[0][0]*vIn.x + m[1][0]*vIn.y + m[2][0]*vIn.z + m[3][0]*vIn.w;
		rs.y = m[0][1]*vIn.x + m[1][1]*vIn.y + m[2][1]*vIn.z + m[3][1]*vIn.w;
		rs.z = m[0][2]*vIn.x + m[1][2]*vIn.y + m[2][2]*vIn.z + m[3][2]*vIn.w;
		rs.w = m[0][3]*vIn.x + m[1][3]*vIn.y + m[2][3]*vIn.z + m[3][3]*vIn.w;
	}
	inline void Matrix4::MultiplyFPU(Matrix4 &mOut, const Matrix4& M1, const Matrix4& M2)
	{
		Matrix4 TempMat;
		for (int i=0;i<4;i++) //col
		{
			for (int j=0;j<4;j++) // row
			{
				TempMat.m[i][j] = M1.m[0][j]*M2.m[i][0] + M1.m[1][j]*M2.m[i][1] + M1.m[2][j]*M2.m[i][2] + M1.m[3][j]*M2.m[i][3];
			}
		}
		memcpy(&mOut,&TempMat,sizeof(Matrix4));
	}

	inline void Matrix4::Multiply(Matrix4 &mOut, const Matrix4 &M1, const Matrix4 &M2)
	{
		Matrix4 rs;
		Matrix4_M128 TempMat;
		Matrix4_M128 mA(M1);
		mA.Multiply(TempMat, M2);
		TempMat.ToMatrix4(rs);
		mOut = rs;
	}
	inline float Matrix4::Inverse(Matrix4 &mOut_d) const
	{
		Matrix4 mat;
		Matrix4_M128 m_m(*this);
		Matrix4_M128 mr;
		float rs = m_m.Inverse(mr);
		mr.ToMatrix4(mat);
		mOut_d = mat;
		return rs;
	}

	// Matrix4_M128

	inline void Matrix4_M128::ToMatrix4(Matrix4 & mOut) const
	{
		_mm_storeu_ps(mOut.values, C1);
		_mm_storeu_ps(mOut.values+4, C2);
		_mm_storeu_ps(mOut.values+8, C3);
		_mm_storeu_ps(mOut.values+12, C4);
	}
	inline void Matrix4_M128::Transform(Vec4_M128 & rs, const Vec4& vIn) const
	{
		__m128 r;
		r = _mm_mul_ps(C1, _mm_set_ps1(vIn.x));
		r = _mm_add_ps(r, _mm_mul_ps(C2, _mm_set_ps1(vIn.y)));
		r = _mm_add_ps(r, _mm_mul_ps(C3, _mm_set_ps1(vIn.z)));
		r = _mm_add_ps(r, _mm_mul_ps(C4, _mm_set_ps1(vIn.w)));
		rs.vec = r;
	}
	inline void Matrix4_M128::Transform(Vec4 & rs, const Vec4& vIn) const
	{
		Vec4_M128 r;
		Transform(r, vIn);
		_mm_store_ps((float*)&rs, r.vec);
	}
	inline void Matrix4_M128::Transform(Vec4_M128 & rs, const Vec3& vIn) const
	{
		__m128 r;
		r = _mm_mul_ps(C1, _mm_set_ps1(vIn.x));
		r = _mm_add_ps(r, _mm_mul_ps(C2, _mm_set_ps1(vIn.y)));
		r = _mm_add_ps(r, _mm_mul_ps(C3, _mm_set_ps1(vIn.z)));
		rs.vec = r;
	}
	inline void Matrix4_M128::Transform(Vec3 & rs, const Vec3& vIn) const
	{
		Vec4_M128 r;
		Transform(r, vIn);
		rs.x = r.vec.m128_f32[0];
		rs.y = r.vec.m128_f32[1];
		rs.z = r.vec.m128_f32[2];
	}
	inline void Matrix4_M128::Transform(Vec4_M128 & rs, const Vec4_M128& vIn) const
	{
		__m128 r;
		__m128 x,y,z,w;
		x = _mm_shuffle_ps(vIn.vec, vIn.vec, _MM_SHUFFLE(0, 0, 0, 0));
		r = _mm_mul_ps(C1, x);
		y = _mm_shuffle_ps(vIn.vec, vIn.vec, _MM_SHUFFLE(1, 1, 1, 1));
		r = _mm_add_ps(r, _mm_mul_ps(C2, y));
		z = _mm_shuffle_ps(vIn.vec, vIn.vec, _MM_SHUFFLE(2, 2, 2, 2));
		r = _mm_add_ps(r, _mm_mul_ps(C3, z));
		w = _mm_shuffle_ps(vIn.vec, vIn.vec, _MM_SHUFFLE(3, 3, 3, 3));
		r = _mm_add_ps(r, _mm_mul_ps(C4, w));
		rs.vec = r;
	}
	inline void Matrix4_M128::TransformNormal(Vec4_M128 & rs, const Vec4& vIn) const
	{
		__m128 r;
		r = _mm_mul_ps(C1, _mm_set_ps1(vIn.x));
		r = _mm_add_ps(r, _mm_mul_ps(C2, _mm_set_ps1(vIn.y)));
		r = _mm_add_ps(r, _mm_mul_ps(C3, _mm_set_ps1(vIn.z)));
		rs.vec = r;
	}
	inline void Matrix4_M128::TransformNormal(Vec4 & rs, const Vec4& vIn) const
	{
		Vec4_M128 r;
		TransformNormal(r, vIn);
		_mm_store_ps((float*)&rs, r.vec);
		rs.w = 0.0f;
	}
	inline void Matrix4_M128::TransformNormal(Vec4_M128 & rs, const Vec3& vIn) const
	{
		__m128 r;
		r = _mm_mul_ps(C1, _mm_set_ps1(vIn.x));
		r = _mm_add_ps(r, _mm_mul_ps(C2, _mm_set_ps1(vIn.y)));
		r = _mm_add_ps(r, _mm_mul_ps(C3, _mm_set_ps1(vIn.z)));
		rs.vec = r;
	}
	inline void Matrix4_M128::TransformNormal(Vec3 & rs, const Vec3& vIn) const
	{
		Vec4_M128 r;
		TransformNormal(r, vIn);
		rs.x = r.vec.m128_f32[0];
		rs.y = r.vec.m128_f32[1];
		rs.z = r.vec.m128_f32[2];
	}
	inline void Matrix4_M128::Multiply(Matrix4_M128 & rs, const Matrix4 & mB) const
	{
		register __m128 T0, T1, T2, T3, R0, R1, R2, R3;
		T0 = _mm_set_ps1(mB.values[0]);
		T1 = _mm_set_ps1(mB.values[1]);
		T2 = _mm_set_ps1(mB.values[2]);
		T3 = _mm_set_ps1(mB.values[3]);
		R0 = _mm_mul_ps(C1, T0);
		T0 = _mm_set_ps1(mB.values[4]);
		R1 = _mm_mul_ps(C2, T1);
		R1 = _mm_add_ps(R1, R0);
		R2 = _mm_mul_ps(C3, T2);
		T1 = _mm_set_ps1(mB.values[5]);
		R3 = _mm_mul_ps(C4, T3);
		R2 = _mm_add_ps(R2, R1);
		T2 = _mm_set_ps1(mB.values[6]);
		rs.C1 = _mm_add_ps(R3, R2);
		R0 = _mm_mul_ps(C1, T0);
		T3 = _mm_set_ps1(mB.values[7]);
		R1 = _mm_mul_ps(C2, T1);
		T0 = _mm_set_ps1(mB.values[8]);
		R2 = _mm_mul_ps(C3, T2);
		R1 = _mm_add_ps(R1, R0);
		T1 = _mm_set_ps1(mB.values[9]);
		R3 = _mm_mul_ps(C4, T3);
		R2 = _mm_add_ps(R2, R1);
		rs.C2 = _mm_add_ps(R3, R2);
		T2 = _mm_set_ps1(mB.values[10]);
		R0 = _mm_mul_ps(C1, T0);
		T3 = _mm_set_ps1(mB.values[11]);
		R1 = _mm_mul_ps(C2, T1);
		T0 = _mm_set_ps1(mB.values[12]);
		R2 = _mm_mul_ps(C3, T2);
		R1 = _mm_add_ps(R1, R0);
		T2 = _mm_set_ps1(mB.values[14]);
		R3 = _mm_mul_ps(C4, T3);
		R2 = _mm_add_ps(R2, R1);
		T1 = _mm_set_ps1(mB.values[13]);
		rs.C3 = _mm_add_ps(R3, R2);
		R0 = _mm_mul_ps(C1, T0);
		R1 = _mm_mul_ps(C2, T1);
		T3 = _mm_set_ps1(mB.values[15]);
		R2 = _mm_mul_ps(C3, T2);
		R1 = _mm_add_ps(R1, R0);
		R3 = _mm_mul_ps(C4, T3);
		R2 = _mm_add_ps(R2, R1);
		rs.C4 = _mm_add_ps(R3, R2);
	}
	inline void Matrix4_M128::Multiply(Matrix4_M128 & rs, const Matrix4_M128 & mB) const
	{
		register __m128 T0, T1, T2, T3, R0, R1, R2, R3;
		T0 = _mm_shuffle_ps(mB.C1, mB.C1, _MM_SHUFFLE(0,0,0,0));
		T1 = _mm_shuffle_ps(mB.C1, mB.C1, _MM_SHUFFLE(1,1,1,1));
		T2 = _mm_shuffle_ps(mB.C1, mB.C1, _MM_SHUFFLE(2,2,2,2));
		T3 = _mm_shuffle_ps(mB.C1, mB.C1, _MM_SHUFFLE(3,3,3,3));
		R0 = _mm_mul_ps(C1, T0);
		R1 = _mm_mul_ps(C2, T1);
		R2 = _mm_mul_ps(C3, T2);
		R3 = _mm_mul_ps(C4, T3);
		R1 = _mm_add_ps(R1, R0);
		R2 = _mm_add_ps(R2, R1);
		rs.C1 = _mm_add_ps(R3, R2);

		T0 = _mm_shuffle_ps(mB.C2, mB.C2, _MM_SHUFFLE(0,0,0,0));
		T1 = _mm_shuffle_ps(mB.C2, mB.C2, _MM_SHUFFLE(1,1,1,1));
		T2 = _mm_shuffle_ps(mB.C2, mB.C2, _MM_SHUFFLE(2,2,2,2));
		T3 = _mm_shuffle_ps(mB.C2, mB.C2, _MM_SHUFFLE(3,3,3,3));
		R0 = _mm_mul_ps(C1, T0);
		R1 = _mm_mul_ps(C2, T1);
		R2 = _mm_mul_ps(C3, T2);
		R3 = _mm_mul_ps(C4, T3);
		R1 = _mm_add_ps(R1, R0);
		R2 = _mm_add_ps(R2, R1);
		rs.C2 = _mm_add_ps(R3, R2);

		T0 = _mm_shuffle_ps(mB.C3, mB.C3, _MM_SHUFFLE(0,0,0,0));
		T1 = _mm_shuffle_ps(mB.C3, mB.C3, _MM_SHUFFLE(1,1,1,1));
		T2 = _mm_shuffle_ps(mB.C3, mB.C3, _MM_SHUFFLE(2,2,2,2));
		T3 = _mm_shuffle_ps(mB.C3, mB.C3, _MM_SHUFFLE(3,3,3,3));
		R0 = _mm_mul_ps(C1, T0);
		R1 = _mm_mul_ps(C2, T1);
		R2 = _mm_mul_ps(C3, T2);
		R3 = _mm_mul_ps(C4, T3);
		R1 = _mm_add_ps(R1, R0);
		R2 = _mm_add_ps(R2, R1);
		rs.C3 = _mm_add_ps(R3, R2);

		T0 = _mm_shuffle_ps(mB.C4, mB.C4, _MM_SHUFFLE(0,0,0,0));
		T1 = _mm_shuffle_ps(mB.C4, mB.C4, _MM_SHUFFLE(1,1,1,1));
		T2 = _mm_shuffle_ps(mB.C4, mB.C4, _MM_SHUFFLE(2,2,2,2));
		T3 = _mm_shuffle_ps(mB.C4, mB.C4, _MM_SHUFFLE(3,3,3,3));
		R0 = _mm_mul_ps(C1, T0);
		R1 = _mm_mul_ps(C2, T1);
		R2 = _mm_mul_ps(C3, T2);
		R3 = _mm_mul_ps(C4, T3);
		R1 = _mm_add_ps(R1, R0);
		R2 = _mm_add_ps(R2, R1);
		rs.C4 = _mm_add_ps(R3, R2);
	}
	static const __m128 VecOne;

	inline void CartesianToSphere(const Vec3 & dir, float & u, float & v)
	{
		const float inv2Pi = 0.5f/PI;
		v = acos(dir.y);
		u = atan2(dir.z, dir.x);
		if (u<0.0f)
			u += PI * 2.0f;
		u *= inv2Pi;
		v *= 1.0f/PI;
	}

	inline void SphereToCartesian(Vec3 & dir, float u, float v)
	{
		dir.y = cos(v);
		float s = sin(v);
		dir.x = cos(u) * s;
		dir.z = sin(u) * s;
	}

	inline void GetOrthoVec(Vec3 & vout, const Vec3 & vin)
	{
		Vec3 absV(abs(vin.x), abs(vin.y), abs(vin.z));
		if (absV.x <= absV.y && absV.x <= absV.z)
			Vec3::Cross(vout, vin, Vec3(1.0f, 0.0f, 0.0f));
		else if (absV.y <= absV.x && absV.y <= absV.z)
			Vec3::Cross(vout, vin, Vec3(0.0f, 1.0f, 0.0f));
		else
			Vec3::Cross(vout, vin, Vec3(0.0f, 0.0f, 1.0f));
	}

	inline unsigned int ones32(register unsigned int x)
	{
		/* 32-bit recursive reduction using SWAR...
		   but first step is mapping 2-bit values
		   into sum of 2 1-bit values in sneaky way
		*/
		x -= ((x >> 1) & 0x55555555);
		x = (((x >> 2) & 0x33333333) + (x & 0x33333333));
		x = (((x >> 4) + x) & 0x0f0f0f0f);
		x += (x >> 8);
		x += (x >> 16);
		return(x & 0x0000003f);
	}

	inline unsigned int Log2Floor(register unsigned int x)
	{
		x |= (x >> 1);
		x |= (x >> 2);
		x |= (x >> 4);
		x |= (x >> 8);
		x |= (x >> 16);
		return(ones32(x >> 1));
	}

	inline unsigned int Log2Ceil(register unsigned int x)
	{
		register int y = (x & (x - 1));
		y |= -y;
		y >>= (32 - 1);
		x |= (x >> 1);
		x |= (x >> 2);
		x |= (x >> 4);
		x |= (x >> 8);
		x |= (x >> 16);
		return(ones32(x >> 1) - y);
	}
}

#endif

namespace RenderGen
{
	using namespace CoreLib::Basic;
	using namespace VectorMath;

	class ImageRef
	{
	public:
		Vec4 * Pixels;
		int Width, Height;
	};

	class Image
	{
	public:
		List<Vec4> Pixels;
		int Width, Height;

		Image()
		{
			Width = Height = 0;
		}

		Image(int w, int h)
		{
			Pixels.SetSize(w * h);
			Width = w;
			Height = h;
		}

		ImageRef CreateRef()
		{
			ImageRef rs;
			rs.Pixels = Pixels.Buffer();
			rs.Width = Width;
			rs.Height = Height;
			return rs;
		}
		void Save(String fileName); // Save the image in pfm or bmp format
	};

	struct MeshFace
	{
		float PlaneU, PlaneV, PlaneD;
		short MaterialId;
		short ProjectionAxis;
		float K_beta_u, K_beta_v, K_beta_d, K_alpha_u;
		float K_gamma_u, K_gamma_v, K_gamma_d, K_alpha_v;
		union
		{
			struct
			{
				int Normal1, Normal2, Normal3;
			};
			int Normals[3];
		};
		union
		{
			struct
			{
				int TexCoord1, TexCoord2, TexCoord3;
			};
			int TexCoords[3];
		};
	};

	class Scene
	{
	public:
		float ZMin, ZMax;
		float FOV;
		List<MeshFace> Triangles;
		List<Vec2> TexCoords;
		List<Vec3> Normals;
	};

	class Ray
	{
	public:
		Vec3 Origin;
		Vec3 Direction;
		Vec3 ReciprocalDirection;
		float tMin, tMax;
	};

	typedef void (*RenderFunction)(ImageRef image, Scene * scene);

}

#endif
