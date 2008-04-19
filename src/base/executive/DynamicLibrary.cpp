#include "DynamicLibrary.hpp"

#include "GmatBaseException.hpp"

// Platform specific library loader routines
#ifdef __WIN32__
#include <windows.h>
#else
#include <dlfcn.h>

/// @todo Put in the Mac/Linux switching logic for dynamic library extensions
// Linux version:
#define UNIX_EXTENSION ".so"

// Mac Version:
//#define UNIX_EXTENSION ".dylib"

#endif


DynamicLibrary::DynamicLibrary(const std::string &name, const std::string &path) :
   libName          (name),
   libPath          (path),
   libHandle        (NULL)
{
}


DynamicLibrary::~DynamicLibrary()
{
   if (libHandle != NULL)
      #ifdef __WIN32__
         FreeLibrary((HINSTANCE)libHandle);
      #else
         dlclose(libHandle);
      #endif
}


DynamicLibrary::DynamicLibrary(const DynamicLibrary& dlib)
{
}

DynamicLibrary& DynamicLibrary::operator=(const DynamicLibrary& dlib)
{
   if (&dlib != this)
   {
      
   }
   
   return *this;
}

bool DynamicLibrary::LoadDynamicLibrary()
{
   std::string nameWithPath;
   
   nameWithPath = libPath + libName;

   #ifdef __WIN32__
      libHandle = LoadLibrary(libName.c_str());
   #else
      nameWithPath += UNIX_EXTENSION;
      libHandle = dlopen(nameWithPath.c_str(), RTLD_LAZY);
   #endif
      
   if (libHandle == NULL)
      return false;
   
   return true;
}

void *DynamicLibrary::GetFunction(std::string &funName)
{
   if (libHandle == NULL)
      throw GmatBaseException("Library " + libName + 
            " has not been opened successfully; cannot search for function \"" + 
            funName + "\"\n");
   
   void *func = NULL;
   
   #ifdef __WIN32__
      func = (void*) GetProcAddress((HINSTANCE)libHandle, funName.c_str());
   #else
      func = dlsym(libHandle, funName.c_str());
   #endif
      
   if (func == NULL)
      throw GmatBaseException("Library " + libName + 
            " cannot locate the function \"" + 
            funName + "\"\n");
   
   return func;
}
