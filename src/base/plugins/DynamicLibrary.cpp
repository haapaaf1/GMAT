//$Id$
//------------------------------------------------------------------------------
//                              DynamicLibrary
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/04/18
//
/**
 * Implementation for library code loaded at run time.
 * 
 * This is prototype code.
 */
//------------------------------------------------------------------------------


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

void (*DynamicLibrary::GetFunction(const std::string &funName))()
{
   if (libHandle == NULL)
      throw GmatBaseException("Library " + libName + 
            " has not been opened successfully; cannot search for function \"" + 
            funName + "\"\n");
   
   void (*func)() = NULL;
   
   #ifdef __WIN32__
      func = (void(*)())GetProcAddress((HINSTANCE)libHandle, funName.c_str());
   #else
      func = (void(*)())dlsym(libHandle, funName.c_str());
   #endif
      
   if (func == NULL)
      throw GmatBaseException("Library " + libName + 
            " cannot locate the function \"" + 
            funName + "\"\n");
   
   return func;
}


Integer DynamicLibrary::GetFactoryCount()
{
   if (libHandle == NULL)
      throw GmatBaseException("Library " + libName + " has not been opened "
            "successfully; cannot search for factories \n");
   
   Integer (*FactoryCount)() = NULL;

   try
   {
      FactoryCount = (Integer(*)())GetFunction("FactoryCount");
   }
   catch (GmatBaseException& ex)
   {
      return 0;
   }
   
   return FactoryCount();   
}


Factory* DynamicLibrary::GetGmatFactory(Integer index)
{
   if (libHandle == NULL)
      throw GmatBaseException("Library " + libName + " has not been opened "
            "successfully; cannot search for factories \n");
   
   bool    (*GetFactory)(Integer, Factory*) = NULL;
   Factory *theFactory = NULL;

   GetFactory = (bool(*)(Integer, Factory*))GetFunction("GetFactoryPointer");
   if (GetFactory(index, theFactory) == false)
      MessageInterface::ShowMessage(
            "Cannot access factory #%d in the \"%s\" library\n", index, 
            libName.c_str());
      
   return theFactory;
}
