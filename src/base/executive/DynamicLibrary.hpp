#ifndef DYNAMICLIBRARY_HPP_
#define DYNAMICLIBRARY_HPP_


#include "gmatdefs.hpp"


class DynamicLibrary
{
public:
   DynamicLibrary(const std::string &name, const std::string &path = "./");
   virtual ~DynamicLibrary();
   DynamicLibrary(const DynamicLibrary& dlib);
   DynamicLibrary& operator=(const DynamicLibrary& dlib);

   bool                 LoadDynamicLibrary();
   void *               GetFunction(std::string &funName);
   
protected:
   std::string          libName;
   std::string          libPath;
   void *               libHandle;
};

#endif /*DYNAMICLIBRARY_HPP_*/
