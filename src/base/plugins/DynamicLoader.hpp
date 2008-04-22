#ifndef DYNAMICLOADER_HPP_
#define DYNAMICLOADER_HPP_

#include "gmatdefs.hpp"
#include <vector>
#include "DynamicLibrary.hpp"

class DynamicLoader
{
public:
   DynamicLoader        *Instance();
   
   
private:
   DynamicLoader();
   virtual ~DynamicLoader();
   
   static DynamicLoader                     *theLoader;
   static std::vector<DynamicLibrary *>     openedLibraries;
};

#endif /*DYNAMICLOADER_HPP_*/
