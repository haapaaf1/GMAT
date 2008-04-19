#include "DynamicLoader.hpp"


DynamicLoader *DynamicLoader::theLoader = NULL;


DynamicLoader* DynamicLoader::Instance()
{
   if (theLoader == NULL)
      theLoader = new DynamicLoader();
   return theLoader;
}


DynamicLoader::DynamicLoader()
{
}


DynamicLoader::~DynamicLoader()
{
}
