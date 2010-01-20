//$Id$
//------------------------------------------------------------------------------
//                             MatlabInterface
//------------------------------------------------------------------------------
// Purpose:
//    declares MatlabInterface class.
//
// Modifications:
//    2002.11.04  L. Jun - Created.
//    2008.10.15  L. Jun - Made a singleton class
/*
 * Declares MatlabInterface functions. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#ifndef h_MatlabInterface_h
#define h_MatlabInterface_h

#if defined __USE_MATLAB__
#include "engine.h"      // for Matlab Engine
#endif

#include <string>
#include <map>

class MatlabInterface
{

public:

   static MatlabInterface* Instance();
   
   int   Open(const std::string &engineName = "");
   int   Close(const std::string &engineName = "");
   
   std::string GetLastEngineName(); // Call this one after Open()
   
   int   PutRealArray(const std::string &matlabVarName, int numRows, int numCols,
                      const double *inArray);
   int   GetRealArray(const std::string &matlabVarName, int numElements,
                      double outArray[]);
   int   GetString(const std::string &matlabVarName, std::string &outStr);
   int   EvalString(const std::string &evalString);
   int   SetOutputBuffer(int size);
   char* GetOutputBuffer();
   bool  IsOpen(const std::string &engineName = "");
   void  RunMatlabString(std::string evalString); 
   
private:
   
   MatlabInterface();
   ~MatlabInterface();
   
#if defined __USE_MATLAB__
   static MatlabInterface *instance;
   static const int MAX_OUT_SIZE;
   Engine *enginePtr;
   std::map<std::string, Engine*> matlabEngineMap;
   std::string lastEngineName;
   int accessCount;
   char *outBuffer;
#endif
   
};

#endif // h_MatlabInterface_h
