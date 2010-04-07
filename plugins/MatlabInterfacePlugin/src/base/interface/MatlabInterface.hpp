//$Id$
//------------------------------------------------------------------------------
//                             MatlabInterface
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Linda Jun/GSFC
// Created: 2002/11/04
//
// Modifications:
//    2008.10.15  L. Jun - Made a singleton class
/*
 * Declares MatlabInterface functions. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#ifndef MatlabInterface_hpp
#define MatlabInterface_hpp

#include "Interface.hpp"

#ifdef __USE_MATLAB__
#include "engine.h"           // for Matlab Engine
#endif

#include <string>
#include <map>

class GMAT_API MatlabInterface : public Interface
{

public:

   ///@note GmatGlobal uses the same enum
   enum MatlabMode
   {
      SINGLE_USE = 20,
      SHARED,
      NO_MATLAB,  // MATLAB is not installed
   };
   
   static MatlabInterface* Instance();
   
   Integer       Open(const std::string &name = "");
   Integer       Close(const std::string &name = "");
   
   Integer       PutRealArray(const std::string &matlabVarName, Integer numRows,
                              Integer numCols, const double *inArray);
   Integer       GetRealArray(const std::string &matlabVarName, Integer numElements,
                              double outArray[]);
   Integer       GetString(const std::string &matlabVarName, std::string &outStr);
   Integer       EvalString(const std::string &evalString);
   Integer       SetOutputBuffer(Integer size);
   char*         GetOutputBuffer();
   bool          IsOpen(const std::string &engineName = "");
   void          RunMatlabString(std::string evalString); 
   void          SetMatlabMode(Integer mode);
   Integer       GetMatlabMode();
   
   
private:
   
   // inherited from GmatBase
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* orig);
   
   MatlabInterface(const std::string &name);
   virtual ~MatlabInterface();
   MatlabInterface(const MatlabInterface &mi);
   MatlabInterface& operator=(const MatlabInterface& mi);
   
#ifdef __USE_MATLAB__
   static MatlabInterface *instance;
   static const Integer MAX_OUT_SIZE;
   Engine *enginePtr;   
   std::map<std::string, Engine*> matlabEngineMap;
   std::string lastEngineName;
   std::string message;
   Integer accessCount;
   Integer matlabMode;
   char *outBuffer;
   
   Integer OpenEngineOnMac();
   Integer CloseEngineOnMac();
   Integer OpenSharedEngine();
   Integer CloseSharedEngine();
   Integer OpenSingleEngine(const std::string &engineName);
   Integer CloseSingleEngine(const std::string &engineName);
#endif
};

#endif // MatlabInterface_hpp
