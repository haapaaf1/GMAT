//$Id$
//------------------------------------------------------------------------------
//                            GmatOdtbxFunctions
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
// ODTBX: Orbit Determination Toolbox
//
// **Legal**
//
// Developed jointly by NASA/GSFC, Emergent Space Technologies, Inc.
// and Thinking Systems, Inc. under the FDSS contract, Task 28
//
// Author: Darrel J. Conway (Thinking Systems)
// Created: 2011/03/22
//
/**
 * Definition for library code interfaces needed by GMAT.
 */
//------------------------------------------------------------------------------

#ifndef GmatPluginFunctions_hpp
#define GmatPluginFunctions_hpp

#include "GmatOdtbx_defs.hpp"
#include "Factory.hpp"

class MessageReceiver;

extern "C"
{
   // Interfaces used by GMAT
   Integer          GMATODTBX_API GetFactoryCount();
   Factory          GMATODTBX_API *GetFactoryPointer(Integer index);
   void             GMATODTBX_API SetMessageReceiver(MessageReceiver* mr);

   // Interfaces used by ODTBX
   const char GMATODTBX_API *getLastMessage();
   int GMATODTBX_API StartGmat();

   int GMATODTBX_API LoadScript(const char* scriptName);
   int GMATODTBX_API RunScript();
   int GMATODTBX_API LoadAndRunScript(const char* scriptName);

   int GMATODTBX_API FindOdeModel(const char* modelName);
   int GMATODTBX_API GetStateSize();
   const char GMATODTBX_API *GetStateDescription();
   int GMATODTBX_API SetState(double epoch, double state[], int stateDim);
   double GMATODTBX_API *GetState();

   double GMATODTBX_API *GetDerivativesForState(double epoch, double state[], 
         int stateDim, double dt, int order, int *pdim);
   double GMATODTBX_API *GetDerivatives(double dt, int order, int *pdim);

   int GMATODTBX_API CountObjects();
   const char GMATODTBX_API *GetObjectName(int which);
   const char GMATODTBX_API *GetRunSummary();


   // Internal helper functions
   ODEModel *GetODEModel(GmatCommand *cmd, std::string modelName = "");
   PropSetup *GetFirstPropagator(GmatCommand *cmd);
};


#endif /*GMATPLUGINFUNCTIONS_H_*/
