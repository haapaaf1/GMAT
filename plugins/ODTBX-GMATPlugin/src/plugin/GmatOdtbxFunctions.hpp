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

#include "gmatdefs.hpp"
#include "GmatOdtbx_defs.hpp"

class Factory;
class ODEModel;
class PropSetup;
class MessageReceiver;

extern "C"
{
   // If this library grows to include add-on code for GMAT, the GMAT plugin
   // interface needs these functions:
   Integer GetFactoryCount();
   Factory* GetFactoryPointer(Integer index);
   void SetMessageReceiver(MessageReceiver* mr);

   // Load the function defs that are visible to ODTBX.  These are split into a 
   // separate file to simplify the generation of the MATLAB m-file defining 
   // the interface
   #include "MatlabFunctions.hpp"

   // Internal helper functions
   void GetODEModel(GmatCommand *cmd, std::string modelName = "");
   PropSetup *GetFirstPropagator(GmatCommand *cmd);
};

#endif /*GmatPluginFunctions_hpp*/
