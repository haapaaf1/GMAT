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
// Created: 2011/05/17
//
/**
 * Functions called on the ODTBX side of the interface
 *
 * This file is parsed by MATLAB to define the functions that are accessed using 
 * MATLAB's loadlibrary/calllib functions. These are all pure C functions; the 
 * file is loaded by GmatOdtbxFunctions.hpp when building the interface library.
 */
//------------------------------------------------------------------------------

#ifndef MatlabFunctions_hpp
#define MatlabFunctions_hpp

#include "GmatOdtbx_defs.hpp"

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

#endif /*GMATPLUGINFUNCTIONS_H_*/
