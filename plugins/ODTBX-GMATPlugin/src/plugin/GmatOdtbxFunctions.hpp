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
   Integer          GMATODTBX_API GetFactoryCount();
   Factory          GMATODTBX_API *GetFactoryPointer(Integer index);
   void             GMATODTBX_API SetMessageReceiver(MessageReceiver* mr);
};


#endif /*GMATPLUGINFUNCTIONS_H_*/
