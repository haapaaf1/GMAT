//$Id$
//------------------------------------------------------------------------------
//                            GmatPluginFunctions
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Linda Jun (NASA/GSFC)
// Created: 2010/03/30
//
/**
 * Definition for library code interfaces.
 * 
 * This is prototype code.
 */
//------------------------------------------------------------------------------
#ifndef GmatPluginFunctions_hpp
#define GmatPluginFunctions_hpp

#include "matlabinterface_defs.hpp"
#include "Factory.hpp"

class MessageReceiver;

extern "C"
{
   Integer    MATLAB_API GetFactoryCount();
   Factory    MATLAB_API *GetFactoryPointer(Integer index);
   void       MATLAB_API SetMessageReceiver(MessageReceiver* mr);
};


#endif /*GmatPluginFunctions_hpp*/
