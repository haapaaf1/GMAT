//$Id: GmatPluginFunctions.hpp 9490 2011-04-27 16:28:40Z tdnguye2 $
//------------------------------------------------------------------------------
//                            GmatPluginFunctions
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Tuan Nguyen (NASA/GSFC)
// Created: 2011/06/14
//
/**
 * Definition for library code interfaces.
 * 
 * This is prototype code.
 */
//------------------------------------------------------------------------------

#ifndef GmatPluginFunctions_hpp
#define GmatPluginFunctions_hpp

#include "marsgram_defs.hpp"
#include "Factory.hpp"

class MessageReceiver;

extern "C"
{
   Integer    MARSGRAM_API GetFactoryCount();
   Factory    MARSGRAM_API *GetFactoryPointer(Integer index);
   void       MARSGRAM_API SetMessageReceiver(MessageReceiver* mr);
};


#endif /*GmatPluginFunctions_hpp*/
