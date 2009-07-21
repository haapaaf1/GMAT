//$Id$
//------------------------------------------------------------------------------
//                         ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#include "PropagationEnabledCommand.hpp"


PropagationEnabledCommand::PropagationEnabledCommand(const std::string &typeStr) :
   GmatCommand          (typeStr)
{
}


PropagationEnabledCommand::~PropagationEnabledCommand()
{
}


PropagationEnabledCommand::PropagationEnabledCommand(const PropagationEnabledCommand& pec) :
   GmatCommand          (pec)
{

}


PropagationEnabledCommand& PropagationEnabledCommand::operator=(const PropagationEnabledCommand& pec)
{
   if (this == &pec)
   {


   }

   return *this;
}

