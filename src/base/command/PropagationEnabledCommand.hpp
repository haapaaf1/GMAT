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


#ifndef PropagationEnabledCommand_hpp
#define PropagationEnabledCommand_hpp

#include "GmatCommand.hpp"
#include "PropSetup.hpp"


class PropagationEnabledCommand : public GmatCommand
{
public:
   PropagationEnabledCommand(const std::string &typeStr);
   virtual ~PropagationEnabledCommand();
   PropagationEnabledCommand(const PropagationEnabledCommand& pec);
   PropagationEnabledCommand& operator=(const PropagationEnabledCommand& pec);

protected:
   /// The PropSetup used by this command
   PropSetup      *thePropagator;

};

#endif /* PropagationEnabledCommand_hpp */
