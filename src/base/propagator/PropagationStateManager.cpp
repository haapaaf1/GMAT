//$Id$
//------------------------------------------------------------------------------
//                           PropagationStateManager
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/12/15
//
/**
 * Implementation of the PropagationStateManager base class.  This is the class
 * for state managers used in GMAT's propagation subsystem.
 */
//------------------------------------------------------------------------------

#include "PropagationStateManager.hpp"

PropagationStateManager::PropagationStateManager(Integer size) :
   StateManager         (size)
{
}

PropagationStateManager::~PropagationStateManager()
{
}

PropagationStateManager::
         PropagationStateManager(const PropagationStateManager& psm) :
   StateManager         (psm)
{
}

PropagationStateManager& 
         PropagationStateManager::operator=(const PropagationStateManager& psm)
{
   if (this != &psm)
   {
      StateManager::operator=(psm);
   }
   
   return *this;
}
