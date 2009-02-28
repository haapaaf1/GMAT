//$Id$
//------------------------------------------------------------------------------
//                                  StateManager
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
 * Imnplementation of the StateManager base class.  This is the class for state 
 * managers used in GMAT's propagators and solvers.
 */
//------------------------------------------------------------------------------

#include "StateManager.hpp"

StateManager::StateManager(Integer size) :
   stateSize      (size),
   state          (size),
   current        (NULL)
{
}

StateManager::~StateManager()
{
}

StateManager::StateManager(const StateManager& sm) :
   stateSize   (sm.stateSize),
   state       (sm.state),
   current     (NULL)
{
}

StateManager& StateManager::operator=(const StateManager& sm)
{
   if (this != &sm)
   {
      stateSize = sm.stateSize;
      state = sm.state;
      
      // For now, copies start empty.  This may change later.
      objects.clear();
      epochIDs.clear();
      elements.clear();
      current = NULL;
   }
   
   return *this;
}


bool StateManager::UpdateState() 
{
   return true;
}


GmatState* StateManager::GetState()
{
   return &state;
}


ObjectArray* StateManager::GetStateObjects()
{
   return &objects;
}
