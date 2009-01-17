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
 * Definition of the StateManager base class.  This is the class for state 
 * managers used in GMAT's propagators and solvers.
 */
//------------------------------------------------------------------------------

#ifndef StateManager_hpp
#define StateManager_hpp

#include "GmatState.hpp"
#include "StateVectorIds.hpp"
#include <map>

/**
 * The state manager base class.
 */
class StateManager
{
public:
	StateManager(Integer size = 0);
	virtual ~StateManager();
   StateManager(const StateManager& sm);
   StateManager& operator=(const StateManager& sm);
   
   // Abstract methods
   virtual bool SetObject(GmatBase* theObject) = 0;
   virtual bool SetProperty(std::string propName) = 0;
   virtual bool BuildState() = 0;
   virtual bool MapObjectsToVector() = 0;
   virtual bool MapVectorToObjects() = 0;

   virtual bool UpdateState();
   virtual GmatState* GetState();
   
protected:
   /// Size of the managed state vector
   Integer                    stateSize;
   GmatState                  state;
   
   std::vector<GmatBase*>     objects;
   std::vector<Integer>       epochIDs;
   std::map<GmatBase*, StringArray*>  elements;
   GmatBase*                  current;
};

#endif /*StateManager_hpp*/
