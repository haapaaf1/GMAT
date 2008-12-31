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
 * Definition of the PropagationStateManager base class.  This is the class for 
 * state managers used in GMAT's propagators and solvers.
 */
//------------------------------------------------------------------------------

#ifndef PropagationStateManager_hpp
#define PropagationStateManager_hpp

#include "StateManager.hpp"

/**
 * The state manager used in the propagation subsystem.
 */
class PropagationStateManager : public StateManager
{
public:
	PropagationStateManager(Integer size = 0);
	virtual ~PropagationStateManager();
	PropagationStateManager(const PropagationStateManager& psm);
	PropagationStateManager& operator=(const PropagationStateManager& psm);
	
	virtual bool SetObject(GmatBase* theObject);
	virtual bool SetProperty(std::string propName);
	
	
};

#endif /*PropagationStateManager_hpp*/
