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
#include "GmatBase.hpp"

#include "MessageInterface.hpp"

PropagationStateManager::PropagationStateManager(Integer size) :
   StateManager         (size)
{
}

PropagationStateManager::~PropagationStateManager()
{
//   for (StringArray::iterator i = elements.begin(); i != elements.end(); ++i)
//      delete (i.second());
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


bool PropagationStateManager::SetObject(GmatBase* theObject)
{
   MessageInterface::ShowMessage("Setting object %s\n", 
         theObject->GetName().c_str());

   // Be sure object is not already in the list 
   if (find(objects.begin(), objects.end(), theObject) != objects.end())
      return false;     // Could throw here, but that would stop everything

   // todo: validate that theObject can be propagated 

   objects.push_back(theObject);
   current = theObject;
   StringArray *objectProps = new StringArray;
   elements[current] = objectProps;
   *objectProps = current->GetDefaultPropItems();
   
      MessageInterface::ShowMessage("Object set; current points to %s\n", 
         current->GetName().c_str());
   
   return true;
}


bool PropagationStateManager::SetProperty(std::string propName)
{
   MessageInterface::ShowMessage("Entered SetProperty(%s); current = %ld\n",
         propName.c_str(), current);
   if (current) 
   {
      elements[current]->push_back(propName);
      
         MessageInterface::ShowMessage("Current property List:\n");
         for (StringArray::iterator i = elements[current]->begin(); 
              i != elements[current]->end(); ++i)
            MessageInterface::ShowMessage("   %s\n", i->c_str());

      return true;
   }
   return false;   
}
