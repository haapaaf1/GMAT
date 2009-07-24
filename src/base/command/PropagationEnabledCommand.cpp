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
#include "MessageInterface.hpp"

#define DEBUG_INITIALIZATION


PropagationEnabledCommand::PropagationEnabledCommand(const std::string &typeStr) :
   GmatCommand          (typeStr)
{
}


PropagationEnabledCommand::~PropagationEnabledCommand()
{
   for (std::vector<PropSetup*>::iterator i = propagators.begin();
         i != propagators.end(); ++i)
      if (*i)
         delete (*i);
}


PropagationEnabledCommand::PropagationEnabledCommand(const PropagationEnabledCommand& pec) :
   GmatCommand          (pec)
{
   for (std::vector<PropSetup*>::const_iterator i = pec.propagators.begin();
         i != pec.propagators.end(); ++i)
   {
      propagators.push_back((PropSetup*)(*i)->Clone());
   }
}


PropagationEnabledCommand& PropagationEnabledCommand::operator=(const PropagationEnabledCommand& pec)
{
   if (this == &pec)
   {
      // Copy over the PropSetups
      for (std::vector<PropSetup*>::iterator i = propagators.begin();
            i != propagators.end(); ++i)
      {
         if (*i)
            delete (*i);
      }
      propagators.clear();
      for (std::vector<PropSetup*>::const_iterator i = pec.propagators.begin();
            i != pec.propagators.end(); ++i)
      {
         propagators.push_back((PropSetup*)(*i)->Clone());
      }

      // Copy over the prop object lists

   }

   return *this;
}

bool PropagationEnabledCommand::Initialize()
{
   bool retval = false;

   if (GmatCommand::Initialize())
   {
      // Set the participant pointers
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("Found %d lists of prop object names\n",
               propObjectNames.size());
      #endif
      for (UnsignedInt i = 0; i < propObjectNames.size(); ++i)
      {
         PropObjectArray *objects;
         #ifdef DEBUG_INITIALIZATION
            MessageInterface::ShowMessage("List %d contains %d prop objects\n",
                  i+1, propObjectNames[i].size());
         #endif

         if (propObjects.size() > i)
         {
            objects = propObjects[i];
            objects->clear();
         }
         else
         {
            objects = new PropObjectArray;
            propObjects.push_back(objects);
         }

         GmatBase *thisObject;

         for (StringArray::iterator j = propObjectNames[i].begin();
               j != propObjectNames[i].end(); ++j)
         {
            if (objectMap->find(*j) != objectMap->end())
            {
               thisObject = (*objectMap)[*j];
            }
            else if (globalObjectMap->find(*j) != objectMap->end())
            {
               thisObject = (*globalObjectMap)[*j];
            }
            else
               throw CommandException("Cannot initialize RunSimulator command "
                     "-- the space object named " + (*j) + " cannot be found.");

            // Only put propagatable objects -- SpaceObjects -- in the list
            if (thisObject != NULL)
            {
               if (thisObject->IsOfType(Gmat::SPACEOBJECT))
               {
                  objects->push_back((SpaceObject*)thisObject);
                  #ifdef DEBUG_INITIALIZATION
                     MessageInterface::ShowMessage("   Added the space object "
                           "named %s\n", thisObject->GetName().c_str());
                  #endif
               }
               #ifdef DEBUG_INITIALIZATION
               else
                  MessageInterface::ShowMessage("   Found %s, not a space "
                        "object\n", thisObject->GetName().c_str());
               #endif
            }
         }
      }

      // Now we have everything we need to init the prop subsystem
      retval = AssemblePropagators();

      if (retval == true)
      {
         retval = false;
         MessageInterface::ShowMessage("PEC Initialize() succeeded, but reporting "
               "failure for now\n");
      }
      else
         MessageInterface::ShowMessage(
               "PEC Initialize() failed to initialize the PropSetups\n");

   }

   return retval;
}

//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------
bool PropagationEnabledCommand::PrepareToPropagate()
{
   bool retval = false;

   return retval;
}


bool PropagationEnabledCommand::AssemblePropagators()
{
   bool retval = true;

   PropObjectArray *currentObjects;
   PropagationStateManager *psm;
   for (UnsignedInt i = 0; i < propagators.size(); ++i)
   {
      // For each PropSetup, set the SpaceObjects
      currentObjects = propObjects[i];
      psm = propagators[i]->GetPropStateManager();
      for (UnsignedInt j = 0; j < currentObjects->size(); ++j)
      {
         #ifdef DEBUG_INITIALIZATION
            MessageInterface::ShowMessage(
                  "Adding SpaceObject %s to PropSetup %s\n",
                  (*currentObjects)[j]->GetName().c_str(),
                  propagators[i]->GetName().c_str());
         #endif

         psm->SetObject((SpaceObject*)((*currentObjects)[j]));
      }

      // Now initialize the current PropSetup
      if (propagators[i]->Initialize() == false)
         return false;
   }

   return retval;
}

bool PropagationEnabledCommand::Step(Real dt)
{
   bool retval = true;

//   ODEModel* fm = thePropagator->GetODEModel();
//   Real baseEpoch = 21545.0;
//   if (dt != 0.0)
//   {
//      retval = thePropagator->GetPropagator()->Step(dt);
//      fm->UpdateSpaceObject(dt/GmatTimeUtil::SECS_PER_DAY);
//
//      // orbit related parameters use spacecraft for data
//      Real elapsedTime = fm->GetTime();
//      Real currEpoch = baseEpoch + elapsedTime /
//            GmatTimeUtil::SECS_PER_DAY;
//
//      // Update spacecraft epoch, without argument the spacecraft epoch
//      // won't get updated for consecutive Propagate command
//      fm->UpdateSpaceObject(currEpoch);
//      baseEpoch = currEpoch;
//   }

   return retval;
}
