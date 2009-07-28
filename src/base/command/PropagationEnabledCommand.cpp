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

#include "ODEModel.hpp"

#define DEBUG_INITIALIZATION
//#define DEBUG_EXECUTION


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

      #ifdef DEBUG_INITIALIZATION
         if (retval == true)
            MessageInterface::ShowMessage("PEC Initialize() succeeded\n");
         else
            MessageInterface::ShowMessage(
                  "PEC Initialize() failed to initialize the PropSetups\n");
      #endif

   }

   return retval;
}

//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------
bool PropagationEnabledCommand::PrepareToPropagate()
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage(
            "PropagationEnabledCommand::PrepareToPropagate() entered\n");
   #endif
   bool retval = false;

   dim = 0;

   if (hasFired == true)
   {
      // Handle the transient forces
      for (std::vector<PropObjectArray*>::iterator poa = propObjects.begin();
           poa != propObjects.end(); ++poa)
      {
         for (PropObjectArray::iterator sc = (*poa)->begin();
               sc != (*poa)->end(); ++sc)
         {
            if (((SpaceObject*)(*sc))->IsManeuvering())
            {
               #ifdef DEBUG_FINITE_MANEUVER
                  MessageInterface::ShowMessage(
                     "SpaceObject %s is maneuvering\n", (*sc)->GetName().c_str());
               #endif

// todo: Transient forces here
//               // Add the force
//               for (UnsignedInt index = 0; index < propagators.size(); ++index)
//               {
//                  for (std::vector<PhysicalModel*>::iterator i = transientForces->begin();
//                       i != transientForces->end(); ++i)
//                  {
//                     #ifdef DEBUG_TRANSIENT_FORCES
//                     MessageInterface::ShowMessage
//                        ("Propagate::PrepareToPropagate() Adding transientForce<%p>'%s'\n",
//                         *i, (*i)->GetName().c_str());
//                     #endif
//                     prop[index]->GetODEModel()->AddForce(*i);
//
//                     // todo: Rebuild ODEModel by calling BuildModelFromMap()
//                  }
//               }
            }
         }
      }

      for (Integer n = 0; n < (Integer)propagators.size(); ++n)
      {
         elapsedTime[n] = 0.0;
         currEpoch[n]   = 0.0;
         fm[n]->SetTime(0.0);
         fm[n]->SetPropStateManager(propagators[n]->GetPropStateManager());
         fm[n]->UpdateInitialData();
         dim += fm[n]->GetDimension();

         p[n]->Initialize();
         p[n]->Update(true /*direction > 0.0*/);
//         state = fm[n]->GetState();
         j2kState = fm[n]->GetJ2KState();
      }

      baseEpoch.clear();

      for (Integer n = 0; n < (Integer)propagators.size(); ++n)
      {
         if (propObjectNames[n].empty())
            throw CommandException(
               "Propagator has no associated space objects.");

         GmatBase* sat1 = FindObject(*(propObjectNames[n].begin()));
         baseEpoch.push_back(sat1->GetRealParameter(epochID));
         elapsedTime[n] = fm[n]->GetTime();
         currEpoch[n] = baseEpoch[n] + elapsedTime[n] /
            GmatTimeUtil::SECS_PER_DAY;
      }

      inProgress = true;
   }
   else
   {
      // Set the prop state managers for the PropSetup ODEModels
      for (std::vector<PropSetup*>::iterator i=propagators.begin(); i != propagators.end(); ++i)
      {
         ODEModel *ode = (*i)->GetODEModel();
         if (ode != NULL)    // Only do this for the PropSetups that integrate
            ode->SetPropStateManager((*i)->GetPropStateManager());
      }

      // Initialize the subsystem
      Initialize();

      // Loop through the PropSetups and build the models
      for (std::vector<PropSetup*>::iterator i=propagators.begin(); i != propagators.end(); ++i)
      {
         ODEModel *ode = (*i)->GetODEModel();
         if (ode != NULL)    // Only do this for the PropSetups that integrate
         {
            // Build the ODE model
            ode->SetPropStateManager((*i)->GetPropStateManager());
            if (ode->BuildModelFromMap() == false)
               throw CommandException("Unable to assemble the ODE model for " +
                     (*i)->GetName());
         }
      }

      p.clear();
      fm.clear();
      psm.clear();
      baseEpoch.clear();

      for (Integer n = 0; n < (Integer)propagators.size(); ++n)
      {
         elapsedTime.push_back(0.0);

         p.push_back(propagators[n]->GetPropagator());
         fm.push_back(propagators[n]->GetODEModel());
         dim += fm[n]->GetDimension();

         psm.push_back(propagators[n]->GetPropStateManager());
         currEpoch.push_back(psm[n]->GetState()->GetEpoch());

         p[n]->Initialize();
         psm[n]->MapObjectsToVector();

         p[n]->Update(true/*direction > 0.0*/);
//         state = fm[n]->GetState();
         j2kState = fm[n]->GetJ2KState();
         baseEpoch.push_back(psm[n]->GetState()->GetEpoch());

         hasFired = true;
         inProgress = true;
      }
   }

   if (pubdata)
   {
      #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (pubdata, "pubdata", "Propagate::PrepareToPropagate()",
             "deleting pub data");
      #endif
      delete [] pubdata;
   }
   pubdata = new Real[dim+1];
   #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (pubdata, "pubdata", "Propagate::PrepareToPropagate()",
          "pubdata = new Real[dim+1]");
   #endif

   // Publish the data
   pubdata[0] = currEpoch[0];
   memcpy(&pubdata[1], j2kState, dim*sizeof(Real));

   #ifdef DEBUG_PUBLISH_DATA
      MessageInterface::ShowMessage
         ("Propagate::PrepareToPropagate() '%s' publishing initial %d data to "
          "stream %d, 1st data = %f\n", GetGeneratingString(Gmat::NO_COMMENTS).c_str(),
          dim+1, streamID, pubdata[0]);
   #endif

   #ifdef __USE_OLD_PUB_CODE__
      publisher->Publish(streamID, pubdata, dim+1);
   #else
      publisher->Publish(this, streamID, pubdata, dim+1);
   #endif

   return retval;
}


bool PropagationEnabledCommand::AssemblePropagators()
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage(
            "PropagationEnabledCommand::AssemblePropagators() entered\n");
   #endif
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
   #ifdef DEBUG_EXECUTION
      MessageInterface::ShowMessage(
            "PropagationEnabledCommand::Step() entered\n");
   #endif
   bool retval = false;

   MessageInterface::ShowMessage("Taking a step; ");

   ODEModel *fm = propagators[0]->GetODEModel();
   Real baseEpoch = (*(propObjects[0]))[0]->GetEpoch();

   MessageInterface::ShowMessage("Epoch = %.12lf...", baseEpoch);

//   if (dt != 0.0)
//   {
//      retval = thePropagator->GetPropagator()->Step(dt);
      fm->UpdateSpaceObject(dt/GmatTimeUtil::SECS_PER_DAY);
//
//      // orbit related parameters use spacecraft for data
      Real elapsedTime = 100.0;//fm->GetTime();
      Real currEpoch = baseEpoch + elapsedTime /
            GmatTimeUtil::SECS_PER_DAY;
//
//      // Update spacecraft epoch, without argument the spacecraft epoch
//      // won't get updated for consecutive Propagate command
      fm->UpdateSpaceObject(currEpoch);
      baseEpoch = currEpoch;
//   }

   MessageInterface::ShowMessage("Stepped to epoch %.12lf\n", currEpoch);

   return retval;
}
