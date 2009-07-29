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
#include "PropagationStateManager.hpp"

#define DEBUG_INITIALIZATION
//#define DEBUG_EXECUTION


PropagationEnabledCommand::PropagationEnabledCommand(const std::string &typeStr) :
   GmatCommand          (typeStr),
   overridePropInit     (false),
   hasFired             (false),
   inProgress           (false),
   dim                  (0),
   epochID              (-1),
   j2kState             (NULL),
   pubdata              (NULL)
{
}


PropagationEnabledCommand::~PropagationEnabledCommand()
{
   if (!overridePropInit)
   {
      for (std::vector<PropSetup*>::iterator i = propagators.begin();
            i != propagators.end(); ++i)
      {
         if (*i)
            delete (*i);
      }
      propagators.clear();
   }

   for (std::vector<PropObjectArray*>::iterator i = propObjects.begin();
         i != propObjects.end(); ++i)
      delete (*i);
   propObjects.clear();

   if (pubdata)
      delete [] pubdata;
}


PropagationEnabledCommand::PropagationEnabledCommand(const PropagationEnabledCommand& pec) :
   GmatCommand          (pec),
   overridePropInit     (pec.overridePropInit),
   hasFired             (false),
   inProgress           (false),
   dim                  (pec.dim),
   epochID              (pec.epochID),
   j2kState             (NULL),
   pubdata              (NULL)
{
   initialized = false;
   propagatorNames = pec.propagatorNames;
   for (UnsignedInt i = 0; i < pec.propObjectNames.size(); ++i)
      propObjectNames.push_back(pec.propObjectNames[i]);
}


PropagationEnabledCommand& PropagationEnabledCommand::operator=(const PropagationEnabledCommand& pec)
{
   if (this == &pec)
   {
      overridePropInit    = pec.overridePropInit;
      hasFired            = false;
      inProgress          = false;
      dim                 = pec.dim;
      epochID             = pec.epochID;
      initialized         = false;

      j2kState            = NULL;
      if (pubdata)
         delete [] pubdata;
      pubdata             = NULL;

      for (std::vector<PropSetup*>::const_iterator i = propagators.begin();
            i != propagators.end(); ++i)
         delete (*i);
      propagators.clear();
      propagatorNames = pec.propagatorNames;
      for (UnsignedInt i = 0; i < pec.propObjectNames.size(); ++i)
         propObjectNames.push_back(pec.propObjectNames[i]);
   }

   return *this;
}

bool PropagationEnabledCommand::Initialize()
{
   bool retval = false;

   if (GmatCommand::Initialize())
   {
      inProgress = false;
      hasFired = false;
//      UnsignedInt index = 0;

      for (std::vector<PropObjectArray*>::iterator o = propObjects.begin();
            o != propObjects.end(); ++o)
      {
         delete (*o);
      }
      propObjects.clear();

//      SpaceObject *so;
      std::string pName;
//      GmatBase *mapObj = NULL;

      //// Ensure that we are using fresh objects when buffering stops
      //EmptyBuffer();

      // Remove old PropSetups
      if (!overridePropInit)
      {
         if (propagators.size() > 0)
         {
            for (std::vector<PropSetup*>::iterator ps = propagators.begin();
                  ps != propagators.end(); ++ps)
            {
               #ifdef DEBUG_MEMORY
               MemoryTracker::Instance()->Remove
                  (oldPs, oldPs->GetName(), "PropagationEnabledCommand::"
                        "Initialize()", "deleting oldPs");
               #endif
               delete (*ps);
            }

            propagators.clear();
            p.clear();
            fm.clear();
         }

         // Todo Build the prop clones and set the related pointers
      }

      // Set the participant pointers
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("Found %d lists of prop object names\n",
               propObjectNames.size());
      #endif

      // Now set the pointers for the objects that get propagated
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
            // todo Memory debug and deallocation for this object
            objects = new PropObjectArray;
            propObjects.push_back(objects);
         }

         StringArray names = propObjectNames[i];
         PropSetup *currentPS = propagators[i];
         Propagator *currentP = currentPS->GetPropagator();
         ODEModel *currentODE = currentPS->GetODEModel();
         PropagationStateManager *currentPSM = currentPS->GetPropStateManager();

         StringArray owners, elements;
         /// @todo Check to see if All and All.Epoch belong for all modes.
         owners.push_back("All");
         elements.push_back("All.epoch");

         for (StringArray::iterator j = names.begin(); j != names.end(); ++j)
         {
            GmatBase *obj = FindObject(*j);
            if (obj == NULL)
               throw CommandException("Cannot find the object named " + (*j) +
                     " needed for propagation in the command\n" +
                     GetGeneratingString());
            if (obj->IsOfType(Gmat::SPACEOBJECT))
            {
               objects->push_back((SpaceObject*)obj);
               #ifdef DEBUG_INITIALIZATION
                  MessageInterface::ShowMessage("   Added the space object "
                        "named %s\n", obj->GetName().c_str());
               #endif

               // Now load up the PSM
               currentPSM->SetObject(obj);

               SpaceObject *so = (SpaceObject*)obj;
               if (epochID == -1)
                  epochID = so->GetParameterID("A1Epoch");
//               if (so->IsManeuvering())
//                  finiteBurnActive = true;

//               AddToBuffer(so);

//               if (so->GetType() == Gmat::FORMATION)
//                  FillFormation(so, owners, elements);
//               else
//               {
//                  SetNames(so->GetName(), owners, elements);
//               }
            }
            #ifdef DEBUG_INITIALIZATION
               else
                  MessageInterface::ShowMessage("   Found %s, not a space "
                        "object\n", obj->GetName().c_str());
            #endif
         }

         if (currentPSM->BuildState() == false)
            throw CommandException("Could not build the state for the "
                  "command \n" + generatingString);
         if (currentPSM->MapObjectsToVector() == false)
            throw CommandException("Could not map state objects for the "
                  "command\n" + generatingString);

         currentODE->SetState(currentPSM->GetState());

         // Set solar system to ForceModel for Propagate inside a GmatFunction(loj: 2008.06.06)
         currentODE->SetSolarSystem(solarSys);

//         // Check for finite thrusts and update the force model if there are any
//         if (finiteBurnActive == true)
//            AddTransientForce(satName[index], currentODE);

         streamID = publisher->RegisterPublishedData(this, streamID, owners, elements);

         currentP->SetPhysicalModel(currentODE);
//         currentP->SetRealParameter("InitialStepSize",
//               fabs(currentP->GetRealParameter("InitialStepSize")) * direction);
         currentP->Initialize();

         // Set spacecraft parameters for forces that need them
         if (currentODE->SetupSpacecraftData((ObjectArray*)objects, 0) <= 0)
            throw PropagatorException("Propagate::Initialize -- "
                  "ODE model cannot set spacecraft parameters");

      }

      // Now we have everything we need to init the prop subsystem
      retval = true;
      initialized = true;
      // retval = AssemblePropagators();

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



//for (StringArray::iterator i = propName.begin(); i != propName.end(); ++i)
//{
//   if (satName.size() <= index)
//      throw CommandException("Size mismatch for SpaceObject names\n");
//
//   if ((*i)[0] == '-')
//      pName = i->substr(1);
//   else
//     pName = *i;
//
//   if ((mapObj = FindObject(pName)) == NULL)
//      throw CommandException(
//         "Propagate command cannot find Propagator Setup \"" + (pName) +
//         "\"\n");
//
//   if (satName[index]->empty())
//      throw CommandException(
//         "Propagate command does not have a SpaceObject for " + (pName) +
//         " in \n\"" + generatingString + "\"\n");
//
//   if (stopWhen.empty())
//      singleStepMode = true;
//   else
//      singleStepMode = false;
//
//   PropSetup *clonedProp = (PropSetup *)(mapObj->Clone());
//   #ifdef DEBUG_MEMORY
//   MemoryTracker::Instance()->Add
//      (clonedProp, clonedProp->GetName(), "Propagate::Initialize()",
//       "(PropSetup *)(mapObj->Clone())");
//   #endif
//   //prop.push_back((PropSetup *)(mapObj->Clone()));
//   prop.push_back(clonedProp);
//   if (!prop[index])
//      return false;
//
//   Propagator *p = prop[index]->GetPropagator();
//   if (!p)
//      throw CommandException("Propagator not set in PropSetup\n");
//
//   // Toss the spacecraft into the prop state manager
//
//   ODEModel *odem = prop[index]->GetODEModel();
//   if (!odem)
//      throw CommandException("ForceModel not set in PropSetup\n");
//
//   PropagationStateManager *psm = prop[index]->GetPropStateManager();
//   StringArray::iterator scName;
//   StringArray owners, elements;
//
//   /// @todo Check to see if All and All.Epoch belong in place for all modes.
//   owners.push_back("All");
//   elements.push_back("All.epoch");
   //
   //   bool finiteBurnActive = false;
   //
//   for (scName = satName[index]->begin(); scName != satName[index]->end();
//        ++scName)
//   {
//      #if DEBUG_PROPAGATE_INIT
//         MessageInterface::ShowMessage(
//               "   Adding '%s' to prop state manager '%s'\n",
//            scName->c_str(), i->c_str());
//      #endif
//      if ((mapObj = FindObject(*scName)) == NULL)
//      {
//         #if DEBUG_PROPAGATE_INIT
//            MessageInterface::ShowMessage("   '%s' is not an object; "
//                  "attempting to set as a prop property\n",
//                  scName->c_str());
//         #endif
//         if (psm->SetProperty(*scName) == false)
//         {
//            std::string errmsg = "Unknown SpaceObject property \"";
//         errmsg += *scName;
//         errmsg += "\"";
//         throw CommandException(errmsg);
//         }
//      }
//      else
//      {
//         psm->SetObject(mapObj);
//
//         so = (SpaceObject*)mapObj;
//         if (epochID == -1)
//            epochID = so->GetParameterID("A1Epoch");
//         if (so->IsManeuvering())
//            finiteBurnActive = true;
//         sats.push_back(so);
//         AddToBuffer(so);
//
//         if (so->GetType() == Gmat::FORMATION)
//            FillFormation(so, owners, elements);
//         else
//         {
//            SetNames(so->GetName(), owners, elements);
//         }
//      }
//   }
//
//   if (psm->BuildState() == false)
//      throw CommandException("Could not build the state for the command \n" +
//            generatingString);
//   if (psm->MapObjectsToVector() == false)
//      throw CommandException("Could not map state objects for the command\n" +
//            generatingString);
//
//   odem->SetState(psm->GetState());
//
//   // Set solar system to ForceModel for Propagate inside a GmatFunction(loj: 2008.06.06)
//   odem->SetSolarSystem(solarSys);
//
//   // Check for finite thrusts and update the force model if there are any
//   if (finiteBurnActive == true)
//      AddTransientForce(satName[index], odem);



//
//   #ifdef DEBUG_PUBLISH_DATA
//   MessageInterface::ShowMessage
//      ("Propagate::Initialize() '%s' registering published data\n",
//       GetGeneratingString(Gmat::NO_COMMENTS).c_str());
//   #endif
//
//   streamID = publisher->RegisterPublishedData(this, streamID, owners, elements);
//
//   p->SetPhysicalModel(odem);
//   p->SetRealParameter("InitialStepSize",
//      fabs(p->GetRealParameter("InitialStepSize")) * direction);
//   p->Initialize();
//
//   // Set spacecraft parameters for forces that need them
//   if (odem->SetupSpacecraftData(&sats, 0) <= 0)
//      throw PropagatorException("Propagate::Initialize -- "
//            "ODE model cannot set spacecraft parameters");
//
//
//   ++index;
//} // End of loop through PropSetups
//
//initialized = true;
//
//stopSats.clear();
//// Setup spacecraft array used for stopping conditions
//for (StringArray::iterator sc = stopSatNames.begin();
//     sc != stopSatNames.end(); ++sc)
//{
//   if ((mapObj = FindObject(*sc)) == NULL)
//   {
//      std::string errmsg = "Unknown SpaceObject \"";
//      errmsg += *sc;
//      errmsg += "\" used in stopping conditions";
//      throw CommandException(errmsg);
//   }
//   so = (SpaceObject*)mapObj;
//   stopSats.push_back(so);
//}
//
//#if DEBUG_PROPAGATE_INIT
//   for (UnsignedInt i=0; i<stopSats.size(); i++)
//      MessageInterface::ShowMessage(
//         "Propagate::Initialize() stopSats[%d]=%s\n", i,
//         stopSats[i]->GetName().c_str());
//#endif
//
//if ((stopWhen.size() == 0) && !singleStepMode)
//   throw CommandException("No stopping conditions specified!");
//
//if (solarSys != NULL)
//{
//   StringArray refNames;
//
//   for (UnsignedInt i=0; i<stopWhen.size(); i++)
//   {
//      stopWhen[i]->SetSolarSystem(solarSys);
//
//      //Set StopCondition parameters
//      refNames = stopWhen[i]->GetRefObjectNameArray(Gmat::PARAMETER);
//
//      for (UnsignedInt j=0; j<refNames.size(); j++)
//      {
//         #if DEBUG_PROPAGATE_INIT
//            MessageInterface::ShowMessage("===> refNames=<%s>\n",
//               refNames[j].c_str());
//         #endif
//         mapObj = FindObject(refNames[j]);
//         stopWhen[i]->SetRefObject(mapObj,
//                                   Gmat::PARAMETER, refNames[j]);
//      }
//
//      stopWhen[i]->Initialize();
//      stopWhen[i]->SetSpacecraft((SpaceObject*)sats[0]);
//
//      if (!stopWhen[i]->IsInitialized())
//      {
//         initialized = false;
//         MessageInterface::ShowMessage(
//            "Propagate::Initialize() StopCondition %s is not initialized.\n",
//            stopWhen[i]->GetName().c_str());
//         break;
//      }
//   }
//}
//else
//{
//   initialized = false;
//   MessageInterface::ShowMessage
//      ("Propagate::Initialize() SolarSystem not set in StopCondition");
//}
//
//#if DEBUG_PROPAGATE_EXE
//   MessageInterface::ShowMessage("Propagate::Initialize() complete.\n");
//#endif
//
//#ifdef DEBUG_PROPAGATE_DIRECTION
//   MessageInterface::ShowMessage("Propagate::Initialize():"
//                                 " Propagators Identified:\n");
//   for (StringArray::iterator i = propName.begin(); i != propName.end();
//        ++i)
//      MessageInterface::ShowMessage("   \"%s\" running %s\n", i->c_str(),
//      (direction > 0.0 ? "forwards" : "backwards"));
//#endif
//
//if (singleStepMode)
//{
//   commandSummary = "Command Summary: ";
//   commandSummary += typeName;
//   commandSummary += " Command\nSummary not available in single step mode\n";
//}
//
//#ifdef DUMP_PLANET_DATA
//   if (body[0] == NULL)
//      body[0] = solarSys->GetBody("Earth");
//   if (body[1] == NULL)
//      body[1] = solarSys->GetBody("Sun");
//   if (body[2] == NULL)
//      body[2] = solarSys->GetBody("Luna");
//   if (body[3] == NULL)
//      body[3] = solarSys->GetBody("Mercury");
//   if (body[4] == NULL)
//      body[4] = solarSys->GetBody("Venus");
//   if (body[5] == NULL)
//      body[5] = solarSys->GetBody("Mars");
//   if (body[6] == NULL)
//      body[6] = solarSys->GetBody("Jupiter");
//   if (body[7] == NULL)
//      body[7] = solarSys->GetBody("Saturn");
//   if (body[8] == NULL)
//      body[8] = solarSys->GetBody("Uranus");
//   if (body[9] == NULL)
//      body[9] = solarSys->GetBody("Neptune");
//   if (body[10] == NULL)
//      body[10] = solarSys->GetBody("Pluto");
//
//   bodiesDefined = 11;
//#endif
//
//return initialized;



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
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
               "   PrepareToPropagate() in hasFired state\n");
      #endif

//      // Handle the transient forces
//      for (std::vector<PropObjectArray*>::iterator poa = propObjects.begin();
//           poa != propObjects.end(); ++poa)
//      {
//         for (PropObjectArray::iterator sc = (*poa)->begin();
//               sc != (*poa)->end(); ++sc)
//         {
//            if (((SpaceObject*)(*sc))->IsManeuvering())
//            {
//               #ifdef DEBUG_FINITE_MANEUVER
//                  MessageInterface::ShowMessage(
//                     "SpaceObject %s is maneuvering\n", (*sc)->GetName().c_str());
//               #endif
//
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
//            }
//         }
//      }

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
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage(
               "PropagationEnabledCommand::PrepareToPropagate() first entry\n");
      #endif

//      // Set the prop state managers for the PropSetup ODEModels
//      for (std::vector<PropSetup*>::iterator i=propagators.begin();
//            i != propagators.end(); ++i)
//      {
//         #ifdef DEBUG_INITIALIZATION
//            MessageInterface::ShowMessage(
//                  "   Setting PSM on ODEModel for propagator %s\n",
//                  (*i)->GetName().c_str());
//         #endif
//
//         ODEModel *ode = (*i)->GetODEModel();
//         if (ode != NULL)    // Only do this for the PropSetups that integrate
//            ode->SetPropStateManager((*i)->GetPropStateManager());
//      }
//
//      // Initialize the subsystem
//      Initialize();

      // Loop through the PropSetups and build the models
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("   Looping through %d propagators\n",
               propagators.size());
      #endif
      for (std::vector<PropSetup*>::iterator i=propagators.begin();
            i != propagators.end(); ++i)
      {
         #ifdef DEBUG_INITIALIZATION
            MessageInterface::ShowMessage(
                  "   Setting PSM on ODEModel for propagator %s\n",
                  (*i)->GetName().c_str());
         #endif
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

      for (UnsignedInt n = 0; n < propagators.size(); ++n)
      {
         #ifdef DEBUG_INITIALIZATION
            MessageInterface::ShowMessage(
                  "   Setting pointers for propagator %s\n",
                  propagators[n]->GetName().c_str());
         #endif
         elapsedTime.push_back(0.0);

         p.push_back(propagators[n]->GetPropagator());
         fm.push_back(propagators[n]->GetODEModel());
         dim += fm[n]->GetDimension();

         psm.push_back(propagators[n]->GetPropStateManager());
         currEpoch.push_back(psm[n]->GetState()->GetEpoch());

         #ifdef DEBUG_INITIALIZATION
            MessageInterface::ShowMessage(
                  "   Initializing propagator %s\n",
                  propagators[n]->GetName().c_str());
         #endif
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

#ifdef DEBUG_INITIALIZATION
   MessageInterface::ShowMessage(
         "PropagationEnabledCommand::PrepareToPropagate() finished\n");
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
   bool retval = true;

   for (UnsignedInt i = 0; i < fm.size(); ++i)
   {
      fm[i]->UpdateInitialData();
      fm[i]->BufferState();
   }

   std::vector<Propagator*>::iterator current = p.begin();
   // Step all of the propagators by the input amount
   while (current != p.end())
   {
      if (!(*current)->Step(dt))
      {
         char size[32];
         std::sprintf(size, "%.12lf", dt);
         throw CommandException("Propagator " + (*current)->GetName() +
            " failed to take a good final step (size = " + size + ")\n");
      }

      ++current;
   }

   for (UnsignedInt i = 0; i < fm.size(); ++i)
   {
      // orbit related parameters use spacecraft for data
      elapsedTime[i] = fm[i]->GetTime();
      currEpoch[i] = baseEpoch[i] + elapsedTime[i] /
         GmatTimeUtil::SECS_PER_DAY;

      // Update spacecraft epoch, without argument the spacecraft epoch
      // won't get updated for consecutive Propagate command
      fm[i]->UpdateSpaceObject(currEpoch[i]);
   }

   // Publish the data here
   pubdata[0] = currEpoch[0];
   memcpy(&pubdata[1], j2kState, dim*sizeof(Real));

   #ifdef __USE_OLD_PUB_CODE__
      publisher->Publish(streamID, pubdata, dim+1);
   #else
      publisher->Publish(this, streamID, pubdata, dim+1);
   #endif

   return retval;
}
