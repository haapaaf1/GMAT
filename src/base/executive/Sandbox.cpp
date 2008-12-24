//$Id$
//------------------------------------------------------------------------------
//                                 Sandbox
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Darrel J. Conway
// Created: 2003/10/08
//
/**
 * Implementation for the GMAT Sandbox class
 */
//------------------------------------------------------------------------------

#include "Sandbox.hpp"
#include "Moderator.hpp"
#include "SandboxException.hpp"
#include "Parameter.hpp"
#include "FiniteThrust.hpp"
#include "GmatFunction.hpp"
#include "CallFunction.hpp"
#include "Assignment.hpp"
#include "BranchCommand.hpp"
#include "SubscriberException.hpp"
#include "CommandUtil.hpp"         // for GetCommandSeqString()
#include "MessageInterface.hpp"

#include <algorithm>       // for find

//#define DISABLE_SOLAR_SYSTEM_CLONING

//#define DISALLOW_NESTED_GMAT_FUNCTIONS

//#define DEBUG_SANDBOX_INIT
//#define DEBUG_MODERATOR_CALLBACK
//#define DEBUG_SANDBOX_GMATFUNCTION
//#define DEBUG_SANDBOX_OBJ_INIT
//#define DEBUG_SANDBOX_OBJ_ADD
//#define DEBUG_SANDBOX_OBJECT_MAPS
//#define DBGLVL_SANDBOX_RUN 1

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif

#ifdef DEBUG_SANDBOX_INIT
   std::map<std::string, GmatBase *>::iterator omIter;
#endif


//------------------------------------------------------------------------------
// Sandbox::Sandbox()
//------------------------------------------------------------------------------
/**
 *  Default constructor.
 */
//------------------------------------------------------------------------------
Sandbox::Sandbox() :
   solarSys          (NULL),
   internalCoordSys  (NULL),
   publisher         (NULL),
   sequence          (NULL),
   current           (NULL),
   moderator         (NULL),
   state             (IDLE),
   interruptCount    (45),
   pollFrequency     (50),
   objInit           (NULL)
{
   #ifdef DEBUG_SANDBOX_CLONING
      // List of the objects that can safely be cloned.  This list will be removed
      // when the cloning has been tested for all of GMAT's classes.
      clonable.push_back(Gmat::SPACECRAFT);
      clonable.push_back(Gmat::FORMATION);
      clonable.push_back(Gmat::SPACEOBJECT);
      clonable.push_back(Gmat::GROUND_STATION);
      clonable.push_back(Gmat::BURN);
      clonable.push_back(Gmat::IMPULSIVE_BURN);
      clonable.push_back(Gmat::FINITE_BURN);
      clonable.push_back(Gmat::COMMAND);
      clonable.push_back(Gmat::PROPAGATOR);
      clonable.push_back(Gmat::FORCE_MODEL);
      clonable.push_back(Gmat::PHYSICAL_MODEL);
      clonable.push_back(Gmat::TRANSIENT_FORCE);
      clonable.push_back(Gmat::INTERPOLATOR);
      clonable.push_back(Gmat::SPACE_POINT);
      clonable.push_back(Gmat::CELESTIAL_BODY);
      clonable.push_back(Gmat::CALCULATED_POINT);
      clonable.push_back(Gmat::LIBRATION_POINT);
      clonable.push_back(Gmat::BARYCENTER);
      clonable.push_back(Gmat::ATMOSPHERE);
      clonable.push_back(Gmat::PARAMETER);
      clonable.push_back(Gmat::STOP_CONDITION);
      clonable.push_back(Gmat::SOLVER);
      clonable.push_back(Gmat::SUBSCRIBER);
      clonable.push_back(Gmat::PROP_SETUP);
      clonable.push_back(Gmat::FUNCTION);
      clonable.push_back(Gmat::FUEL_TANK);
      clonable.push_back(Gmat::THRUSTER);
      clonable.push_back(Gmat::HARDWARE);
      clonable.push_back(Gmat::COORDINATE_SYSTEM);
      clonable.push_back(Gmat::AXIS_SYSTEM);
   #endif

   // SolarSystem instances are handled separately from the other objects
   // clonable.push_back(Gmat::SOLAR_SYSTEM);
}


//------------------------------------------------------------------------------
// ~Sandbox()
//------------------------------------------------------------------------------
/**
 *  Destructor.
 */
//------------------------------------------------------------------------------
Sandbox::~Sandbox()
{
   #ifndef DISABLE_SOLAR_SYSTEM_CLONING   
      if (solarSys)
      {
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (solarSys, solarSys->GetName(), "Sandbox::~Sandbox()",
             " deleting cloned solarSys");
         #endif
         delete solarSys;
      }
   #endif
   
   if (sequence)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (sequence, "sequence", "Sandbox::~Sandbox()",
          " deleting mission sequence");
      #endif
      delete sequence;
   }
   
   if (objInit)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (objInit, "objInit", "Sandbox::~Sandbox()", " deleting objInit");
      #endif
      delete objInit;
   }
   
   // Delete the local objects
   Clear();
}


//------------------------------------------------------------------------------
// Setup methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// GmatBase* AddObject(GmatBase *obj)
//------------------------------------------------------------------------------
/**
 *  Adds an object to the Sandbox's object container.
 *
 *  Objects are added to the Sandbox by cloning the objects.  That way local
 *  copies can be manipulated without affecting the objects managed by the
 *  ConfigurationManager.
 *
 *  @param <obj> The object that needs to be included in the Sandbox.
 *
 *  @return Cloned object pointer if the object was added to the Sandbox's
 *          container, NULL if it was not.
 */
//------------------------------------------------------------------------------
//Changed to return GmatBase* (loj: 2008.11.06)
GmatBase* Sandbox::AddObject(GmatBase *obj)
{
   if (obj == NULL)
      return NULL;
   
   #ifdef DEBUG_SANDBOX_OBJ_ADD
      MessageInterface::ShowMessage
         ("Sandbox::AddObject() objTypeName=%s, objName=%s\n",
          obj->GetTypeName().c_str(), obj->GetName().c_str());
   #endif
      
   if ((state != INITIALIZED) && (state != STOPPED) && (state != IDLE))
          MessageInterface::ShowMessage(
             "Unexpected state transition in the Sandbox\n");

   state = IDLE;
   
   std::string name = obj->GetName();
   if (name == "")
      return NULL;  // No unnamed objects in the Sandbox tables
   
   GmatBase *cloned = obj;
   
   // Check to see if the object is already in the map
   //if (objectMap.find(name) == objectMap.end())
   if (FindObject(name) == NULL)
   {
      // If not, store the new object pointer
      #ifdef DEBUG_SANDBOX_CLONING
      if (find(clonable.begin(), clonable.end(), obj->GetType()) !=
          clonable.end())
      {
      #endif
         #ifdef DEBUG_SANDBOX_OBJECT_MAPS
         MessageInterface::ShowMessage(
            "Cloning object %s of type %s\n", obj->GetName().c_str(),
            obj->GetTypeName().c_str());
         #endif
         
         cloned = obj->Clone();
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Add
            (cloned, obj->GetName(), "Sandbox::AddObject()",
             "*cloned = obj->Clone()");
         #endif
         SetObjectByNameInMap(name, cloned);
      #ifdef DEBUG_SANDBOX_CLONING
      }
      else
         SetObjectByNameInMap(name, obj);
      #endif
   }
   else
   {
      MessageInterface::ShowMessage
         ("in Sandbox::AddObject() %s is already in the map\n", name.c_str());
   }
   
   return cloned;
}


//------------------------------------------------------------------------------
// bool AddCommand(GmatCommand *cmd)
//------------------------------------------------------------------------------
/**
 *  Adds a command to the Sandbox's command sequence.
 *
 *  Command are added to the command srquence by appending them ti the command
 *  list, using the GmatCommand::Append() method.
 *
 *  @param <cmd> The command that needs to be added to this Sandbox's sequence.
 *
 *  @return true if the command was added to the sequence, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::AddCommand(GmatCommand *cmd)
{

   if ((state != INITIALIZED) && (state != STOPPED) && (state != IDLE))
          MessageInterface::ShowMessage(
             "Unexpected state transition in the Sandbox\n");

  state = IDLE;


   if (!cmd)
      return false;

   if (cmd == sequence)
      return true;

   if (sequence)
      return sequence->Append(cmd);


   sequence = cmd;
   return true;
}


//------------------------------------------------------------------------------
// bool AddSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 *  Sets the SolarSystem for this Sandbox by cloning the input solar system.
 *
 *  @param <ss> The SolarSystem this Sandbox's will use.
 *
 *  @return true if the solar system was added to the Sandbox, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::AddSolarSystem(SolarSystem *ss)
{
   if ((state != INITIALIZED) && (state != STOPPED) && (state != IDLE))
          MessageInterface::ShowMessage(
             "Unexpected state transition in the Sandbox\n");
   state = IDLE;
   
   if (!ss)
      return false;
   
#ifdef DISABLE_SOLAR_SYSTEM_CLONING
   solarSys = ss;
#else
   MessageInterface::ShowMessage("Cloning the solar system in the Sandbox\n");
   solarSys = (SolarSystem*)(ss->Clone());
   #ifdef DEBUG_MEMORY
   MemoryTracker::Instance()->Add
      (solarSys, solarSys->GetName(), "Sandbox::AddSolarSystem()",
       "solarSys = (SolarSystem*)(ss->Clone())");
   #endif
   
   #ifdef DEBUG_SS_CLONING
   MessageInterface::ShowMessage("Sandbox cloned the solar system: %p\n", solarSys);
   #endif
#endif
   return true;
}


//------------------------------------------------------------------------------
// bool SetInternalCoordSystem(CoordinateSystem *cs)
//------------------------------------------------------------------------------
/**
 *  Sets the internal coordinate system used by the Sandbox.
 *
 *  @param <cs> The internal coordinate system.
 *
 *  @return true if the command was added to the sequence, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::SetInternalCoordSystem(CoordinateSystem *cs)
{
   if ((state != INITIALIZED) && (state != STOPPED) && (state != IDLE))
          MessageInterface::ShowMessage(
             "Unexpected state transition in the Sandbox\n");

   state = IDLE;

   if (!cs)
      return false;

   /// @todo Check initialization and cloning for the internal CoordinateSystem.
   //internalCoordSys = (CoordinateSystem*)(cs->Clone());
   internalCoordSys = cs;
   return true;
}


//------------------------------------------------------------------------------
// bool SetPublisher(Publisher *pub)
//------------------------------------------------------------------------------
/**
 *  Sets the Publisher so the Sandbox can pipe data to the rest of GMAT.
 *
 *  @param <pub> The GMAT Publisher.
 *
 *  @return true if the command was added to the sequence, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::SetPublisher(Publisher *pub)
{

   if ((state != INITIALIZED) && (state != STOPPED) && (state != IDLE))
          MessageInterface::ShowMessage(
             "Unexpected state transition in the Sandbox\n");
   state = IDLE;


   if (pub) {
      publisher = pub;
      // Now publisher needs internal coordinate system
      publisher->SetInternalCoordSystem(internalCoordSys);
      return true;
   }


   if (!publisher)
      return false;


   return true;
}


//------------------------------------------------------------------------------
// GmatBase* GetInternalObject(std::string name, Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 *  Accesses objects managed by this Sandbox.
 *
 *  @param <name> The name of the object.
 *  @param <name> type of object requested.
 *
 *  @return The pointer to the object.
 */
//------------------------------------------------------------------------------
GmatBase* Sandbox::GetInternalObject(std::string name, Gmat::ObjectType type)
{
   #ifdef DEBUG_INTERNAL_OBJ
   MessageInterface::ShowMessage
      ("Sandbox::GetInternalObject() name=%s, type=%d\n", name.c_str(), type);
   #endif
   
   GmatBase* obj = NULL;
   
   if ((obj = FindObject(name)) != NULL) 
   {
      if (type != Gmat::UNKNOWN_OBJECT)
      {
         if (obj->GetType() != type) 
         {
            std::string errorStr = "GetInternalObject type mismatch for ";
            errorStr += name;
            throw SandboxException(errorStr);
         }
      }
   }
   else 
   {
      std::string errorStr = "Sandbox::GetInternalObject(" + name +
                             "...) Could not find \"";
      errorStr += name;
      errorStr += "\" in the Sandbox.";
      
      #ifdef DEBUG_SANDBOX_OBJECT_MAPS
         MessageInterface::ShowMessage("Here is the current object map:\n");
         for (std::map<std::string, GmatBase *>::iterator i = objectMap.begin();
              i != objectMap.end(); ++i)
            MessageInterface::ShowMessage("   %s\n", i->first.c_str());
         MessageInterface::ShowMessage("Here is the current global object map:\n");
         for (std::map<std::string, GmatBase *>::iterator i = globalObjectMap.begin();
              i != globalObjectMap.end(); ++i)
            MessageInterface::ShowMessage("   %s\n", i->first.c_str());
      #endif
      
      throw SandboxException(errorStr);
   }

   return obj;
}


//------------------------------------------------------------------------------
// Execution methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 *  Established the internal linkages between objects needed prior to running a
 *  mission sequence.
 *
 *  @return true if everything was connected properly, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::Initialize()
{
   #ifdef DEBUG_SANDBOX_INIT
      MessageInterface::ShowMessage("Initializing the Sandbox\n");
      MessageInterface::ShowMessage("At the start, the Sandbox Object Map contains:\n");
      for (omIter = objectMap.begin(); omIter != objectMap.end(); ++omIter)
         MessageInterface::ShowMessage("   %s of type %s\n",
               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
      MessageInterface::ShowMessage("At the start, the Global Object Map contains:\n");
      for (omIter = globalObjectMap.begin(); omIter != globalObjectMap.end(); ++omIter)
         MessageInterface::ShowMessage("   %s of type %s\n",
               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
      MessageInterface::ShowMessage(" ........ \n");
   #endif

   bool rv = false;


   if (moderator == NULL)
      moderator = Moderator::Instance();
   
   // this should be clear() (loj: 2008.11.03)
   //transientForces.empty();
   transientForces.clear();
   
   
   // Already initialized
   if (state == INITIALIZED)
      return true;


   current = sequence;
   if (!current)
      throw SandboxException("No mission sequence defined in the Sandbox!");


   if (!internalCoordSys)
      throw SandboxException(
         "No reference (internal) coordinate system defined in the Sandbox!");


   std::map<std::string, GmatBase *>::iterator omi;
   GmatBase *obj = NULL;
   std::string oName;
   std::string j2kName;


   // Set the solar system references
   if (solarSys == NULL)
      throw SandboxException("No solar system defined in the Sandbox!");
   
   // Initialize the solar system, internal coord system, etc.

   // Set J2000 Body for all SpacePoint derivatives before anything else
   // NOTE - at this point, everything should be in the SandboxObjectMap,
   // and the GlobalObjectMap should be empty
   #ifdef DEBUG_SANDBOX_OBJ_INIT
      MessageInterface::ShowMessage("About to create the ObjectInitializer ... \n");
      MessageInterface::ShowMessage(" and the objInit pointer is %s\n",
            (objInit? "NOT NULL" : "NULL!!!"));
   #endif

   if (objInit)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (objInit, "objInit", "Sandbox::Initialize()", " deleting objInit");
      #endif
      delete objInit;  // if Initialize is called more than once, delete 'old' objInit
   }
   
   objInit = new ObjectInitializer(solarSys, &objectMap, &globalObjectMap, internalCoordSys);
   
   #ifdef DEBUG_MEMORY
   MemoryTracker::Instance()->Add
      (objInit, "objInit", "Sandbox::Initialize()", "objInit = new ObjectInitializer");
   #endif
   try
   {
      #ifdef DEBUG_SANDBOX_OBJ_INIT
         MessageInterface::ShowMessage(
               "About to call the ObjectInitializer::InitializeObjects ... \n");
      #endif
      objInit->InitializeObjects();
   }
   catch (BaseException &be)
   {
      SandboxException se("");
      se.SetDetails("Error initializing objects in Sandbox.\n%s\n",
                    be.GetFullMessage().c_str());
      throw se;
      //throw SandboxException("Error initializing objects in Sandbox");
   }
   
   // Move global objects to the Global Object Store
   combinedObjectMap = objectMap;
   StringArray movedObjects;
   for (omi = objectMap.begin(); omi != objectMap.end(); ++omi)
   {
      obj = omi->second;
      #ifdef DEBUG_SANDBOX_INIT
         MessageInterface::ShowMessage(
            "Sandbox::checking object %s (of type %s) \n",
            (omi->first).c_str(), (obj->GetTypeName()).c_str());
      #endif
      // Check the isGlobal flag
      if (obj->GetIsGlobal())
      {
         #ifdef DEBUG_SANDBOX_INIT
            MessageInterface::ShowMessage(
               "Sandbox::moving object %s to the Global Object Store\n",
               (omi->first).c_str());
         #endif
         globalObjectMap.insert(*omi);
         movedObjects.push_back(omi->first);
      }
   }
   for (unsigned int ii = 0; ii < movedObjects.size(); ii++)
      objectMap.erase(movedObjects.at(ii));
   movedObjects.clear();  
   
   #ifdef DEBUG_SANDBOX_INIT
      MessageInterface::ShowMessage("--- Right AFTER moving things to the GOS --- \n");
      MessageInterface::ShowMessage("The Sandbox Object Map contains:\n");
      for (omIter = objectMap.begin(); omIter != objectMap.end(); ++omIter)
         MessageInterface::ShowMessage("   %s of type %s\n",
               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
      MessageInterface::ShowMessage("The Global Object Map contains:\n");
      for (omIter = globalObjectMap.begin(); omIter != globalObjectMap.end(); ++omIter)
         MessageInterface::ShowMessage("   %s of type %s\n",
               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
   #endif
   
   #ifdef DEBUG_SANDBOX_INIT
      MessageInterface::ShowMessage(
         "Sandbox::Initialize() Initializing Commands...\n");
   #endif
   
   
   //MessageInterface::ShowMessage("=====> Initialize commands\n");
   // Initialize commands
   while (current)
   {
      #ifdef DEBUG_SANDBOX_INIT
      MessageInterface::ShowMessage
         ("Initializing %s command\n   \"%s\"\n",
          current->GetTypeName().c_str(),
          current->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
      #endif
      
      #ifdef DEBUG_SANDBOX_GMATFUNCTION
         MessageInterface::ShowMessage(
               "Initializing %s command\n",
               current->GetTypeName().c_str());
      #endif
         
      current->SetObjectMap(&objectMap);
      current->SetGlobalObjectMap(&globalObjectMap);
      current->SetSolarSystem(solarSys);
      current->SetTransientForces(&transientForces);
      
      // Handle GmatFunctions
      if ((current->IsOfType("CallFunction")) ||
          (current->IsOfType("Assignment")))
      {
         #ifdef DEBUG_SANDBOX_GMATFUNCTION
            MessageInterface::ShowMessage(
               "CallFunction or Assignment found in MCS: calling HandleGmatFunction \n");
         #endif
         HandleGmatFunction(current, &combinedObjectMap);
         current->SetInternalCoordSystem(internalCoordSys);
      }
      if (current->IsOfType("BranchCommand"))
      {
         std::vector<GmatCommand*> cmdList = ((BranchCommand*) current)->GetCommandsWithGmatFunctions();
         Integer sz = (Integer) cmdList.size();
         #ifdef DEBUG_SANDBOX_GMATFUNCTION
            MessageInterface::ShowMessage("... returning %d functions with GmatFunctions\n", sz);
         #endif
         for (Integer jj = 0; jj < sz; jj++)
         {
            HandleGmatFunction(cmdList.at(jj), &combinedObjectMap);
            (cmdList.at(jj))->SetInternalCoordSystem(internalCoordSys);
         }
      }
      
      rv = current->Initialize();
      if (!rv)
         return false;
      
      // Check to see if the command needs a server startup
      if (current->NeedsServerStartup())
         if (moderator->StartServer() == false)
            throw SandboxException("Unable to start the server needed by the " +
                     (current->GetTypeName()) + " command");

      current = current->GetNext();
   }


   #ifdef DEBUG_SANDBOX_INIT
      MessageInterface::ShowMessage(
         "Sandbox::Initialize() Successfully initialized\n");
   #endif

   state = INITIALIZED;
   
   //MessageInterface::ShowMessage("=====> Initialize successful\n");
   return rv;
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
/**
 *  Runs the mission sequence.
 *
 *  This method walks through the command linked list, firing each GmatCommand
 *  as it is encountered by calling Execute() on the commands.  Between command
 *  executions, the method check with the Moderator to see if the user has
 *  requested that the sequence be paused or halted.
 *
 *  @return true if the mission sequence was executed, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::Execute()
{

   #if DBGLVL_SANDBOX_RUN > 1
   MessageInterface::ShowMessage("Sandbox::Execute() Here is the current object map:\n");
   for (std::map<std::string, GmatBase *>::iterator i = objectMap.begin();
        i != objectMap.end(); ++i)
      MessageInterface::ShowMessage("   (%p) %s\n", i->second, i->first.c_str());
   MessageInterface::ShowMessage("Sandbox::Execute() Here is the current global object map:\n");
   for (std::map<std::string, GmatBase *>::iterator i = globalObjectMap.begin();
        i != globalObjectMap.end(); ++i)
      MessageInterface::ShowMessage("   (%p) %s\n", i->second, i->first.c_str());

   MessageInterface::ShowMessage("Sandbox::Execute() Here is the mission sequence:\n");
   std::string seq = GmatCommandUtil::GetCommandSeqString(sequence);
   MessageInterface::ShowMessage(seq);
   #endif
   
   bool rv = true;

   state = RUNNING;
   Gmat::RunState runState = Gmat::IDLE, currentState = Gmat::RUNNING;
   GmatCommand *prev = NULL;
   
   current = sequence;
   if (!current)
      return false;

   try
   {
      while (current)
      {
         // First check to see if the run should be interrupted
         if (Interrupt())
         {
            #ifdef DEBUG_MODERATOR_CALLBACK
            MessageInterface::ShowMessage("   Interrupted in %s command\n",
                                          current->GetTypeName().c_str());
            #endif
            
            
            if (state == PAUSED)
            {
               continue;
            }
            else
            {
               //MessageInterface::ShowMessage("Sandbox::Execution interrupted.\n");
               sequence->RunComplete();
               
               // notify subscribers end of run
               currentState = Gmat::IDLE;
               publisher->SetRunState(currentState);
               publisher->NotifyEndOfRun();
               
               throw SandboxException("Execution interrupted");
               //return rv;
            }
         }
         
         #if DBGLVL_SANDBOX_RUN
         if (current != prev)
         {
            MessageInterface::ShowMessage
               ("Sandbox::Execution running %s\n", current->GetTypeName().c_str());
            
            #if DBGLVL_SANDBOX_RUN > 1
            MessageInterface::ShowMessage
               ("command = \n<%s>\n", current->GetGeneratingString().c_str());
            #endif
         }
         #endif
         
         if (currentState != runState)
         {
            publisher->SetRunState(currentState);
            runState = currentState;
         }
         
         rv = current->Execute();
         
         if (!rv)
         {
            std::string str = "\"" + current->GetTypeName() +
               "\" Command failed to run to completion\n";
            
            #if DBGLVL_SANDBOX_RUN > 1
            MessageInterface::ShowMessage
               ("%sCommand Text is\n\"%s\n", str.c_str(),
                current->GetGeneratingString().c_str());
            #endif
            
            throw SandboxException(str);
         }
         
         prev = current;
         current = current->GetNext();
      }
   }
   catch (BaseException &e)
   {
      sequence->RunComplete();
      
      #if DBGLVL_SANDBOX_RUN
      MessageInterface::ShowMessage
         ("   Sandbox rethrowing %s\n", e.GetFullMessage().c_str());
      #endif
      
      throw;
   }
   
   sequence->RunComplete();
   state = STOPPED;
   
   // notify subscribers end of run
   currentState = Gmat::IDLE;
   publisher->SetRunState(currentState);
   publisher->NotifyEndOfRun();
   
   return rv;
}



//------------------------------------------------------------------------------
// bool Interrupt()
//------------------------------------------------------------------------------
/**
 *  Tests to see if the mission sequence should be interrupted.
 *
 *  @return true if the Moderator wants to interrupt execution, false if not.
 */
//------------------------------------------------------------------------------
bool Sandbox::Interrupt()
{
   // Ask the moderator for the current RunState; only check at fixed frequency
   if (++interruptCount == pollFrequency)
   {
      Gmat::RunState interruptType =  moderator->GetUserInterrupt();
   
      switch (interruptType)
      {
         case Gmat::PAUSED:   // Pause
            state = PAUSED;
            break;
   
         case Gmat::IDLE:     // Stop puts GMAT into the Idle state
            state = STOPPED;
            break;
   
         case Gmat::RUNNING:   // MCS is running
            state = RUNNING;
            break;
   
         default:
            break;
      }
      interruptCount = 0;
   }
   
   if ((state == PAUSED) || (state == STOPPED))
      return true;

   return false;
}



//------------------------------------------------------------------------------
// void Clear()
//------------------------------------------------------------------------------
/**
 *  Cleans up the local object store.
 */
//------------------------------------------------------------------------------
void Sandbox::Clear()
{
   sequence  = NULL;
   current   = NULL;

   // Delete the all cloned objects
   std::map<std::string, GmatBase *>::iterator omi;
   
   #ifdef DEBUG_SANDBOX_OBJECT_MAPS
   MessageInterface::ShowMessage("Sandbox OMI List\n");
   for (omi = objectMap.begin(); omi != objectMap.end(); omi++)
   {
      MessageInterface::ShowMessage("   %s", (omi->first).c_str());
      MessageInterface::ShowMessage(" of type %s\n",
         (omi->second)->GetTypeName().c_str());
   }
   MessageInterface::ShowMessage("Sandbox GOMI List\n");
   for (omi = globalObjectMap.begin(); omi != globalObjectMap.end(); omi++)
   {
      MessageInterface::ShowMessage("   %s", (omi->first).c_str());
      MessageInterface::ShowMessage(" of type %s\n",
         (omi->second)->GetTypeName().c_str());
   }
   #endif
   
   #ifdef DEBUG_MORE_MEMORY
   MessageInterface::ShowMessage
      ("--- Sandbox::Clear() deleting %d objects\n", objectMap.size());
   #endif
   
   for (omi = objectMap.begin(); omi != objectMap.end(); omi++)
   {
      if ((omi->second)->GetType() == Gmat::SUBSCRIBER)
         publisher->Unsubscribe((Subscriber*)(omi->second));
      
      #ifdef DEBUG_SANDBOX_OBJECT_MAPS
         MessageInterface::ShowMessage("Sandbox clearing %s\n",
            (omi->first).c_str());
      #endif
      
      #ifdef DEBUG_SANDBOX_CLONING
         if (find(clonable.begin(), clonable.end(),
             (omi->second)->GetType()) != clonable.end())
      #endif
      {
         #ifdef DEBUG_SANDBOX_OBJECT_MAPS
            MessageInterface::ShowMessage("Deleting '%s'\n",
               (omi->second)->GetName().c_str());
         #endif
         #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (omi->second, omi->first, "Sandbox::Clear()",
                " deleting cloned obj from objectMap");
         #endif
         delete omi->second;
         //objectMap.erase(omi);
      }
   }
   for (omi = globalObjectMap.begin(); omi != globalObjectMap.end(); omi++)
   {
      if ((omi->second)->GetType() == Gmat::SUBSCRIBER)
         publisher->Unsubscribe((Subscriber*)(omi->second));
      
      #ifdef DEBUG_SANDBOX_OBJECT_MAPS
         MessageInterface::ShowMessage("Sandbox clearing %s\n",
            (omi->first).c_str());
      #endif

      #ifdef DEBUG_SANDBOX_CLONING
         if (find(clonable.begin(), clonable.end(),
             (omi->second)->GetType()) != clonable.end())
      #endif
      {
         #ifdef DEBUG_SANDBOX_OBJECT_MAPS
            MessageInterface::ShowMessage("Deleting '%s'\n",
               (omi->second)->GetName().c_str());
         #endif
         #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (omi->second, omi->first, "Sandbox::Clear()",
                " deleting cloned obj from globalObjectMap");
         #endif
         delete omi->second;
         //objectMap.erase(omi);
      }
   }
   
   #ifdef DEBUG_MORE_MEMORY
   MessageInterface::ShowMessage
      ("--- Sandbox::Clear() deleting done\n");
   #endif
   
   // delete subscribers
   publisher = NULL;

#ifndef DISABLE_SOLAR_SYSTEM_CLONING
   if (solarSys != NULL)
   {
      #ifdef DEBUG_SS_CLONING
      MessageInterface::ShowMessage
         ("Sandbox deleting the solar system clone: %p\n", solarSys);
      #endif
      
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (solarSys, solarSys->GetName(), "Sandbox::Clear()", " deleting solarSys");
      #endif
      delete solarSys;
   }
   
   solarSys = NULL;
#endif
   
   // who deletes objects?  ConfigManager::RemoveAllItems() deleletes them
   objectMap.clear();
   globalObjectMap.clear();
   
   // who pushes forces to transientForces?
   // Should we delete transient forces here? (loj: 2008.11.03)
   for (std::vector<PhysicalModel*>::iterator tf = transientForces.begin();
        tf != transientForces.end(); ++tf)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         ((*tf), "transient force", "Sandbox::Clear()");
      #endif
      delete (*tf);
   }
   transientForces.clear();
   
   // Update the sandbox state
   if ((state != STOPPED) && (state != IDLE))
          MessageInterface::ShowMessage(
             "Unexpected state transition in the Sandbox\n");

   state     = IDLE;
}


//------------------------------------------------------------------------------
// bool AddSubscriber(Subscriber *sub)
//------------------------------------------------------------------------------
/**
 *  Add Subcribers to the Sandbox and registers them with the Publisher.
 *
 *  @param <sub> The subscriber.
 *
 *  @return true if the Subscriber was registered.
 */
//------------------------------------------------------------------------------
bool Sandbox::AddSubscriber(Subscriber *sub)
{
   // add subscriber to sandbox by AddObject() so that cloned subscribers
   // can be deleted when clear (loj: 2008.11.06)
   Subscriber *newSub = (Subscriber*)AddObject(sub);
   if (newSub != NULL)
   {
      publisher->Subscribe(newSub);
      return true;
   }
   
   return false;
}


//------------------------------------------------------------------------------
// GmatBase* Sandbox::FindObject(const std::string &name)
//------------------------------------------------------------------------------
/**
 *  Finds an object by name, searching through the SandboxObjectMap first,
 *  then the GlobalObjectMap
 *
 *  @param <spname> The name of the SpacePoint.
 *
 *  @return A pointer to the SpacePoint, or NULL if it does not exist in the
 *          Sandbox.
 */
//------------------------------------------------------------------------------
GmatBase* Sandbox::FindObject(const std::string &name)
{
   if (objectMap.find(name) == objectMap.end())
   {
     // If not found in the LOS, check the Global Object Store (GOS)
      if (globalObjectMap.find(name) == globalObjectMap.end())
         return NULL;
      else return globalObjectMap[name];
   }
   else
      return objectMap[name];
}

//------------------------------------------------------------------------------
// bool Sandbox::SetObjectByNameInMap(const std::string &name,
//                                    GmatBase *obj)
//------------------------------------------------------------------------------
/**
 *  Sets the object pointer for the given name in the object map(s).  NOTE that
 *  an object should only exist in one of the object maps, so both IFs should
 *  not evaluate to TRUE.
 *
 *  @param <name> The name of the object.
 *  @param <obj>  The object pointer.
 *
 *  @return true if successful; flase otherwise
 */
//------------------------------------------------------------------------------
bool Sandbox::SetObjectByNameInMap(const std::string &name,
                                   GmatBase *obj)
{
   #ifdef DEBUG_SANDBOX_OBJECT_MAPS
   MessageInterface::ShowMessage
      ("Sandbox::SetObjectByNameInMap() name = %s\n",
       name.c_str());
   #endif
   bool found = false;
   // if it's already in a map, set the object pointer for the name
   if (objectMap.find(name) != objectMap.end())
   {
      objectMap[name] = obj;
      #ifdef DEBUG_SANDBOX_OBJECT_MAPS
      MessageInterface::ShowMessage
         ("Sandbox::SetObjectByNameInMap() set object name = %s in objectMap\n",
          name.c_str());
      #endif
      found = true;
   }
   if (globalObjectMap.find(name) != globalObjectMap.end())
   {
      globalObjectMap[name] = obj;
      #ifdef DEBUG_SANDBOX_OBJECT_MAPS
      MessageInterface::ShowMessage
         ("Sandbox::SetObjectByNameInMap() set object name = %s in globalObjectMap\n",
          name.c_str());
      #endif      
      found = true;
   }
   // if not already in the map, add it to the objectMap
   // (globals are added to the globalObjectMap later)
   if (!found)
      objectMap.insert(std::make_pair(name,obj));
   
   #ifdef DEBUG_SANDBOX_OBJECT_MAPS
   MessageInterface::ShowMessage
      ("Sandbox::SetObjectByNameInMap() returning found = %s\n",
       (found? "TRUE" : "FALSE"));
   #endif   
   return found;
}

//------------------------------------------------------------------------------
// bool Sandbox::HandleGmatFunction(GmatCommand *cmd,
//                                  std::map<std::string, GmatBase *> *usingMap)
//------------------------------------------------------------------------------
/**
 *  Handles any GmatFunctions included in the sequence.  The input cmd is the 
 *  CallFunction or Assignment command to process - it may itself contain a nested
 *  GmatFunction.  If it does, this method willbe called recursively to process
 *  the nested GmatFunctions.
 *
 *  @param <name>     The cmd.
 *  @param <usingMap> The map to send to the Interpreter (via the Moderator).
 *
 *  @return true if successful; flase otherwise
 */
//------------------------------------------------------------------------------
bool Sandbox::HandleGmatFunction(GmatCommand *cmd,
              std::map<std::string, GmatBase *> *usingMap)
{
   #ifdef DEBUG_SANDBOX_GMATFUNCTION
      MessageInterface::ShowMessage(
         "Now entering HandleGmatFunction with command of type %s, '%s'\n",
         (cmd->GetTypeName()).c_str(), cmd->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
   #endif
      
   bool OK = false;
   GmatGlobal *global = GmatGlobal::Instance();
   std::string matlabExt = global->GetMatlabFuncNameExt();
   StringArray gfList;
   bool        isMatlabFunction = false;
   if (cmd->GetTypeName() == "CallFunction") 
   {
      std::string cfName = cmd->GetStringParameter("FunctionName");
      gfList.push_back(cfName);
   }
   else if (cmd->IsOfType("Assignment"))  
   {
      gfList = ((Assignment*) cmd)->GetGmatFunctionNames();
   }
   
   #ifdef DEBUG_SANDBOX_GMATFUNCTION
   MessageInterface::ShowMessage("   Has %d GmatFunctions\n", gfList.size());
   #endif
   
   for (unsigned int ii = 0; ii < gfList.size(); ii++)
   {
      std::string fName = gfList.at(ii);
      Function *f;
      isMatlabFunction = false;
      // if it's a Matlab Function, remove the extension from the name before looking in the GOS
      // (as Matlab function names are placed into the GOS without the extension)
      if (fName.find(matlabExt) != fName.npos)
      {
         fName = fName.substr(0, fName.find(matlabExt));
         #ifdef DEBUG_SANDBOX_GMATFUNCTION
         MessageInterface::ShowMessage
            ("   actual matlab function name='%s'\n", fName.c_str());
         #endif
         isMatlabFunction = true;
      }
      #ifdef DEBUG_SANDBOX_GMATFUNCTION
         MessageInterface::ShowMessage("Now searching GOS for object %s\n",
            (gfList.at(ii)).c_str());
      #endif
      // if there is not already a function of that name, create it
      if (globalObjectMap.find(fName) == globalObjectMap.end())
      {
         if (isMatlabFunction)
            f = moderator->CreateFunction("MatlabFunction",fName, 0);
         else
            f = moderator->CreateFunction("GmatFunction",fName, 0);
         if (!f) 
            throw SandboxException("Sandbox::HandleGmatFunction - error creating new function\n");
         globalObjectMap.insert(std::make_pair(fName,f));
      }
      else // it's already in the GOS, so just grab it
         f = (Function*) globalObjectMap[fName];

      if (cmd->GetTypeName() == "CallFunction")  
      {
         ((CallFunction*)cmd)->SetRefObject(f,Gmat::FUNCTION,fName);
         cmd->SetStringParameter("FunctionName", fName);
      }
      else if (cmd->IsOfType("Assignment"))
         ((Assignment*) cmd)->SetFunction(f);
      
      #ifdef DEBUG_SANDBOX_GMATFUNCTION
      MessageInterface::ShowMessage(
         "Now handling function \"%s\", whose fcs is %s set, ",
         (f->GetStringParameter("FunctionName")).c_str(), 
         ((f->IsFunctionControlSequenceSet())? "already" : "NOT"));
      MessageInterface::ShowMessage
         ("script errors were %sfound.\n", f->ScriptErrorFound() ? "" : "not ");
      #endif
      
      // if function is GmatFunction and no FCS has built and no script error found,
      // build FCS
      if ((f->GetTypeName() == "GmatFunction") &&
          (!(f->IsFunctionControlSequenceSet())) &&
          (!(f->ScriptErrorFound())))
      {
         #ifdef DEBUG_SANDBOX_GMATFUNCTION
         MessageInterface::ShowMessage(
            "About to call InterpretGmatFunction for function %s\n",
            (f->GetStringParameter("FunctionName")).c_str());
         #endif
         GmatCommand* fcs = moderator->InterpretGmatFunction(f, usingMap, solarSys);
         if (fcs == NULL)
            throw SandboxException("Sandbox::HandleGmatFunction - error creating FCS\n");
         
         f->SetFunctionControlSequence(fcs);
         GmatCommand* fcsCmd = fcs;
         while (fcsCmd)
         {
            #ifdef DISALLOW_NESTED_GMAT_FUNCTIONS
            if (fcsCmd->HasAFunction())
            {
               std::string errMsg = "Sandbox::HandleGmatFunction (";
               errMsg += fName + ") - nested or recursive GmatFunctions not yet supported.\n";
               throw SandboxException(errMsg);
            }
            #endif
            if ((fcsCmd->GetTypeName() == "CallFunction") ||
                (fcsCmd->IsOfType("Assignment")))
            {
               #ifdef DEBUG_SANDBOX_GMATFUNCTION
               MessageInterface::ShowMessage(
                  "CallFunction or Assignment (%s)'%s' detected in FCS... now processing\n",
                  (fcsCmd->GetTypeName()).c_str(),
                  fcsCmd->GetGeneratingString(Gmat::NO_COMMENTS).c_str());
               #endif
               
               //Let's handle GmatFunction first
               OK += HandleGmatFunction(fcsCmd, &globalObjectMap);
               // do not set the non-global object map here; it will need to be
               // setup y the FunctionManager at execution
               fcsCmd->SetGlobalObjectMap(&globalObjectMap);
               fcsCmd->SetSolarSystem(solarSys);
               fcsCmd->SetTransientForces(&transientForces);
               if (fcsCmd->GetTypeName() == "CallFunction") 
                  ((CallFunction *)fcsCmd)->SetInternalCoordSystem(internalCoordSys);
            }
            if (fcsCmd->IsOfType("BranchCommand"))
            {
               std::vector<GmatCommand*> cmdList = ((BranchCommand*) fcsCmd)->GetCommandsWithGmatFunctions();
               Integer sz = (Integer) cmdList.size();
               for (Integer jj = 0; jj < sz; jj++)
               {
                  HandleGmatFunction(cmdList.at(jj), &globalObjectMap);
                  if ((cmdList.at(jj))->GetTypeName() == "CallFunction") 
                     ((CallFunction *)cmdList.at(jj))->SetInternalCoordSystem(internalCoordSys);
               }
            }
            fcsCmd = fcsCmd->GetNext();
         }
      }
   }
   return OK;
}
