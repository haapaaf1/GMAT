//$Id$
//------------------------------------------------------------------------------
//                                 Sandbox
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
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
 * Definition for the GMAT Sandbox class
 */
//------------------------------------------------------------------------------


#ifndef Sandbox_hpp
#define Sandbox_hpp

#include "gmatdefs.hpp"

// Core classes
#include "GmatBase.hpp"
#include "GmatCommand.hpp"
#include "Publisher.hpp"

#include "Spacecraft.hpp"
#include "PropSetup.hpp"
#include "Subscriber.hpp"
#include "Parameter.hpp"            // Remove when temporary code satisfied
#include "SolarSystem.hpp"
#include "CoordinateSystem.hpp"
#include "Burn.hpp"
#include "GmatFunction.hpp"
#include "ObjectInitializer.hpp"


//#define DEBUG_SANDBOX_CLONING


class Moderator;        // Forward reference for the moderator pointer

class GMAT_API Sandbox
{
public:
   Sandbox();
   ~Sandbox();
   
   // Setup methods
   GmatBase* AddObject(GmatBase *obj);   
   bool AddCommand(GmatCommand *cmd);
   bool AddSolarSystem(SolarSystem *ss);
   bool AddTriggerManagers(const std::vector<TriggerManager*> *trigs);
   bool AddSubscriber(Subscriber *sub);
   bool SetInternalCoordSystem(CoordinateSystem *ss);
   bool SetPublisher(Publisher *pub = NULL);
   
   GmatBase* GetInternalObject(std::string name,
                               Gmat::ObjectType type = Gmat::UNKNOWN_OBJECT);
   
   // Execution methods
   bool Initialize();
   bool Execute();
   bool Interrupt();
   void Clear();

protected:
    
private:
   /// Enumerated values for the current Sandbox state
   enum runMode
   {
      IDLE = 7001,
      INITIALIZED,
      RUNNING,
      PAUSED,
      STOPPED,
      RESET
   };
    
   /// Object store for this run
   std::map<std::string, GmatBase *> objectMap;
   /// Global object store for this run
   std::map<std::string, GmatBase *> globalObjectMap;
   /// Combined object store for passing to interpreter (via Moderator)
   std::map<std::string, GmatBase *> combinedObjectMap;
   /// Solar System model for this Sandbox
   SolarSystem                       *solarSys;
   /// CoordinateSystem used internally
   CoordinateSystem                  *internalCoordSys;
   /// GMAT Publisher singleton
   Publisher                         *publisher;
   /// Command sequence for the run
   GmatCommand                       *sequence;
   /// The executing Command
   GmatCommand                       *current;
   /// Pointer to the moderator, used to check Pause/Stop status
   Moderator                         *moderator;
   /// Execution mode for the sandbox
   runMode                           state;
   /// Counter for interrupt polling
   Integer                           interruptCount;
   /// Polling frequency
   Integer                           pollFrequency;
   /// The object initializer
   ObjectInitializer                 *objInit;
   
   // Additions made for finite maneuvers
   /// List of FiniteThrust objects that are currently available
   std::vector<PhysicalModel *>      transientForces;
   /// Trigger managers for this Sandbox
   std::vector<TriggerManager*>      triggerManagers;

   Sandbox(const Sandbox&);
   Sandbox& operator=(const Sandbox&);
   
   GmatBase* FindObject(const std::string &name);
   bool      SetObjectByNameInMap(const std::string &name, GmatBase *obj);
   bool      HandleGmatFunction(GmatCommand *cmd,
                                std::map<std::string, GmatBase *> *usingMap);
   void      SetGlobalRefObject(GmatCommand *cmd);
   void      ShowObjectMap(ObjectMap &om, const std::string &title);
   
   #ifdef DEBUG_SANDBOX_CLONING
      std::vector<Gmat::ObjectType>  clonable;
   #endif
};

#endif
