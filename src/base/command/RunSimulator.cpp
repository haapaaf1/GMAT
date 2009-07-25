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


#include "RunSimulator.hpp"

#include "MessageInterface.hpp"

#define DEBUG_INITIALIZATION
#define DEBUG_SIMULATOR_EXECUTION

//#define USE_HACK

RunSimulator::RunSimulator() :
   RunSolver               ("RunSimulator"),
   theSimulator            (NULL),
   commandRunning          (false),
   commandComplete         (false)
{

}

RunSimulator::~RunSimulator()
{

}

RunSimulator::RunSimulator(const RunSimulator & rs) :
   RunSolver               (rs),
   theSimulator            (NULL),
   commandRunning          (false),
   commandComplete         (false)

{
}

RunSimulator & RunSimulator::operator=(const RunSimulator & rs)
{
   if (&rs != this)
   {
      theSimulator    = NULL;
      commandRunning  = false;
      commandComplete = false;
   }

   return *this;
}

GmatBase *RunSimulator::Clone() const
{
   return new RunSimulator(*this);
}


//------------------------------------------------------------------------------
// std::string GetRefObjectName(const Gmat::ObjectType type) const
//------------------------------------------------------------------------------
/**
 * Accesses names for referenced objects.
 *
 * @param <type> Type of object requested.
 *
 * @return the referenced object's name.
 */
//------------------------------------------------------------------------------
std::string RunSimulator::GetRefObjectName(const Gmat::ObjectType type) const
{
   switch (type)
   {
      case Gmat::SOLVER:
         #ifdef DEBUG_RUN_SIMULATOR
            MessageInterface::ShowMessage
               ("Getting EndFiniteBurn reference burn names\n");
         #endif
         return solverName;

      default:
         ;
   }

   return RunSolver::GetRefObjectName(type);
}



//------------------------------------------------------------------------------
// bool SetRefObjectName(const Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Sets names for referenced objects.
 *
 * @param <type> Type of the object.
 * @param <name> Name of the object.
 *
 * @return true if the name was set, false if not.
 */
//------------------------------------------------------------------------------
bool RunSimulator::SetRefObjectName(const Gmat::ObjectType type,
                                     const std::string &name)
{
   if (type == Gmat::SOLVER)
   {
      solverName = name;
      return true;
   }

   return RunSolver::SetRefObjectName(type, name);
}



//------------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//------------------------------------------------------------------------------
/**
 * Renames referenced objects.
 *
 * @param type Type of the object that is renamed.
 * @param oldName The current name for the object.
 * @param newName The name the object has when this operation is complete.
 *
 * @return true on success.
 */
//------------------------------------------------------------------------------
bool RunSimulator::RenameRefObject(const Gmat::ObjectType type,
                                    const std::string &oldName,
                                    const std::string &newName)
{
   // EndFiniteBurn needs to know about Burn and Spacecraft only
   if (type != Gmat::SOLVER)
      return RunSolver::RenameRefObject(type, oldName, newName);

   if (solverName == oldName)
   {
      solverName = newName;
      return true;
   }

   return false;
}


//------------------------------------------------------------------------------
//  const std::string GetGeneratingString()
//------------------------------------------------------------------------------
/**
 * Method used to retrieve the string that was parsed to build this GmatCommand.
 *
 * This method is used to retrieve the GmatCommand string from the script that
 * was parsed to build the GmatCommand.  It is used to save the script line, so
 * that the script can be written to a file without inverting the steps taken to
 * set up the internal object data.  As a side benefit, the script line is
 * available in the GmatCommand structure for debugging purposes.
 *
 * @param <mode>    Specifies the type of serialization requested.
 * @param <prefix>  Optional prefix appended to the object's name. (Used for
 *                  indentation)
 * @param <useName> Name that replaces the object's name (Not yet used
 *                  in commands).
 *
 * @return The script line that defines this GmatCommand.
 */
//------------------------------------------------------------------------------
const std::string& RunSimulator::GetGeneratingString(Gmat::WriteMode mode,
                                                    const std::string &prefix,
                                                    const std::string &useName)
{
   generatingString = prefix + "RunSimulator " + solverName + ";";

   return RunSolver::GetGeneratingString(mode, prefix, useName);
}


bool RunSimulator::Initialize()
{
   bool retval = false;

   // First set the simulator object
   if (solverName == "")
      throw CommandException("Cannot initialize RunSimulator command -- the "
            "simulator name is not specified.");

   // Clear the old clone if it was set
   if (theSimulator != NULL)
      delete theSimulator;

   if (objectMap->find(solverName) != objectMap->end())
   {
      theSimulator = (Simulator*)((*objectMap)[solverName]->Clone());
   }
   else if (globalObjectMap->find(solverName) != objectMap->end())
   {
      theSimulator = (Simulator*)((*globalObjectMap)[solverName]->Clone());
   }
   else
      throw CommandException("Cannot initialize RunSimulator command -- the "
            "simulator named " + solverName + " cannot be found.");

   // Next comes the propagator
   PropSetup *obj = theSimulator->GetPropagator();

   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("Propagator at address %p ", obj);
      if (obj != NULL)
         MessageInterface::ShowMessage("is named %s\n",
               obj->GetName().c_str());
      else
         MessageInterface::ShowMessage("is not yet set\n");
   #endif

   #ifdef USE_HACK
      MessageInterface::ShowMessage("WARNING!!!  Running with a code hack in "
            "place -- NOT production code!!!\n", obj);
      if (obj == NULL)
      {
         if (objectMap->find("ODProp") != objectMap->end())
         {
            obj = (PropSetup*)((*objectMap)["ODProp"]->Clone());
         }
         else if (globalObjectMap->find("ODProp") != objectMap->end())
         {
            obj = (PropSetup*)((*globalObjectMap)["ODProp"]->Clone());
         }
      }
   #endif

   if (obj != NULL)
   {
      if (obj->IsOfType(Gmat::PROP_SETUP))
      {
         PropSetup *ps = (PropSetup*)obj->Clone();

         // RunSimulator only manages one PropSetup.  If that changes, so
         // does this code
         if (propagators.size() > 0)
         {
            for (std::vector<PropSetup*>::iterator p = propagators.begin();
                  p != propagators.end(); ++p)
            {
               delete (*p);
            }
            propagators.clear();
         }
         propagators.push_back(ps);
         retval = true;
      }
   }
   else
      throw CommandException("Cannot initialize RunSimulator command; the "
            "propagator pointer in the Simulator " +
            theSimulator->GetName() + " is NULL.");

   // Now set the participant list
   MeasurementManager *mm = theSimulator->GetMeasurementManager();
   StringArray participants = mm->GetParticipantList();

   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("RunSimulator command found %d "
            "participants\n", participants.size());
   #endif

   propObjectNames.clear();
   propObjectNames.push_back(participants);

   // Now we can initialize the propagation subsystem by calling up the
   // inheritance tree.
   retval = RunSolver::Initialize();

   #ifdef DEBUG_INITIALIZATION
      if (retval == false)
         MessageInterface::ShowMessage("RunSimulator command failed to "
               "initialize; RunSolver::Initialize() call failed.\n");
   #endif

   return retval;
}


bool RunSimulator::Execute()
{
   #ifdef DEBUG_SIMULATOR_EXECUTION
      MessageInterface::ShowMessage("The \"%s\" command is running...\n",
            GetTypeName().c_str());
   #endif

   // Here we should check to see if the command is currently propagating and
   // finish that first...

   // Respond to the state in the state machine
   Solver::SolverState state = theSimulator->GetState();
   while (state != Solver::FINISHED)
   {
      switch (state)
      {
         case Solver::INITIALIZING:
            PrepareToSimulate();
            break;

         case Solver::PROPAGATING:
            Propagate();
            break;

         case Solver::CALCULATING:
            Calculate();
            break;

         case Solver::LOCATING:
            // The LOCATING state shouldn't trigger until we have event location
            // implemented, so this case should not fire.
            LocateEvent();
            break;

         case Solver::SIMULATING:
            Simulate();
            break;

         case Solver::FINISHED:
            Finalize();
            break;

         default:
            break;
      }

      state = theSimulator->AdvanceState();
   }

   return true;
}


GmatCommand* RunSimulator::GetNext()
{
   if (commandRunning)
      return this;
   return next;
}


// Methods triggered by the finite state machine


void RunSimulator::PrepareToSimulate()
{
#ifdef DEBUG_SIMULATOR_EXECUTION
   MessageInterface::ShowMessage("Entered RunSimulator::PrepareToSimulate()\n");
#endif
   // This is like "PrepareToPropagate()" in the Propagate command, but simpler
   // because we don't need to worry about stopping conditions and lots of
   // potential propagation methods

   commandRunning  = true;
   commandComplete = false;
}

void RunSimulator::Propagate()
{
#ifdef DEBUG_SIMULATOR_EXECUTION
   MessageInterface::ShowMessage("Entered RunSimulator::Propagate()\n");
#endif
   Real dt = theSimulator->GetTimeStep();
   Step(dt);


}

void RunSimulator::Calculate()
{
#ifdef DEBUG_SIMULATOR_EXECUTION
   MessageInterface::ShowMessage("Entered RunSimulator::Calculate()\n");
#endif
   // We might not need anything here -- it's all Simulator side work
}

void RunSimulator::LocateEvent()
{
#ifdef DEBUG_SIMULATOR_EXECUTION
   MessageInterface::ShowMessage("Entered RunSimulator::LocateEvent()\n");
#endif
   // We'll figure this out later
}

void RunSimulator::Simulate()
{
#ifdef DEBUG_SIMULATOR_EXECUTION
   MessageInterface::ShowMessage("Entered RunSimulator::Simulate()\n");
#endif
   // We might not need anything here -- it's all Simulator side work
}

void RunSimulator::Finalize()
{
#ifdef DEBUG_SIMULATOR_EXECUTION
   MessageInterface::ShowMessage("Entered RunSimulator::Finalize()\n");
#endif
   // Do cleanup here
   commandComplete = true;
   commandRunning  = false;
}
