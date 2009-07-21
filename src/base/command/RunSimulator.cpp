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

RunSimulator::RunSimulator() :
   RunSolver("RunSimulator")
{

}

RunSimulator::~RunSimulator()
{

}

RunSimulator::RunSimulator(const RunSimulator & rs) :
   RunSolver(rs)
{
}

RunSimulator & RunSimulator::operator=(const RunSimulator & rs)
{
   if (&rs != this)
   {

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
   return false;
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
            PrepareToEstimate();
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



// Methods triggered by the finite state machine


void RunSimulator::PrepareToEstimate()
{
   // This is like "PrepareToPropagate()" in the Propagate command, but simpler
   // because we don't need to worry about stopping conditions and lots of
   // potential propagation methods
}

void RunSimulator::Propagate()
{
   // Here we get the prop step from the simulator and call Step on the
   // propagator.  We might need to put in interrupt processing here so the
   // user can pause or stop the simulation with the toolbar buttons.
}

void RunSimulator::Calculate()
{
   // We might not need anything here -- it's all Simulator side work
}

void RunSimulator::LocateEvent()
{
   // We'll figure this out later
}

void RunSimulator::Simulate()
{
   // We might not need anything here -- it's all Simulator side work
}

void RunSimulator::Finalize()
{
   // Do cleanup here
}
