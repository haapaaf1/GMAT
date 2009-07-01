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



bool RunSimulator::Initialize()
{
   return false;
}


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
