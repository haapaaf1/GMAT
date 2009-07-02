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


#include "Simulator.hpp"

Simulator::Simulator(const std::string &name) :
   Solver         ("Simulator", name)
{
   // TODO Auto-generated constructor stub

}

Simulator::~Simulator()
{
   // TODO Auto-generated destructor stub
}

Simulator* Simulator::operator =(const Simulator & sim)
{
   if (&sim != this)
   {

   }
   
   return this;
}



Simulator::Simulator(const Simulator & sim) :
   Solver         (sim)
{
}


GmatBase* Simulator::Clone() const
{
   return new Simulator(*this);
}


bool Simulator::Finalize()
{
   return false;
}



bool Simulator::Initialize()
{
   return false;
}



Solver::SolverState Simulator::AdvanceState()
{
   switch (currentState)
   {
      case INITIALIZING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; INITIALIZING\n");
         #endif
         ReportProgress();
         CompleteInitialization();
         break;

      case PROPAGATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; PROPAGATING\n");
         #endif
         ReportProgress();
         FindTimeStep();
         break;

      case CALCULATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; SIMULATING\n");
         #endif
         ReportProgress();
         CalculateData();
         break;

// Placeholder for event location
//      case LOCATING:
//         #ifdef DEBUG_STATE_MACHINE
//            MessageInterface::ShowMessage("Entered state machine; LOCATING\n");
//         #endif
//         ReportProgress();
//         ProcessEvent();
//         break;

      case SIMULATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; CALCULATING\n");
         #endif
         ReportProgress();
         SimulateData();
         break;

      case FINISHED:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; FINISHED\n");
         #endif
         RunComplete();
         ReportProgress();
         break;

      default:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
               "Bad state for a differential corrector.\n");
         #endif
         /* throw EstimatorException("Solver state not supported for the simulator")*/;
   }

   return currentState;

}


void Simulator::CompleteInitialization()
{
   // Initialize all here; set up the MeasurementManager and get simState from it

   currentEpoch = simState->GetEpoch();
   nextSimulationEpoch = simulationStart;

   if (currentEpoch == nextSimulationEpoch)
      currentState = SIMULATING;
   else
   {
      timestep = (nextSimulationEpoch - currentEpoch) * 86400.0;
      currentState = PROPAGATING;
   }
}


void Simulator::FindTimeStep()
{
   if (currentEpoch > simulationEnd)
   {
      currentState = FINISHED;
   }
   else if (currentEpoch == nextSimulationEpoch)
   {
      currentState = SIMULATING;
   }
   else
   {
      // Calculate the time step in seconds and stay in the PROPAGATING state
      timestep = (nextSimulationEpoch - currentEpoch) * 86400.0;
   }
}


void Simulator::CalculateData()
{
   // Tell the measurement manager to calculate the simulation data
   if (measManager.CalculateMeasurements() == false)
   {
      // No measurements were possible
      FindNextSimulationEpoch();

      if ((currentEpoch < simulationEnd) && (nextSimulationEpoch < simulationEnd))
         currentState = PROPAGATING;
      else
         currentState = FINISHED;
   }
   else
   {
      // Measurements are possible!

// We'll uncomment this and fix it when event location is ready
//      if (there are events)
//         currentState = LOCATING;
//      else
         currentState = SIMULATING;
   }
}


// Placeholder for the event calculation code
// void Simulator::ProcessEvent()
//{
//
//}


void Simulator::SimulateData()
{
   // Tell the measurement manager to add noise and write the measurements
   if (measManager.WriteMeasurements() == false)
      /*throw EstimatorException("Measurement writing failed")*/;

   // Prep for the next measurement simulation
   FindNextSimulationEpoch();

   if ((currentEpoch < simulationEnd) && (nextSimulationEpoch < simulationEnd))
      currentState = PROPAGATING;
   else
      currentState = FINISHED;
}


void Simulator::RunComplete()
{
   // Do the cleanup stuff here
}


Real Simulator::GetTimeStep()
{
   return timestep;
}

// This might become more complicated down the road
void Simulator::FindNextSimulationEpoch()
{
   nextSimulationEpoch = currentEpoch + simulationStep;
}


void Simulator::ReportProgress()
{
   // Writes to the message window and sends data to WriteToTextFile().
}


void Simulator::WriteToTextFile(Solver::SolverState)
{
   // This is where the simulator text file gets written
}


// Methods required by base classes
// Not needed for simulation
Integer Simulator::SetSolverResults(Real*, const std::string&, const std::string&)
{
   return -1;
}

// Not needed for simulation
void Simulator::SetResultValue(Integer, Real, const std::string&)
{
}
