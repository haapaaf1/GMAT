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
#include "SolverException.hpp"

//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

const std::string
Simulator::PARAMETER_TEXT[SimulatorParamCount -SolverParamCount] =
{
   "Measurements",
   "Propagator",
   "InitialEpochFormat",
   "InitialEpoch",
   "FinalEpochFormat",
   "FinalEpoch",
   "MeasurementTimeStep",
};

const Gmat::ParameterType
Simulator::PARAMETER_TYPE[SimulatorParamCount - SolverParamCount] =
{
   Gmat::OBJECTARRAY_TYPE,
   Gmat::STRING_TYPE,
   Gmat::ENUMERATION_TYPE,
   Gmat::STRING_TYPE,
   Gmat::ENUMERATION_TYPE,
   Gmat::STRING_TYPE,
   Gmat::REAL_TYPE,
};

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------


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



void Simulator::WriteToTextFile(Solver::SolverState)
{
   // This is where the simulator text file gets written
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
            MessageInterface::ShowMessage("Entered Simulator state machine: INITIALIZING\n");
         #endif
         ReportProgress();
         CompleteInitialization();
         break;

      case PROPAGATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered Simulator state machine: PROPAGATING\n");
         #endif
         ReportProgress();
         FindTimeStep();
         break;

      case CALCULATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered Simulator state machine: SIMULATING\n");
         #endif
         ReportProgress();
         CalculateData();
         break;

// Placeholder for event location
//      case LOCATING:
//         #ifdef DEBUG_STATE_MACHINE
//            MessageInterface::ShowMessage("Entered Simulator state machine: LOCATING\n");
//         #endif
//         ReportProgress();
//         ProcessEvent();
//         break;

      case SIMULATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered Simulator state machine: CALCULATING\n");
         #endif
         ReportProgress();
         SimulateData();
         break;

      case FINISHED:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered Simulator state machine: FINISHED\n");
         #endif
         RunComplete();
         ReportProgress();
         break;

      default:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered Simulator state machine: "
               "Bad state for a differential corrector.\n");
         #endif
         /* throw EstimatorException("Solver state not supported for the simulator")*/;
   }

   return currentState;

}

bool Simulator::Finalize()
{
   return false;
}


Real Simulator::GetTimeStep()
{
   return timestep;
}

//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string Simulator::GetParameterText(const Integer id) const
{
   if (id >= SolverParamCount && id < SimulatorParamCount)
      return PARAMETER_TEXT[id - SolverParamCount];
   return Solver::GetParameterText(id);
}

//---------------------------------------------------------------------------
//  std::string GetParameterUnit(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the unit for the parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return unit for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string Simulator::GetParameterUnit(const Integer id) const
{
   return Solver::GetParameterUnit(id); // TBD
}

//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Integer     Simulator::GetParameterID(const std::string &str) const
{
   for (Integer i = SolverParamCount; i < SimulatorParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - SolverParamCount])
         return i;
   }
   
   return Solver::GetParameterID(str);
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Simulator::GetParameterType(const Integer id) const
{
   if (id >= SolverParamCount && id < SimulatorParamCount)
      return PARAMETER_TYPE[id - SolverParamCount];
      
   return Solver::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string Simulator::GetParameterTypeString(const Integer id) const
{
   return Solver::PARAM_TYPE_STRING[GetParameterType(id)];
}

//------------------------------------------------------------------------------
//  Real  GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the Real parameter value, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter value.
 *
 * @return  Real value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Real        Simulator::GetRealParameter(const Integer id) const
{
   if (id == MEASUREMENT_TIME_STEP)               return timestep;

   return Solver::GetRealParameter(id);
}

//------------------------------------------------------------------------------
//  Real  SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * This method sets the Real parameter value, given the input parameter ID.
 *
 * @param <id> ID for the parameter whose value to change.
 * @param <value> value for the parameter.
 *
 * @return  Real value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Real Simulator::SetRealParameter(const Integer id, const Real value)
{
   if (id == MEASUREMENT_TIME_STEP)
   {
      timestep = value;
      return true;
   }
   
   return Solver::SetRealParameter(id, value);
}

//------------------------------------------------------------------------------
//  std::string  GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  string value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string Simulator::GetStringParameter(const Integer id) const
{
   if (id == PROPAGATOR)             return propagatorName;
   if (id == INITIAL_EPOCH_FORMAT)   return initialEpochFormat;
   if (id == INITIAL_EPOCH)          return initialEpoch;
   if (id == FINAL_EPOCH_FORMAT)     return finalEpochFormat;
   if (id == FINAL_EPOCH)            return finalEpoch;
   
   return Solver::GetStringParameter(id);
}

std::string Simulator::GetStringParameter(const Integer id,
                                          const Integer index) const
{   
   if (id == MEASUREMENTS)
   {
      if (index < 0 || (index >= (Integer) (measList.size())))
      {
         std::string errmsg = "Simulator::GetStringParameter - Index into measurement names ";
         errmsg += instanceName + " is out of range.\n";
         throw SolverException(errmsg);
      }
      return measList.at(index);
   }
   return Solver::GetStringParameter(id, index);
}


//------------------------------------------------------------------------------
//  bool  SetStringParameter(const Integer id, const std::string value)
//------------------------------------------------------------------------------
/**
 * This method sets the string parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 * @param <value> string value for the requested parameter.
 *
 * @exception <SolarSystemException> thrown if value is out of range
 *
 * @return  success flag.
 *
 */
//------------------------------------------------------------------------------
bool Simulator::SetStringParameter(const Integer id,
                                   const std::string &value) // const?
{
   if (id == PROPAGATOR)
   {
      propagatorName = value;  // get propSetup here???
      return true;
   }
   if (id == INITIAL_EPOCH_FORMAT)
   {
      initialEpochFormat = value;
      return true;
   }
   if (id == INITIAL_EPOCH)
   {
      initialEpoch = value;
      return true;
   }
   if (id == FINAL_EPOCH_FORMAT)
   {
      finalEpochFormat = value;
      return true;
   }
   if (id == FINAL_EPOCH)   
   {
      finalEpoch = value;
      return true;
   }

   return Solver::SetStringParameter(id, value);
}
   
bool Simulator::SetStringParameter(const Integer id, const std::string &value,
                                   const Integer index)
{
   if (id == MEASUREMENTS)
   {
      if (index < 0 || index >= (Integer) measList.size())
      {
            std::string errmsg = "CelestialBody::GetStringParameter - Index into measurement names ";
            errmsg += instanceName + " is out of range.\n";
            throw SolverException(errmsg);
      }
      measList.at(index) = value;
      return true;
   }
   return Solver::SetStringParameter(id, value, index);
}

//------------------------------------------------------------------------------
//  const StringArray&   GetStringArrayParameter((const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the StringArray parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  StringArray value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
const StringArray& Simulator::GetStringArrayParameter(const Integer id) const
{
   if (id == MEASUREMENTS)
      return measList; // temporary
   return Solver::GetStringArrayParameter(id);
}



//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------

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


// This might become more complicated down the road
void Simulator::FindNextSimulationEpoch()
{
   nextSimulationEpoch = currentEpoch + simulationStep;
}


void Simulator::ReportProgress()
{
   // Writes to the message window and sends data to WriteToTextFile().
}

//------------------------------------------------------------------------------
// unused methods
//------------------------------------------------------------------------------

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



