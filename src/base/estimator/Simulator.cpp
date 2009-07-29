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
// Author: Darrel J. Conway, Thinking Systems, Inc. & Wendy Shoan/GSFC/GSSB
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------

#include "Simulator.hpp"
#include "GmatState.hpp"
#include "PropagationStateManager.hpp"
#include "SolverException.hpp"
#include "RealUtilities.hpp"
#include "TimeTypes.hpp"
#include "MessageInterface.hpp"
#include <sstream>

#define DEBUG_STATE_MACHINE
#define DEBUG_SIMULATOR_WRITE
//#define DEBUG_SIMULATOR_INITIALIZATION

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


//------------------------------------------------------------------------------
//  Simulator()
// <default constructor>
//------------------------------------------------------------------------------
/**
 * This is the default cosntructor for the Simulator class.
 *
 */
//------------------------------------------------------------------------------
Simulator::Simulator(const std::string& name) :
   Solver              ("Simulator", name),
   propagator          (NULL),
   propagatorName      (""),
   simState            (NULL),
   simulationStart     (21545.0000000),
   simulationEnd       (21546.0000000),
   nextSimulationEpoch (21545.0000000),
   currentEpoch        (21545.0000000),
   initialEpochFormat  ("TAIModJulian"),
   initialEpoch        ("21545.0000000"),
   finalEpochFormat    ("TAIModJulian"),
   finalEpoch          ("21545.0000000"),
   simulationStep      (60.0),
   timeStep            (60.0)
{
   objectTypeNames.push_back("Simulator");
   parameterCount = SimulatorParamCount;
}

//------------------------------------------------------------------------------
//  Simulator(const Simulator &sim)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the Simulator class as a copy of the
 * specified Simulator class (copy constructor).
 *
 * @param <sim> Simulator object to copy.
 */
//------------------------------------------------------------------------------
Simulator::Simulator(const Simulator& sim) :
   Solver              (sim),
   propagatorName      (sim.propagatorName),
   simState            (NULL),   // should this be cloned?
   simulationStart     (sim.simulationStart),
   simulationEnd       (sim.simulationEnd),
   nextSimulationEpoch (sim.nextSimulationEpoch),
   currentEpoch        (sim.currentEpoch),
   initialEpochFormat  (sim.initialEpochFormat),
   initialEpoch        (sim.initialEpoch),
   finalEpochFormat    (sim.finalEpochFormat),
   finalEpoch          (sim.finalEpoch),
   simulationStep      (sim.simulationStep),
   timeStep            (sim.timeStep),
   measManager         (sim.measManager),
   measList            (sim.measList)
{  
   propagator = NULL;
   if (sim.propagator) propagator = ((PropSetup*) (sim.propagator)->Clone());
}

//------------------------------------------------------------------------------
//  Simulator& operator= (const Simulator& sim)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the Simulator class.
 *
 * @param <sim> the Simulator object whose data to assign to "this"
 *            solar system.
 *
 * @return "this" Simulator with data of input Simulator sim.
 */
//------------------------------------------------------------------------------
Simulator& Simulator::operator =(const Simulator& sim)
{
   if (&sim != this)
   {
      Solver::operator=(sim);

      if (propagator != NULL)
         delete propagator;
      if (sim.propagator) propagator          = ((PropSetup*) (sim.propagator)->Clone());
      else                propagator          = NULL;
      propagatorName      = sim.propagatorName;
      simState            = NULL;   // or clone it here??
      simulationStart     = sim.simulationStart;
      simulationEnd       = sim.simulationEnd;
      nextSimulationEpoch = sim.nextSimulationEpoch;
      currentEpoch        = sim.currentEpoch;
      initialEpochFormat  = sim.initialEpochFormat;
      initialEpoch        = sim.initialEpoch;
      finalEpochFormat    = sim.finalEpochFormat;
      finalEpoch          = sim.finalEpoch;
      simulationStep      = sim.simulationStep;
      timeStep            = sim.timeStep;
      measManager         = sim.measManager;
      measList            = sim.measList;
   }
   
   return *this;
}

//------------------------------------------------------------------------------
//  ~Simulator()
//------------------------------------------------------------------------------
/**
 * Destructor for the Simulator class.
 */
//------------------------------------------------------------------------------
Simulator::~Simulator()
{
   // do I need to delete the simState here?? TBD
   if (propagator)
      delete propagator;  // is this correct?
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Simulator.
 *
 * @return clone of the Simulator.
 */
//------------------------------------------------------------------------------
GmatBase* Simulator::Clone() const
{
   return new Simulator(*this);
}



//------------------------------------------------------------------------------
//  void WriteToTextFile(SolverState stateToUse)
//------------------------------------------------------------------------------
/**
 * Writes state data to the simulator text file.
 * 
 * @param stateToUse  sate to use for writing the information to the text file.
 */
//------------------------------------------------------------------------------
void Simulator::WriteToTextFile(SolverState stateToUse)
{
   #ifdef DEBUG_SIMULATOR_WRITE
   MessageInterface::ShowMessage
      ("Sim::WriteToTextFile() entered, stateToUse=%d, solverTextFile='%s', "
       "textFileOpen=%d, initialized=%d\n", stateToUse, solverTextFile.c_str(),
       textFile.is_open(), initialized);
   #endif
   
   if (!showProgress)
      return;
   
   if (!textFile.is_open())
      OpenSolverTextFile();
   
   StringArray::iterator current;
//   Integer i, j;
   if (initialized)
   {
      switch (currentState)
      {
      case INITIALIZING:
         // This state is basically a "paused state" used for the Target
         // command to finalize the initial data for the variables and
         // goals.  All that is written here is the header information.
         {
            textFile << "************************************************"
                     << "********\n"
                     << "*** Performing Simulation "
                     << "(using \"" << instanceName << "\")\n";

            // Write out the setup data
            textFile << "*** " ;

            // Iterate through TBD, writing them to
            // the file
//               for (current = variableNames.begin(), i = 0;
//                    current != variableNames.end(); ++current)
//               {
//                  if (current != variableNames.begin())
//                     progress << ", ";
//                  progress << *current;
//               }

            textFile << "\n****************************"
                     << "****************************";
         }
         break;

      case PROPAGATING:
         textFile << "\n";
         break;

      case CALCULATING:
         textFile << "\n";
         // Iterate through the TBD variables, writing them to the string
//            for (current = variableNames.begin(), i = 0;
//                 current != variableNames.end(); ++current)
//            {
//               if (current != variableNames.begin())
//                  progress << ", ";
//               progress << *current << " = " << variable.at(i++);
//            }
         break;

      case SIMULATING:
         // TBD
         textFile << "\n";

         break;

      case FINISHED:
         // TBD
         textFile << "\n";

         break;

      default:
         throw SolverException(
            "Solver state not supported for the simulator");
      }
   }
}

//------------------------------------------------------------------------------
//  bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the simulator - checks for unset references and does some
 * validation checking.
 */
//------------------------------------------------------------------------------
bool Simulator::Initialize()
{
   // check the validity of the input start and end times
   if (simulationEnd < simulationStart)
      throw SolverException(
            "Simulator error - simulation end time is before simulation start time.\n");
   // Check to make sure required objects have been set
   if (!propagator)
      throw SolverException(
            "Simulator error - no propagator set for simulator object.\n");
   if (measList.empty())
      throw SolverException("Simulator error - no measurements set.\n");
   
   return true;
}


//------------------------------------------------------------------------------
//  Solver::SolverState AdvanceState()
//------------------------------------------------------------------------------
/**
 * Advances the simulator to the next state.
 */
//------------------------------------------------------------------------------
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
            MessageInterface::ShowMessage("Entered Simulator state machine: CALCULATING\n");
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
            MessageInterface::ShowMessage("Entered Simulator state machine: SIMULATING\n");
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
               "Bad state for a simulator.\n");
         #endif
         /* throw EstimatorException("Solver state not supported for the simulator")*/;
   }

   return currentState;

}

//------------------------------------------------------------------------------
//  bool Finalize()
//------------------------------------------------------------------------------
/**
 * Finalizes the simulator.
 */
//------------------------------------------------------------------------------
bool Simulator::Finalize()
{
   return false;
}


//------------------------------------------------------------------------------
//  Real GetTimeStep()
//------------------------------------------------------------------------------
/**
 * Returns the time step of the simulator.
 */
//------------------------------------------------------------------------------
Real Simulator::GetTimeStep()
{
   return timeStep;
}

//------------------------------------------------------------------------------
//  Real GetPropagator()
//------------------------------------------------------------------------------
/**
 * Returns a pointer to the PropSetup object.
 */
//------------------------------------------------------------------------------
PropSetup* Simulator::GetPropagator()
{
   return propagator;
}

//------------------------------------------------------------------------------
//  MeasurementManager* GetMeasurementManager()
//------------------------------------------------------------------------------
/**
 * Returns a pointer to the MeasurmentMabager object.
 */
//------------------------------------------------------------------------------
MeasurementManager*  Simulator::GetMeasurementManager()
{
   return &measManager;
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
   if (id == MEASUREMENT_TIME_STEP)               return timeStep;

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
      timeStep = value;
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
      StringArray measList = measManager.GetMeasurementNames();
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
//  bool  SetStringParameter(const Integer id, const std::string &value)
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
   
//------------------------------------------------------------------------------
//  bool  SetStringParameter(const Integer id, const std::string &value,
//                           const Integer index)
//------------------------------------------------------------------------------
/**
 * This method sets the string parameter value, given the input
 * parameter ID and the index.
 *
 * @param <id> ID for the requested parameter.
 * @param <value> string value for the requested parameter.
 * @param <index> index into the StringArray.
 *
 * @exception <SolverException> thrown if value is out of range
 *
 * @return  success flag.
 *
 */
//------------------------------------------------------------------------------
bool Simulator::SetStringParameter(const Integer id, const std::string &value,
                                   const Integer index)
{
   if (id == MEASUREMENTS)
   {
      Integer sz = (Integer) measList.size();
      if (index == sz) // needs to be added to the end of the list
      {
         measList.push_back(value);
         measManager.AddMeasurementName(value);
      }
      else if ((index) < 0 || (index > sz)) // out of bounds
      {
         std::string errmsg = "Simulator::SetStringParameter error - index into measurement ";
         errmsg += "array is out of bounds.\n";
         throw SolverException(errmsg);
      }
      else // is in bounds 
      {
         measList.at(index) = value;
         measManager.AddMeasurementName(value);
      }
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
   {
      return measList; // temporary
   }
   return Solver::GetStringArrayParameter(id);
}



//------------------------------------------------------------------------------
// bool RenameRefObject(const Gmat::ObjectType type,
//------------------------------------------------------------------------------
/**
 * Renames references objects
 *
 * @param type The type of object that is renamed
 * @param oldName The name of the object that is changing
 * @param newName the new object name
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Simulator::RenameRefObject(const Gmat::ObjectType type,
      const std::string & oldName, const std::string & newName)
{
   /// @todo Simulator rename code needs to be implemented
   return Solver::RenameRefObject(type, oldName, newName);
}

bool Simulator::SetRefObjectName(const Gmat::ObjectType type, const std::string & name)
{
   return Solver::SetRefObjectName(type, name);
}

const ObjectTypeArray & Simulator::GetRefObjectTypeArray()
{
   return Solver::GetRefObjectTypeArray();
}

//------------------------------------------------------------------------------
// const StringArray& MeasurementModel::GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Initialization method that identifies the reference objects needed
 *
 * @param type The ObjectType for the references; UNKNOWN_OBJECT retrieves all
 *
 * @return A StringArray with all of the object names.
 */
//------------------------------------------------------------------------------
const StringArray& Simulator::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   #ifdef DEBUG_SIMULATOR_INITIALIZATION
      MessageInterface::ShowMessage(
            "Simulator::GetRefObjectNameArray(%d) entered\n", type);
   #endif

   refObjectList.clear();

   if ((type == Gmat::UNKNOWN_OBJECT) || (type == Gmat::PROP_SETUP) ||
       (type == Gmat::MEASUREMENT_MODEL))
   {
      if ((type == Gmat::UNKNOWN_OBJECT) || (type == Gmat::PROP_SETUP))
      {
         #ifdef DEBUG_SIMULATOR_INITIALIZATION
            MessageInterface::ShowMessage(
                  "   Adding propagator: %s\n", propagatorName.c_str());
         #endif
            if (find(refObjectList.begin(), refObjectList.end(),
                  propagatorName) == refObjectList.end())
               refObjectList.push_back(propagatorName);
      }

      if ((type == Gmat::UNKNOWN_OBJECT) || (type == Gmat::MEASUREMENT_MODEL))
      {
         // Add the measurements this simulator needs

//         // Wendy: Here you could do this:
//         StringArray measList = measManager.GetMeasurementNames();

         for (StringArray::iterator i = measList.begin();
               i != measList.end(); ++i)
         {
            #ifdef DEBUG_SIMULATOR_INITIALIZATION
               MessageInterface::ShowMessage(
                     "   Adding measurement: %s\n", i->c_str());
            #endif
            if (find(refObjectList.begin(), refObjectList.end(), *i) ==
                  refObjectList.end())
               refObjectList.push_back(*i);
         }
      }
   }
   else
   {
      // Fill in any base class needs
      refObjectList = Solver::GetRefObjectNameArray(type);
   }

   return refObjectList;
}

std::string Simulator::GetRefObjectName(const Gmat::ObjectType type) const
{
   return Solver::GetRefObjectName(type);
}

GmatBase* Simulator::GetRefObject(const Gmat::ObjectType type, const std::string & name)
{
   return Solver::GetRefObject(type, name);
}

GmatBase* Simulator::GetRefObject(const Gmat::ObjectType type, const std::string & name, const Integer index)
{
   return Solver::GetRefObject(type, name, index);
}

bool Simulator::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
      const std::string & name)
{
   #ifdef DEBUG_SIMULATOR_INITIALIZATION
      MessageInterface::ShowMessage("Setting ref object %s with type %s\n",
            name.c_str(), obj->GetTypeName().c_str());
   #endif

   if (name == propagatorName)
   {
      if (type == Gmat::PROP_SETUP)
      {
         if (propagator != NULL)
            delete propagator;
         propagator = (PropSetup*)obj->Clone();
         return true;
      }
   }

   StringArray measList = measManager.GetMeasurementNames();

   if (find(measList.begin(), measList.end(), name) != measList.end())
   {
      if (obj->IsOfType(Gmat::MEASUREMENT_MODEL))
      {
         measManager.AddMeasurement((MeasurementModel *)obj);
         return true;
      }
   }

   return Solver::SetRefObject(obj, type, name);
}

ObjectArray& Simulator::GetRefObjectArray(const std::string & typeString)
{
   return Solver::GetRefObjectArray(typeString);
}

bool Simulator::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
      const std::string & name, const Integer index)
{
   #ifdef DEBUG_SIMULATOR_INITIALIZATION
      MessageInterface::ShowMessage(""
            "Setting indexed ref object %s with type %s\n", name.c_str(),
            obj->GetTypeName().c_str());
   #endif

   return Solver::SetRefObject(obj, type, name, index);
}

ObjectArray & Simulator::GetRefObjectArray(const Gmat::ObjectType type)
{
   return Solver::GetRefObjectArray(type);
}


//------------------------------------------------------------------------------
//  bool TakeAction(const std::string &action, const std::string &actionData)
//------------------------------------------------------------------------------
/**
 * This method performs an action on the instance.
 *
 * TakeAction is a method overridden from GmatBase.  The only action defined for
 * a Simulator is "Reset" which resets the state to INITIALIZING
 *
 * @param <action>      Text label for the action.
 * @param <actionData>  Related action data, if needed.
 *
 * @return  flag indicating successful completion or not.
 */
//------------------------------------------------------------------------------
bool Simulator::TakeAction(const std::string &action,
                           const std::string &actionData)
{
   // @todo  Complete Reset action (?) and add others if needed
   if (action == "Reset")
   {
      currentState = INITIALIZING;
      initialized = false;
      return true;
   }

   return Solver::TakeAction(action, actionData);
}


//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  void CompleteInitialization()
//------------------------------------------------------------------------------
/**
 * This method completes initialization for the Simulator object, initializing
 * its MeasurementManager, retrieving the epoch and setting the state.
 *
 */
//------------------------------------------------------------------------------
void Simulator::CompleteInitialization()
{
   PropagationStateManager *psm = propagator->GetPropStateManager();
   GmatState               *gs  = psm->GetState();
   
   currentEpoch        = gs->GetEpoch(); 
   nextSimulationEpoch = simulationStart;
   
   // tell the measManager to complete its initialization
   bool measOK = measManager.Initialize();
   if (!measOK)
      throw SolverException(
            "Simulator::CompleteInitialization - error initializing MeasurementManager.\n");

   if (GmatMathUtil::IsEqual(currentEpoch,nextSimulationEpoch))
      currentState = CALCULATING;
   else
   {
      timeStep = (nextSimulationEpoch - currentEpoch) * GmatTimeUtil::SECS_PER_DAY;
      currentState = PROPAGATING;
   }
   initialized = true;
}


//------------------------------------------------------------------------------
//  void FindTimeStep()
//------------------------------------------------------------------------------
/**
 * This method determines whether the simulation is finished or still 
 * calculating, and if neither, computes the timeStep.
 *
 */
//------------------------------------------------------------------------------
void Simulator::FindTimeStep()
{
   if (currentEpoch > simulationEnd)
   {
      currentState = FINISHED;
   }
   else if (GmatMathUtil::IsEqual(currentEpoch,nextSimulationEpoch))
   {
      currentState = CALCULATING;
   }
   else
   {
      // Calculate the time step in seconds and stay in the PROPAGATING state;
      // timeStep could be positive or negative
      timeStep = (nextSimulationEpoch - currentEpoch) * GmatTimeUtil::SECS_PER_DAY;
   }
}


//------------------------------------------------------------------------------
//  void CalculateData()
//------------------------------------------------------------------------------
/**
 * This method determines whether or not measurements are possible, and advances
 * the state.
 *
 */
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
//  void ProcessEvent()
//------------------------------------------------------------------------------
/**
 * This method processes an event.     TBD
 *
 */
//------------------------------------------------------------------------------
// void Simulator::ProcessEvent()
//{
//
//}


//------------------------------------------------------------------------------
//  void CalculateData()
//------------------------------------------------------------------------------
/**
 * This method tells its MeasurementManager to add noise and write the data,
 * finds the nextSimulationEpoch, and advances the state.
 *
 */
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//  void RunComplete()
//------------------------------------------------------------------------------
/**
 * This method updates the simulator text file at the end of a simulator run.
 */
//------------------------------------------------------------------------------
void Simulator::RunComplete()
{
   WriteToTextFile();
   // tell the MeasurementManager to close files and finalize
   measManager.Finalize();
}


//------------------------------------------------------------------------------
//  void FindNextSimulationEpoch()
//------------------------------------------------------------------------------
/**
 * This method computes the nextSimulationEpoch.
 * 
 * @note This might become more complicated down the road.
 */
//------------------------------------------------------------------------------
void Simulator::FindNextSimulationEpoch()
{
   // we are assuming that the simulationStep is always non-negative
   nextSimulationEpoch = currentEpoch +
         simulationStep / GmatTimeUtil::SECS_PER_DAY;

   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("Current epoch = %.12lf; "
            "next sim epoch = %.12lf, sim end = %.12lf\n", currentEpoch,
            nextSimulationEpoch, simulationEnd);
   #endif
}


//------------------------------------------------------------------------------
//  std::string GetProgressString()
//------------------------------------------------------------------------------
/**
 * Generates a string for reporting the current simulator state.
 */
//------------------------------------------------------------------------------
std::string Simulator::GetProgressString()
{
   StringArray::iterator current;
//   Integer i;
   std::stringstream progress;
   progress.str("");
   progress.precision(12);

   if (initialized)
   {
      switch (currentState)
      {
         case INITIALIZING:
            // This state is basically a "paused state" used for the Target
            // command to finalize the initial data for the variables and
            // goals.  All that is written here is the header information.
            {
               progress << "************************************************"
                        << "********\n"
                        << "*** Performing Simulation "
                        << "(using \"" << instanceName << "\")\n";

               // Write out the setup data
               progress << "*** " ;

               // Iterate through TBD, writing them to
               // the file
//               for (current = variableNames.begin(), i = 0;
//                    current != variableNames.end(); ++current)
//               {
//                  if (current != variableNames.begin())
//                     progress << ", ";
//                  progress << *current;
//               }

               progress << "\n****************************"
                        << "****************************";
            }
            break;

         case PROPAGATING:
            progress << "\n";
            break;

         case CALCULATING:
            progress << "\n";
            // Iterate through the TBD variables, writing them to the string
//            for (current = variableNames.begin(), i = 0;
//                 current != variableNames.end(); ++current)
//            {
//               if (current != variableNames.begin())
//                  progress << ", ";
//               progress << *current << " = " << variable.at(i++);
//            }
            break;

         case SIMULATING:
            // TBD
            progress << "\n";

            break;

         case FINISHED:
            // TBD
            progress << "\n";

            break;

         default:
            throw SolverException(
               "Solver state not supported for the simulator");
      }
   }
   else
      return Solver::GetProgressString();

   return progress.str();
}


void Simulator::UpdateCurrentEpoch(GmatEpoch newEpoch)
{
   currentEpoch = newEpoch;
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



