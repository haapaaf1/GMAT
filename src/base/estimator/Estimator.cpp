//$Header$
//------------------------------------------------------------------------------
//                                Estimator
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/20
//
/**
 * Base class for estimator tools.
 */
//------------------------------------------------------------------------------


#include <sstream>
#include "Estimator.hpp"
#include "MessageInterface.hpp"
#include "FileManager.hpp"

//#define DEBUG_ESTIMATOR_INIT
//#define DEBUG_ESTIMATOR_CALC

//---------------------------------
// static data
//---------------------------------

const std::string
Estimator::PARAMETER_TEXT[EstimatorParamCount - GmatBaseParamCount] =
{
   "ShowProgress",
   "ReportStyle",
   "EstimatorTextFile",
   "Propagator",
   "Participants",
   "Measurements",
   // What is needed here?
   "Variables",
   "MaximumIterations",
   "NumberOfVariables",
};


const Gmat::ParameterType
Estimator::PARAMETER_TYPE[EstimatorParamCount - GmatBaseParamCount] =
{
   Gmat::BOOLEAN_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::STRINGARRAY_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE,
};

const std::string
Estimator::STYLE_TEXT[MaxStyle - NORMAL_STYLE] =
{
   "Normal",
   "Concise",
   "Verbose",
   "Debug"
};


//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
//  Estimator(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Core constructor for Estimator objects.
 *
 * @param type Text description of the type of estimator constructed
 *             (e.g. "NonLinearBatchLeastSquares")
 * @param name The estimator's name
 */
//------------------------------------------------------------------------------
Estimator::Estimator(const std::string &type, const std::string &name) :
   Solver                  (type, name),
//   GmatBase                (Gmat::ESTIMATOR, type, name),
   currentState            (INITIALIZING),
   iterationsTaken         (0),
   maxIterations           (25),
//   variableMinimum         (NULL),
//   variableMaximum         (NULL),
   initialized             (false),
   textFileMode            ("Normal"),
   showProgress            (true),
   progressStyle           (NORMAL_STYLE),
   debugString             (""),
//   variableCount           (0),
//   variable                (NULL),
   instanceNumber          (0),       // 0 indicates 1st instance w/ this name
   propName                (""),
   propagator              (NULL)
{
   objectTypes.push_back(Gmat::ESTIMATOR);
   objectTypeNames.push_back("Estimator");
   estimatorTextFile = "estimator_";
   estimatorTextFile  = type;
   estimatorTextFile += instanceName;
   estimatorTextFile += ".data";
}


//------------------------------------------------------------------------------
//  Estimator(std::string type, std::string name)
//------------------------------------------------------------------------------
/**
 *  Estimator destructor.
 */
//------------------------------------------------------------------------------
Estimator::~Estimator()
{
   // Added per Linda, 2/7/07
   if (textFile.is_open())
      textFile.close();
}


//------------------------------------------------------------------------------
//  Estimator(const Estimator &est)
//------------------------------------------------------------------------------
/**
 * Copy constructor for Estimator objects.
 *
 * @param est The estimator that is copied
 */
//------------------------------------------------------------------------------
Estimator::Estimator(const Estimator &est) :
//   GmatBase                (est),
   Solver                  (est),
   currentState            (est.currentState),
//   covarianceMatrix        (est.currentCovariance),
   iterationsTaken         (0),
   maxIterations           (est.maxIterations),
//   variableMinimum         (NULL),
//   variableMaximum         (NULL),
   initialized             (false),
   textFileMode            (est.textFileMode),
   showProgress            (est.showProgress),
   progressStyle           (est.progressStyle),
   debugString             (est.debugString),
//   variableCount           (est.variableCount),
//   variable                (NULL),
   estimatorTextFile          (est.estimatorTextFile),
   instanceNumber          (est.instanceNumber),
   propName                (est.propName),
   propagator              (NULL),
   participantNames        (est.participantNames),
   participants            (est.participants),
   measModelNames          (est.measModelNames),
   measModels              (est.measModels)
{
   #ifdef DEBUG_ESTIMATOR_INIT
      MessageInterface::ShowMessage(
         "In Estimator::Estimator (copy constructor)\n");
   #endif
   variableNames.clear();
}


//------------------------------------------------------------------------------
//  Estimator& operator=(const Estimator &est)
//------------------------------------------------------------------------------
/**
 * Assignment operator for estimators
 *
 * @return this Estimator, set to the same parameters as the input estimator.
 */
//------------------------------------------------------------------------------
Estimator& Estimator::operator=(const Estimator &est)
{
    if (&est == this)
        return *this;

   variableCount         = est.variableCount;
   iterationsTaken       = 0;
   maxIterations         = est.maxIterations;
   initialized           = false;
   estimatorTextFile        = est.estimatorTextFile;

   variableNames.clear();
   variable.clear();
   //perturbation.clear();
   variableMinimum.clear();
   variableMaximum.clear();
   //variableMaximumStep.clear();

   currentState          = est.currentState;
//   currentCovariance     = est.currentCovariance;
   textFileMode          = est.textFileMode;
   showProgress          = est.showProgress;
   progressStyle         = est.progressStyle;
   debugString           = est.debugString;

    return *this;
}

//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Derived classes implement this method to set object pointers and validate
 * internal data structures.
 *
 *  @return true on success, false (or throws a EstimatorException) on failure
 */
//------------------------------------------------------------------------------
bool Estimator::Initialize()
{
   // Prepare the text file for output
   if (estimatorTextFile != "")
   {
      // Added per Linda, 2/7/07
      FileManager *fm;
      fm = FileManager::Instance();
      std::string outPath = fm->GetFullPathname(FileManager::OUTPUT_PATH);
      estimatorTextFile = outPath + estimatorTextFile;

      if (textFile.is_open())
         textFile.close();

      if (instanceNumber == 1)
         textFile.open(estimatorTextFile.c_str());
      else
         textFile.open(estimatorTextFile.c_str(), std::ios::app);
      if (!textFile.is_open())
         throw EstimatorException("Error opening targeter text file " +
                               estimatorTextFile);
      textFile.precision(16);
      WriteToTextFile();
   }
   initialized = true;
   iterationsTaken = 0;
   #ifdef DEBUG_ESTIMATOR_INIT
      MessageInterface::ShowMessage(
         "In Estimator::Initialize completed\n");
   #endif
   return true;
}


bool Estimator::Finalize()
{
   // Close the estimator text file
   if (textFile.is_open())
   {
      textFile.flush();
      textFile.close();
   }
   return true;
}


//------------------------------------------------------------------------------
//  Integer SetEstimatorVariables(Real *data, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Derived classes use this method to pass in parameter data specific to
 * the algorithm implemented.
 *
 * @param <data> An array of data appropriate to the variables used in the
 *               algorithm.
 * @param <name> A label for the data parameter.  Defaults to the empty
 *               string.
 *
 * @return The ID used for the variable.
 */
//------------------------------------------------------------------------------
Integer Estimator::SetEstimatorVariables(Real *data,
                                                  const std::string &name)
{
   if (variableNames[variableCount] != name)
      throw EstimatorException("Mismatch between parsed and configured variable");

   try
   {
      variable.at(variableCount) = data[0];
      perturbation.at(variableCount) = data[1];
   }
   catch(const std::exception &re)
   {
      throw EstimatorException(
            "Range error setting variable or perturbation in SetEstimatorVariables\n");
   }
   // Sanity check min and max
   if (data[2] >= data[3])
   {
      std::stringstream errMsg;
      errMsg << "Minimum allowed variable value (received " << data[2]
             << ") must be less than maximum (received " << data[3] << ")";
      throw EstimatorException(errMsg.str());
   }

   try
   {
      variableMinimum.at(variableCount)           = data[2];
      variableMaximum.at(variableCount)           = data[3];
   }
   catch(const std::exception &re)
   {
      throw EstimatorException(
            "Range error setting variable min/max in SetEstimatorVariables\n");
   }

   ++variableCount;

   return variableCount-1;
}


//------------------------------------------------------------------------------
//  Real GetEstimatorVariable(Integer id)
//------------------------------------------------------------------------------
/**
 * Interface used to access Variable values.
 *
 * @param <id> The ID used for the variable.
 *
 * @return The value used for this variable
 */
//------------------------------------------------------------------------------
Real Estimator::GetEstimatorVariable(Integer id)
{
   if (id >= variableCount)
      throw EstimatorException(
         "Estimator member requested a parameter outside the range "
         "of the configured variables.");

   //return variable[id];
   return variable.at(id);
}

//------------------------------------------------------------------------------
//  EstimatorState GetState()
//------------------------------------------------------------------------------
/**
 * Determine the state-machine state of this instance of the Estimator.
 *
 * @return current state
 */
//------------------------------------------------------------------------------
//Estimator::EstimatorState Estimator::GetState()
Estimator::SolverState Estimator::GetState()
{
   return currentState;
}

//------------------------------------------------------------------------------
//  Estimator::EstimatorState AdvanceState()
//------------------------------------------------------------------------------
/**
 * The method used to iterate until a solution is found.  Derived classes
 * use this method to implement their solution technique.
 *
 * @return estimator state at the end of the process.
 */
//------------------------------------------------------------------------------
//Estimator::EstimatorState Estimator::AdvanceState()
Estimator::SolverState Estimator::AdvanceState()
{
   // Default behavior -- just walk the state machine
   switch (currentState)
   {
      case INITIALIZING:
         currentState = PROPAGATING;
         break;

      case PROPAGATING:
           // Figure out the next needed epoch here
         currentState = CALCULATING;
         break;

      case CALCULATING:
         CalculateParameters();
         currentState = ESTIMATING;
         break;

      case ESTIMATING:
         currentState = CHECKINGRUN;
         break;

      case CHECKINGRUN:
         CheckCompletion();
         currentState = FINISHED;
         break;

      case FINISHED:
         RunComplete();
         currentState = INITIALIZING;
         break;

      default:
         throw EstimatorException("Undefined Estimator state");
   };

   ReportProgress();
   return currentState;
}


// Access methods overriden from the base class

//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string Estimator::GetParameterText(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < EstimatorParamCount))
   {
      //MessageInterface::ShowMessage("'%s':\n",
      //   PARAMETER_TEXT[id - GmatBaseParamCount].c_str());
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   }
   return GmatBase::GetParameterText(id);
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
 */
//------------------------------------------------------------------------------
Integer Estimator::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < EstimatorParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
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
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Estimator::GetParameterType(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < EstimatorParamCount))
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
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
 */
//------------------------------------------------------------------------------
std::string Estimator::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool Estimator::IsParameterReadOnly(const Integer id) const
{
   if (id == NUMBER_OF_VARIABLES)  return true;
   return GmatBase::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool Estimator::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


//------------------------------------------------------------------------------
//  Integer  GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns an Integer parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  Integer value of the requested parameter.
 */
//------------------------------------------------------------------------------
Integer Estimator::GetIntegerParameter(const Integer id) const
{
   if (id == maxIterationsID)
      return maxIterations;
   if (id == NUMBER_OF_VARIABLES)
      return variableCount;

   return GmatBase::GetIntegerParameter(id);
}


//------------------------------------------------------------------------------
//  Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
/**
 * This method sets an Integer parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 * @param <value> Integer value for the parameter.
 *
 * @return  The value of the parameter at the completion of the call.
 */
//------------------------------------------------------------------------------
Integer Estimator::SetIntegerParameter(const Integer id,
                                    const Integer value)
{
   if (id == maxIterationsID)
   {
      if (value > 0)
         maxIterations = value;
      else
//         MessageInterface::ShowMessage(
//            "Iteration count for %s must be > 0; requested value was %d\n",
//            instanceName.c_str(), value);
         throw EstimatorException(
            "The value entered for the maximum iterations on " + instanceName +
            " is not an allowed value. The allowed value is: [Integer > 0].");
      return maxIterations;
   }

   return GmatBase::SetIntegerParameter(id, value);
}

//------------------------------------------------------------------------------
//  bool  GetBooleanParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the Boolean parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  Boolean value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
bool Estimator::GetBooleanParameter(const Integer id) const
{
    if (id == ShowProgressID)
        return showProgress;

    return GmatBase::GetBooleanParameter(id);
}

//------------------------------------------------------------------------------
//  Integer SetBooleanParameter(const Integer id, const bool value)
//------------------------------------------------------------------------------
/**
 * This method sets a Boolean parameter value, given the input
 * parameter ID.
 *
 * @param <id>    ID for the requested parameter.
 * @param <value> Boolean value for the parameter.
 *
 * @return  The value of the parameter at the completion of the call.
 */
//------------------------------------------------------------------------------
bool Estimator::SetBooleanParameter(const Integer id, const bool value)
{
   if (id == ShowProgressID)
   {
      showProgress = value;
      return showProgress;
   }

   return GmatBase::SetBooleanParameter(id, value);
}


//---------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The string stored for this parameter, or throw ab=n exception if
 *         there is no string association.
 */
std::string Estimator::GetStringParameter(const Integer id) const
{
   if (id == ReportStyle)
      return textFileMode;
    if (id == estimatorTextFileID)
      return estimatorTextFile;
    if (id == PropagatorName)
       return propName;
   return GmatBase::GetStringParameter(id);
}


//---------------------------------------------------------------------------
//  std::string GetStringParameter(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The string stored for this parameter, or throw ab=n exception if
 *         there is no string association.
 */
std::string Estimator::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string &value)
//---------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 * @param <value> The new string for this parameter.
 *
 * @return true if the string is stored, throw if the parameter is not stored.
 */
bool Estimator::SetStringParameter(const Integer id, const std::string &value)
{
//   MessageInterface::ShowMessage("Setting string %s on id %d\n")
   if (id == ReportStyle)
   {
//      std::string stylelist ;

      for (Integer i = NORMAL_STYLE; i < MaxStyle; ++i)
      {
//         stylelist += ", " + STYLE_TEXT[i-NORMAL_STYLE];

         if (value == STYLE_TEXT[i-NORMAL_STYLE])
         {
            textFileMode = value;
            progressStyle = i;
            return true;
         }
      }
      throw EstimatorException(
         "The value of \"" + value + "\" for field \"Report Style\""
         " on object \"" + instanceName + "\" is not an allowed value.\n"
         "The allowed values are: [Normal, Concise, Verbose, Debug].");
//         "The allowed values are: [ " + stylelist + " ]. ");
//      throw EstimatorException("Requested estimator report style, " + value +
//         ", is nor supported for " + typeName + " estimators.");
   }

   if (id == estimatorTextFileID)
   {
      estimatorTextFile = value;
      return true;
   }

   if (id == PropagatorName)
   {
      propName = value;
      return true;
   }

   if (id == ParticipantNames)
   {
      participantNames.push_back(value);
      return true;
   }

   if (id == MeasurementModels)
   {
      measModelNames.push_back(value);
      return true;
   }

   if (id == variableNamesID)
   {
      variableNames.push_back(value);
      return true;
   }

   return GmatBase::SetStringParameter(id, value);
}
//---------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const std::string &value)
//---------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param <id> The integer ID for the parameter.
 * @param <value> The new string for this parameter.
 *
 * @return true if the string is stored, throw if the parameter is not stored.
 */
bool Estimator::SetStringParameter(const std::string &label,
                                const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


// compiler complained again - so here they are ....
std::string Estimator::GetStringParameter(const Integer id,
                                                  const Integer index) const
{
   return GmatBase::GetStringParameter(id, index);
}

bool Estimator::SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index)
{
   return GmatBase::SetStringParameter(id, value, index);
}

std::string Estimator::GetStringParameter(const std::string &label,
                                                  const Integer index) const
{
   return GmatBase::GetStringParameter(label, index);
}

bool Estimator::SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index)
{
   return GmatBase::SetStringParameter(label, value, index);
}

//------------------------------------------------------------------------------
//  std::string  GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  StringArray value of the requested parameter.
 */
//------------------------------------------------------------------------------
const StringArray& Estimator::GetStringArrayParameter(const Integer id) const
{
   if (id == variableNamesID)
      return variableNames;

   if (id == ParticipantNames)
   {
      MessageInterface::ShowMessage("   Retrieving %d names\n", participantNames.size());
      return participantNames;
   }

   if (id == MeasurementModels)
      return measModelNames;

   return GmatBase::GetStringArrayParameter(id);
}

GmatBase* Estimator::GetRefObject(const Gmat::ObjectType type,
                                  const std::string &name)
{
   GmatBase* retval = NULL;

   if (type == Gmat::SPACE_POINT)
   {
      for (ObjectArray::iterator i = participants.begin();
           i != participants.end(); ++i)
      {
         if ((*i)->GetName() == name)
         {
            retval = *i;
            break;
         }
      }
   }

   if (type == Gmat::PROP_SETUP)
   {
      retval = (GmatBase*)propagator;
   }

   if (type == Gmat::MEASUREMENT_MODEL)
   {
      // Needs code
   }

   if (retval != NULL)
      return retval;
   return Solver::GetRefObject(type, name);
}


bool Estimator::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name)
{
   bool retval = false;

   if (obj->IsOfType(Gmat::SPACE_POINT))
   {
      if (find(participants.begin(), participants.end(), obj) == participants.end())
      {
         participants.push_back(obj);
         retval = true;
      }
   }

   if (obj->IsOfType(Gmat::PROP_SETUP))
   {
      // Do we need to save this here?
      propagator = (PropSetup*)obj;
      retval = true;
   }

   if (obj->IsOfType(Gmat::MEASUREMENT_MODEL))
   {
//      if (find(measModels.begin(), measModels.end(), obj) == measModels.end())
//      {
//         measModels.push_back((MeasurementModel*)obj);
         retval = true;
//      }
   }

   return retval;
}


//------------------------------------------------------------------------------
//  void ReportProgress()
//------------------------------------------------------------------------------
/**
 * Shows the progress string to the user.
 *
 * This default version just passes the progress string to the MessageInterface.
 */
//------------------------------------------------------------------------------
void Estimator::ReportProgress()
{
   if (showProgress)
   {
      MessageInterface::ShowMessage("%s\n", GetProgressString().c_str());
   }
}

//------------------------------------------------------------------------------
//  void SetDebugString(const std::string &str)
//------------------------------------------------------------------------------
/**
 * Fills the buffer with run data for (user space) debug mode in the Estimators.
 *
 * @param <str> The data passed from the command stream.
 */
//------------------------------------------------------------------------------
void Estimator::SetDebugString(const std::string &str)
{
   debugString = str;
}



////------------------------------------------------------------------------------
////  void CompleteInitialization()
////------------------------------------------------------------------------------
///**
// * Finalized the initialization process by setting the current state for the
// * state machine to the entry state for the estimator.  The default method provided
// * here sets the state to the NOMINAL state.
// */
////------------------------------------------------------------------------------
//void Estimator::CompleteInitialization()
//{
//    currentState = NOMINAL;
//}


//------------------------------------------------------------------------------
//  void RunNominal()
//------------------------------------------------------------------------------
/**
 * Executes a nominal run and then advances the state machine to the next state.
 *
 * This default method just advances the state.
 */
//------------------------------------------------------------------------------
void Estimator::RunNominal()
{
//    currentState = (EstimatorState)(currentState+1);
}


////------------------------------------------------------------------------------
////  void RunPerturbation()
////------------------------------------------------------------------------------
///**
// * Executes a perturbation run and then advances the state machine to the next
// * state.
// *
// * This default method just advances the state.
// */
////------------------------------------------------------------------------------
//void Estimator::RunPerturbation()
//{
////    currentState = (EstimatorState)(currentState+1);
//}


////------------------------------------------------------------------------------
////  void RunIteration()
////------------------------------------------------------------------------------
///**
// * Executes an iteration run and then advances the state machine to the next
// * state.
// *
// * This default method just advances the state.
// */
////------------------------------------------------------------------------------
//void Estimator::RunIteration()
//{
////    currentState = (EstimatorState)(currentState+1);
//}


////------------------------------------------------------------------------------
////  void CalculateParameters()
////------------------------------------------------------------------------------
///**
// * Executes a Calculates parameters needed by the state machine for the next
// * nominal run, and then advances the state machine to the next state.
// *
// * This default method just advances the state.
// */
////------------------------------------------------------------------------------
//void Estimator::CalculateParameters()
//{
////    currentState = (EstimatorState)(currentState+1);
//}


//------------------------------------------------------------------------------
//  void CheckCompletion()
//------------------------------------------------------------------------------
/**
 * Checks to see if the Estimator has converged.
 *
 * This default method just advances the state.
 */
//------------------------------------------------------------------------------
void Estimator::CheckCompletion()
{
//    currentState = (EstimatorState)(currentState+1);
}

////------------------------------------------------------------------------------
////  void RunExternal()
////------------------------------------------------------------------------------
///**
// * Launhes an external process that drives the Estimator.
// *
// * This default method just ???? (not a clue).
// */
////------------------------------------------------------------------------------
//void Estimator::RunExternal()
//{
//   //currentState = FINISHED;  // what to do here?
//   currentState = (EstimatorState)(currentState+1);
//}


//------------------------------------------------------------------------------
//  void RunComplete()
//------------------------------------------------------------------------------
/**
 * Finalized the data at the end of a run.
 *
 * This default method just sets the state to FINISHED.
 */
//------------------------------------------------------------------------------
void Estimator::RunComplete()
{
    currentState = FINISHED;
}


//------------------------------------------------------------------------------
//  std::string GetProgressString()
//------------------------------------------------------------------------------
/**
 * Generates a string that is written out by estimators when showProgress is true.
 */
//------------------------------------------------------------------------------
std::string Estimator::GetProgressString()
{
   return "Estimator progress string not yet implemented for " + typeName;
}

//------------------------------------------------------------------------------
//  void FreeArrays()
//------------------------------------------------------------------------------
/**
 * Frees the memory used by the targeter, so it can be reused later in the
 * sequence.  This method is also called by the destructor when the script is
 * cleared.
 */
//------------------------------------------------------------------------------
void Estimator::FreeArrays()
{
   /*
   if (textFile.is_open())
   {
      textFile.flush();
      textFile.close();
   }

   if (variable)
   {
      delete [] variable;
      variable = NULL;
   }

    if (perturbation)
   {
      delete [] perturbation;
      perturbation = NULL;
   }

   if (variableMinimum)
   {
      delete [] variableMinimum;
      variableMinimum = NULL;
   }

   if (variableMaximum)
   {
      delete [] variableMaximum;
      variableMaximum = NULL;
   }

   if (variableMaximumStep)
   {
      delete [] variableMaximumStep;
      variableMaximumStep = NULL;
   }
   */
   //variableNames.clear(); // ????
   variable.clear();
   perturbation.clear();
   variableMinimum.clear();
   variableMaximum.clear();
   variableMaximumStep.clear();
   pertDirection.clear();

   Solver::FreeArrays();
}

/// For nonlinear systems, we need to define a function f that
/// predicts the estimator state at time t2 given a prior
/// state at time t1 and the associated control vector u during that time period
/// h is a function that computes an estimated observation at time t given
/// the current state and control vectors.
///
/// x(t1) = f(x(t0),u(t0),t0)
/// y(t1) = h(x(t1),u(t1),t1)
///
LaVectorDouble Estimator::f()
{
  return x;
}
LaVectorDouble Estimator::f(LaVectorDouble x, Real t0, Real t1)
{
  return x;
}
LaVectorDouble Estimator::f(LaVectorDouble x, LaVectorDouble u, Real t0, Real t1)
{
  return x;
}

LaVectorDouble Estimator::h(StringArray observationTypes)
{
  return y;
}
LaVectorDouble Estimator::h(StringArray observationTypes, LaVectorDouble x, Real t1)
{
  return y;
}
LaVectorDouble Estimator::h(StringArray observationTypes, LaVectorDouble x, LaVectorDouble u, Real t1)
{
  return y;
}

/// H is a function that computes the Jacobian of y w.r.t. x
LaGenMatDouble Estimator::H()
{
  return LaGenMatDouble::eye(observationTypeCount,stateCount);
}
LaGenMatDouble Estimator::H(LaVectorDouble x, Real t0)
{
  return LaGenMatDouble::eye(observationTypeCount,stateCount);
}

/// Phi is a function that computes the Jacobian of f w.r.t. x
LaGenMatDouble Estimator::Phi()
{
    return LaGenMatDouble::eye(stateCount); 
}

LaGenMatDouble Estimator::Phi(LaVectorDouble x, Real t0)
{
    return LaGenMatDouble::eye(stateCount); 
}
