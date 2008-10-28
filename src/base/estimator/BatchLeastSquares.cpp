//$Id$
//------------------------------------------------------------------------------
//                         BatchLeastSquares
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
 * Implementation for the batch least squares estimation method
 *
 */
//------------------------------------------------------------------------------


#include "BatchLeastSquares.hpp"
#include "Rmatrix.hpp"
#include "RealUtilities.hpp"     // for GmatMathUtil::Abs()
#include "MessageInterface.hpp"

#include <cmath>
#include <sstream>

//#define DEBUG_STATE_MACHINE
//#define DEBUG_DC_INIT 1
//#define DEBUG_STATE_MACHINE
//#define DEBUG_STATE_TRANSITIONS

//---------------------------------
// static data
//---------------------------------

//const std::string
//BatchLeastSquares::PARAMETER_TEXT[BatchLeastSquaresParamCount -
//                                      EstimatorParamCount] =
//{
//};
//
//const Gmat::ParameterType
//BatchLeastSquares::PARAMETER_TYPE[BatchLeastSquaresParamCount -
//                                      EstimatorParamCount] =
//{
//};


//---------------------------------
// public methods
//---------------------------------

BatchLeastSquares::BatchLeastSquares(std::string name) :
   Estimator                  ("BatchLeastSquares", name),
//   variableCount           (0),
   //goalCount               (0),
//   iterationsTaken         (0),
//   maxIterations           (25),
//   variable                (NULL),
   //perturbation            (NULL),
//   variableMinimum         (NULL),
//   variableMaximum         (NULL),
   //variableMaximumStep     (NULL),
   //goal                    (NULL),
   tolerance               (NULL),
   nominal                 (NULL),
   achieved                (NULL) //,
   //jacobian                (NULL),
//   informationMatrix         (NULL),
   //indx                    (NULL),
   //b                       (NULL),
   //ludMatrix               (NULL),
   //useCentralDifferences   (false)  //,
//   initialized             (false),
   //instanceNumber          (0)       // 0 indicates 1st instance w/ this name
{
   #if DEBUG_DC_INIT
   MessageInterface::ShowMessage
      ("BatchLeastSquares::DC(constructor) entered\n");
   #endif
   objectTypeNames.push_back("BatchLeastSquares");
   parameterCount = BatchLeastSquaresParamCount;

   // textFileMode = "Verbose";
   //estimatorTextFile = "targeter_";
   //estimatorTextFile += instanceName;
   //estimatorTextFile += ".data";
}


BatchLeastSquares::~BatchLeastSquares()
{
   FreeArrays();
}


BatchLeastSquares::BatchLeastSquares(const BatchLeastSquares &dc) :
   Estimator                  (dc),
   //variableCount           (dc.variableCount),
   goalCount               (dc.goalCount),
   //iterationsTaken         (0),
   //maxIterations           (dc.maxIterations),
   //variable                (NULL),
   //perturbation            (NULL),
   //variableMinimum         (NULL),
   //variableMaximum         (NULL),
   //variableMaximumStep     (NULL),
   goal                    (NULL),
   tolerance               (NULL),
   nominal                 (NULL),
   achieved                (NULL),
   jacobian                (NULL),
   inverseJacobian         (NULL),
   //pertNumber              (dc.pertNumber),
   indx                    (NULL),
   b                       (NULL),
   ludMatrix               (NULL),
   useCentralDifferences   (dc.useCentralDifferences)  //,
   //initialized             (false),
   //estimatorTextFile          (dc.estimatorTextFile),
   //instanceNumber          (dc.instanceNumber)
{
   #if DEBUG_DC_INIT
   MessageInterface::ShowMessage
      ("BatchLeastSquares::DC(COPY constructor) entered\n");
   #endif
  //variableNames.clear(); -> Estimator
  goalNames.clear();

   parameterCount = dc.parameterCount;
}


BatchLeastSquares&
    BatchLeastSquares::operator=(const BatchLeastSquares& dc)
{
    if (&dc == this)
        return *this;

   Estimator::operator=(dc);

   FreeArrays();

   //variableNames.clear();
   goalNames.clear();

   //variableCount         = dc.variableCount;
   goalCount             = dc.goalCount;
   //iterationsTaken       = 0;
   //maxIterations         = dc.maxIterations;
   useCentralDifferences = dc.useCentralDifferences;
   //initialized           = false;
   //estimatorTextFile        = dc.estimatorTextFile;
   //instanceNumber        = dc.instanceNumber;
   //pertNumber            = dc.pertNumber;

   return *this;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the BatchLeastSquares.
 *
 * @return clone of the BatchLeastSquares.
 */
//------------------------------------------------------------------------------
GmatBase* BatchLeastSquares::Clone() const
{
   GmatBase *clone = new BatchLeastSquares(*this);
   return (clone);
}


//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 *
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void BatchLeastSquares::Copy(const GmatBase* orig)
{
   operator=(*((BatchLeastSquares *)(orig)));
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
std::string BatchLeastSquares::GetParameterText(const Integer id) const
{
   if ((id >= EstimatorParamCount) && (id < BatchLeastSquaresParamCount))
      return PARAMETER_TEXT[id - EstimatorParamCount];
   return Estimator::GetParameterText(id);
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
Integer BatchLeastSquares::GetParameterID(const std::string &str) const
{
   for (Integer i = EstimatorParamCount; i < BatchLeastSquaresParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - EstimatorParamCount])
         return i;
   }

   return Estimator::GetParameterID(str);
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
Gmat::ParameterType BatchLeastSquares::GetParameterType(
                                              const Integer id) const
{
   if ((id >= EstimatorParamCount) && (id < BatchLeastSquaresParamCount))
      return PARAMETER_TYPE[id - EstimatorParamCount];

   return Estimator::GetParameterType(id);
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
std::string BatchLeastSquares::GetParameterTypeString(
                                      const Integer id) const
{
   return Estimator::PARAM_TYPE_STRING[GetParameterType(id)];
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
Integer BatchLeastSquares::GetIntegerParameter(const Integer id) const
{
   //if (id == maxIterationsID)
   //   return maxIterations;

   return Estimator::GetIntegerParameter(id);
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
Integer BatchLeastSquares::SetIntegerParameter(const Integer id,
                                                   const Integer value)
{
   //if (id == maxIterationsID)
   //{
   //   if (value > 0)
   //      maxIterations = value;
   //   else
   //      MessageInterface::ShowMessage(
   //         "Iteration count for %s must be > 0; requested value was %d\n",
   //         instanceName.c_str(), value);
   //   return maxIterations;
   //}

   return Estimator::SetIntegerParameter(id, value);
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
bool BatchLeastSquares::GetBooleanParameter(const Integer id) const
{
//    if (id == useCentralDifferencingID)
//        return useCentralDifferences;

    return Estimator::GetBooleanParameter(id);
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
bool BatchLeastSquares::SetBooleanParameter(const Integer id,
                                                const bool value)
{
//   if (id == useCentralDifferencingID)
//   {
//      useCentralDifferences = value;
//      return useCentralDifferences;
//   }

   return Estimator::SetBooleanParameter(id, value);
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
 */
//------------------------------------------------------------------------------
std::string BatchLeastSquares::GetStringParameter(const Integer id) const
{
    //if (id == estimatorTextFileID)
    //    return estimatorTextFile;

    return Estimator::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  Integer SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
/**
 * This method sets a string or string array parameter value, given the input
 * parameter ID.
 *
 * @param <id>    ID for the requested parameter.
 * @param <value> string value for the parameter.
 *
 * @return  The value of the parameter at the completion of the call.
 */
//------------------------------------------------------------------------------
bool BatchLeastSquares::SetStringParameter(const Integer id,
                                               const std::string &value)
{
    //if (id == estimatorTextFileID) {
    //    estimatorTextFile = value;
    //    return true;
    //}

    //if (id == variableNamesID) {
    //    variableNames.push_back(value);
    //    return true;
    //}

//    if (id == goalNamesID) {
//        goalNames.push_back(value);
//        return true;
//    }

    return Estimator::SetStringParameter(id, value);
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
const StringArray& BatchLeastSquares::GetStringArrayParameter(
                                                        const Integer id) const
{
    //if (id == variableNamesID)
    //    return variableNames;

//    if (id == goalNamesID)
//        return goalNames;

    return Estimator::GetStringArrayParameter(id);
}


//------------------------------------------------------------------------------
//  bool TakeAction(const std::string &action, const std::string &actionData)
//------------------------------------------------------------------------------
/**
 * This method performs an action on the instance.
 *
 * TakeAction is a method overridden from GmatBase.  The only action defined for
 * a BatchLeastSquares is "IncrementInstanceCount", which the Sandbox uses
 * to tell an instance if if it is a reused instance (i.e. a clone) of the
 * configured instance of the BatchLeastSquares.
 *
 * @param <action>      Text label for the action.
 * @param <actionData>  Related action data, if needed.
 *
 * @return  The value of the parameter at the completion of the call.
 */
//------------------------------------------------------------------------------
bool BatchLeastSquares::TakeAction(const std::string &action,
                                       const std::string &actionData)
{
   if (action == "IncrementInstanceCount")
   {
      ++instanceNumber;
      return true;
   }

   if (action == "Reset")
   {
      currentState = INITIALIZING;
      // initialized = false;
      // Set nominal out of range to force retarget when in a loop
      for (Integer i = 0; i < goalCount; ++i)
      {
         nominal[i] = goal[i] + 10.0 * tolerance[i];
      }
   }

   return Estimator::TakeAction(action, actionData);
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
/*
Integer BatchLeastSquares::SetEstimatorVariables(Real *data,
                                                  const std::string &name)
{
   if (variableNames[variableCount] != name)
      throw EstimatorException("Mismatch between parsed and configured variable");

   variable[variableCount] = data[0];
   perturbation[variableCount] = data[1];
   // Sanity check min and max
   if (data[2] >= data[3])
   {
      std::stringstream errMsg;
      errMsg << "Minimum allowed variable value (received " << data[2]
             << ") must be less than maximum (received " << data[3] << ")";
      throw EstimatorException(errMsg.str());
   }
   if (data[4] <= 0.0)
   {
      std::stringstream errMsg;
      errMsg << "Largest allowed step must be positive! (received "
             << data[4] << ")";
      throw EstimatorException(errMsg.str());
   }

   variableMinimum[variableCount] = data[2];
   variableMaximum[variableCount] = data[3];
   variableMaximumStep[variableCount] = data[4];
   ++variableCount;

   return variableCount-1;
}
*/

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
/*
Real BatchLeastSquares::GetEstimatorVariable(Integer id)
{
   if (id >= variableCount)
      throw EstimatorException(
         "BatchLeastSquares member requested a parameter outside the range "
         "of the configured variables.");

   return variable[id];
}
*/

//------------------------------------------------------------------------------
// Integer SetEstimatorResults(Real *data, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Sets up the data fields used for the results of an iteration.
 *
 * @param <data> An array of data appropriate to the results used in the
 *               algorithm (for instance, tolerances for targeter goals).
 * @param <name> A label for the data parameter.  Defaults to the empty
 *               string.
 *
 * @return The ID used for this variable.
 */
//------------------------------------------------------------------------------
Integer BatchLeastSquares::SetEstimatorResults(Real *data,
                                                const std::string &name,
                                                const std::string &type)
{
    if (goalNames[goalCount] != name)
        throw EstimatorException("Mismatch between parsed and configured goal");
    goal[goalCount] = data[0];
    tolerance[goalCount] = data[1];
    ++goalCount;
    return goalCount-1;
}


//------------------------------------------------------------------------------
// bool UpdateEstimatorGoal(Integer id, Real newValue)
//------------------------------------------------------------------------------
/**
 * Updates the targeter goals, for floating end point targeting.
 *
 * @param <id>       Id for the goal that is being reset.
 * @param <newValue> The new goal value.
 *
 * @return true on success, throws on failure.
 */
//------------------------------------------------------------------------------
bool BatchLeastSquares::UpdateEstimatorGoal(Integer id, Real newValue)
{
   // Only update during nominal runs
   if (currentState == NOMINAL) {
      if (id >= goalCount)
         throw EstimatorException(
            "BatchLeastSquares member requested a parameter outside the "
            "range of the configured goals.");

      goal[id] = newValue;
   }
   return true;
}


//------------------------------------------------------------------------------
// void SetResultValue(Integer id, Real value)
//------------------------------------------------------------------------------
/**
 * Passes in the results obtained from a run in the BatchLeastSquares loop.
 *
 * @param <id>    The ID used for this result.
 * @param <value> The corresponding result.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::SetResultValue(Integer id, Real value,
                                           const std::string &resultType)
{
    if (currentState == NOMINAL) {
        nominal[id] = value;
    }

    if (currentState == PERTURBING) {
        achieved[pertNumber][id] = value;
    }
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the BatchLeastSquares prior to targeting.
 */
//------------------------------------------------------------------------------
bool BatchLeastSquares::Initialize()
{
   // Setup the variable data structures
   Integer localVariableCount = variableNames.size();
   Integer localGoalCount = goalNames.size();

   #if DEBUG_DC_INIT
   MessageInterface::ShowMessage
      ("BatchLeastSquares::Initialize() localVariableCount=%d, "
       "localGoalCount=%d\n", localVariableCount, localGoalCount);
   #endif

   if (localVariableCount == 0 || localGoalCount == 0)
   {
      std::string errorMessage = "Targeter cannot initialize: ";
      errorMessage += "No goals or variables are set.\n";
      throw EstimatorException(errorMessage);
   }

   if (localGoalCount > localVariableCount)
   {
      std::string errorMessage = "Targeter cannot initialize: ";
      errorMessage += "More goals than variables\n";
      throw EstimatorException(errorMessage);
   }


   FreeArrays();

   //variable            = new Real[localVariableCount];
   //perturbation        = new Real[localVariableCount];
   //variableMinimum     = new Real[localVariableCount];
   //variableMaximum     = new Real[localVariableCount];
   //variableMaximumStep = new Real[localVariableCount];

   // Setup the goal data structures
   goal      = new Real[localGoalCount];
   tolerance = new Real[localGoalCount];
   nominal   = new Real[localGoalCount];

   // And the sensitivity matrix
   Integer i;
   achieved        = new Real*[localVariableCount];
   jacobian        = new Real*[localVariableCount];
   inverseJacobian = new Real*[localVariableCount];
   ludMatrix       = new Real*[localVariableCount];
   for (i = 0; i < localVariableCount; ++i)
   {
      jacobian[i]        = new Real[localVariableCount];
      inverseJacobian[i] = new Real[localVariableCount];
      achieved[i]        = new Real[localGoalCount];
      ludMatrix[i]       = new Real[localVariableCount];

      // Initialize to the identity matrix
      jacobian[i][i] = 1.0;
      inverseJacobian[i][i] = 1.0;

      // Set default values for min and max parameters
      //variable[i]            =  0.0;
      //variableMinimum[i]     = -9.999e300;
      //variableMaximum[i]     =  9.999e300;
      //variableMaximumStep[i] =  9.999e300;
      //perturbation[i]        =  1.0e-04;
   }

   Estimator::Initialize(); // for commented stuff, moved to Estimator

   // Prepare the text file for output
   //if (estimatorTextFile != "")
   //{
   //   if (instanceNumber == 1)
   //      textFile.open(estimatorTextFile.c_str());
   //   else
   //      textFile.open(estimatorTextFile.c_str(), std::ios::app);
   //   if (!textFile.is_open())
   //      throw EstimatorException("Error opening targeter text file " +
   //                            estimatorTextFile);
   //   textFile.precision(16);
   //   WriteToTextFile();
   //}


   // Allocate the LU arrays
   indx = new Integer[variableCount];
   b = new Real[variableCount];

   //initialized = true;  // moved to Estimator
   //iterationsTaken = 0;
   #if DEBUG_DC_INIT
   MessageInterface::ShowMessage
      ("BatchLeastSquares::Initialize() completed\n");
   #endif
   return true;
}


//------------------------------------------------------------------------------
//  Estimator::EstimatorState AdvanceState()
//------------------------------------------------------------------------------
/**
 * The method used to walk the BatchLeastSquares through its state machine.
 *
 * @return estimator state at the end of the process.
 */
//------------------------------------------------------------------------------
//Estimator::EstimatorState BatchLeastSquares::AdvanceState()
Estimator::SolverState BatchLeastSquares::AdvanceState()
{
//   return Estimator::AdvanceState();

   switch (currentState)
   {
      case INITIALIZING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; INITIALIZING\n");
         #endif
         iterationsTaken = 0;
         WriteToTextFile();
         ReportProgress();
         CompleteInitialization();
         break;

      case PROPAGATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; NOMINAL\n");
         #endif
         ReportProgress();
         RunNominal();
         ReportProgress();
         break;

	 //case PERTURBING:
         //#ifdef DEBUG_STATE_MACHINE
         //   MessageInterface::ShowMessage("Entered state machine; PERTURBING\n");
         //#endif
         //ReportProgress();
         //RunPerturbation();
         //break;

      case CALCULATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; CALCULATING\n");
         #endif
         ReportProgress();
         CalculateParameters();
         break;

      case ESTIMATING:
         ReportProgress();
         Estimate();
         break;

      case CHECKINGRUN:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; CHECKINGRUN\n");
         #endif
         CheckCompletion();
         ++iterationsTaken;
         if (iterationsTaken > maxIterations)
         {
            MessageInterface::ShowMessage("Differential corrector %s %s\n",
               instanceName.c_str(),
               "has exceeded to maximum number of allowed iterations.");
            currentState = FINISHED;
         }
         break;

      case FINISHED:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; FINISHED\n");
         #endif
         RunComplete();
         ReportProgress();
         break;

	 //case ITERATING:             // Intentional drop-through

      default:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
               "Bad state for a differential corrector.\n");
         #endif
         throw EstimatorException("Estimator state not supported for the targeter");
   }

   return currentState;
}

void BatchLeastSquares::CompleteInitialization()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS initializing\n");
   #endif

   WriteToTextFile();
   // TODO: Any additional initialization needed
   currentState = PROPAGATING;
}
//------------------------------------------------------------------------------
//  void RunNominal()
//------------------------------------------------------------------------------
/**
 * Run out the nominal sequence, generating the "current" estimator data.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::RunNominal()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS propagating\n");
   #endif

   // On success, set the state to the next machine state
   WriteToTextFile();
   // TODO: Find the next time step here
   currentState = CALCULATING;
}


//------------------------------------------------------------------------------
//  void CalculateParameters()
//------------------------------------------------------------------------------
/**
 * Updates the values for the state variables based upon the observation error
 * and the information matrix.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::CalculateParameters()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS accumulating\n");
   #endif

   bool moreData = false;

   #ifdef DEBUG_STATE_TRANSITIONS
      // Dummy code to test state transitions:
      static Integer dummyValue = 0;
      if (dummyValue < 5)
      {
         ++dummyValue;
         moreData = true;
      }
      else
      {
         dummyValue = 0;
         moreData = false;
      }
   #endif

   // TODO: Add accumulation here
   // TODO: Add code to check if at end of measurements here

   if (moreData == true)
      currentState = PROPAGATING;
   else
      currentState = ESTIMATING;
}


void BatchLeastSquares::Estimate()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS estimating\n");
   #endif

   // TODO: Add code to calculate the state updates
   currentState = CHECKINGRUN;
}


//------------------------------------------------------------------------------
//  void CheckCompletion()
//------------------------------------------------------------------------------
/**
 * Determine whether or not the targeting run has converged.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::CheckCompletion()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS checking for Convergence\n");
   #endif

   WriteToTextFile();
   bool converged = false;          // Assume not converged convergence
   // TODO: Add code to check for convergence

   #ifdef DEBUG_STATE_TRANSITIONS
      // Dummy code to test state transitions:
      static Integer dummyValue = 0;
      if (dummyValue < 3)
      {
         ++dummyValue;
         converged = false;
      }
      else
      {
         dummyValue = 0;
         converged = true;
      }
   #endif

   if (!converged)
   {
      // TODO: Add code to reset the state data prior to restarting prop
      // TODO: Add code to reset the measurement models to the first data point
      // TODO: Reset the prop-to epoch to the initial one
      currentState = PROPAGATING;
   }
   else
      // If converged, we're done
      currentState = FINISHED;
}


//------------------------------------------------------------------------------
//  void RunComplete()
//------------------------------------------------------------------------------
/**
 * Updates the estimator text file at the end of a estimator run.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::RunComplete()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS finished\n");
   #endif

   WriteToTextFile();
}


//------------------------------------------------------------------------------
//  void CalculateInformationMatrix()
//------------------------------------------------------------------------------
/**
 * Calculates the Information Matrix.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::CalculateInformationMatrix()
{

  // needs to be completed

}


//------------------------------------------------------------------------------
//  void InvertInformationMatrix()
//------------------------------------------------------------------------------
/**
 * Inverts the information matrix.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::InvertJacobian()
{
   // These are NOT RIGHT, just added so the code compiles
   Rmatrix info(variableCount, variableCount),
   InformationMatrix(variableCount, variableCount),
   inverseInformationMatrix(variableCount, variableCount);

   for (Integer i = 0; i < variableCount; ++i)
      for (Integer j = 0; j < variableCount; ++j)
         info(i,j) = InformationMatrix(i, j);
//         info(i,j) = InformationMatrix[i][j];

   Rmatrix inv = info.Inverse();
   for (Integer i = 0; i < variableCount; ++i)
      for (Integer j = 0; j < variableCount; ++j)
         inverseInformationMatrix(i, j) = inv(i,j);
//         inverseInformationMatrix[i][j] = inv(i,j);
}


//------------------------------------------------------------------------------
//  void FreeArrays()
//------------------------------------------------------------------------------
/**
 * Frees the memory used by the estimator, so it can be reused later in the
 * sequence.  This method is also called by the destructor when the script is
 * cleared.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::FreeArrays()
{
   Estimator::FreeArrays();

   if (textFile.is_open())
   {
      textFile.flush();
      textFile.close();
   }

// These are all handled in the ancestor classes.
//   if (variable)
//   {
//      delete [] variable;
//      variable = NULL;
//   }
//
//   if (variableMinimum)
//   {
//      delete [] variableMinimum;
//      variableMinimum = NULL;
//   }
//
//   if (variableMaximum)
//   {
//      delete [] variableMaximum;
//      variableMaximum = NULL;
//   }

   if (goal)
   {
      delete [] goal;
      goal = NULL;
   }

   if (tolerance)
   {
      delete [] tolerance;
      tolerance = NULL;
   }

   if (nominal)
   {
      delete [] nominal;
      nominal = NULL;
   }

//   if (InformationMatrix)
//   {
//      for (Integer i = 0; i < variableCount; ++i)
//         delete [] InformationMatrix[i];
//      delete [] InformationMatrix;
//      InformationMatrix = NULL;
//   }
//
//   if (inverseInformationMatrix)
//   {
//      for (Integer i = 0; i < variableCount; ++i)
//         delete [] inverseInformationMatrix[i];
//      delete [] inverseInformationMatrix;
//      inverseInformationMatrix = NULL;
//   }
}


//------------------------------------------------------------------------------
//  std::string GetProgressString()
//------------------------------------------------------------------------------
/**
 * Generates a string that reporting the current estimator state.
 */
//------------------------------------------------------------------------------
std::string BatchLeastSquares::GetProgressString()
{
   StringArray::iterator current;
   Integer i;
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
               Integer localVariableCount = variableNames.size(),
                       localGoalCount = goalNames.size();
               progress << "************************************************"
                        << "********\n"
                        << "*** Performing Batch Least Squares "
                        << "(using \"" << instanceName << "\")\n";

               // Write out the setup data
               progress << "*** " << localVariableCount << " variables; "
                        << localGoalCount << " goals\n   Variables:  ";

               // Iterate through the variables and goals, writing them to
               // the file
               for (current = variableNames.begin(), i = 0;
                    current != variableNames.end(); ++current)
               {
                  if (current != variableNames.begin())
                     progress << ", ";
                  progress << *current;
               }

               progress << "\n   Goals:  ";

               for (current = goalNames.begin(), i = 0;
                    current != goalNames.end(); ++current)
               {
                  if (current != goalNames.begin())
                     progress << ", ";
                  progress << *current;
               }

               progress << "\n****************************"
                        << "****************************";
            }
            break;

         case NOMINAL:
            progress << instanceName << " Iteration " << iterationsTaken+1
                     << "; Nominal Pass\n   Variables:  ";
            // Iterate through the variables, writing them to the string
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
            {
               if (current != variableNames.begin())
                  progress << ", ";
               //progress << *current << " = " << variable[i++];
               progress << *current << " = " << variable.at(i++);
            }
            break;

	    //case PERTURBING:

         case CALCULATING:
            // Just forces a blank line
            break;

         case CHECKINGRUN:
            // Iterate through the goals, writing them to the file
            progress << "   Goals and achieved values:\n      ";

            for (current = goalNames.begin(), i = 0;
                 current != goalNames.end(); ++current)
            {
               if (current != goalNames.begin())
                  progress << ",  ";
               progress << *current << "  Desired: " << goal[i]
                        << "  Achieved: " << nominal[i];
               ++i;
            }

            break;

         case FINISHED:
            progress << "\n*** Batch Least Squares Estimation Completed in " << iterationsTaken
                     << " iterations";

            if (iterationsTaken > maxIterations)
               progress << "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
                     << "!!! WARNING: BLS Estimator did NOT converge!"
                     << "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                     << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!";

            progress << "\nFinal Variable values:\n";
            // Iterate through the variables, writing them to the string
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
               //progress << "   " << *current << " = " << variable[i++] << "\n";
               progress << "   " << *current << " = " << variable.at(i++) << "\n";
            break;

         case ITERATING:     // Intentional fall through

            progress << "   Completed iteration " << iterationsTaken
                     << ", pert " << pertNumber+1 << " ("
                     << variableNames[pertNumber] << " = "
                     << variable.at(pertNumber) << ")\n";
                     //<< variable[pertNumber] << ")\n";
            break;

         default:
            throw EstimatorException(
               "Estimator state not supported for the targeter");
      }
   }
   else
      return Estimator::GetProgressString();

   return progress.str();
}


//------------------------------------------------------------------------------
//  void WriteToTextFile()
//------------------------------------------------------------------------------
/**
 * Writes state data to the targeter text file.
 */
//------------------------------------------------------------------------------
//void BatchLeastSquares::WriteToTextFile(EstimatorState stateToUse)
void BatchLeastSquares::WriteToTextFile(SolverState stateToUse)
{
   StringArray::iterator current;
   Integer i, j;
   if (initialized)
   {
      switch (currentState)
      {
         case INITIALIZING:
            // This state is basically a "paused state" used for the Target
            // command to finalize the initial data for the variables and
            // goals.  All that is written here is the header information.
            {
               Integer localVariableCount = variableNames.size(),
                       localGoalCount = goalNames.size();
               textFile << "************************************************"
                        << "********\n"
                        << "*** Targeter Text File\n"
                        << "*** \n"
                        << "*** Using Differential Correction\n***\n";

               // Write out the setup data
               textFile << "*** " << localVariableCount << " variables\n*** "
                        << localGoalCount << " goals\n***\n*** "
                        << "Variables:\n***    ";

               // Iterate through the variables and goals, writing them to
               // the file
               for (current = variableNames.begin(), i = 0;
                    current != variableNames.end(); ++current)
               {
                  textFile << *current << "\n***    ";
               }

               textFile << "\n*** Goals:\n***    ";

               for (current = goalNames.begin(), i = 0;
                    current != goalNames.end(); ++current)
               {
                  textFile << *current << "\n***    ";
               }

               textFile << "\n****************************"
                        << "****************************\n"
                        << std::endl;
            }
            break;

         case NOMINAL:
            textFile << "Iteration " << iterationsTaken+1
                     << "\nRunning Nominal Pass\nVariables:\n   ";
            // Iterate through the variables, writing them to the file
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
            {
               //textFile << *current << " = " << variable[i++] << "\n   ";
               textFile << *current << " = " << variable.at(i++) << "\n   ";
            }
            textFile << std::endl;
            break;

         case PERTURBING:
            if ((textFileMode == "Verbose") || (textFileMode == "Debug"))
            {
               if (pertNumber != 0)
               {
                  // Iterate through the goals, writing them to the file
                  textFile << "Goals and achieved values:\n   ";

                  for (current = goalNames.begin(), i = 0;
                       current != goalNames.end(); ++current)
                  {
                     textFile << *current << "  Desired: " << goal[i]
                              << " Achieved: " << achieved[pertNumber-1][i]
                              << "\n   ";
                     ++i;
                  }
                  textFile << std::endl;
               }
               textFile << "Perturbing with variable values:\n   ";
               for (current = variableNames.begin(), i = 0;
                    current != variableNames.end(); ++current)
               {
                  //textFile << *current << " = " << variable[i++] << "\n   ";
                  textFile << *current << " = " << variable.at(i++) << "\n   ";
               }
               textFile << std::endl;
            }

            if (textFileMode == "Debug")
            {
               textFile << "------------------------------------------------\n"
                        << "Command stream data:\n"
                        << debugString << "\n"
                        << "------------------------------------------------\n";
            }

            break;

         case CALCULATING:
            if (textFileMode == "Verbose")
            {
               textFile << "Calculating" << std::endl;

               // Iterate through the goals, writing them to the file
               textFile << "Goals and achieved values:\n   ";

               for (current = goalNames.begin(), i = 0;
                    current != goalNames.end(); ++current)
               {
                   textFile << *current << "  Desired: " << goal[i]
                            << " Achieved: " << achieved[variableCount-1][i]
                            << "\n    ";
                   ++i;
               }
               textFile << std::endl;
            }

            textFile << "\nJacobian (Sensitivity matrix):\n";
            for (i = 0; i < variableCount; ++i)
            {
               for (j = 0; j < goalCount; ++j)
               {
                  textFile << "   " << jacobian[i][j];
               }
               textFile << "\n";
            }

            textFile << "\n\nInverse Jacobian:\n";
            for (i = 0; i < variableCount; ++i)
            {
               for (j = 0; j < goalCount; ++j)
               {
                  textFile << "   " << inverseJacobian[i][j];
               }
               textFile << "\n";
            }

            textFile << "\n\nNew variable estimates:\n   ";
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
            {
               //textFile << *current << " = " << variable[i++] << "\n   ";
               textFile << *current << " = " << variable.at(i++) << "\n   ";
            }
            textFile << std::endl;
            break;

         case CHECKINGRUN:
            // Iterate through the goals, writing them to the file
            textFile << "Goals and achieved values:\n   ";

            for (current = goalNames.begin(), i = 0;
                 current != goalNames.end(); ++current)
            {
               textFile << *current << "  Desired: " << goal[i]
                        << " Achieved: " << nominal[i]
                        << "\n   Tolerance: " << tolerance[i]
                        << "\n   ";
               ++i;
            }

            textFile << "\n*****************************"
                     << "***************************\n"
                     << std::endl;
            break;

         case FINISHED:
            textFile << "\n****************************"
                     << "****************************\n"
                     << "*** Targeting Completed in " << iterationsTaken
                     << " iterations"
                     << "\n****************************"
                     << "****************************\n"
                     << std::endl;

            break;

         case ITERATING:     // Intentional fall through
         default:
            throw EstimatorException(
               "Estimator state not supported for the targeter");
      }
   }
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
void BatchLeastSquares::ReportProgress()
{
   if (showProgress)
   {
      // MessageInterface::ShowMessage("\n");
   }
}

