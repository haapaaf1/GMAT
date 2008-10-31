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
   Estimator                  ("BatchLeastSquares", name)
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
}


BatchLeastSquares::BatchLeastSquares(const BatchLeastSquares &dc) :
   Estimator                  (dc)
{
   #if DEBUG_DC_INIT
   MessageInterface::ShowMessage
      ("BatchLeastSquares::DC(COPY constructor) entered\n");
   #endif
   parameterCount = dc.parameterCount;
}


BatchLeastSquares&
    BatchLeastSquares::operator=(const BatchLeastSquares& dc)
{
    if (&dc == this)
        return *this;

   Estimator::operator=(dc);
   
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
   }

   return Estimator::TakeAction(action, actionData);
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

   Estimator::Initialize(); // for commented stuff, moved to Estimator
   
   // Initilize epoch
   epoch = 0.0;
   
   // Initialize stateCount to zero
   stateCount = 0;
   
   // Initialize observer count to zero
   observerCount = 0;
   
   // Initialize observer index to zero
   observerIndex = 0;   
   
   // Set index of observation to be processed to zero
   obIndex = 0;
   
   // Initialize weighting matrix to identity matrix
   W = LaGenMatDouble::eye(observationCount);
   
   // Initialize state update to zero
   estimatorStateCorrection = LaGenMatDouble::zeros(stateCount);
   
   // Initialize time step
   timeStep = 0;
   
   // Initialize converged flag
   converged = false;

   
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
         FindTimeStep();
         ReportProgress();
         break;

      case CALCULATING:
         #ifdef DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; CALCULATING\n");
         #endif
         ReportProgress();
         Accumulate();
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
	 if (!converged)
	 {
            Update();
            ++iterationsTaken;
	    if (iterationsTaken > maxIterations)
	    {
		MessageInterface::ShowMessage("Differential corrector %s %s\n",
		instanceName.c_str(),
		"has exceeded to maximum number of allowed iterations.");
		currentState = FINISHED;
	    }
	    else
		Reinitialize();

	 }
	 else
	    // If converged, we're done
	    currentState = FINISHED;

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
//------------------------------------------------------------------------------
//  void CompleteInitialization()
//------------------------------------------------------------------------------
/**
 * Run out the nominal sequence, generating the "current" estimator data.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::CompleteInitialization()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS initializing\n");
   #endif

   WriteToTextFile();
   // TODO: Any additional initialization needed
   
   // Set index of observation to be processed to zero
   obIndex = 0;
   
   // Find satellite state
   // TODO: Make this a loop for all owners
   GmatBase* objID = *solveForOwners.begin();
   theSat = (Spacecraft*)objID;
   ps = &theSat->GetState();
   Integer n = ps->GetSize();
   
   // Get stations involved in this
   for (ObjectArray::iterator j = participants.begin();
           j != participants.end(); ++j)
   {
	if ((*j)->IsOfType(Gmat::GROUND_STATION))
        {
   
	    // TODO: Make this work for any number of ground stations
	    // We found an observer so increment observerCount
	    observerCount++;
	    theGroundStation = (GroundStation*)*j;
	}
   }

   // epoch is GMAT's A.1 modified Julian epoch
   // Set time step to be difference between epoch and time of first ob
   epoch = theSat->GetEpoch();
   timeStep = y(obIndex) - epoch;
   
   // Assign weighting matrix to desired initial values
   // W = LaGenMatDouble::eye(observationCount);
   
   // Initialize estimated state to current state
   // X is LaVectorDouble
   Real* x1 = ps->GetState();
   
   // Initialize a new LaVectorDouble vector using the Real* array x1
   LaVectorDouble xtemp(x1,n);
   
   // Copy xtemp into x.
   x.copy(xtemp);
   
   // Initialize stateCount
   stateCount = n;
   
   // Initialize state update to zero
   estimatorStateCorrection = LaGenMatDouble::zeros(stateCount);
   
   currentState = PROPAGATING;
}
//------------------------------------------------------------------------------
//  Real FindTimeStep()
//------------------------------------------------------------------------------
/**
 * Run out the nominal sequence, generating the "current" estimator data.
 */
//------------------------------------------------------------------------------
Real BatchLeastSquares::FindTimeStep()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS propagating\n");
   #endif

   // On success, set the state to the next machine state
   WriteToTextFile();

   // Find the next time step here in seconds
   // convert from GMAT's A.1 Modified Julian Date
   // TODO: Fix time vector so that there is a integer and real part
   //       for higher precision work
   timeStep = (observationTimes(obIndex+1) - observationTimes(obIndex))*86400.0;
   
   currentState = CALCULATING;
   
   return timeStep;
}


//------------------------------------------------------------------------------
//  void Accumulate()
//------------------------------------------------------------------------------
/**
 * Computes the difference between observed and computed quantities and partials.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::Accumulate()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS accumulating\n");
   #endif

   bool moreData = false;
    
   // Compute observed minus computed measurement
   for (std::vector<MeasurementModel*>::iterator i = measModels.begin(); i != measModels.end(); ++i)
   {
       MeasurementModel *current = *i;
       Integer m = current->GetNumMeasurements();
       LaVectorDouble ycomputed(m);
       
       if(current->ComputeMeasurement(theGroundStation,theSat,ycomputed))
       {
	    z(LaIndex(obIndex,obIndex+m)) = y(LaIndex(obIndex,obIndex+m))-ycomputed;
       }
       
       // Get partial derivatives
       LaGenMatDouble thisH(m,6);
       current->ComputeCartesianPartialDerivative(theGroundStation,theSat,thisH);
       
       // Construct H matrix       
       Integer i = obIndex+m*observerIndex;
       Integer j = i+m;
       H(LaIndex(i,j),LaIndex(0,5)) = thisH(LaIndex(0,m-1),LaIndex(0,5));
       
       // Increment observer Index
       observerIndex++;
   }
   
   
   // Code to check if at end of measurements

      if (obIndex < observationCount)
   {
	++obIndex;
	moreData = true;
   }
   else
   {
	obIndex = 0;
	moreData = false;
   }
   
   if (moreData == true)
      currentState = PROPAGATING;
   else
      currentState = ESTIMATING;
}

//------------------------------------------------------------------------------
//  void Update()
//------------------------------------------------------------------------------
/**
 * Update the states of objects based upon current estimates.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::Update()
{
    x = x + estimatorStateCorrection;   
}

//------------------------------------------------------------------------------
//  void Reinitialize()
//------------------------------------------------------------------------------
/**
 * Reinitialize the states of objects based upon current estimates.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::Reinitialize()
{
    // Set satellite epoch back to what it originally was
    ps->SetEpoch(epoch);
    // Set the initial satellite state to updated state estimate
    // TODO:: make this general by extracting subvectors
    ps->SetState(x.addr(),6);
    currentState = PROPAGATING;
}

//------------------------------------------------------------------------------
//  void Estimate()
//------------------------------------------------------------------------------
/**
 * Calculate the estimated state.
 */
//------------------------------------------------------------------------------
void BatchLeastSquares::Estimate()
{
   #ifdef DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage("BLS estimating\n");
   #endif

   // Compute Information Matrix using Blas matrix multipication routines
   //informationMatrix = H^T*W*H;
    LaGenMatDouble temp1(observationCount,stateCount);
    Blas_Mat_Mat_Mult(W,H,temp1);
    Blas_Mat_Trans_Mat_Mult(H,temp1,informationMatrix);
      
      
   // Copy information matrix into covariance matrix. 
   // Then compute inverse in place.
      LaVectorLongInt piv(P.size(0)); // pivot vector
      LaLUInverseIP(P,piv);
      
   // Compute state correction
   // deltaX = P^-1*H^t*W*z
      LaVectorDouble temp(observationCount);     
      Blas_Mat_Vec_Mult(W,z,temp);
      Blas_Mat_Trans_Vec_Mult(H,temp,temp);
      Blas_Mat_Vec_Mult(P,temp,estimatorStateCorrection);
   
   // Compute cost
   
   LaVectorDouble Jtemp = z-H*estimatorStateCorrection;
   Blas_Mat_Vec_Mult(W,Jtemp,temp);
   J(obIndex) = 0.5*Blas_Dot_Prod(Jtemp,temp);
      
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
   converged = false;          // Assume not converged convergence
   // TODO: Add code to check for convergence
     
   if ((J(obIndex)-J(obIndex-1))/J(obIndex) < globalConvergenceTolerance/Blas_NormF(W))
   {
       converged = true;
   }
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
void BatchLeastSquares::InvertInformationMatrix()
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
//  std::string GetProgressString()
//------------------------------------------------------------------------------
/**
 * Generates a string that reporting the current estimator state.
 */
//------------------------------------------------------------------------------
std::string BatchLeastSquares::GetProgressString()
{
   std::stringstream progress;
   progress.str("");
   progress.precision(12);

   if (initialized)
   {
      switch (currentState)
      {
         case INITIALIZING:
/*
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
 */
            break;

         case NOMINAL:
/*
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
 */
            break;

	    //case PERTURBING:

         case CALCULATING:
            // Just forces a blank line
            break;

         case CHECKINGRUN:
/*
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
 */

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
/*
            progress << "\nFinal Variable values:\n";
            // Iterate through the variables, writing them to the string
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
               //progress << "   " << *current << " = " << variable[i++] << "\n";
               progress << "   " << *current << " = " << variable.at(i++) << "\n";
 */

	    break;

         case ITERATING:     // Intentional fall through
/*
            progress << "   Completed iteration " << iterationsTaken
                     << ", pert " << pertNumber+1 << " ("
                     << variableNames[pertNumber] << " = "
                     << variable.at(pertNumber) << ")\n";
                     //<< variable[pertNumber] << ")\n";
 */
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
   if (initialized)
   {
      switch (currentState)
      {
         case INITIALIZING:
/*	     
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
 */
            break;

         case NOMINAL:
/*
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
 */
	    break;

         case PERTURBING:
/*
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
 */

            break;

         case CALCULATING:
/*
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
 */
            break;

         case CHECKINGRUN:
/*
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
 */
            break;

         case FINISHED:
            textFile << "\n****************************"
                     << "****************************\n"
                     << "*** BatchLeastSquares Completed in " << iterationsTaken
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

