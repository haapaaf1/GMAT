//$Id$
//------------------------------------------------------------------------------
//                              SteepestDescent
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2007/05/23
//
/**
 * Implementation for the steepest descent optimizer. 
 */
//------------------------------------------------------------------------------


#include "SteepestDescent.hpp"
#include "MessageInterface.hpp"

#define SD_DEBUG_STATE_MACHINE
//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

const std::string
SteepestDescent::PARAMETER_TEXT[SteepestDescentParamCount - SolverParamCount] =
{
   "Objective",
//   "Constraint",
   "UseCentralDifferences"
};

const Gmat::ParameterType
SteepestDescent::PARAMETER_TYPE[SteepestDescentParamCount - SolverParamCount] =
{
   Gmat::STRINGARRAY_TYPE,
//   Gmat::STRINGARRAY_TYPE,
   Gmat::BOOLEAN_TYPE
};



//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

SteepestDescent::SteepestDescent(const std::string &name) :
   InternalOptimizer      ("SteepestDescent", name)
{
   objectTypeNames.push_back("SteepestDescent");
   objectiveFnName = "SDObjective";
   tolerance       = 1.0e-5;
   maxIterations   = 200;
}


SteepestDescent::~SteepestDescent()
{
}


SteepestDescent::SteepestDescent(const SteepestDescent& sd) :
   InternalOptimizer    (sd)
{
}


SteepestDescent& SteepestDescent::operator=(const SteepestDescent& sd)
{
   if (&sd != this)
   {
      InternalOptimizer::operator=(sd);
   }
   
   return *this;
}


GmatBase* SteepestDescent::Clone() const
{
   return new SteepestDescent(*this);
}



//------------------------------------------------------------------------------
//  bool TakeAction(const std::string &action, const std::string &actionData)
//------------------------------------------------------------------------------
/**
 * This method performs an action on the instance.
 *
 * TakeAction is a method overridden from GmatBase.  The only action defined for
 * a DifferentialCorrector is "IncrementInstanceCount", which the Sandbox uses
 * to tell an instance if if it is a reused instance (i.e. a clone) of the
 * configured instance of the DifferentialCorrector.
 *
 * @param <action>      Text label for the action.
 * @param <actionData>  Related action data, if needed.
 *
 * @return  The value of the parameter at the completion of the call.
 */
//------------------------------------------------------------------------------
bool SteepestDescent::TakeAction(const std::string &action,
                                       const std::string &actionData)
{
//   if (action == "IncrementInstanceCount")
//   {
//      ++instanceCount;
//      return true;
//   }
 
   if (action == "Reset")
   {
      currentState = INITIALIZING;
      return true;
   }

   return InternalOptimizer::TakeAction(action, actionData);
}


//------------------------------------------------------------------------------
// Integer SetSolverResults(Real *data, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Implements the steepest descent state machine.
 * 
 * @return true if the state maching step worked, false if an error was 
 * detected.
 */
//------------------------------------------------------------------------------
Solver::SolverState  SteepestDescent::AdvanceState()
{
   switch (currentState)
   {
      case INITIALIZING:
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "INITIALIZING\n");
         #endif
         iterationsTaken = 0;
         WriteToTextFile();
//         ReportProgress();
         CompleteInitialization();
      
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "SteepestDescent State Transitions from %d to %d\n", 
               INITIALIZING, currentState);
         #endif
         break;
      
      case NOMINAL:
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "NOMINAL\n");
         #endif
//         ReportProgress();
         RunNominal();
//         ReportProgress();
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "SteepestDescent State Transitions from %d to %d\n", NOMINAL,
               currentState);
         #endif
         // ReportProgress();
         break;
   
      case PERTURBING:
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "PERTURBING\n");
         #endif
         RunPerturbation();
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "SteepestDescent State Transitions from %d to %d\n", PERTURBING,
               currentState);
         #endif
         // ReportProgress();
         break;
   
      case Solver::CALCULATING:
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "CALCULATING\n");
         #endif
//         ReportProgress();
         CalculateParameters();
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "SteepestDescent State Transitions from %d to %d\n", CALCULATING,
               currentState);
         #endif
         break;
            
      case CHECKINGRUN:
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "CHECKINGRUN\n");
         #endif
         CheckCompletion();
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "SteepestDescent State Transitions from %d to %d\n", CHECKINGRUN,
               currentState);
         #endif
         // ReportProgress();
         break;
   
      case FINISHED:
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "FINISHED\n");
         #endif
         RunComplete();
         #ifdef SD_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "SteepestDescent State Transitions from %d to %d\n", FINISHED,
               currentState);
         #endif
         // ReportProgress();
         break;
         
      default:
         throw SolverException(
                  "Steepest Descent Solver \"" + instanceName + 
                  "\" encountered an unexpected state.");
   }
      
   return currentState;
}

//------------------------------------------------------------------------------
// 
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
bool SteepestDescent::Optimize()
{
   return true;
}


//------------------------------------------------------------------------------
// Integer SetSolverResults(Real *data, const std::string &name)
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
Integer SteepestDescent::SetSolverResults(Real *data,
                                          const std::string &name,
                                          const std::string &type)
{
   if (type == "Objective")
      objectiveName = name;
 
   if (type == "EqConstraint")
      equalityConstraints.push_back(name);
   
   if (type == "IneqConstraint")
      inequalityConstraints.push_back(name);
 
   return InternalOptimizer::SetSolverResults(data, name, type);
}


//------------------------------------------------------------------------------
// void SetResultValue(Integer id, Real value)
//------------------------------------------------------------------------------
/**
 * Passes in the results obtained from a run in the DifferentialCorrector loop.
 * 
 * @param <id>    The ID used for this result.
 * @param <value> The corresponding result.
 */
//------------------------------------------------------------------------------
void SteepestDescent::SetResultValue(Integer id, Real value,
                                           const std::string &resultType)
{
   MessageInterface::ShowMessage("Setting SD result for id = %d, type = %s\n", 
         id, resultType.c_str());

   // Gradients use the objective function
   if (resultType == "Objective")
   {
      if (currentState == NOMINAL) 
      {
         gradientCalculator.Achieved(-1, id, 0.0, value);
      }
           
      if (currentState == PERTURBING) 
      {
         gradientCalculator.Achieved(pertNumber, id, perturbation[pertNumber], 
                                     value);
      }
   }
   
   /// Add code for the constraint feeds here
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the DifferentialCorrector prior to targeting.
 */
//------------------------------------------------------------------------------
bool SteepestDescent::Initialize()
{
   // Variable initialization is in the Solver code
   bool retval = InternalOptimizer::Initialize();
   
   if (retval)
      retval = gradientCalculator.Initialize(variable.size());
   
   if (retval)
   {
      equalityCount   = equalityConstraints.size();
      constraintCount = inequalityConstraints.size() + equalityCount;
      
      if (constraintCount > 0)
         retval = jacobianCalculator.Initialize(variableCount, constraintCount);
   }
   
   #if DEBUG_SD_INIT
   MessageInterface::ShowMessage
      ("SteepestDescent::Initialize() completed\n");
   #endif

   return retval;
}

//------------------------------------------------------------------------------
// Protected methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// State machine methods
//------------------------------------------------------------------------------
void SteepestDescent::RunNominal()
{
   pertNumber = -1;
   currentState = PERTURBING;
}

void SteepestDescent::RunPerturbation()
{
   // Calculate the perts, one at a time
   if (pertNumber != -1)
      // Back out the last pert applied
      variable.at(pertNumber) = lastUnperturbedValue;
   ++pertNumber;

   if (pertNumber == variableCount)  // Current set of perts have been run
   {
      currentState = CALCULATING;
      pertNumber = -1;
      return;
   }

   lastUnperturbedValue = variable.at(pertNumber);
   variable.at(pertNumber) += perturbation.at(pertNumber);
   pertDirection.at(pertNumber) = 1.0;
   
   if (variable[pertNumber] > variableMaximum[pertNumber])
   {
      pertDirection.at(pertNumber) = -1.0;
      variable[pertNumber] -= 2.0 * perturbation[pertNumber];
   }    
   if (variable[pertNumber] < variableMinimum[pertNumber])
   {
      pertDirection.at(pertNumber) = -1.0;
      variable[pertNumber] -= 2.0 * perturbation[pertNumber];
   }
       
   WriteToTextFile();
}


void SteepestDescent::CalculateParameters()
{
   gradientCalculator.Calculate(gradient);
   currentState = CHECKINGRUN;
}


void SteepestDescent::CheckCompletion()
{
   currentState = FINISHED;
}


void SteepestDescent::RunComplete()
{
}


//------------------------------------------------------------------------------
// Math methods used when runnignthe state machine
//------------------------------------------------------------------------------

void SteepestDescent::CalculateJacobian()
{
}


void SteepestDescent::LineSearch()
{
}


//------------------------------------------------------------------------------
// Utility methods
//------------------------------------------------------------------------------

void SteepestDescent::FreeArrays()
{
}


//------------------------------------------------------------------------------
//  void WriteToTextFile()
//------------------------------------------------------------------------------
/**
 * Utility function used by the solvers to generate a progress file.
 * 
 * @param <stateToUse> SolverState used for the report; if this parameter is 
 *                     different from the default value (UNDEFINED_STATE), 
 *                     it is used.  If the value is UNDEFINED_STATE, then the 
 *                     value of currentState is used. 
 */
//------------------------------------------------------------------------------
void SteepestDescent::WriteToTextFile(SolverState stateToUse)
{
}
