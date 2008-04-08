// $Id$
//------------------------------------------------------------------------------
//                              VF13ad
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/04/03
//
/**
 * Implementation for the VF13ad optimizer. 
 * 
 * This is prototype code.  Interested parties need to roll the VF13ad optimizer
 * code into a separate library and link to it.  Contact Thinking Systems for
 * details.
 */
//------------------------------------------------------------------------------


#include "VF13ad.hpp"
#include "MessageInterface.hpp"

extern "C"
{
   #include "VF13.h"
};

//#undef max     // f2c.h defines an incompatible version of max

//#define VF13_DEBUG_STATE_MACHINE
//#define DEBUG_VF13_INIT
//#define DEBUG_VF13_CALL

//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

const std::string
VF13ad::PARAMETER_TEXT[VF13adParamCount - SolverParamCount] =
{
//   "Objective",
//   "Constraint",
   "UseCentralDifferences"
};

const Gmat::ParameterType
VF13ad::PARAMETER_TYPE[VF13adParamCount - SolverParamCount] =
{
   Gmat::STRINGARRAY_TYPE,
//   Gmat::STRINGARRAY_TYPE,
   Gmat::BOOLEAN_TYPE
};



//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

VF13ad::VF13ad(const std::string &name) :
   InternalOptimizer       ("VF13ad", name),
   retCode                 (-1)
{
   objectTypeNames.push_back("VF13ad");
   objectiveFnName = "SDObjective";
   tolerance       = 1.0e-5;
   maxIterations   = 200;
}


VF13ad::~VF13ad()
{
}


VF13ad::VF13ad(const VF13ad& sd) :
   InternalOptimizer       (sd),
   jacobian                (sd.jacobian),
   retCode                 (sd.retCode)
{
}


VF13ad& VF13ad::operator=(const VF13ad& sd)
{
   if (&sd != this)
   {
      InternalOptimizer::operator=(sd);

      gradient = sd.gradient;
      jacobian = sd.jacobian;
   }
   
   return *this;
}


GmatBase* VF13ad::Clone() const
{
   return new VF13ad(*this);
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
bool VF13ad::TakeAction(const std::string &action,
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
 * Implements the VF13ad state machine.
 * 
 * @return true if the state maching step worked, false if an error was 
 * detected.
 */
//------------------------------------------------------------------------------
Solver::SolverState  VF13ad::AdvanceState()
{
   switch (currentState)
   {
      case INITIALIZING:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "INITIALIZING\n");
         #endif
         iterationsTaken = 0;
         WriteToTextFile();
//         ReportProgress();
         CompleteInitialization();
      
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "VF13ad State Transitions from %d to %d\n", 
               INITIALIZING, currentState);
         #endif
         break;
      
      case NOMINAL:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "NOMINAL\n");
         #endif
//         ReportProgress();
         RunNominal();
//         ReportProgress();
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "VF13ad State Transitions from %d to %d\n", NOMINAL,
               currentState);
         #endif
         // ReportProgress();
         break;
   
      case PERTURBING:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "PERTURBING\n");
         #endif
         RunPerturbation();
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "VF13ad State Transitions from %d to %d\n", PERTURBING,
               currentState);
         #endif
         // ReportProgress();
         break;
   
      case Solver::CALCULATING:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "CALCULATING\n");
         #endif
//         ReportProgress();
         CalculateParameters();
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "VF13ad State Transitions from %d to %d\n", CALCULATING,
               currentState);
         #endif
         break;
            
      case CHECKINGRUN:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "CHECKINGRUN\n");
         #endif
         CheckCompletion();
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "VF13ad State Transitions from %d to %d\n", CHECKINGRUN,
               currentState);
         #endif
         // ReportProgress();
         break;
   
      case FINISHED:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "FINISHED\n");
         #endif
         RunComplete();
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage(
               "VF13ad State Transitions from %d to %d\n", FINISHED,
               currentState);
         #endif
         // ReportProgress();
         break;
         
      default:
         throw SolverException(
                  "VF13ad Solver \"" + instanceName + 
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
bool VF13ad::Optimize()
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
Integer VF13ad::SetSolverResults(Real *data,
                                          const std::string &name,
                                          const std::string &type)
{
   #ifdef DEBUG_VF13ad
      MessageInterface::ShowMessage("*** Setting Results for '%s' of type '%s'\n",
            name.c_str(), type.c_str());
   #endif

   if (type == "Objective")
      objectiveName = name;
 
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
void VF13ad::SetResultValue(Integer id, Real value,
                                           const std::string &resultType)
{
#ifdef DEBUG_VF13ad
   MessageInterface::ShowMessage("Setting SD result for id = %d, type = %s\n", 
         id, resultType.c_str());
#endif
   
   // Gradients use the objective function
   if (resultType == "Objective")
   {
      if (currentState == NOMINAL) 
      {
         // id (2nd parameter here) for gradients is always 0
         cost = value;
         gradientCalculator.Achieved(-1, 0, 0.0, value);
      }
           
      if (currentState == PERTURBING) 
      {
         gradientCalculator.Achieved(pertNumber, 0, perturbation[pertNumber], 
                                     value);
      }
   }
   else
   {
      // build the correct ID number
      Integer idToUse;
      if (resultType == "EqConstraint")
         idToUse = id - 1000;
      else
         idToUse = id - 2000 + eqConstraintCount;
      
      if (currentState == NOMINAL) 
      {
         jacobianCalculator.Achieved(-1, idToUse, 0.0, value);
      }
           
      if (currentState == PERTURBING) 
      {
         jacobianCalculator.Achieved(pertNumber, idToUse, perturbation[pertNumber], 
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
bool VF13ad::Initialize()
{
   // Variable initialization is in the Solver code
   bool retval = InternalOptimizer::Initialize();
   
   if (retval)
      retval = gradientCalculator.Initialize(registeredVariableCount);
   
   if (retval)
   {
      if (registeredComponentCount > 0)
         retval = jacobianCalculator.Initialize(registeredVariableCount, 
               registeredComponentCount);
   }
   
   #ifdef DEBUG_VF13_INIT
   MessageInterface::ShowMessage
      ("VF13ad::Initialize() completed; %d variables and %d constraints\n",
       registeredVariableCount, registeredComponentCount);
   #endif

   retCode = -1;
   
   return retval;
}

//------------------------------------------------------------------------------
// Protected methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// State machine methods
//------------------------------------------------------------------------------
void VF13ad::RunNominal()
{
   pertNumber = -1;
   currentState = PERTURBING;
}

void VF13ad::RunPerturbation()
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


void VF13ad::CalculateParameters()
{
   gradientCalculator.Calculate(gradient);
   if (eqConstraintCount + ineqConstraintCount > 0)
      jacobianCalculator.Calculate(jacobian);
   currentState = CHECKINGRUN;
}


void VF13ad::CheckCompletion()
{
   // Most or all of these should move into initialization
   Real     *workspace;
   Real     *grad;
   Real     *vars;
   Real     *constraints;
   Real     *cJacobian;
   Integer  workspaceLength;
   Integer  numConstraints = eqConstraintCount + ineqConstraintCount;
   Integer  varLength = variableCount + 1;
   
   Integer  iprint   = 0; 
   Integer  *integerWorkspace;
   
   #ifdef DEBUG_VF13_CALL
      MessageInterface::ShowMessage("VF13ad Check Completion, input settings:\n"
            "   N:   %d\n   M:   %d\nMEQ: %d\n",
            variableCount, numConstraints, eqConstraintCount);
   #endif 
   
   workspaceLength = (Integer)(5.0 * variableCount * variableCount / 2.0 + 
         43.0 * variableCount / 2.0 + 14 + 6 * numConstraints + 2);

   workspace = new Real[workspaceLength];
   grad = new Real[variableCount];
   vars = new Real[variableCount];
   if (numConstraints > 0)
   {
      #ifdef DEBUG_VF13_CALL
         MessageInterface::ShowMessage("%d variables and %d constraints\n",
               varLength - 1, numConstraints);
      #endif
      cJacobian   = new Real[varLength*numConstraints];
      constraints = new Real[numConstraints];
   }
   else
   {
      cJacobian = NULL;
      constraints = NULL;
   }
   integerWorkspace = new Integer[varLength];

   for (Integer i = 0; i < variableCount; ++i)
   {
      vars[i] = variable[i];
      grad[i] = gradient[i];
      
      for (Integer j = 0; j < numConstraints; ++j)
      {
         // Note that VF13ad indexes opposite to standard Jacobian notations --
         // the rows increment the constraints for a variable variation, while
         // the columns increment the variables for specific constraints Sigh.
         cJacobian[i + varLength * j] = jacobian[i + variableCount * j];
      }
   }

//   #ifdef DEBUG_VF13_CALL
      MessageInterface::ShowMessage("   X:  [");
      for (Integer i = 0; i < variableCount; ++i)
      {
         MessageInterface::ShowMessage("%.12lf", vars[i]);
         if (i < variableCount - 1)
            MessageInterface::ShowMessage(", ");
      }
      MessageInterface::ShowMessage("]\n");
   
      MessageInterface::ShowMessage("   Grad: [");
      for (Integer i = 0; i < variableCount; ++i)
      {
         MessageInterface::ShowMessage("%.12lf", grad[i]);
         if (i < variableCount - 1)
            MessageInterface::ShowMessage(", ");
      }
      MessageInterface::ShowMessage("]\n");

      if (numConstraints > 0)
      {
         MessageInterface::ShowMessage("   CJ: ");
         for (Integer i = 0; i < variableCount; ++i)
         {
            for (Integer j = 0; j < numConstraints; ++j)
            {
               if (j == 0)
                  MessageInterface::ShowMessage("[");
               MessageInterface::ShowMessage("%.12lf", 
                     cJacobian[i*numConstraints+j]);
               if (i < numConstraints - 1)
                  MessageInterface::ShowMessage(", ");
            }
            MessageInterface::ShowMessage("]\n        ");
         }
         MessageInterface::ShowMessage("\n");
      }   
   
      MessageInterface::ShowMessage("Before calling vf13ad, retcode = %d\n", 
            retCode);
      MessageInterface::ShowMessage("   vars = [");
      for (Integer i = 0; i < variableCount; ++i)
      {
         MessageInterface::ShowMessage("%.12lf", vars[i]);
         if (i < variableCount-1)
            MessageInterface::ShowMessage(", ");
      }
      MessageInterface::ShowMessage("]\n");
      MessageInterface::ShowMessage("   cost = %.12lf\n", cost);
      MessageInterface::ShowMessage("   grad = [");
      for (Integer i = 0; i < variableCount; ++i)
      {
         MessageInterface::ShowMessage("%.12lf", grad[i]);
         if (i < variableCount-1)
            MessageInterface::ShowMessage(", ");
      }
      MessageInterface::ShowMessage("]\n");

      MessageInterface::ShowMessage("   numVars              %d\n", 
            variableCount); 
      MessageInterface::ShowMessage("   numConstraints       %d\n", 
            numConstraints);
      MessageInterface::ShowMessage("   numEqConstraints     %d\n", 
            eqConstraintCount);
      MessageInterface::ShowMessage("   vars[0]              %.12lf\n", 
            vars[0]);
      MessageInterface::ShowMessage("   objective            %.12lf\n", cost);
      MessageInterface::ShowMessage("   gradient[0]          %.12lf\n", 
            grad[0]);
      if (numConstraints > 0)
      {
         MessageInterface::ShowMessage("   constraints[0]       %.12lf\n", 
               constraints[0]);
         MessageInterface::ShowMessage("   cJacobian[0]         %.12lf\n", 
               cJacobian[0]);
      }
      MessageInterface::ShowMessage("   varLength            %d\n", varLength);
      MessageInterface::ShowMessage("   maxFunctionEvals     %d\n", 
            maxIterations);
      MessageInterface::ShowMessage("   accuracy             %.12le\n", 
            tolerance);
      MessageInterface::ShowMessage("   iprint               %d\n", iprint);
      MessageInterface::ShowMessage("   retCode              %d\n\n", retCode);
//   #endif

   vf13ad_(
         &variableCount,
         &numConstraints,
         &eqConstraintCount,
         vars,              
         &cost,             
         grad,              
         constraints,       
         cJacobian,         
         &varLength,        
         &maxIterations, 
         &tolerance, 
         &iprint, 
         &retCode, 
         workspace,         
         &workspaceLength,  
         integerWorkspace);
   
   MessageInterface::ShowMessage(InterpretRetCode(retCode));
   
//   #ifdef DEBUG_VF13_CALL
      MessageInterface::ShowMessage("After calling vf13ad, retcode = %d\n", 
               retCode);
         MessageInterface::ShowMessage("   vars = [");
         for (Integer i = 0; i < variableCount; ++i)
         {
            MessageInterface::ShowMessage("%.12lf", vars[i]); 
            if (i < variableCount-1)
               MessageInterface::ShowMessage(", ");
         }
         MessageInterface::ShowMessage("]\n");
//   #endif   

   if (retCode == 0)
   {
      for (Integer i = 0; i < variableCount; ++i)
         variable[i] = vars[i];

      currentState = NOMINAL;
   }
   else
      currentState = FINISHED;
   
   delete [] workspace;
   delete [] grad;
   delete [] vars;
   if (numConstraints > 0)
   {
      delete [] cJacobian;
      delete [] constraints;
   }
   delete [] integerWorkspace;
}


void VF13ad::RunComplete()
{
}


//------------------------------------------------------------------------------
// Math methods used when runnignthe state machine
//------------------------------------------------------------------------------

void VF13ad::CalculateJacobian()
{
}


void VF13ad::LineSearch()
{
}


//------------------------------------------------------------------------------
// Utility methods
//------------------------------------------------------------------------------

void VF13ad::FreeArrays()
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
void VF13ad::WriteToTextFile(SolverState stateToUse)
{
}


std::string VF13ad::InterpretRetCode(Integer retCode)
{
   std::string retString = instanceName;
   
   switch (retCode)
   {
      case 0:
         retString += ": Optimization is proceeding as expected.\n";
         break;
         
      case 1:
         retString += " converged to within target accuracy.\n";
         break;
         
      case 2:
         retString += " failed to converge: "
            "Maximum number of iterations exceeded.\n";
         break;
         
      case 3:
         retString += " failed to converge: "
            "Line search exceeded allowed number of attempts.\n";
         break;
         
      case 4:
         retString += " failed to converge: "
            "Search direction is uphill; "
            "the desired accuracy cannot be achieved.\n";
         break;
         
      case 5:
         retString += " failed to converge: "
            "Failed to find a set of variables that satisfies all "
            "constraints.\n";
         break;
         
      case 6:
         retString += " failed to converge: "
            "The calculated variable metric is not positive definite.\n";
         break;
         
      case 7:
         retString += " failed to converge: "
            "The workspace provided to the algorithm is too small.\n";
         break;
         
      case 8:
         retString += " failed to converge: "
            "Either the number of variables is not positive, or the number of "
            "equality constraints detected is not between zero and the total "
            "number of constraints.\n";
         break;
         
      default:
         retString += " terminated with an unknown error code.\n";
   }
   
   return retString;
}
