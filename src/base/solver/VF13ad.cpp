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
#include <sstream>

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
VF13ad::PARAMETER_TEXT[VF13adParamCount - OptimizerParamCount] =
{
   "goalName",
   "UseCentralDifferences"
};

const Gmat::ParameterType
VF13ad::PARAMETER_TYPE[VF13adParamCount - OptimizerParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::BOOLEAN_TYPE
};



//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

VF13ad::VF13ad(const std::string &name) :
   InternalOptimizer       ("VF13ad", name),
   retCode                 (-101),
   useCentralDifferences   (false),
   workspaceLength         (200),
   numConstraints          (0),
   varLength               (1),
   iprint                  (1), 
   integerWorkspace        (NULL),
   workspace               (NULL),
   grad                    (NULL),
   vars                    (NULL),
   constraints             (NULL),
   cJacobian               (NULL)
{
   objectTypeNames.push_back("VF13ad");
   objectiveFnName = "SDObjective";
   tolerance       = 1.0e-5;
   maxIterations   = 200;
   parameterCount = VF13adParamCount;
   AllowRangeLimits = false;
}


VF13ad::~VF13ad()
{
}


VF13ad::VF13ad(const VF13ad& sd) :
   InternalOptimizer       (sd),
   jacobian                (sd.jacobian),
   retCode                 (sd.retCode),
   useCentralDifferences   (sd.useCentralDifferences),
   workspaceLength         (sd.workspaceLength),
   numConstraints          (sd.numConstraints),
   varLength               (sd.varLength),
   iprint                  (sd.iprint), 
   integerWorkspace        (NULL),
   workspace               (NULL),
   grad                    (NULL),
   vars                    (NULL),
   constraints             (NULL),
   cJacobian               (NULL)
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

bool VF13ad::IsParameterReadOnly(const Integer id) const
{
   if (id == goalNameID)
      return true;

   return InternalOptimizer::IsParameterReadOnly(id);
}

bool VF13ad::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


//---------------------------------------------------------------------------
//  Gmat::ParameterType GetParameterType(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the enumerated type of the object.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The enumeration for the type of the parameter, or
 *         UNKNOWN_PARAMETER_TYPE.
 */
Gmat::ParameterType VF13ad::GetParameterType(const Integer id) const
{
   if (id >= InternalOptimizerParamCount && id < VF13adParamCount)
      return PARAMETER_TYPE[id - InternalOptimizerParamCount];
      
   return InternalOptimizer::GetParameterType(id);
}


//---------------------------------------------------------------------------
//  std::string GetParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the string associated with a parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return Text description for the type of the parameter, or the empty
 *         string ("").
 */
std::string VF13ad::GetParameterTypeString(const Integer id) const
{
   return InternalOptimizer::PARAM_TYPE_STRING[GetParameterType(id)];
}


//---------------------------------------------------------------------------
//  std::string GetParameterText(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the description for the parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return String description for the requested parameter.
 *
 * @note The parameter strings should not include any white space
 */
std::string VF13ad::GetParameterText(const Integer id) const
{
   if (id >= InternalOptimizerParamCount && id < VF13adParamCount)
      return PARAMETER_TEXT[id - InternalOptimizerParamCount];
   return InternalOptimizer::GetParameterText(id);
}


//---------------------------------------------------------------------------
//  Integer GetParameterID(const std::string &str) const
//---------------------------------------------------------------------------
/**
 * Retrieve the ID for the parameter given its description.
 *
 * @param <str> Description for the parameter.
 *
 * @return the parameter ID, or -1 if there is no associated ID.
 */
//---------------------------------------------------------------------------
Integer VF13ad::GetParameterID(const std::string &str) const
{
   for (Integer i = InternalOptimizerParamCount; i < VF13adParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - InternalOptimizerParamCount])
         return i;
   }
   
   return InternalOptimizer::GetParameterID(str);
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
std::string VF13ad::GetStringParameter(const Integer id) const
{
   if (id == goalNameID)
      return objectiveName;
      
   return InternalOptimizer::GetStringParameter(id);
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
bool VF13ad::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == goalNameID)
   {
      objectiveName = value;
      return true;
   }
   
   return InternalOptimizer::SetStringParameter(id, value);
}

//---------------------------------------------------------------------------
//  std::string GetStringParameter(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <label> The (string) label for the parameter.
 *
 * @return The string stored for this parameter, or the empty string if there
 *         is no string association.
 */
std::string VF13ad::GetStringParameter(const std::string &label) const
{
   Integer id = GetParameterID(label);
   return GetStringParameter(id);
}


//---------------------------------------------------------------------------
//  bool SetStringParameter(const std::string &label, const std::string &value)
//---------------------------------------------------------------------------
/**
 * Change the value of a string parameter.
 *
 * @param <label> The (string) label for the parameter.
 * @param <value> The new string for this parameter.
 *
 * @return true if the string is stored, false if not.
 */
bool VF13ad::SetStringParameter(const std::string &label, 
                                  const std::string &value)
{
   Integer id = GetParameterID(label);
   return SetStringParameter(id, value);
}


//---------------------------------------------------------------------------
//  bool GetBooleanParameter(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve a boolean parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return the boolean value for this parameter, or throw an exception if the
 *         parameter access in invalid.
 */
bool VF13ad::GetBooleanParameter(const Integer id) const
{
   if (id == useCentralDifferencesID)
      return useCentralDifferences;
   
   return InternalOptimizer::GetBooleanParameter(id);
}


//---------------------------------------------------------------------------
//  bool SetBooleanParameter(const Integer id, const bool value)
//---------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 *
 * @param id The integer ID for the parameter.
 * @param value The new value.
 * 
 * @return the boolean value for this parameter, or throw an exception if the 
 *         parameter is invalid or not boolean.
 */
bool VF13ad::SetBooleanParameter(const Integer id, const bool value)
{
   if (id == useCentralDifferencesID)
   {
      useCentralDifferences = value;
      return true;
   }
   
   return InternalOptimizer::SetBooleanParameter(id, value);
}

//---------------------------------------------------------------------------
//  bool GetBooleanParameter(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Retrieve a boolean parameter.
 *
 * @param <label> The (string) label for the parameter.
 *
 * @return the boolean value for this parameter, or false if the parameter is
 *         not boolean.
 */
bool VF13ad::GetBooleanParameter(const std::string &label) const
{
   Integer id = GetParameterID(label);
   return GetBooleanParameter(id);
}


//---------------------------------------------------------------------------
//  bool SetBooleanParameter(const std::string &label, const bool value)
//---------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 *
 * @param <label> The (string) label for the parameter.
 *
 * @return the boolean value for this parameter, or false if the parameter is
 *         not boolean.
 */
bool VF13ad::SetBooleanParameter(const std::string &label, const bool value)
{
   Integer id = GetParameterID(label);
   return SetBooleanParameter(id, value);
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
      FreeArrays();
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
         ReportProgress();
         if (vars == NULL)
            Initialize();
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
         ReportProgress();
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
         ReportProgress();
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
         break;
   
      case FINISHED:
         #ifdef VF13_DEBUG_STATE_MACHINE
            MessageInterface::ShowMessage("Entered state machine; "
                  "FINISHED\n");
         #endif
         ReportProgress();
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
      {
         idToUse = id - 1000;
         if (currentState == NOMINAL) 
            eqConstraintValues[idToUse] = value;
      }
      else
      {
         idToUse = id - 2000;
         if (currentState == NOMINAL) 
            ineqConstraintValues[idToUse] = value;
         idToUse += eqConstraintCount;
      }

      if (currentState == NOMINAL) 
      {
         jacobianCalculator.Achieved(-1, idToUse, 0.0, value);
      }
           
      if (currentState == PERTURBING) 
      {
         jacobianCalculator.Achieved(pertNumber, idToUse, 
               perturbation[pertNumber], value);
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
   #ifdef DEBUG_VF13_INIT
      MessageInterface::ShowMessage("VF13ad::Initialize() entered for %s; "
            "%d variables and %d constraints\n", instanceName.c_str(), 
            registeredVariableCount, registeredComponentCount);
   #endif
      
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
   
   for (int i = 0; i < registeredVariableCount; ++i)
   {
      gradient.push_back(0.0);
      for (int j = 0; j < registeredComponentCount; ++j)
         jacobian.push_back(0.0);
   }
   
   retCode = -101;
   numConstraints = registeredComponentCount;
   varLength = registeredVariableCount + 1;
   iprint   = 1; 
   
   workspaceLength = (Integer)(5.0 * registeredVariableCount * 
         registeredVariableCount / 2.0 + 43.0 * registeredVariableCount / 2.0 + 
         14 + 6 * registeredComponentCount + 2);
   
   workspace = new Real[workspaceLength];
   grad = new Real[registeredVariableCount];
   vars = new Real[registeredVariableCount];
   if (registeredComponentCount > 0)
   {
      #ifdef DEBUG_VF13_CALL
         MessageInterface::ShowMessage("%d variables and %d constraints\n",
               varLength - 1, numConstraints);
      #endif
      cJacobian   = new Real[varLength*registeredComponentCount];
      constraints = new Real[registeredComponentCount];
   }
   else
   {
      cJacobian = NULL;
      constraints = NULL;
   }
   integerWorkspace = new Integer[varLength];

   #ifdef DEBUG_VF13_INIT
      MessageInterface::ShowMessage
         ("VF13ad::Initialize() completed; %d variables and %d constraints\n",
          registeredVariableCount, registeredComponentCount);
   #endif

   // Constrain the step sizes (default is a very large number, and thus 
   // basically unconstrained.
   for (Integer i = 0; i < registeredVariableCount; ++i)
      workspace[numConstraints + i] = variableMaximumStep[i];
         
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
   
   // Ranging is not supported in VF13ad
   //   if (variable[pertNumber] > variableMaximum[pertNumber])
   //   {
   //      pertDirection.at(pertNumber) = -1.0;
   //      variable.at(pertNumber) -= 2.0 * perturbation.at(pertNumber);
   //   }    
   //   if (variable.at(pertNumber) < variableMinimum.at(pertNumber))
   //   {
   //      pertDirection.at(pertNumber) = -1.0;
   //      variable.at(pertNumber) -= 2.0 * perturbation.at(pertNumber);
   //   }
       
   WriteToTextFile();
}


void VF13ad::CalculateParameters()
{
   if (objectiveDefined)
      gradientCalculator.Calculate(gradient);
   if (eqConstraintCount + ineqConstraintCount > 0)
      jacobianCalculator.Calculate(jacobian);
   currentState = CHECKINGRUN;
   
   #ifdef VF13_DEBUG_STATE_MACHINE
      MessageInterface::ShowMessage(
            "Raw Gradient and Jacobian Information\n   Gradient: [");
      for (std::vector<Real>::iterator i = gradient.begin(); 
           i != gradient.end(); ++i)
         MessageInterface::ShowMessage(" %.12lf ", *i);
      MessageInterface::ShowMessage("]\n   Jacobian: [");
      for (std::vector<Real>::iterator i = jacobian.begin(); 
           i != jacobian.end(); ++i)
         MessageInterface::ShowMessage(" %.12lf ", *i);
      MessageInterface::ShowMessage("]\n\n");
   #endif
}


void VF13ad::CheckCompletion()
{
   if (vars == NULL)
      throw SolverException("Solver is uninitialized!\n");

   #ifdef DEBUG_VF13_CALL
      MessageInterface::ShowMessage("VF13ad Check Completion, input settings:\n"
            "   N:   %d\n   M:   %d\nMEQ: %d\n",
            variableCount, numConstraints, eqConstraintCount);
   #endif 
   
   for (Integer i = 0; i < variableCount; ++i)
   {
      vars[i] = variable.at(i);
      
      if (objectiveDefined)
         grad[i] = gradient.at(i);
      else
         grad[i] = 0.0;
      
      for (Integer j = 0; j < numConstraints; ++j)
      {
         if (j < eqConstraintCount)
            cJacobian[i + varLength * j] = jacobian.at(i + variableCount * j);
         else
            cJacobian[i + varLength * j] = -jacobian.at(i + variableCount * j);
      }
   }

   for (Integer j = 0; j < eqConstraintCount; ++j)
      constraints[j] = eqConstraintValues.at(j);
   for (Integer j = eqConstraintCount; j < numConstraints; ++j)
      constraints[j] = -ineqConstraintValues.at(j - eqConstraintCount);
      

   #ifdef DEBUG_VF13_CALL
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
               if (j < numConstraints - 1)
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
      
      MessageInterface::ShowMessage("   cJac = ");
      for (Integer i = 0; i < numConstraints; ++i)
      {
         for (Integer j = 0; j < variableCount; ++j)
         {
            if (j == 0)
               MessageInterface::ShowMessage("[");
            MessageInterface::ShowMessage("%.12lf", cJacobian[i * varLength + j]);
            if (j < variableCount-1)
               MessageInterface::ShowMessage(", ");
         }
         MessageInterface::ShowMessage("]\n          ");
      }
      MessageInterface::ShowMessage("\n");
      
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
   #endif

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
   
   #ifdef DEBUG_VF13_CALL
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
   #endif   

   if (retCode == 0)
   {
      for (Integer i = 0; i < variableCount; ++i)
         variable.at(i) = vars[i];

      currentState = NOMINAL;
   }
   else
      currentState = FINISHED;
   
   ++iterationsTaken;
}


void VF13ad::RunComplete()
{
}


//------------------------------------------------------------------------------
// Utility methods
//------------------------------------------------------------------------------

void VF13ad::FreeArrays()
{
   
   delete [] workspace;
   delete [] grad;
   delete [] vars;
   if (numConstraints > 0)
   {
      delete [] cJacobian;
      delete [] constraints;
   }
   delete [] integerWorkspace;

   
   workspace = grad = vars = cJacobian = constraints = NULL;
   integerWorkspace = NULL;

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
   std::string retString = "";
   
   switch (retCode)
   {
      case -1:
      case -101:
      case -110:
      case -111:
         retString += ": Optimization ready to start.\n";
         break;
      
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


//------------------------------------------------------------------------------
//  std::string GetProgressString()
//------------------------------------------------------------------------------
/**
 * Generates a string that reporting the current differential corrector state.
 */
//------------------------------------------------------------------------------
std::string VF13ad::GetProgressString()
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
         case NOMINAL:
            progress << instanceName << " Iteration " << iterationsTaken+1
                     << "; Nominal Pass\n   Variables:  ";
            // Iterate through the variables, writing them to the string
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
            {
               if (current != variableNames.begin())
                  progress << ", ";
               progress << *current << " = " << variable[i++];
            }
            progress << "\n   " << instanceName << " State" 
                     << InterpretRetCode(retCode);
            break;

         case PERTURBING:  // does this apply to optimization??
            progress << "   Completed iteration " << iterationsTaken
                     << ", pert " << pertNumber+1 << " ("
                     << variableNames[pertNumber] << " = "
                     << variable[pertNumber] << ")\n";
            break;

         case CALCULATING:
            // Just forces a blank line
            break;

         case CHECKINGRUN:
            if (eqConstraintCount > 0)
            {
               // Iterate through the constraints, writing them to the file
               progress << "   Equality Constraint Variances:\n";
   
               for (current = eqConstraintNames.begin(), i = 0;
                    current != eqConstraintNames.end(); ++current)
               {
                  progress << "      Delta " << (*current) << " = " 
                           << constraints[i] << "\n";
                  ++i;
               }
            }

            if (ineqConstraintCount > 0)
            {
               progress << "   Inequality Constraint Variances:\n";

               for (current = ineqConstraintNames.begin(), i = eqConstraintCount;
                    current != ineqConstraintNames.end(); ++current)
               {
                  progress << "      Delta " << (*current) << " = " 
                           << constraints[i] << "\n";
                  ++i;
               }
            }

            break;

         case FINISHED:
            progress << "\n*** Optimization Completed in " << iterationsTaken
                     << " iterations";
                     
            if (iterationsTaken > maxIterations)
               progress << "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                        << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
                        << "!!! WARNING: Optimizer did NOT converge in "
                        << maxIterations << " iterations!"
                        << "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                        << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
            
            progress << "\nFinal Variable values:\n";
            // Iterate through the variables, writing them to the string
            for (current = variableNames.begin(), i = 0;
                 current != variableNames.end(); ++current)
               progress << "   " << *current << " = " << variable[i++] << "\n";
            progress << instanceName << InterpretRetCode(retCode) << "\n";

            break;

         default:
            progress << Optimizer::GetProgressString();
      }
   }
   else
      return Solver::GetProgressString();
      
   return progress.str();

}

