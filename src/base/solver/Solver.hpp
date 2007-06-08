//$Header$
//------------------------------------------------------------------------------
//                                Solver
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2003/12/29
//
/**
 * Base class for Targeters, Optimizers, and other parametric scanning tools. 
 */
//------------------------------------------------------------------------------


#ifndef Solver_hpp
#define Solver_hpp

#include <fstream>          // for std::ofstream

#include "GmatBase.hpp"
#include "SolverException.hpp"


/**
 * @brief Base class for targeters, optimizers, and parameter scanning tools.
 * 
 * The Solver subsystem provides the numerical engines that adjust input 
 * parameters (the "variables") and measure the results of these perturbations.  
 * The system works as a state machine.  The specific path through the state 
 * machine depends on the solver implementation.  This class defines the state 
 * values used, and the core methods that use these states and that report on 
 * the results of the states.
 */
class GMAT_API Solver : public GmatBase
{
public:
   /// Enumeration defining the states in the state machine
   enum SolverState
   {
      INITIALIZING = 10001,
      NOMINAL,
      PERTURBING,
      ITERATING,
      CALCULATING,
      CHECKINGRUN,
      RUNEXTERNAL,
      FINISHED,
      UNDEFINED_STATE         // This one should stay at the end of the list.
   };
   
   /// Enumeration for solver progress report formats
   enum Report_Style
   {
      NORMAL_STYLE = 11001,
      CONCISE_STYLE,
      VERBOSE_STYLE,
      DEBUG_STYLE,
      MaxStyle
   };
    
public:
   Solver(const std::string &type, const std::string &name);
   virtual ~Solver();
   Solver(const Solver& sol);
   Solver&             operator=(const Solver& sol);

   virtual SolverState GetState();
   virtual SolverState GetNestedState();
   virtual SolverState AdvanceState();
   virtual StringArray AdvanceNestedState(std::vector<Real> vars);
   virtual bool        UpdateSolverGoal(Integer id, Real newValue);
   
   // Access methods overriden from the base class
   virtual std::string GetParameterText(const Integer id) const;
   virtual Integer     GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                       GetParameterType(const Integer id) const;
   virtual std::string GetParameterTypeString(const Integer id) const;
   virtual bool        IsParameterReadOnly(const Integer id) const;
   virtual bool        IsParameterReadOnly(const std::string &label) const;

   virtual Integer     GetIntegerParameter(const Integer id) const;
   virtual Integer     SetIntegerParameter(const Integer id,
                                           const Integer value);
   virtual std::string GetStringParameter(const Integer id) const;
   virtual std::string GetStringParameter(const std::string &label) const;
   virtual bool        SetStringParameter(const Integer id, 
                                          const std::string &value);
   virtual bool        SetStringParameter(const std::string &label, 
                                          const std::string &value);
   // compiler complained again - so here they are ....
   virtual std::string GetStringParameter(const Integer id,
                                          const Integer index) const;
   virtual bool        SetStringParameter(const Integer id, 
                                          const std::string &value,
                                          const Integer index);
   virtual std::string GetStringParameter(const std::string &label,
                                          const Integer index) const;
   virtual bool        SetStringParameter(const std::string &label, 
                                          const std::string &value,
                                          const Integer index);

   virtual const StringArray&
                       GetStringArrayParameter(const Integer id) const;
   virtual bool        GetBooleanParameter(const Integer id) const;
   virtual bool        SetBooleanParameter(const Integer id,
                                           const bool value);
                                           
   virtual void        ReportProgress();
   virtual void        SetDebugString(const std::string &str);
    
   virtual bool        Initialize();
   virtual bool        Finalize();
   
    
   virtual Integer     SetSolverVariables(Real *data,
                                          const std::string &name);

   virtual Real        GetSolverVariable(Integer id);
    
   //---------------------------------------------------------------------------
   //  Integer SetSolverResults(Real *data, std::string name)
   //---------------------------------------------------------------------------
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
   //---------------------------------------------------------------------------
   virtual Integer     SetSolverResults(Real *data,
                                        const std::string &name,
                                        const std::string &type = "") = 0;
    
   //---------------------------------------------------------------------------
   //  void SetResultValue(Integer id, Real value)
   //---------------------------------------------------------------------------
   /**
    * Passes in the results obtained from a run in the solver loop.
    *
    * @param <id>    The ID used for this result.
    * @param <value> The corresponding result.
    */
   //---------------------------------------------------------------------------
   virtual void        SetResultValue(Integer id, Real value,
                                      const std::string &resultType = "") = 0;

protected:
   /// Current state for the state machine
   SolverState         currentState;
   /// current nested state
   SolverState         nestedState;
   /// Output mode: Compact, Normal, and Verbose
   std::string         textFileMode;
   /// Toggle for showing solver status
   bool                showProgress;
   /// Flag used to adjust targeter progress reports 
   Integer             progressStyle;
   /// String for debug information in debug mode
   std::string         debugString;
   /// The number of variables in the solver problem
   Integer             variableCount;
   /// List of variables
   StringArray         variableNames;
   /// Array used to track the variables in the solver run
   //Real                *variable;
   std::vector<Real>   variable;
   /// The number of iterations taken (increments when the matrix is inverted)
   Integer             iterationsTaken;
   /// Maximum number of iterations allowed
   Integer              maxIterations;
   /// Array used to track the perturbations on each variable
   //Real                 *perturbation;
   std::vector<Real>    perturbation;
   /// Limits on the lowest value of the variables
   //Real                 *variableMinimum;
   std::vector<Real>    variableMinimum;
   /// Limits on the lowest value of the variables
   //Real                 *variableMaximum;
   std::vector<Real>    variableMaximum;
   /// Limits on individual changes in the variables
   //Real                 *variableMaximumStep;
   std::vector<Real>    variableMaximumStep;
   /// Current perturbation being run.
   Integer              pertNumber;
   /// Unperturbed value for the most recent applied pert, used to restore
   Real                 lastUnperturbedValue;
   /// Used to keep Jacobian calculations tracking when we bump into a limit
   std::vector<Real>    pertDirection;

   /// Flag used to ensure the targeter is ready to go
   bool                 initialized;

   // Reporting parameters
   /// Name of the targeter text file.  An empty string turns the file off.
   std::string          solverTextFile;
   /// Used to indicate if data should append to the text file
   Integer              instanceNumber;
   /// The solver text file
   std::ofstream        textFile;
      
   /// Generic solver parameters.
   enum
   {
      ShowProgressID   = GmatBaseParamCount,
      ReportStyle,
      solverTextFileID,
      variableNamesID,
      maxIterationsID,
      NUMBER_OF_VARIABLES,
      SolverParamCount
   };
   
   static const std::string    PARAMETER_TEXT[SolverParamCount -
                                              GmatBaseParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[SolverParamCount -
                                              GmatBaseParamCount];
   static const std::string    STYLE_TEXT[MaxStyle - NORMAL_STYLE];


   // Methods that correspond to the solver states.  Derived classes should
   // implement the methods that correspond to the Solver's state machine.  The
   // default implementation just advances the state to the "next" state in the
   // list.
   virtual void        CompleteInitialization();
   virtual void        RunNominal();
   virtual void        RunPerturbation();
   virtual void        RunIteration();
   virtual void        CalculateParameters();
   virtual void        CheckCompletion();
   virtual void        RunExternal();
   virtual void        RunComplete();
   
   virtual std::string GetProgressString();
   virtual void        FreeArrays();
    
   //---------------------------------------------------------------------------
   //  void WriteToTextFile()
   //---------------------------------------------------------------------------
   /**
    * Utility function used by the solvers to generate a progress file.
    * 
    * @param <stateToUse> SolverState used for the report; if this parameter is 
    *                     different from the default value (UNDEFINED_STATE), 
    *                     it is used.  If the value is UNDEFINED_STATE, then the 
    *                     value of currentState is used. 
    */
   //---------------------------------------------------------------------------
   virtual void        WriteToTextFile(SolverState stateToUse = UNDEFINED_STATE) = 0;
};


#endif // Solver_hpp
