//$Header$
//------------------------------------------------------------------------------
//                                Estimator
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by Dr. Matthew P. Wilkins, Schafer Corporation
// and by Thinking Systems, Inc. under contract number NNG06CA54C
//
// Authors: Matthew P. Wilkins, Schafer Corporation
//          Darrel J. Conway, Thinking Systems, Inc.
//
// Created: 2008/05/20
// Modified: 2008/10/01
//
/**
 * Base class for estimator tools.
 */
//------------------------------------------------------------------------------


#ifndef Estimator_hpp
#define Estimator_hpp

#include <fstream>          // for std::ofstream
#include <Spacecraft.hpp>

#include "GmatBase.hpp"
#include "Solver.hpp"
#include "EstimatorException.hpp"
// For matrix and vector definitions
#include "lapackpp.h" 
// Blas Level-1 Routines: Vector-Scalar and Vector-Vector operations
#include "blas1pp.h"
// Blas Level-2 Routines: Vector-Matrix operations
#include "blas2pp.h"
// Blas Level-3 Routines: Matrix-Matrix operations and Matrix norms
#include "blas3pp.h"
// Functions for solving systems of equations
#include "laslv.h"
// Singular value decomposition
#include "lasvd.h"

using namespace la;

// DJC Additions
// #include "MeasurementModel.hpp"

/**
 *  How to set a stopwatch timer using TNT
 *	Stopwatch Q;
 *	Q.start;
 *	{code to benchmark}
 *	Q.stop;
 * double time_elapsed = Q.read();
 */

// Forward references
class PropSetup;
class MeasurementModel;

/**
 * @brief Base class for estimator tools.
 *
 * The Estimator subsystem provides the numerical engines that find a
 * state at a desired epoch time that minimizes the error with respect
 * to observations of the dynamical system. This minimization of the error
 * is accomplished in many different ways depending upon the estimator chosen.
 * Choose wisely.
 *
 * The system works as a state machine.  The specific path through the state
 * machine depends on the estimator implementation.  This class defines the state
 * values used, and the core methods that use these states and that report on
 * the results of the states.
 */
class GMAT_API Estimator : public Solver // GmatBase
{
public:
//   /// Enumeration defining the states in the state machine
//   enum EstimatorState
//   {
//      INITIALIZING = 10001,
//      ITERATING,
//      ESTIMATING,
//      CHECKINGRUN,
//      RUNEXTERNAL,
//      FINISHED,
//      UNDEFINED_STATE         // This one should stay at the end of the list.
//   };

//   /// Enumeration for estimator progress report formats
//   enum Report_Style
//   {
//      NORMAL_STYLE = 11001,
//      CONCISE_STYLE,
//      VERBOSE_STYLE,
//      DEBUG_STYLE,
//      MaxStyle
//   };

public:

   Estimator(const std::string &type, const std::string &name);
   virtual ~Estimator();
   Estimator(const Estimator& sol);
   Estimator&             operator=(const Estimator& sol);

//   virtual EstimatorState GetState();
//   virtual EstimatorState AdvanceState();
   virtual SolverState GetState();
   virtual SolverState AdvanceState();
  //virtual bool        UpdateEstimatorGoal(Integer id, Real newValue);

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

   virtual GmatBase*   GetRefObject(const Gmat::ObjectType type,
                                        const std::string &name);
   virtual bool        SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                        const std::string &name = "");

   virtual const StringArray&
                       GetStringArrayParameter(const Integer id) const;
   virtual bool        GetBooleanParameter(const Integer id) const;
   virtual bool        SetBooleanParameter(const Integer id,
                                           const bool value);
   Real GetTimeStep();
   void SetTimeStep(Real &dt);

   virtual void        ReportProgress();
   virtual void        SetDebugString(const std::string &str);

   virtual bool        Initialize();
   virtual bool        Finalize();


   virtual Integer     SetEstimatorVariables(Real *data,
                                          const std::string &name);

   virtual Real        GetEstimatorVariable(Integer id);

   //---------------------------------------------------------------------------
   //  Integer SetEstimatorResults(Real *data, std::string name)
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
   virtual Integer     SetEstimatorResults(Real *data,
                                        const std::string &name,
                                        const std::string &type = "") = 0;

   //---------------------------------------------------------------------------
   //  void SetResultValue(Integer id, Real value)
   //---------------------------------------------------------------------------
   /**
    * Passes in the results obtained from a run in the estimator loop.
    *
    * @param <id>    The ID used for this result.
    * @param <value> The corresponding result.
    */
   //---------------------------------------------------------------------------
   virtual void        SetResultValue(Integer id, Real value,
				     const std::string &resultType = "") = 0;
// DJC additions
   virtual Real        FindTimeStep();
   
protected:

   /// Current state for the state machine
   SolverState          currentState;
   /// The estimator epoch
   Real			epoch;  
/// State vector of parameters to estimate
   /// These parameters are estimated and their error covariance is
   /// solved for.
   LaVectorDouble	x;
   /// The number of state variables in the estimator problem.
   Integer		stateCount;
   /// List of state variables
   StringArray		stateNames;
   /// Vector of controls (thrusters, momentum gyros, etc)
   LaVectorDouble	u;
   /// Consider parameters
   /// Consider parameters are parameters that are known to affect
   /// the state variables but are thought to be not well observable
   /// from the avaialble data. Consider parameters are treated as
   /// constants but their error covariance is added in to the final analysis.
   LaVectorDouble	considerParameters;
   /// The number of consider parameters in the estimator problem
   Integer		considerCount;
   /// List of consider parameter variables
   StringArray		considerNames;
   /// Neglect parameters
   /// Similar to consider parameters, these parameters are constant
   /// but we set their error covariance is to zero. The ability
   /// to neglect certain parameters that we would normally estimate
   /// or consider can give us insight into the solution of a problem at hand.
   LaVectorDouble	neglectParameters;
   /// The number of neglect parameter in the estimator problem
   Integer		neglectCount;
   /// List of neglect parameter variables
   StringArray		neglectNames;


   /// Linearized systems take the following form:
   ///
   /// x(t1) = Ax(t0) + Bu(t0)
   /// y(t1) = Cx(t1) + Du(t1)
   ///
   /// So we must define the appropriate matrices

   LaGenMatDouble             A;
   LaGenMatDouble             B;
   LaGenMatDouble             C;
   LaGenMatDouble             D;

   /// Weights associated with the estimator states
   /// Typically this matrix is diagonal with values equal to
   /// 1 over the error variance associated with each state.
   LaGenMatDouble             W;
   /// Current covariance matrix - symmetric, positive definite
   LaGenMatDouble             P;
   /// Variance of the model error known as process noise used for filtering
   LaGenMatDouble             Q;
   /// Variance of the measurement noise typically used for weighting purposes
   LaGenMatDouble             R;
   // Estimator gain
   LaGenMatDouble	      K;

   // Information matrix - symmetric, positive definite
   LaGenMatDouble            informationMatrix;
   /// H is the Jacobian of y w.r.t. x
   LaGenMatDouble	H;
   /// Phi is the Jacobian of f w.r.t. x
   LaGenMatDouble       Phi;
   /// The current state transition matrix
   LaGenMatDouble	stateTransitionMatrix;
   
   
   // Cost function evaluation
   LaVectorDouble	J;
   // Global Convergence Tolerance
   Real			globalConvergenceTolerance;

   /// Vector of observation times
   LaVectorDouble	observationTimes;
   /// Vector of observations
   LaVectorDouble	y;
   /// Vector containing differences between observed and computed measurements
   LaVectorDouble	z;
   // The number of observations in the estimator problem
   Integer              observationCount;
   /// The number of observation types in the estimator problem
   Integer		observationTypeCount;
   /// The number of observation stations in the estimator problem
   Integer		observerCount;
   // Index of which observer we are working with
   Integer		observerIndex;
   // Index of the observation to be processed
   Integer              obIndex;

   /// Vector of state biases
   LaVectorDouble	stateBiases;
   /// Vector of measurement biases
   LaVectorDouble	measurementBiases;
   /// Array used to track the differential corrections on each state variable
   LaVectorDouble	estimatorStateCorrection;

   /// For nonlinear systems, we need to define a function f that
   /// predicts the estimator state at time t2 given a prior
   /// state at time t1 and the associated control vector u during that time period
   /// h is a function that computes an estimated observation at time t given
   /// the current state and control vectors.
   ///
   /// x(t1) = f(x(t0),u(t0),t0)
   /// y(t1) = h(x(t1),u(t1),t1)
   ///
   virtual LaVectorDouble f(Real &t0, Real &t1);
   virtual LaVectorDouble f(LaVectorDouble &x, Real &t0, Real &t1);
   virtual LaVectorDouble f(LaVectorDouble &x, LaVectorDouble &u, Real &t0,
                            Real &t1);

   virtual LaVectorDouble h(StringArray &observationTypes, Real &t1);
   virtual LaVectorDouble h(StringArray &observationTypes, LaVectorDouble &x,
                            Real &t1);
   virtual LaVectorDouble h(StringArray &observationTypes, LaVectorDouble &x,
                            LaVectorDouble &u, Real &t1);

   /// H is a function that computes the Jacobian of y w.r.t. x
   virtual LaGenMatDouble ComputeH();
   virtual LaGenMatDouble ComputeH(LaVectorDouble &x, Real &t0);

   /// Compute Phi is a function that returns the Jacobian of f w.r.t. x
   virtual LaGenMatDouble ComputePhi();
   virtual LaGenMatDouble ComputePhi(LaVectorDouble &x, Real &t0);
   
   // This returns the current State Transtion Matrix associated with a given
   // space object
   LaGenMatDouble GetStateTransitionMatrix(const Spacecraft *theSat);
   LaGenMatDouble GetStateTransitionMatrix(const Spacecraft *theSat,
                                Integer &numRows = 0, Integer &numColumns = 0);

   /// The number of iterations taken ( used for batch processing )
   Integer             iterationsTaken;
   // The integration time step to be taken in seconds
   Real			timeStep;
   /// Maximum number of iterations allowed ( used for batch processing )
   Integer             maxIterations;
   /// Limits on the lowest value of the state variables
   // this is a sanity check not a constraint
   // e.g. for eccentricity this value would be 0
   LaVectorDouble stateMinimum;
   /// Limits on the highest value of the state variables
   // this is a sanity check not a constraint
   // e.g. for eccentricity this value would be 1
   LaVectorDouble stateMaximum;


   /// Flag used to ensure the estimator is ready to go
   bool                 initialized;
   /// Flag used to test if the estimator has converged
   bool			converged;

   /// Output mode: Compact, Normal, and Verbose
   std::string         textFileMode;
   /// Toggle for showing estimator status
   bool                showProgress;
   /// Flag used to adjust estimator progress reports
   Integer             progressStyle;
   /// String for debug information in debug mode
   std::string         debugString;
   // Reporting parameters
   /// Name of the estimator text file.  An empty string turns the file off.
   std::string          estimatorTextFile;
   /// Used to indicate if data should append to the text file
   Integer              instanceNumber;
   /// The estimator text file
   std::ofstream        textFile;

   // DJC Additions
   /// Name of the numerical integrator setup used
   std::string          propName;
   /// The setup
   PropSetup            *propagator;
   /// Participant names
   StringArray          participantNames;
   /// The participants
   ObjectArray          participants;
   /// The data sources
   ObjectArray          dataSources;
   /// Measurement Model names
   StringArray          measModelNames;
   /// SolveFor parameter IDs
   StringArray          solveForParms;
   /// SolveFor Objects
   ObjectArray          solveForOwners;
   /// SolveFor Attribute Ids
   std::vector<Integer> solveForIds;
   /// SolveFor Attribute Sizes
   std::vector<Integer> solveForLengths;

   /// Measurement models used in the estimation
   std::vector<MeasurementModel*>
                        measModels;

   /// Generic estimator parameters.
   enum
   {
      ShowProgressID   = GmatBaseParamCount,
      ReportStyle,
      estimatorTextFileID,
      // DJC additions
      PropagatorName,
      ParticipantNames,
      MeasurementModels,
      SolveForParameters,
      variableNamesID,
      maxIterationsID,
      NUMBER_OF_VARIABLES,
      EstimatorParamCount
   };

   static const std::string    PARAMETER_TEXT[EstimatorParamCount -
                                              GmatBaseParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[EstimatorParamCount -
                                              GmatBaseParamCount];
   static const std::string    STYLE_TEXT[MaxStyle - NORMAL_STYLE];

   // Methods that correspond to the estimator states.  Derived classes should
   // implement the methods that correspond to the Estimator's state machine.  The
   // default implementation just advances the state to the "next" state in the
   // list.
   virtual void        CompleteInitialization();
   virtual void        Accumulate();
   virtual void        Update();
   virtual LaGenMatDouble        ComputeGain();
   virtual void        Reinitialize();
   virtual void        CheckCompletion();
   virtual void        RunComplete();

   virtual std::string GetProgressString();

   //---------------------------------------------------------------------------
   //  void WriteToTextFile()
   //---------------------------------------------------------------------------
   /**
    * Utility function used by the estimators to generate a progress file.
    *
    * @param <stateToUse> EstimatorState used for the report; if this parameter is
    *                     different from the default value (UNDEFINED_STATE),
    *                     it is used.  If the value is UNDEFINED_STATE, then the
    *                     value of currentState is used.
    */
   //---------------------------------------------------------------------------
   virtual void        WriteToTextFile(SolverState stateToUse = UNDEFINED_STATE) = 0;
//   virtual void        WriteToTextFile(EstimatorState stateToUse = UNDEFINED_STATE) = 0;
};


#endif // Estimator_hpp
