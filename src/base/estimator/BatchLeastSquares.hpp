//$Header$
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
 * Definition for the differential corrector targeter.
 */
//------------------------------------------------------------------------------


#ifndef BatchLeastSquares_hpp
#define BatchLeastSquares_hpp


#include "Solver.hpp"
#include "Estimator.hpp"
#include <fstream>          // for std::ofstream

/**
 * This class implements the first targeter in GMAT.
 *
 * @todo refactor this class with the Solver class so that elements common to
 *       targeting, scanning, and optimizing are all in the base class.  This
 *       task should be done when the first instance of one of the other
 *       approaches is implemented.
 */
class GMAT_API BatchLeastSquares : public Estimator
{
public:
   BatchLeastSquares(std::string name);
   virtual ~BatchLeastSquares();
   BatchLeastSquares(const BatchLeastSquares &dc);
   BatchLeastSquares& operator=(const BatchLeastSquares& dc);

   virtual bool        Initialize();
//   virtual EstimatorState AdvanceState();
   virtual SolverState AdvanceState();

   // inherited from GmatBase
   virtual GmatBase*   Clone() const;
   virtual void        Copy(const GmatBase* orig);

   // Access methods overriden from the base class
   virtual std::string GetParameterText(const Integer id) const;
   virtual Integer     GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                       GetParameterType(const Integer id) const;
   virtual std::string GetParameterTypeString(const Integer id) const;

   virtual Integer     GetIntegerParameter(const Integer id) const;
   virtual Integer     SetIntegerParameter(const Integer id,
                                           const Integer value);
   virtual bool        GetBooleanParameter(const Integer id) const;
   virtual bool        SetBooleanParameter(const Integer id,
                                           const bool value);
   virtual std::string GetStringParameter(const Integer id) const;
   virtual bool        SetStringParameter(const Integer id,
                                          const std::string &value);
   virtual const StringArray&
                       GetStringArrayParameter(const Integer id) const;
   virtual bool        TakeAction(const std::string &action,
                                  const std::string &actionData = "");

   // Estimator interfaces used to talk to the Vary and Achieve commands
   //virtual Integer     SetEstimatorVariables(Real *data, const std::string &name);
   //virtual Real        GetEstimatorVariable(Integer id);
   virtual Integer     SetEstimatorResults(Real *data, const std::string &name,
                                        const std::string &type = "");
//   virtual bool        UpdateSolverGoal(Integer id, Real newValue);
   virtual bool        UpdateEstimatorGoal(Integer id, Real newValue);
   virtual void        SetResultValue(Integer id, Real value,
                                      const std::string &resultType = "");

protected:
   // Core data members used for the targeter numerics
   /// The number of goals in the targeting problem
   Integer                     goalCount;
   /// Value desired for the goals
   Real                        *goal;
   /// Accuracy for the goals
   Real                        *tolerance;
   /// Array used to track the achieved value during a nominal run
   Real                        *nominal;
   /// Array used to track the achieved values when variables are perturbed
   Real                        **achieved;
   /// The sensitivity matrix
   Real                        **jacobian;
   /// The inverted sensitivity matrix
   Real                        **inverseJacobian;

   /// Permutation vector used by the LU Decomposition routine.
   Integer                     *indx;
   /// Vector used for the back substitution
   Real                        *b;
   /// LU Decomposition of the Jacobian
   Real                        **ludMatrix;

   // Control parameters
   /// Used to turn on central differencing.  Currently not implemented.
   bool                        useCentralDifferences;

   /// List of goals
   StringArray                 goalNames;

   // Parameter IDs
   enum
   {
      goalNamesID = EstimatorParamCount,
      useCentralDifferencingID,
      BatchLeastSquaresParamCount
   };

   static const std::string    PARAMETER_TEXT[BatchLeastSquaresParamCount -
                                              EstimatorParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[BatchLeastSquaresParamCount -
                                              EstimatorParamCount];

   // Methods

   virtual void                CompleteInitialization();
   virtual void                RunNominal();
//   virtual void                RunPerturbation();
   virtual void                Estimate();
   virtual void                CalculateParameters();
   virtual void                CheckCompletion();
   virtual void                RunComplete();

   // Methods used to perform differential correction
   void                        CalculateJacobian();
   void                        InvertJacobian();

   void                        FreeArrays();
   virtual std::string         GetProgressString();
   virtual void                WriteToTextFile(
                                  SolverState stateToUse = UNDEFINED_STATE);
//   virtual void                WriteToTextFile(
//                                  EstimatorState stateToUse = UNDEFINED_STATE);


   // Need these implemented
   void                        CalculateInformationMatrix();
   void                        InvertInformationMatrix() {}

   // Fixed a pue virtual method issue
   virtual Integer     SetSolverResults(Real *data, const std::string &name,
                                        const std::string &type = "")
   { return -1; }

};

#endif // BatchLeastSquares_hpp
