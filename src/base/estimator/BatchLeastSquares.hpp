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
#include "Spacecraft.hpp"
#include "GroundStation.hpp"
#include "MeasurementModel.hpp"
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

   Integer     SetEstimatorResults(Real *data, const std::string &name,
				   const std::string &type = "") {return 0;}
   void        SetResultValue(Integer id, Real value,
			      const std::string &resultType = "") {}

protected:
    
    // TODO: Make this general for any number of satellites
    Spacecraft* theSat;
    PropState* ps;
    GroundStation* theGroundStation;
    
   // Parameter IDs
   enum
   {
      BatchLeastSquaresParamCount = EstimatorParamCount,
   };

//   static const std::string    PARAMETER_TEXT[BatchLeastSquaresParamCount -
//                                              EstimatorParamCount];
//   static const Gmat::ParameterType
//                               PARAMETER_TYPE[BatchLeastSquaresParamCount -
//                                              EstimatorParamCount];

   // Methods

   virtual void                CompleteInitialization();
   virtual Real                FindTimeStep();
   virtual void                Estimate();
   virtual void		       Update();
   virtual void		       Reinitialize();
   virtual void                Accumulate();
   virtual void                CheckCompletion();
   virtual void                RunComplete();

   virtual std::string         GetProgressString();
   virtual void                WriteToTextFile(
                               SolverState stateToUse = UNDEFINED_STATE);
   void                        ReportProgress();

   // Need these implemented
   void                        CalculateInformationMatrix();
   void                        InvertInformationMatrix();

   // Fixed a pure virtual method issue
   virtual Integer     SetSolverResults(Real *data, const std::string &name,
                                        const std::string &type = "")
   { return -1; }

};

#endif // BatchLeastSquares_hpp
