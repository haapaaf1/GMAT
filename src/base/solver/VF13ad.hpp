//$Id$
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
 * Implementation for the steepest descent optimizer. 
 * 
 * This is prototype code.  Interested parties need to roll the VF13ad optimizer
 * code into a separate library and link to it.  Contact Thinking Systems for
 * details.
 */
//------------------------------------------------------------------------------


#ifndef VF13ad_hpp
#define hpp

#include "InternalOptimizer.hpp"
#include "Gradient.hpp"
#include "Jacobian.hpp"


/**
 * The VF13ad optimizer if the prototypical optimization method.  While
 * not the most efficient method, it is the simplest to implement, since all it 
 * needs to do is run nominal trajectories, calculate gradients (via finite 
 * differences if no analytic form exists), scan in the "downhill" direction, 
 * and repeat until the magnitude of the gradient is small enough to declare
 * victory.
 */
class VF13ad : public InternalOptimizer
{
public:
	VF13ad(const std::string &name);
	virtual ~VF13ad();
   VF13ad(const VF13ad& sd);
   VF13ad& operator=(const VF13ad& sd);
   
   virtual Integer      SetSolverResults(Real *data,
                                        const std::string &name,
                                        const std::string &type = "");
   virtual void         SetResultValue(Integer id, Real value,
                                      const std::string &resultType = "");
   virtual GmatBase*    Clone() const;
   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");
   virtual bool         Initialize();
   virtual Solver::SolverState
                        AdvanceState();
   virtual bool         Optimize();
protected:
   std::string          objectiveName;
   
   Gradient             gradientCalculator;
   Jacobian             jacobianCalculator;
   std::vector<Real>    jacobian;
   Integer              retCode;

   enum
   {
      goalNameID = SolverParamCount,
//      constraintNameID,
      useCentralDifferencesID,
      VF13adParamCount
   };

   static const std::string      PARAMETER_TEXT[VF13adParamCount -
                                              SolverParamCount];
   static const Gmat::ParameterType
                                 PARAMETER_TYPE[VF13adParamCount -
                                              SolverParamCount];

   // State machine methods
   virtual void                  RunNominal();
   virtual void                  RunPerturbation();
   virtual void                  CalculateParameters();
   virtual void                  CheckCompletion();
   virtual void                  RunComplete();

   void                          FreeArrays();
   
   virtual void                  WriteToTextFile(
                                    SolverState stateToUse = UNDEFINED_STATE);
   std::string                   InterpretRetCode(Integer retCode);
   std::string                   GetProgressString();
   
   // Working variables used in the VF13 parameters
   Integer                       workspaceLength;
   Integer                       numConstraints;
   Integer                       varLength;
   Integer                       iprint; 
   
   // Arrays allocated in Initialize() and freed through an action(?)
   Integer                       *integerWorkspace;
   Real                          *workspace;
   Real                          *grad;
   Real                          *vars;
   Real                          *constraints;
   Real                          *cJacobian;
};

#endif /*VF13ad_HPP_*/
