/**
 * Base class for gradients, Jacobians, Hessians, and so forth.
 */

#ifndef DERIVATIVEMODEL_HPP_
#define DERIVATIVEMODEL_HPP_

#include "gmatdefs.hpp"

class DerivativeModel
{
protected:
   enum derivativeMode {
      FORWARD_DIFFERENCE,
      CENTRAL_DIFFERENCE,
      BACKWARD_DIFFERENCE,
      USER_SUPPLIED
   };

public:
   DerivativeModel();
   virtual ~DerivativeModel() = 0;         // Abstract to prevent instantiation
   DerivativeModel(const DerivativeModel& dm);
   DerivativeModel&     operator=(const DerivativeModel& dm);
	
   void                 SetDifferenceMode(derivativeMode mode);
   virtual bool         Initialize(UnsignedInt varCount, 
                                   UnsignedInt componentCount = 1);
   virtual void         Achieved(Integer pertNumber, Integer componentId, 
                                 Real dx, Real value, bool plusEffect = true);

protected:
   derivativeMode       calcMode;

   std::vector<Real>    pert;
   std::vector<Real>    plusPertEffect;
   std::vector<Real>    minusPertEffect;
};

#endif /*DERIVATIVEMODEL_HPP_*/
