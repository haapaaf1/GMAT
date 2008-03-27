#ifndef Gradient_hpp
#define Gradient_hpp

#include "Rvector.hpp"

class Gradient
{
protected:
   enum gradientMode {
      FORWARD_DIFFERENCE,
      CENTRAL_DIFFERENCE,
      BACKWARD_DIFFERENCE,
      USER_SUPPLIED
   };

public:
   Gradient();
   virtual ~Gradient();
   Gradient(const Gradient &grad);
   Gradient&            operator=(const Gradient &grad);
   
   void                 SetDifferenceMode(gradientMode mode);
   bool                 Initialize(UnsignedInt varCount);
   void                 Achieved(Integer pertNumber, Real dx, Real value);
   bool                 Calculate(std::vector<Real> &grad);

protected:
   Real                 nominal;
   std::vector<Real>    pert;
   std::vector<Real>    plusPertEffect;
   std::vector<Real>    minusPertEffect;
   std::vector<Real>    gradient;
   
   gradientMode         calcMode;
   
};

#endif /*Gradient_hpp*/
