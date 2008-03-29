#ifndef Gradient_hpp
#define Gradient_hpp

#include "DerivativeModel.hpp"


class Gradient : public DerivativeModel
{
public:
   Gradient();
   virtual ~Gradient();
   Gradient(const Gradient &grad);
   Gradient&            operator=(const Gradient &grad);
   
   virtual bool         Initialize(UnsignedInt varCount, 
                                   UnsignedInt componentCount = 1);
   virtual void         Achieved(Integer pertNumber, Integer componentId, 
                                 Real dx, Real value, bool plusEffect = true);
   bool                 Calculate(std::vector<Real> &grad);

protected:
   Real                 nominal;
   std::vector<Real>    gradient;
};

#endif /*Gradient_hpp*/
