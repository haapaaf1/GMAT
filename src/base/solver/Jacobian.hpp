#ifndef Jacobian_hpp
#define Jacobian_hpp

#include "DerivativeModel.hpp"

class Jacobian : public DerivativeModel
{
public:
	Jacobian();
	virtual ~Jacobian();
	Jacobian(const Jacobian &jac);
	Jacobian&            operator=(const Jacobian &jac);
   
   virtual bool         Initialize(UnsignedInt varCount, 
                                   UnsignedInt componentCount);
   virtual void         Achieved(Integer pertNumber, Integer componentId,
                                 Real dx, Real value, bool plusEffect = true);
   bool                 Calculate(std::vector<Real> &jac);

protected:
   UnsignedInt                         numVariables;
   UnsignedInt                         numComponents;    
   std::vector<Real>                   nominal;
   std::vector<Real>                   jacobian;
};

#endif /*Jacobian_hpp*/
