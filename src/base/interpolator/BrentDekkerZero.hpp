#ifndef BrentDekkerZero_hpp
#define BrentDekkerZero_hpp

#include "gmatdefs.hpp"

class BrentDekkerZero
{
public:
   BrentDekkerZero();
   virtual ~BrentDekkerZero();
   BrentDekkerZero(const BrentDekkerZero &bdz);
   BrentDekkerZero&  operator=(const BrentDekkerZero &bdz);
   
   // Methods used to test the implementation
   Real TestDriver(Real aVal, Real bVal, Real tVal);
   
protected:
   Real a;
   Real b;
   Real macheps;
   Real t;

   Real c;
   Real d;
   Real e;
   Real fa;
   Real fb;
   Real fc;
   Real tol;
   Real m;
   Real p;
   Real q;
   Real r;
   Real s;

   // Methods used to test the implementation
   Real TestFunction(Real value);
   void Interpolate();
   void Extrapolate();
};

#endif /*BrentDekkerZero_hpp*/
