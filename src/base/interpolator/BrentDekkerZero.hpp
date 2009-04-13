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
   
   void SetInterval(Real a0, Real b0, Real fa0, Real fb0, Real tolerance);
   Real FindStep(Real lastStep, Real lastEval);
   bool CheckConvergence();
   
   // Method used to test the implementation
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

   void SwapAC();
   void FindStepParameters();

   // Test function for the implementation
   Real TestFunction(Real value);
};

#endif /*BrentDekkerZero_hpp*/
