#include "BrentDekkerZero.hpp"
#include <cmath>
#include "MessageInterface.hpp"

BrentDekkerZero::BrentDekkerZero() :
   macheps        (1.0e-15)
{
}

BrentDekkerZero::~BrentDekkerZero()
{
}


BrentDekkerZero::BrentDekkerZero(const BrentDekkerZero &bdz)
{
}


BrentDekkerZero&  BrentDekkerZero::operator=(const BrentDekkerZero &bdz)
{
   if (&bdz != this)
   {
      
   }
   
   return *this;
}


Real BrentDekkerZero::TestDriver(Real aVal, Real bVal, Real tVal)
{
   MessageInterface::ShowMessage("Testing Brent-Dekker Zero\n   Inputs:\n");
   MessageInterface::ShowMessage("      a = %.12lf\n      b = %.12lf\n      "
         "tol = %le\n", aVal, bVal, tVal);
   
   a = aVal;
   b = bVal;
   t = tVal;

   fa = TestFunction(a);
   fb = TestFunction(b);
   
   MessageInterface::ShowMessage("      fa = %.12lf\n      fb = %.12lf\n", 
         fa, fb);
   
   Interpolate();
   Extrapolate();
   
   Integer count = 0;
   
   while ((fabs(m) > tol) && (fabs(fb) != 0.0))
   {
      if ((fabs(e) < tol) || (fabs(fa) <= fabs(fb)))
      {
         MessageInterface::ShowMessage("   |e|(%.12lf) < tol(%.12lf)? %s\n",
               e, tol, ((fabs(e) < tol) ? "true" : "false"));
         MessageInterface::ShowMessage("   |fa|(%.12lf) < |fb|(%.12lf)? %s\n",
               fa, fb, ((fabs(fa) <= fabs(fb)) ? "true" : "false"));

         d = e = m;
      }
      else
      {
         s = fb / fa;
         if (a == c)
         {
            // Linear interpolation
            MessageInterface::ShowMessage("   Using Linear Interp\n");
            p = 2 * m * s;
            q = 1 - s;
         }
         else
         {
            // Inverse quadratic interpolation
            MessageInterface::ShowMessage("   Using Inverse Q Interp\n");
            q = fa / fc;
            r = fb / fc;
            p = s * (2.0 * m * q * (q - r) - (b - a) * (r - 1));
            q = (q - 1) * (r - 1) * (s - 1);
         }
         
         if (p > 0.0)
            q = -q;
         else
            p = -p;
         s = e;
         e = d;
         
         if ((2.0 * p < (3.0 * m * q - fabs(tol * q))) && 
             (p < fabs(0.5 * s * q)))
            d = p / q;
         else
            d = e = m;
      }
      
      a = b;
      fa = fb;
      
      if (fabs(d) > tol)
         b += d;
      else
      {
         if (m > 0.0)
            b += tol;
         else
            b -= tol;
      }
      fb = TestFunction(b);
      
      if ((fb > 0.0) && (fc > 0.0))
         Interpolate();
      
      Extrapolate();
      
      MessageInterface::ShowMessage("   Iteration %d:\n", ++count);
      MessageInterface::ShowMessage("      a = %.12lf, f(a) = %.12lf"
            "\n      b = %.12lf, f(b) = %.12lf\n", a, fa, b, fb);
   }
   
   MessageInterface::ShowMessage("   Converged to b = %.12lf\n", b);
   return b;
}

void BrentDekkerZero::Interpolate()
{
   c = a;
   fc = fa;
   d = e = b - a;
}


void BrentDekkerZero::Extrapolate()
{
   if (fabs(fc) < fabs(fb))
   {
      a = b;
      b = c;
      c = a;
      fa = fb;
      fb = fc;
      fc = fa;
   }
   tol = 2.0 * macheps * fabs(b) + t;
   m = 0.5 * (c - b);   
}


Real BrentDekkerZero::TestFunction(Real x)
{
   // Zero at about 0.7531
   return 3.0 * x * x * x - x * x + 7.0 * x - 6.0;  
}
