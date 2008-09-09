//$Header$
//------------------------------------------------------------------------------
//                              RealUtilities
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: M. Weippert, Chu-chi Li, E. Corderman, W. Shoan
// Created: 1995/07/21 for GSS project
// Modified: 2003/09/15 Linda Jun - Put functions under namespace GmatRealUtil.
//                      Added NearestInt().
//
/**
 * This file provides measurement conversion constatns and Math Utilities that
 * are not provided in the C++ Library or provides call-throughs to the
 * routines of the C++ (C) math.h
 */
//------------------------------------------------------------------------------
#ifndef RealUtilities_hpp
#define RealUtilities_hpp

#include "BaseException.hpp"
#include "RealTypes.hpp"

struct RealUtilitiesExceptions
{
   class ArgumentError : public BaseException
   {
       public : ArgumentError(const std::string& message = "")
       //: BaseException("Real Utilities Argument Error: ", message) {};  };
       : BaseException("", message) {};  
   };
   
   class IllegalTime   : public BaseException
   {
	public : IllegalTime(const std::string& message = "")
       //: BaseException("Real Utilities Illegal Time: ", message) {};  };
       : BaseException("", message) {};  
   };
};

namespace GmatMathUtil
{
   //Math constants
   static const Real PI_DEG = 180.0;
   static const Real TWO_PI_DEG = 360.0;
   static const Real PI = 3.14159265358979323846264338327950288419716939937511;
   static const Real TWO_PI = 6.28318530717958647692528676655900576839433879875022;
   static const Real PI_OVER_TWO = 1.57079632679489661923132169163975144209858469968756;
   static const Real E = 2.71828182845904523536028747135266249775724709369996;
   
   //Angle conversion
   static const Real RAD_PER_DEG =
      3.14159265358979323846264338327950288419716939937511 / 180.0;
   static const Real DEG_PER_RAD=
      180.0 / 3.14159265358979323846264338327950288419716939937511;
   
   static const Real ARCSEC_PER_DEGREE = 3600.0;
   static const Real DEG_PER_ARCSEC = 1.0 / 3600.0;
   static const Real RAD_PER_ARCSEC = DEG_PER_ARCSEC * RAD_PER_DEG;
   
   //Mass (kilogram)
   static const Real LBM_TO_KG = 0.45359237;
   static const Real SLUG_TO_KG = 14.59390294;

   //Length (metre)
   static const Real INCH_TO_M = 0.0254;
   static const Real FOOT_TO_M = 0.3048;
   static const Real STATUTE_MILE_TO_M = 1609.344;
   static const Real M_TO_KM = 0.001;
   static const Real KM_TO_M = 1000.0;

   Integer Abs(Integer theNumber);
   Real    Abs(Real theNumber);
   Real    NearestInt(Real theNumber);
   Real    Round(Real theNumber);
   Real    Floor(Real theNumber);
   Real    Ceiling(Real theNumber);
   Real    Mod(Real left, Real right);
   Real    Rem(Real left, Real right);
   void    Quotient(Real top, Real bottom, Integer &result);
   void    Quotient(Real top, Real bottom, Real &result);
   Real    Min(Real left, Real right);
   Real    Max(Real left, Real right);
   bool    IsPositive(Real theNumber);
   bool    IsNegative(Real theNumber);
   bool    IsNonNegative(Real theNumber);
   bool    IsZero(Real theNumber, Real accuracy=GmatRealConst::REAL_EPSILON);
   bool    IsEqual(Real left, Real right, 
                    Real accuracy = GmatRealConst::REAL_EPSILON);
   Integer SignOf(Real theNumber);
   
   Real    Rad(Real angleInDeg, bool modBy2Pi = false);
   Real    Deg(Real angleInRad, bool modBy360 = false);
   Real    DegToRad(Real deg, bool modBy2Pi = false);
   Real    RadToDeg(Real rad, bool modBy360 = false);
   Real    ArcsecToDeg(Real asec,bool modBy360 = false);
   Real    ArcsecToRad(Real asec,bool modBy2Pi = false);
   
   Real    Sin(Real angleInRad, Real cycleInRad=TWO_PI); 
   Real    SinXOverX(Real angleInRad, Real cycleInRad=TWO_PI); //loj: 7/22/05 Uncommented
   Real    Cos(Real angleInRad, Real cycleInRad=TWO_PI); 
   Real    Tan(Real angleInRad, Real cycleInRad=TWO_PI);

   Real    Cosh(Real angleInRad, Real cycleInRad=TWO_PI);
   Real    Sinh(Real angleInRad, Real cycleInRad=TWO_PI);
   Real    Tanh(Real angleInRad, Real cycleInRad=TWO_PI);
   
   Real    ASin(Real x, Real cycleInRad=TWO_PI); 
   Real    ACos(Real x, Real tol=0.0, Real cycleInRad=TWO_PI); 
   Real    ATan(Real y, Real x=1.0, Real cycleInRad=TWO_PI);
   
   Real    ATan2(Real y, Real x=1.0, Real cycleInRad=TWO_PI);
      
   Real    ASinh(Real x, Real cycleInRad=TWO_PI); 
   Real    ACosh(Real x, Real cycleInRad=TWO_PI); 
   Real    ATanh(Real x, Real cycleInRad=TWO_PI); 
   
   Real    Ln(Real x);             // Natural (base e) Logarithm of x
   Real    Log(Real x);            // Natural (base e) Logarithm of x
   Real    Log10(Real x);          // Base 10 Logarithm of x
   Real    Log(Real x, Real base); // Base <base> Logarithm of x
   Real    Log(Real x, Integer base);
   
   void    SetSeed(Integer initialSeed1, Integer initialSeed2); 
   void    GetSeed(Integer& initialSeed1, Integer& initialSeed2);
   //loj:void    SetSeedByClock();
   Real    Number(Real lowerBound=0.0, Real upperBound=1.0); 
   Real    GaussianNumber(Real mu= 0.0, Real sigma=1.0); 
   Real    Ran();
   void    SetRanKey(Real k=0.0); 
   //loj:Real    Cot(Real x);
   Real    Cbrt(Real x);
   Real    Sqrt(Real x);
   
   Real    Exp(Real x);            // Raises e  to the x power(e^x)
   Real    Exp10(Real x);          // Raises 10 to the x power(10^x)
   Real    Pow(Real x, Real y);    // Raises x to the y power(x^y)
   Real    Pow(Real x, Integer y); // Raises x to the y power(x^y)

}

namespace GmatRandNumUtil
{
    
    // These are defined values for Ran0 and Ran1
    // DO NOT MODIFY IN ANY WAY SHAPE OR FORM
    const UnsignedInt IA = 16807;
    const unsigned long int IM = 2147483647;
    const Real AM = 1.0/(Real)IM;
    const long int IQ = 127773;
    const Integer IR = 2836;
    const unsigned long int MASK = 123459876;

    // NTAB, NDIV, EPS, and RNMX are common between Ran1 and Ran2
    // DO NOT MODIFY IN ANY WAY SHAPE OR FORM
    const Integer NTAB = 32;
    const Real NDIV = (1.0+(Real)(IM-1.0)/(Real)NTAB);
    const Real EPS = 0.00000012;
    const Real RNMX = (1.0-EPS);

    // These are defined values for Ran2
    // DO NOT MODIFY IN ANY WAY SHAPE OR FORM
    const unsigned long int IM1 = 2147483563;
    const unsigned long int IM2 = 2147483399;
    const Real AM2 = 1.0/(Real)IM1;
    const unsigned long int IMM1 = IM1-1;
    const UnsignedInt IA1 = 40014;
    const UnsignedInt IA2 = 40692;
    const UnsignedInt IQ1 = 53668;
    const UnsignedInt IQ2 = 52774;
    const UnsignedInt IR1 = 12211;
    const UnsignedInt IR2 = 3791;
    const Real NDIV2 = 1.0+(Real)IMM1/(Real)NTAB;

    // These are defined values for Ran3
    // Any large MBIG and any smaller (but still large) MSEED
    // can be sustituted for the following values.
    const unsigned long int MBIG = 100000000;
    const unsigned long int MSEED = 161803398;
    const Integer MZ = 0;
    const Real FAC = 1.0/(Real)MBIG;

}
#endif // RealUtilities_hpp
