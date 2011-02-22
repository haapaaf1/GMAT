//$Id$
//------------------------------------------------------------------------------
//                              RealUtilities
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
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
#include "GmatConstants.hpp"

struct GMAT_API RealUtilitiesExceptions
{
   class GMAT_API ArgumentError : public BaseException
   {public : ArgumentError(const std::string& message = "")
       : BaseException("", message) {};  };
   
   class GMAT_API IllegalTime   : public BaseException
   {public : IllegalTime(const std::string& message = "")
       : BaseException("", message) {};  };
};

namespace GmatMathUtil
{
   Integer Abs(Integer theNumber);
   Real    Abs(Real theNumber);
   Real    NearestInt(Real theNumber);
   Real    Round(Real theNumber);
   Real    Floor(Real theNumber);
   Real    Fix(Real theNumber);
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
   bool    IsZero(Real theNumber, Real accuracy=GmatRealConstants::REAL_EPSILON);
   bool    IsEqual(Real left, Real right, 
                    Real accuracy = GmatRealConstants::REAL_EPSILON);
   Integer SignOf(Real theNumber);
   bool    IsOdd(Integer theNumber);
   bool    IsEven(Integer theNumber);
   
   Real    Rad(Real angleInDeg, bool modBy2Pi = false);
   Real    Deg(Real angleInRad, bool modBy360 = false);
   Real    DegToRad(Real deg, bool modBy2Pi = false);
   Real    RadToDeg(Real rad, bool modBy360 = false);
   Real    ArcsecToDeg(Real asec,bool modBy360 = false);
   Real    ArcsecToRad(Real asec,bool modBy2Pi = false);
   
   Real    Sin(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    SinXOverX(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    Cos(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    Tan(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);

   Real    Cosh(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    Sinh(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    Tanh(Real angleInRad, Real cycleInRad=GmatMathConstants::TWO_PI);
   
   Real    ASin(Real x, Real tol=GmatRealConstants::REAL_TOL, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    ACos(Real x, Real tol=GmatRealConstants::REAL_TOL, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    ATan(Real y, Real x=1.0, Real cycleInRad=GmatMathConstants::TWO_PI);
   
   Real    ATan2(Real y, Real x=1.0, Real cycleInRad=GmatMathConstants::TWO_PI);
   
   Real    ASinh(Real x, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    ACosh(Real x, Real cycleInRad=GmatMathConstants::TWO_PI);
   Real    ATanh(Real x, Real cycleInRad=GmatMathConstants::TWO_PI);
   
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
   
   bool    IsNaN(Real x);
   bool    IsInf(Real x);
}

// This inline doesn't work with MSVC++ compiler
#ifndef _MSC_VER
#include <cmath>
//------------------------------------------------------------------------------
// bool IsNaN(Real x)
//------------------------------------------------------------------------------
/**
 * Tests if input value is not a number.
 */
//------------------------------------------------------------------------------
inline bool GmatMathUtil::IsNaN(Real x)
{
   return std::isnan(x);
}

//------------------------------------------------------------------------------
// bool IsInf(Real x)
//------------------------------------------------------------------------------
/**
 * Tests if input value is a infinite number.
 */
//------------------------------------------------------------------------------
inline bool GmatMathUtil::IsInf(Real x)
{
   return std::isinf(x);
}
#endif

#endif // RealUtilities_hpp
