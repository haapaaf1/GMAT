//$Id$
//------------------------------------------------------------------------------
//                              RandomNumber
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/09/08
//
/**
 * GMAT's Random Number Class
 *
 * This class implements the random number generator routines found in:
 * 1) Park and Miller, "Random Number Generator: Good Ones are Hard to Find."
 * Communications of the ACM 31 (10), pp 1192-1201.
 * 2) L'Ecuyer, P. 1988, Communications of the ACM, vol 31, pp 742-774.
 * 3) Knuth, D.E. 1981, Seminumerical Algorithms, 2nd ed., vol 2 of "The
 * Art of Computer Programming", Sections 3.2-3.3.
 *
 */
//------------------------------------------------------------------------------

#ifndef _RandomNumber_hpp
#define _RandomNumber_hpp

#include "gmatdefs.hpp"

class RandomNumber
{

public:

    RandomNumber();
    ~RandomNumber();

    // Uniform Random Number
    // ----------------------------
    // -           -   Relative   -
    // - Generator -   Execution  -
    // -           -     Time     -
    // ----------------------------
    // -   Ran0    -  defined 1.0 -
    // -   Ran1    -     ~1.3     -
    // -   Ran2    -     ~2.0     -
    // -   Ran3    -     ~0.6     -
    // ----------------------------

    // Note: Use Ran1 for general use.
    // If you are using more than 100,000,000
    // random numbers then use Ran2.
    // Ran3 is the fastest routine but is not
    // as well studied as Ran1 and Ran2, and
    // is not considered a standard yet. Use
    // Ran3 when you suspect Ran1 or Ran2 of
    // introducing unwanted correlations into
    // a calculation.

    // Uniform distribution between 0.0 and 1.0
    Real Ran0();
    Real Ran1();
    Real Ran2();
    Real Ran3();

    // Uniform distribution between a and b
    // Mean = (a+b)/2
    // Var = (b-a)^2/12
    Real UniformRand(Real a, Real b);
    Real UniformRand2(Real a, Real b);
    Real UniformRand3(Real a, Real b);

    // Exponential Random Number
    // Unit mean
    Real ExponentialRand();
    Real ExponentialRand2();
    Real ExponentialRand3();

    // Gaussian Random Number
    // 0 mean, unit variance
    Real GaussianRand();
    Real GaussianRand(Real mean, Real stdev);
    Real GaussianRand2();
    Real GaussianRand2(Real mean, Real stdev);
    Real GaussianRand3();
    Real GaussianRand3(Real mean, Real stdev);

    // Get/Set/Re Seed
    long int* GetSeed();
    void SetSeed(long int *idum2);
    void ClockSeed();

private:

    // Seed to initialize random number generators
    long int *idum;

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

};

#endif	/* _RandomNumber_hpp */

