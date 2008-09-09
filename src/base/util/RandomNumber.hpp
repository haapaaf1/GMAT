/* 
 * File:   RandomNumber.hpp
 * Author: mwilkins
 *
 * Created on September 5, 2008, 4:37 PM
 */

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
    void ReSeed();
	
private:

    // Seed to initialize random number generators
    long int *idum;
	
};

#endif	/* _RandomNumber_hpp */

