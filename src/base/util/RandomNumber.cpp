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
 * This class implements the double precision SIMD oriented Fast 
 * Mersenne Twister pseudorandom number generator (dSFMT) version 2.1. 
 *
 * SFMT is a new variant of Mersenne Twister (MT) introduced by Mutsuo Saito and 
 * Makoto Matsumoto in 2006. The algorithm was reported at MCQMC 2006 
 * (http://mcqmc.uni-ulm.de/). The article was published in the proceedings of 
 * MCQMC2006. (see Prof. Matsumoto's Papers on random number generation.)
 *
 * SFMT is a Linear Feedbacked Shift Register (LFSR) generator that generates 
 * a 128-bit pseudorandom integer at one step. SFMT is designed with recent 
 * parallelism of modern CPUs, such as multi-stage pipelining and SIMD 
 * (e.g. 128-bit integer) instructions. It supports 32-bit and 64-bit integers, 
 * as well as double precision floating point as output.
 * 
 * SFMT is much faster than MT, in most platforms. Not only the speed, but also 
 * the dimensions of equidistributions at v-bit precision are improved. 
 * In addition, recovery from 0-excess initial state is much faster. See 
 * Master's Thesis of Mutsuo Saito for detail.
 *
 * This program is based on the IEEE Standard for Binary Floating-Point 
 * Arithmetic (ANSI/IEEE Std 754-1985) format. dSFMT ver. 2.xx is completely 
 * different from dSFMT ver. 1.xx. The recursion formula is changed and the 
 * output sequences are changed. This version uses structure of C language. 
 * Don't use different DSFMT_MEXP for compiling dSFMT.c and your program. 
 * This Project provides pseudorandom number generators of various 
 * Mersenne Prime Period: from 2521-1 to 2216091-1.
 *
 * The purpose of dSFMT is to speed up the generation by avoiding the expensive 
 * conversion of integer to double (floating point). dSFMT directly generates 
 * double precision floating point pseudorandom numbers which have the IEEE 
 * Standard for Binary Floating-Point Arithmetic (ANSI/IEEE Std 754-1985) format. 
 * dSFMT is only available on the CPUs which use IEEE 754 format double precision 
 * floating point numbers.
 * 
 * dSFMT doesn't support integer outputs. dSFMT supports the output of double 
 * precision floating point pseudorandom numbers which distribute in the range 
 * of [1, 2), [0, 1), (0, 1] and (0, 1). And it also supports the various 
 * periods form 2607-1 to 2132049-1. (dSFMT ver. 2.1 supports the periods 
 * from 2521-1 to 2216091-1.)
 *
 * References
 *
 * Mutsuo Saito and Makoto Matsumoto, "SIMD-oriented Fast Mersenne Twister: a 
 * 128-bit Pseudorandom Number Generator", Monte Carlo and Quasi-Monte Carlo 
 * Methods 2006, Springer, 2008, pp. 607--622. DOI:10.1007/978-3-540-74496-2_36
 *
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/sfmt.pdf
 * 
 * M. Matsumoto and T. Nishimura, "Mersenne Twister: A 623-dimensionally 
 * equidistributed uniform pseudorandom number generator", ACM Trans. on 
 * Modeling and Computer Simulation Vol. 8, No. 1, January pp.3-30 (1998) 
 * DOI:10.1145/272991.272995
 * 
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf
 *
 */
//------------------------------------------------------------------------------

#include "RandomNumber.hpp"

//------------------------------------------------------------------------------
// RandomNumber()
//------------------------------------------------------------------------------
/**
 * Class constructor that seeds the generator using the clock.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber()
{

    unsigned int clockSeed = time(NULL);

    dsfmt_init_gen_rand(&dsfmt, clockSeed);
    
}

//------------------------------------------------------------------------------
// RandomNumber(const unsigned int mySeed)
//------------------------------------------------------------------------------
/**
 * Class constructor that seeds the generator using a specified seed value.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber(unsigned int mySeed)
{
    dsfmt_init_gen_rand(&dsfmt, mySeed);
}

//------------------------------------------------------------------------------
// RandomNumber(const unsigned int *mySeed)
//------------------------------------------------------------------------------
/**
 * Class constructor that seeds the generator using an array of seed values.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber(unsigned int *mySeed, Integer arraySize)
{
    dsfmt_init_by_array(&dsfmt, mySeed, arraySize);
}

//------------------------------------------------------------------------------
// ~RandomNumber()
//------------------------------------------------------------------------------
/**
 * Class destructor.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::~RandomNumber()
{
}


//------------------------------------------------------------------------------
//  void Seed(unsigned int s)
//------------------------------------------------------------------------------
/**
 *  Set the seed for the random number generator using a specified value.
 *
 *  @param <idum2> input seed
 */
//------------------------------------------------------------------------------
void RandomNumber::Seed(unsigned int s)
{
    dsfmt_init_gen_rand(&dsfmt, s);
}


//------------------------------------------------------------------------------
//  void SeedByArray(unsigned int s)
//------------------------------------------------------------------------------
/**
 *  Set the seed for the random number generator using a specified value.
 *
 *  @param <idum2> input seed
 */
//------------------------------------------------------------------------------
void RandomNumber::SeedByArray(unsigned int *mySeed, Integer arraySize)
{
    dsfmt_init_by_array(&dsfmt, mySeed, arraySize);
}

//------------------------------------------------------------------------------
//  void ClockSeed()
//------------------------------------------------------------------------------
/**
 * Set the seed value based upon the current clock time.
 */
//------------------------------------------------------------------------------
void RandomNumber::ClockSeed()
{

    unsigned int clockSeed = time(NULL);

    dsfmt_init_gen_rand(&dsfmt, clockSeed);

}

//------------------------------------------------------------------------------
//  unsigned int UniformInt()
//------------------------------------------------------------------------------
/**
 *  Returns an unsigned 32-bit integer. This should only be used to seed.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
unsigned int RandomNumber::UniformInt()
{
     return dsfmt_genrand_uint32(&dsfmt);
}

//------------------------------------------------------------------------------
//  Real Uniform()
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range [0,1)
 *  The range includes 0.0 but excludes 1.0;
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::Uniform()
{
     return dsfmt_genrand_close_open(&dsfmt);
}

//------------------------------------------------------------------------------
//  Real UniformPrimitive()
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range [1,2).
 *  The range includes 1.0 but excludes 2.0. This is the primitive MT generator
 *  that all other function calls are based upon. This is the fastest method
 *  for generating a random variate.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::UniformPrimitive()
{
     return dsfmt_genrand_close1_open2(&dsfmt);
}

//------------------------------------------------------------------------------
//  Real UniformOpenOpen()
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range (0,1)
 *  The range excludes both 0.0 and 1.0. This is useful when you need to
 *  avoid a singularity at 0 or 1.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::UniformOpenOpen()
{
     return dsfmt_genrand_open_open(&dsfmt);
}

 //------------------------------------------------------------------------------
//  Real UniformOpenClosed()
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range (0,1]
 *  The range excludes 0.0 but includes 1.0. This is useful when you need to
 *  avoid a singularity at 0.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::UniformOpenClosed()
{
     return dsfmt_genrand_open_close(&dsfmt);
}

//------------------------------------------------------------------------------
//  Real Uniform(Real a, Real b)
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range [a,b)
 *  The mean of this distribution is (a+b)/2.
 *  The variance of this distribution is (b-a)^2/12.
 *
 *  @param <a> Distribution start
 *  @param <b> Distribution end
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::Uniform(const Real a, const Real b)
{

     return a + (b - a)*dsfmt_genrand_close_open(&dsfmt);

}

//------------------------------------------------------------------------------
//  Real Gaussian()
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate (zero mean, unit var)
 *  using the polar form transformation. This method is considered to be faster
 *  and more numerically robust than the Box-Mueller transformation.
 *
 *  See L. Devroye: 'Non-Uniform Random Variate Generation', Springer-Verlag, 
 *                  New York, 1986.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::Gaussian()
{
    
    Real u,v,s;
    
    do
    {
        u = Uniform(-1.0, 1.0);
	v = Uniform(-1.0, 1.0);
	s = u*u + v*v;
    }
    while ( s < 0 || s > 1);

    // This method produces two independent random deviates but we
    // only return one of them. Here is the formula for the seconde deviate.
    // Real z1 = v * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s);
    return u * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s);
}

//------------------------------------------------------------------------------
//  Real Gaussian(const Real mean, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate (zero mean, unit var)
 *  using the polar form transformation. This method is considered to be faster
 *  and more numerically robust than the Box-Mueller transformation.
 *
 *  See L. Devroye: 'Non-Uniform Random Variate Generation', Springer-Verlag, 
 *                  New York, 1986.
 *
 *  @param <mean> Mean of Gaussian distribution
 *  @param <stdev> Standard deviation of Gaussian distribution
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::Gaussian(const Real mean, const Real stdev)
{
    Real u,v,s;
    
    do
    {
        u = Uniform(-1.0, 1.0);
	v = Uniform(-1.0, 1.0);
	s = u*u + v*v;
    }
    while ( s < 0 || s > 1);

    // This method produces two independent random deviates but we
    // only return one of them. Here is the formula for the seconde deviate.
    // Real z1 = v * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s);
    return u * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s) * stdev + mean;
}

//------------------------------------------------------------------------------
//  void UniformArray(unsigned int *myArray, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns an array of uniformly distributed unsigned 32bit integers.
 *  This method is significantly slower than the double precision methods
 *  and should only be used in conjunction with the SeedByArray method.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::UniformArray(unsigned int *myArray, const Integer size)
{
    for (Integer i = 0; i < size; i++)
	myArray[i] = dsfmt_genrand_uint32(&dsfmt);
}

//------------------------------------------------------------------------------
//  void UniformArray(Real *myArray, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns a uniformly distributed random deviate in the range [0,1)
 *  The range includes 0.0 but excludes 1.0;
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::UniformArray(Real *myArray, const Integer size)
{
     dsfmt_fill_array_close_open(&dsfmt,(double*)myArray,size);
}

//------------------------------------------------------------------------------
//  void UniformPrimitiveArray(Real *myArray, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range [1,2).
 *  The range includes 1.0 but excludes 2.0. This is the primitive MT generator
 *  that all other function calls are based upon. This is the fastest method
 *  for generating a random variate.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::UniformPrimitiveArray(Real *myArray, const Integer size)
{
     dsfmt_fill_array_close1_open2(&dsfmt,(double*)myArray,size);
}

//------------------------------------------------------------------------------
//  void UniformOpenOpenArray(Real *myArray, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range (0,1)
 *  The range excludes both 0.0 and 1.0. This is useful when you need to
 *  avoid a singularity at 0 or 1.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::UniformOpenOpenArray(Real *myArray, const Integer size)
{
     dsfmt_fill_array_open_open(&dsfmt,(double*)myArray,size);
}

//------------------------------------------------------------------------------
//  void UniformOpenClosedArray(Real *myArray, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range (0,1]
 *  The range excludes 0.0 but includes 1.0. This is useful when you need to
 *  avoid a singularity at 0.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::UniformOpenClosedArray(Real *myArray, const Integer size)
{
     dsfmt_fill_array_open_close(&dsfmt,(double*)myArray,size);
}

//------------------------------------------------------------------------------
//  void UniformArray(Real *myArray, Real a, Real b, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range [a,b)
 *  The mean of this distribution is (a+b)/2.
 *  The variance of this distribution is (b-a)^2/12.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *  @param <a> Distribution start
 *  @param <b> Distribution end
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::UniformArray(Real *myArray, const Integer size, 
	                         const Real a, const Real b)
{

     dsfmt_fill_array_close_open(&dsfmt,(double*)myArray,size);
     for (Integer i=0; i < size; i++)
     {
         myArray[i] = a + (b - a)*myArray[i];
     }
}

//------------------------------------------------------------------------------
//  void GaussianArray(Real *myArray, const Integer size)
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate (zero mean, unit var)
 *  using the polar form transformation. This method is considered to be faster
 *  and more numerically robust than the Box-Mueller transformation.
 *
 *  See L. Devroye: 'Non-Uniform Random Variate Generation', Springer-Verlag, 
 *                  New York, 1986.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::GaussianArray(Real *myArray, const Integer size)
{
    
    Real u[size],v[size],s;

    UniformArray(u,size,-1.0,1.0);
    UniformArray(v,size,-1.0,1.0);

    for (Integer i=0; i<size; i++)
    {
	s = u[i]*u[i]+v[i]*v[i];
	
	if ( s < 0 || s > 1)
	{
	    // rejected, find another pair

	    Real u1, v1, s1;

	    do
	    {
		u1 = Uniform(-1.0,1.0);
		v1 = Uniform(-1.0,1.0);
		s1 = u1*u1 + v1*v1;
	    }
	    while ( s1 < 0 || s1 > 1);

	    myArray[i++] = u1 * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s1)/s1);	
	    
	    if(i < size-1)
	    {
		myArray[i++] = v1 * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s1)/s1);
	    }
	    
	}
	else
	{
	    // accepted, assign to output array
	    myArray[i] = u[i] * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s);
	    if(++i < size)
	    {
		myArray[i] = v[i] * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s);
	    }   
	}
    }
}

//------------------------------------------------------------------------------
//  void GaussianArray(Real *myArray, const Integer size,
//	                         const Real mean, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate (zero mean, unit var)
 *  using the polar form transformation. This method is considered to be faster
 *  and more numerically robust than the Box-Mueller transformation.
 *
 *  See L. Devroye: 'Non-Uniform Random Variate Generation', Springer-Verlag, 
 *                  New York, 1986.
 *
 *  @param <myArray> Pointer to array where random deviates will be stored
 *  @param <size> size of the array of deviates
 *  @param <mean> Mean of Gaussian distribution
 *  @param <stdev> Standard deviation of Gaussian distribution
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::GaussianArray(Real *myArray, const Integer size,
	                         const Real mean, const Real stdev)
{
   
    Real u[size],v[size],s;

    UniformArray(u,size);
    UniformArray(v,size);

    for (Integer i=0; i<size; i++)
    {
	s = u[i]*u[i]+v[i]*v[i];
	
	if ( s < 0 || s > 1)
	{
	    // rejected, find another pair

	    Real u1, v1, s1;

	    do
	    {
		u1 = Uniform();
		v1 = Uniform();
		s1 = u1*u1 + v1*v1;
	    }
	    while ( s1 < 0 || s1 > 1);

	    myArray[i++] = u1 * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s1)/s1) * stdev + mean;	
	    
	    if(i < size-1)
	    {
		myArray[i++] = v1 * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s1)/s1) * stdev + mean;
	    }
	    
	}
	else
	{
	    // accepted, assign to output array
	    myArray[i] = u[i] * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s) * stdev + mean;
	    if(++i < size)
	    {
		myArray[i] = v[i] * GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Ln(s)/s) * stdev + mean;
	    }   
	}
    }
}