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
 */
//------------------------------------------------------------------------------

#ifndef _RandomNumber_hpp
#define _RandomNumber_hpp

// This is for the GSL libraries so that
// extern C will be used
//#define __cplusplus

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "UtilityException.hpp"
#include <dSFMT.h>

class RandomNumber
{

public:

    RandomNumber();
    RandomNumber(unsigned int mySeed);
    RandomNumber(unsigned int *mySeed, Integer arraySize);
    ~RandomNumber();

    Real Gaussian();
    Real Gaussian(const Real mean, const Real stdev);

    void GaussianArray(Real *myArray, const Integer size);
    void GaussianArray(Real *myArray, const Integer size,  
	               const Real mean, const Real stdev);

    unsigned int UniformInt();
    Real Uniform();
    Real UniformPrimitive();
    Real UniformOpenOpen();
    Real UniformOpenClosed();
    Real Uniform(const Real a, const Real b);

    void UniformArray(unsigned int *myArray, const Integer size);
    void UniformArray(Real *myArray, const Integer size);
    void UniformPrimitiveArray(Real *myArray, const Integer size);
    void UniformOpenOpenArray(Real *myArray, const Integer size);
    void UniformOpenClosedArray(Real *myArray, const Integer size);
    void UniformArray(Real *myArray, const Integer size, 
	              const Real a, const Real b);

    
    // Set and/or Re-set the seed for the random number generator
    void Seed(unsigned int s);
    void SeedByArray(unsigned int *mySeed, Integer arraySize);
    void ClockSeed();

private:

    dsfmt_t dsfmt;

};

#endif	/* _RandomNumber_hpp */

