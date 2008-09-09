#include "RandomNumber.hpp"
#include "RealUtilities.hpp"

using namespace GmatRandNumUtil;

//------------------------------------------------------------------------------
// RandomNumber()
//------------------------------------------------------------------------------
/*
 * Class constructor.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber() : idum (NULL)
{
}

//------------------------------------------------------------------------------
// ~RandomNumber()
//------------------------------------------------------------------------------
/*
 * Class destructor.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::~RandomNumber()
{
}

//------------------------------------------------------------------------------
//  Long* GetIdum()
//------------------------------------------------------------------------------
/*
 *
 */
//------------------------------------------------------------------------------
long int* RandomNumber::GetSeed(){
    return idum;
}

//------------------------------------------------------------------------------
//  void SetIdum()
//------------------------------------------------------------------------------
/*
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::SetSeed(long int *idum2){
    idum = idum2;
}


//------------------------------------------------------------------------------
//  void ReSeed()
//------------------------------------------------------------------------------
/*
 *
 */
//------------------------------------------------------------------------------
void RandomNumber::ReSeed(){
    
    long int myseed = -time(NULL);
    idum = &myseed;
    
}

//------------------------------------------------------------------------------
//  Real Ran0()
//------------------------------------------------------------------------------
/*
 *  "Minimal" random number generator of Park and Miller. Returns a uniform
 *  random deviate between 0.0 and 1.0. Set or reset idum to any integer value
 *  (except the unlikely value MASK) to initialize the sequence; idum must
 *  not be altered between calls for successive deviates in a sequence. The 
 *  period of ran0 is 2^31 - 2 ~ 2.1x10^9. The value 0 must never be used as
 *  the initial seed.
 *
 *  This routine is satisfactoryy for a majority of applications but contains
 *  low-order serial correlations.
 */
//------------------------------------------------------------------------------
 Real RandomNumber::Ran0(){

     long int k;
     Real ans;
     
     *idum ^= MASK;
     k = (*idum)/IQ;
     *idum = IA*(*idum-k*IQ)-IR*k;
     if (*idum < 0) *idum += IM;
     ans = AM*(*idum);
     *idum ^= MASK;
     return ans;
     
}
 
//------------------------------------------------------------------------------
//  Real Ran1()
//------------------------------------------------------------------------------
/*
 *  "Minimal" random number generator of Park and MIller with Bays-Durham
 *  shuffle and added safeguards. Returns a uniform random deviate between
 *  0.0 and 1.0 (exclusive of the endpoint values). Call with idum a negative
 *  integer to initialize; thereafter, do not alter idum between successive
 *  deviates in a sequence. RNMX should appoximate the largest floating value 
 *  that is less than 1.
 *
 *  The routine Ran1 passes those statistical tests that Ran0 is known to fail.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::Ran1(){

    Integer j;
    long int k;
    static long int iy = 0;
    static long int iv[NTAB];
    Real temp;
    
    if (*idum <= 0 || !iy) 
    {
	
	if (-(*idum) < 1) *idum = 1;
	else *idum = -(*idum);
	
	for (j = NTAB+7; j >= 0; j--) 
	{
	    
	    k = (*idum)/IQ;
	    *idum = IA*(*idum-k*IQ)-IR*k;
	    if (*idum < 0) *idum += IM;
	    if (j < NTAB) iv[j] = *idum;
	    
	}
	
	iy = iv[0];
	
    }
    
    k = (*idum)/IQ;
    *idum = IA*(*idum-k*IQ) - IR*k;
    if (*idum < 0) *idum += IM;
    j = iy/NDIV;
    iy = iv[j];
    iv[j] = *idum;
    
    if ((temp = AM*iy) > RNMX) return RNMX;
    else return temp;
    
}
 
//------------------------------------------------------------------------------
//  Real Ran2()
//------------------------------------------------------------------------------
/*
 *  Long period (> 2x10^18) random number generator of L'Ecuyer with
 *  Bays-Durham shuffle and added safeguards. Returns a uniform random deviate
 *  between 0.0 and 1.0 (exclusive of the endpoint values). Call with idum a 
 *  negative integer to initialize; thereafter, do not alter idum between
 *  successive deviates in a sequence. RNMX should approximate the largest
 *  floating value that is less than 1.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::Ran2(){

    Integer j;
    long int k;
    static long int idum2 = 123456789;
    static long int iy = 0;
    static long int iv[NTAB];
    Real temp;
     
    if (*idum <= 0) 
    {
	
	if (-(*idum) < 1) *idum=1;
	else *idum = -(*idum);
	idum2 = (*idum);
	
	for (j = NTAB+7; j>=0; j--)
	{
	    
	    k = (*idum)/IQ1;
	    *idum = IA1*(*idum-k*IQ1)-IR1*k;
	    if (*idum < 0) *idum += IM1;
	    if (j < NTAB) iv[j] = *idum;
	    
	}
	
	iy = iv[0];
	
    }
    
    k = (*idum)/IQ1;
    *idum = IA1*(*idum-k*IQ1) - IR1*k;
    if (*idum < 0) *idum += IM1;
    k = idum2/IQ2;
    idum2 = IA2*(idum2-k*IQ2)-k*IR2;
    if (idum2 < 0) idum2 += IM2;
    j = iy/NDIV2;
    iy = iv[j]-idum2;
    iv[j] = *idum;
    if (iy < 1) iy += IMM1;
    
    if ((temp = AM2*iy) > RNMX) return RNMX;
    else return temp;
    
}
  
 //------------------------------------------------------------------------------
//  Real Ran3()
//------------------------------------------------------------------------------
/*
 *  Returns a uniform random deviate between 0.0 and 1.0. Set idum to any
 *  negative value to initialize or reinitialize the sequence.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::Ran3(){

     static Integer inext, inextp;
     static long int ma[56];
     static int iff = 0;
     long int mj, mk;
     Integer i, ii, k;
     
     if (*idum < 0 || iff == 0) 
     {
	 
	 iff = 1;
	 mj = labs(MSEED-labs(*idum));
	 mj %= MBIG;
	 ma[55] = mj;
	 mk = 1;
	 
	 for (i = 1; i < 54; i++) 
	 {

	     ii = (21*i) % 55;
	     ma[ii] = mk;
	     mk = mj - mk;
	     if (mk < MZ) mk += MBIG;
	     mj = ma[ii];

	 }
	 
	 for (k = 1; k  <4; k++) 
	 {
	     
	     for (i = 1; i < 55; i++) 
	     {

		ma[i] -= ma[1+(i+30) % 55];
		if (ma[i] < MZ) ma[i] += MBIG;

	     }
	     
	 }
	     
	 inext = 0;
	 inextp = 31;
	 *idum = 1;
	 
     }
     
     if (++inext == 56) inext = 1;
     if (++inextp == 56) inextp = 1;

     mj = ma[inext] - ma[inextp];

     if (mj < MZ) mj += MBIG;

     ma[inext] = mj;

     return mj * FAC;
     
}
 
  
//------------------------------------------------------------------------------
//  Real ExponentialRand()
//------------------------------------------------------------------------------
/*
 *  Returns an exponentially distributed, positive, random deviate of unit
 *  mean, using Ran1 as the source of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::ExponentialRand()
 {

     Real dum;
     
     do
	 dum = Ran1();
     while (dum == 0.0);
     
     return -GmatMathUtil::Log(dum);
     
} 

//------------------------------------------------------------------------------
//  Real ExponentialRand2()
//------------------------------------------------------------------------------
/*
 *  Returns an exponentially distributed, positive, random deviate of unit
 *  mean, using Ran2 as the source of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::ExponentialRand2()
 {

     Real dum;
     
     do
	 dum = Ran2();
     while (dum == 0.0);
     
     return -GmatMathUtil::Log(dum);
     
} 

//------------------------------------------------------------------------------
//  Real ExponentialRand3()
//------------------------------------------------------------------------------
/*
 *  Returns an exponentially distributed, positive, random deviate of unit
 *  mean, using Ran3 as the source of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::ExponentialRand3()
 {

     Real dum;
     
     do
	 dum = Ran3();
     while (dum == 0.0);
     
     return -GmatMathUtil::Log(dum);
     
} 

//------------------------------------------------------------------------------
//  Real GaussianRand()
//------------------------------------------------------------------------------
/*
 *  Returns a normally distributed Gaussian random deviate of unit
 *  mean, using Ran1 as the source of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRand(){

     static Integer iset = 0;
     static Real gset;
     Real fac, rsq, v1, v2;
     
     if (*idum < 0) iset = 0;
     if (iset == 0) 
     {
	 
	 do 
	 {
	     
	     v1 = 2.0*Ran1() - 1.0;
	     v2 = 2.0*Ran1() - 1.0;
	     rsq = v1*v1 + v2*v2;
	     
	 } while (rsq >= 1.0 || rsq == 0.0);
	 
	 fac = GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Log(rsq)/rsq);
	 gset = v1*fac;
	 iset = 1;
	 
	 return v2*fac;
	 
     } 
     else 
     {
	 
	 iset = 0;
	 return gset;
	 
     }
     
} 

//------------------------------------------------------------------------------
//  Real GaussianRand2()
//------------------------------------------------------------------------------
/*
 *  Returns a normally distributed Gaussian random deviate of unit
 *  mean, using Ran2 as the source of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRand2(){

     static Integer iset = 0;
     static Real gset;
     Real fac, rsq, v1, v2;
     
     if (*idum < 0) iset = 0;
     if (iset == 0) 
     {
	 
	 do 
	 {
	     
	     v1 = 2.0*Ran2() - 1.0;
	     v2 = 2.0*Ran2() - 1.0;
	     rsq = v1*v1 + v2*v2;
	     
	 } while (rsq >= 1.0 || rsq == 0.0);
	 
	 fac = GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Log(rsq)/rsq);
	 gset = v1*fac;
	 iset = 1;
	 
	 return v2*fac;
	 
     } 
     else 
     {
	 
	 iset = 0;
	 return gset;
	 
     }
     
} 

//------------------------------------------------------------------------------
//  Real GaussianRand3()
//------------------------------------------------------------------------------
/*
 *  Returns a normally distributed Gaussian random deviate of unit
 *  mean, using Ran3 as the source of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRand3(){

     static Integer iset = 0;
     static Real gset;
     Real fac, rsq, v1, v2;
     
     if (*idum < 0) iset = 0;
     if (iset == 0) 
     {
	 
	 do 
	 {
	     
	     v1 = 2.0*Ran3() - 1.0;
	     v2 = 2.0*Ran3() - 1.0;
	     rsq = v1*v1 + v2*v2;
	     
	 } while (rsq >= 1.0 || rsq == 0.0);
	 
	 fac = GmatMathUtil::Sqrt(-2.0*GmatMathUtil::Log(rsq)/rsq);
	 gset = v1*fac;
	 iset = 1;
	 
	 return v2*fac;
	 
     } 
     else 
     {
	 
	 iset = 0;
	 return gset;
	 
     }
     
} 


//------------------------------------------------------------------------------
//  Real GaussianRand(Real mean, Real stdev)
//------------------------------------------------------------------------------
/*
 *  Returns a normally distributed Gaussian random deviate with a
 *  prescribed mean and standard deviation using Ran1 as the source 
 *  of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRand(Real mean, Real stdev)
 {
 
     return GaussianRand()*stdev + mean;
     
 }

 //------------------------------------------------------------------------------
//  Real GaussianRand2(Real mean, Real stdev)
//------------------------------------------------------------------------------
/*
 *  Returns a normally distributed Gaussian random deviate with a
 *  prescribed mean and standard deviation using Ran2 as the source 
 *  of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRand2(Real mean, Real stdev)
 {

     return GaussianRand2()*stdev + mean;
     
 }

 //------------------------------------------------------------------------------
//  Real GaussianRand3(Real mean, Real stdev)
//------------------------------------------------------------------------------
/*
 *  Returns a normally distributed Gaussian random deviate with a
 *  prescribed mean and standard deviation using Ran3 as the source 
 *  of uniform deviates;
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRand3(Real mean, Real stdev)
 {

     return GaussianRand3()*stdev + mean;
     
 }
 
//------------------------------------------------------------------------------
//  Real UniformRand(Real a, Real b)
//------------------------------------------------------------------------------
/*
 *  Returns an uniformly distributed random deviate between a and b
 *  using Ran1 as the source of uniform deviates. The mean of this distribution
 *  is (a+b)/2. The variance of this distribution is (b-a)^2/12.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::UniformRand(Real a, Real b)
 {
 
     return a + (b - a)*Ran1();
     
 }

//------------------------------------------------------------------------------
//  Real UniformRand(Real a, Real b)
//------------------------------------------------------------------------------
/*
 *  Returns an uniformly distributed random deviate between a and b
 *  using Ran2 as the source of uniform deviates. The mean of this distribution
 *  is (a+b)/2. The variance of this distribution is (b-a)^2/12.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::UniformRand2(Real a, Real b)
 {
 
     return a + (b - a)*Ran2();
     
 }

//------------------------------------------------------------------------------
//  Real UniformRand3(Real a, Real b)
//------------------------------------------------------------------------------
/*
 *  Returns an uniformly distributed random deviate between a and b
 *  using Ran3 as the source of uniform deviates. The mean of this distribution
 *  is (a+b)/2. The variance of this distribution is (b-a)^2/12.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::UniformRand3(Real a, Real b)
 {
 
     return a + (b - a)*Ran3();
     
 }
