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
 * This class implements the random number generator routines found in
 * Numerical Recipes in C++, The Art of Scientifice Computing, 2nd Edition.
 * The specific technical references are:
 * 1) Park and Miller, "Random Number Generator: Good Ones are Hard to Find."
 *    Communications of the ACM 31 (10), pp 1192-1201.
 * 2) L'Ecuyer, P. 1988, Communications of the ACM, vol 31, pp 742-774.
 * 3) Knuth, D.E. 1981, Seminumerical Algorithms, 2nd ed., vol 2 of "The
 *    Art of Computer Programming", Sections 3.2-3.3.
 *
 */
//------------------------------------------------------------------------------

#include "RandomNumber.hpp"

//---------------------------------
// static data
//---------------------------------

/// Labels used for the ground station parameters.
const std::string RandomNumber::GENERATOR_TEXT[EndGeneratorTypeReps] =
   {
         "MT19937",
         "RANLXS0",
         "RANLXS1",
         "RANLXS2",
         "RANLXD1",
         "RANLXD2",
         "RANLUX",
         "RANLUX389",
         "CMRG",
         "MRG",
         "TAUS",
         "TAUS2",
         "GFSR4"
   };

//------------------------------------------------------------------------------
// RandomNumber()
//------------------------------------------------------------------------------
/**
 * Class constructor using default generator type and default seed.
 * The default generator is gsl_rng_mt19937 and the default seed is 0.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber()
{

    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    
}

//------------------------------------------------------------------------------
// RandomNumber(const unsigned long int mySeed)
//------------------------------------------------------------------------------
/**
 * Class constructor using default generator type and specified seed value.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber(unsigned long int mySeed)
{

    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    gsl_rng_set(r,mySeed);

}

//------------------------------------------------------------------------------
// RandomNumber(const gsl_rng_type *myGenerator)
//------------------------------------------------------------------------------
/**
 * Class constructor using specified generator type and default seed value.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber(const std::string myGenerator)
{

    T = GetGenerator(myGenerator);
    r = gsl_rng_alloc(T);

}

//------------------------------------------------------------------------------
// RandomNumber(const std::string myGenerator, unsigned long int mySeed)
//------------------------------------------------------------------------------
/**
 * Class constructor using specified generator type and specified seed value.
 *
 */
//------------------------------------------------------------------------------
RandomNumber::RandomNumber(const std::string myGenerator,
                           unsigned long int mySeed)
{

    T = GetGenerator(myGenerator);
    r = gsl_rng_alloc(T);
    gsl_rng_set(r,mySeed);

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
    gsl_rng_free(r);
}


//------------------------------------------------------------------------------
//  void Seed(unsigned long int s)
//------------------------------------------------------------------------------
/**
 *  Set the seed for the random number generators.
 *
 *  @param <idum2> input seed
 */
//------------------------------------------------------------------------------
void RandomNumber::Seed(unsigned long int s)
{
    gsl_rng_set(r,s);
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

    unsigned long int clockSeed = time(NULL);

    gsl_rng_set(r,clockSeed);

}

//------------------------------------------------------------------------------
//  const gsl_rng_type* GetGenerator(const std::string &myGeneratorString)
//------------------------------------------------------------------------------
/**
 * This method returns a pointer of type gsl_rng_type that indicates the
 * type of random number generator to be used.
 *
 * @param <myGeneratorString> string name of the requested generator.
 *
 * @return pointer of type gsl_rng_type.
 */
//------------------------------------------------------------------------------
const gsl_rng_type* RandomNumber::GetGenerator(const std::string &myGeneratorString)
{
    return GetGenerator(GetGeneratorID(myGeneratorString));
}

//------------------------------------------------------------------------------
//  const gsl_rng_type* GetGenerator(const Integer myGeneratorID)
//------------------------------------------------------------------------------
/**
 * This method returns a pointer of type gsl_rng_type that indicates the
 * type of random number generator to be used.
 *
 * @param <myGeneratorID> ID for the requested generator.
 *
 * @return pointer of type gsl_rng_type.
 */
//------------------------------------------------------------------------------
const gsl_rng_type* RandomNumber::GetGenerator(const Integer myGeneratorID)
{

    switch(myGeneratorID)
    {
        case MT19937_ID:
            return gsl_rng_mt19937;
            break;
	case RANLXS0_ID:
            return gsl_rng_ranlxs0;
            break;
	case RANLXS1_ID:
            return gsl_rng_ranlxs1;
            break;
	case RANLXS2_ID:
            return gsl_rng_ranlxs2;
            break;
	case RANLXD1_ID:
            return gsl_rng_ranlxd1;
            break;
	case RANLXD2_ID:
            return gsl_rng_ranlxd2;
            break;
	case RANLUX_ID:
            return gsl_rng_ranlux;
            break;
	case RANLUX389_ID:
            return gsl_rng_ranlux389;
            break;
        case CMRG_ID:
            return gsl_rng_cmrg;
            break;
        case MRG_ID:
            return gsl_rng_mrg;
            break;
        case TAUS_ID:
            return gsl_rng_taus;
            break;
        case TAUS2_ID:
            return gsl_rng_taus2;
            break;
        case GFSR4_ID:
            return gsl_rng_gfsr4;
            break;
        default:
            return gsl_rng_default;
    }

    return gsl_rng_default;

}

//------------------------------------------------------------------------------
//  std::string  GetGeneratorText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the generator text, given the input generator ID.
 *
 * @param id Id for the requested generator text.
 *
 * @return parameter text for the requested generator.
 */
//------------------------------------------------------------------------------
std::string RandomNumber::GetGeneratorText(const Integer id) const
{
   if (id >= 0 && id < EndGeneratorTypeReps)
      return GENERATOR_TEXT[id];
   return std::string("");
}

//------------------------------------------------------------------------------
//  Integer  GetGeneratorID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the generator ID, given the input parameter string.
 *
 * @param str string for the requested generator.
 *
 * @return ID for the requested generator.
 */
//------------------------------------------------------------------------------
Integer RandomNumber::GetGeneratorID(const std::string &str) const
{

   std::string regex = "^" + str + "$";
   
   for (Integer i = 0; i < EndGeneratorTypeReps; i++)
   {
      if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                                 .set_extended(true)
                     ).FullMatch(GENERATOR_TEXT[i]))
         return i;
   }

   return -1;
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
     return gsl_rng_uniform(r);
 }

 //------------------------------------------------------------------------------
//  Real UniformPositive()
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate in the range (0,1)
 *  The range excludes both 0.0 and 1.0. This is useful when you need to
 *  avoid a singularity at 0.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::UniformPositive()
 {
     return gsl_rng_uniform_pos(r);
 }


//------------------------------------------------------------------------------
//  Real Uniform(Real a, Real b)
//------------------------------------------------------------------------------
/**
 *  Returns an uniformly distributed random deviate between a and b using Ran1.
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

     return a + (b - a)*gsl_rng_uniform(r);

 }

//------------------------------------------------------------------------------
//  Real Gaussian()
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate (zero mean, unit var)
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::Gaussian()
{
    return gsl_ran_gaussian(r,1.0);
}

//------------------------------------------------------------------------------
//  Real Gaussian(const Real mean, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate with a
 *  prescribed mean and standard deviation.
 *
 *  @param <mean> Mean of Gaussian distribution
 *  @param <stdev> Standard deviation of Gaussian distribution
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::Gaussian(const Real mean, const Real stdev)
 {

     return gsl_ran_gaussian(r,(double)stdev) + mean;

 }

//------------------------------------------------------------------------------
//  Real GaussianPDF(const Real x)
//------------------------------------------------------------------------------
/**
 *  Returns the probability density p(x) at x for a Gaussian distribution
 *  with unit standard deviation.
 *
 *  p(x) dx = {1 \over \sqrt{2 \pi \sigma^2}} \exp (-x^2 / 2\sigma^2) dx
 *
 *  @param <x> The value at which to evaluate the pdf
 *  @return p(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianPDF(const Real x)
{
    return gsl_ran_gaussian_pdf((double)x,1.0);
}

//------------------------------------------------------------------------------
//  Real GaussianPDF(const Real x, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns the probability density p(x) at x for a Gaussian distribution
 *  with standard deviation sigma.
 *
 *  p(x) dx = {1 \over \sqrt{2 \pi \sigma^2}} \exp (-x^2 / 2\sigma^2) dx
 *
 *  @param <x> The value at which to evaluate the pdf
 *  @param <stdev> The standard deviation of the Gaussian distribution
 *  @return p(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianPDF(const Real x, const Real stdev)
{
    return gsl_ran_gaussian_pdf((double)x, (double)stdev);
}

//------------------------------------------------------------------------------
//  Real GaussianZiggurat()
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate with zero mean
 *  and unit standard deviation using the alternative
 *  Marsaglia-Tsang ziggurat method. The Ziggurat algorithm is the fastest
 *  available algorithm in most cases.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianZiggurat()
 {
     return gsl_ran_gaussian_ziggurat(r,1.0);
 }

//------------------------------------------------------------------------------
//  Real GaussianZiggurat(const Real mean, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate with a
 *  prescribed mean and standard deviation using the alternative
 *  Marsaglia-Tsang ziggurat method. The Ziggurat algorithm is the fastest
 *  available algorithm in most cases.
 *
 *  @param <mean> Mean of Gaussian distribution
 *  @param <stdev> Standard deviation of Gaussian distribution
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianZiggurat(const Real mean, const Real stdev)
 {
     return gsl_ran_gaussian_ziggurat(r,(double)stdev) + mean;
 }

//------------------------------------------------------------------------------
//  Real GaussianRatioMethod()
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate with zero mean
 *  and unit standard deviation using the alternative
 *  Kinderman-Monahan-Leva ratio methods.
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianRatioMethod()
 {
     return gsl_ran_gaussian_ratio_method(r,1.0);
 }

//------------------------------------------------------------------------------
//  Real GaussianRatioMethod(const Real mean, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns a normally distributed Gaussian random deviate with a
 *  prescribed mean and standard deviation using the alternative
 *  Kinderman-Monahan-Leva ratio methods.
 *
 *  @param <mean> Mean of Gaussian distribution
 *  @param <stdev> Standard deviation of Gaussian distribution
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianRatioMethod(const Real mean, const Real stdev)
{
     return gsl_ran_gaussian_ratio_method(r,(double)stdev) + mean;
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_P(const Real x)
//------------------------------------------------------------------------------
/**
 *  Returns the cumulative distribution function P(x) at x
 *  for a Gaussian distribution with unit standard deviation.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @return P(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_P(const Real x)
{
    return gsl_cdf_gaussian_P((double)x,1.0);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_P(const Real x, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns the cumulative distribution function P(x) at x
 *  for a Gaussian distribution with standard deviation sigma.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @param <stdev> The standard deviation of the Gaussian distribution
 *  @return P(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_P(const Real x, const Real stdev)
{
    return gsl_cdf_gaussian_P((double)x, (double)stdev);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_Q(const Real x)
//------------------------------------------------------------------------------
/**
 *  Returns the cumulative distribution function Q(x) at x
 *  for a Gaussian distribution with unit standard deviation.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @return Q(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_Q(const Real x)
{
    return gsl_cdf_gaussian_Q((double)x,1.0);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_Q(const Real x, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns the cumulative distribution function Q(x) at x
 *  for a Gaussian distribution with standard deviation sigma.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @param <stdev> The standard deviation of the Gaussian distribution
 *  @return Q(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_Q(const Real x, const Real stdev)
{
    return gsl_cdf_gaussian_Q((double)x, (double)stdev);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_Pinv(const Real x)
//------------------------------------------------------------------------------
/**
 *  Returns the inverse of the cumulative distribution function P(x) at x
 *  for a Gaussian distribution with unit standard deviation.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @return Pinv(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_Pinv(const Real x)
{
    return gsl_cdf_gaussian_Pinv((double)x,1.0);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_Pinv(const Real x, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns the inverse of the the cumulative distribution function P(x) at x
 *  for a Gaussian distribution with standard deviation sigma.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @param <stdev> The standard deviation of the Gaussian distribution
 *  @return Pinv(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_Pinv(const Real x, const Real stdev)
{
    return gsl_cdf_gaussian_Pinv((double)x, (double)stdev);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_Qinv(const Real x)
//------------------------------------------------------------------------------
/**
 *  Returns the inverse of the the cumulative distribution function Q(x) at x
 *  for a Gaussian distribution with unit standard deviation.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @return Qinv(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_Qinv(const Real x)
{
    return gsl_cdf_gaussian_Qinv((double)x,1.0);
}

//------------------------------------------------------------------------------
//  Real GaussianCDF_Qinv(const Real x, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns the inverse of the the cumulative distribution function Q(x) at x
 *  for a Gaussian distribution with standard deviation sigma.
 *
 *  @param <x> The value at which to evaluate the cdf
 *  @param <stdev> The standard deviation of the Gaussian distribution
 *  @return Qinv(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianCDF_Qinv(const Real x, const Real stdev)
{
    return gsl_cdf_gaussian_Qinv((double)x, (double)stdev);
}

//------------------------------------------------------------------------------
//  Real GaussianTail(const Real a)
//------------------------------------------------------------------------------
/**
 *  This function provides random variates from the upper tail of a Gaussian
 *  distribution with unit standard deviation. The values returned are
 *  larger than the lower limit a, which must be positive. The method is based
 *  on Marsaglia's famous rectangle-wedge-tail algorithm
 *  (Ann. Math. Stat. 32, 894–899 (1961)), with this aspect explained in
 *  Knuth, v2, 3rd ed, p139,586 (exercise 11).
 *
 *  The probability distribution for Gaussian tail random variates is,
 *
 *  p(x) dx = {1 \over N(a;\sigma) \sqrt{2 \pi \sigma^2}} \exp (- x^2/(2 \sigma^2)) dx
 *
 *  for x > a where N(a;\sigma) is the normalization constant,
 *
 *  N(a;\sigma) = (1/2) erfc(a / sqrt(2 sigma^2)).
 *
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianTail(const Real a)
{
    if (a > 0) return gsl_ran_gaussian_tail(r, (double)a, 1.0);
    else
    {
      UtilityException ex;
      ex.SetDetails("GaussianTail: Lower limit 'a' must be positive!");
      throw ex;
    }
}

//------------------------------------------------------------------------------
//  Real GaussianTail(const Real a, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  This function provides random variates from the upper tail of a Gaussian
 *  distribution with standard deviation sigma. The values returned are
 *  larger than the lower limit a, which must be positive. The method is based
 *  on Marsaglia's famous rectangle-wedge-tail algorithm
 *  (Ann. Math. Stat. 32, 894–899 (1961)), with this aspect explained in
 *  Knuth, v2, 3rd ed, p139,586 (exercise 11).
 *
 *  @param <stdev> Standard deviation of Gaussian distribution
 *  @return The random deviate.
 *
 */
//------------------------------------------------------------------------------
 Real RandomNumber::GaussianTail(const Real a, const Real stdev)
 {
    if (a > 0)
        return gsl_ran_gaussian_tail(r, (double)a, (double)stdev);
    else
    {
      UtilityException ex;
      ex.SetDetails("GaussianTail: Lower limit 'a' must be positive!");
      throw ex;
    }    
 }

//------------------------------------------------------------------------------
//  Real GaussianTailPDF(const Real x, const Real a)
//------------------------------------------------------------------------------
/**
 *  Returns the probability density p(x) at x for a Gaussian tail distribution
 *  with unit standard deviation and lower limit a.
 *
 *  The probability distribution for Gaussian tail random variates is,
 *
 *  p(x) dx = {1 \over N(a;\sigma) \sqrt{2 \pi \sigma^2}} \exp (- x^2/(2 \sigma^2)) dx
 *
 *  for x > a where N(a;\sigma) is the normalization constant,
 *
 *  N(a;\sigma) = (1/2) erfc(a / sqrt(2 sigma^2)).
 *
 *  @param <a> The value at which to evaluate the pdf
 *  @return p(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianTailPDF(const Real x, const Real a)
{
    if (a > 0)
        return gsl_ran_gaussian_tail_pdf((double)x, (double)a, 1.0);
    else
    {
      UtilityException ex;
      ex.SetDetails("GaussianTail: Lower limit 'a' must be positive!");
      throw ex;
    }
}

//------------------------------------------------------------------------------
//  Real GaussianTailPDF(const Real x, const Real a, const Real stdev)
//------------------------------------------------------------------------------
/**
 *  Returns the probability density p(x) at x for a Gaussian tail distribution
 *  with specified standard deviation stdev and lower limit a.
 *
 *  The probability distribution for Gaussian tail random variates is,
 *
 *  p(x) dx = {1 \over N(a;\sigma) \sqrt{2 \pi \sigma^2}} \exp (- x^2/(2 \sigma^2)) dx
 *
 *  for x > a where N(a;\sigma) is the normalization constant,
 *
 *  N(a;\sigma) = (1/2) erfc(a / sqrt(2 sigma^2)).
 *
 *  @param <a> The value at which to evaluate the pdf
 *  @param <stdev> The standard deviation of the Gaussian distribution
 *  @return p(x)
 *
 */
//------------------------------------------------------------------------------
Real RandomNumber::GaussianTailPDF(const Real x, const Real a, const Real stdev)
{
    if (a > 0)
        return gsl_ran_gaussian_tail_pdf((double)x, (double)a, (double)stdev);
    else
    {
      UtilityException ex;
      ex.SetDetails("GaussianTail: Lower limit 'a' must be positive!");
      throw ex;
    }
}
