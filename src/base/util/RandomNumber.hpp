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
 * the GNU Scientific Library(GSL). This software is freely disributable
 * under the standard GNU General Public license.
 *
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
#include <pcrecpp.h>
#include <gsl_rng.h>
#include <gsl_randist.h>
#include <gsl_cdf.h>

class RandomNumber
{

public:

    RandomNumber();
    RandomNumber(const std::string myGenerator);
    RandomNumber(unsigned long int mySeed);
    RandomNumber(const std::string myGenerator, unsigned long int mySeed);
    ~RandomNumber();

    //unsigned int Bernoulli(Real p);
    //Real BernoulliPDF(const unsigned int k, Real p);

    //Real Beta(const Real a, const Real b);
    //Real BetaPDF(const Real x, const Real a, const Real b);

    //unsigned int Binomial(Real p, unsigned int n);
    //unsigned int BinomialKnuth(Real p, unsigned int n);
    //unsigned int BinomialTpe(Real p, unsigned int n);
    //Real BinomialPDF(const unsigned int k, const Real p, const unsigned int n);

    //Real Exponential(const Real mu);
    //Real ExponentialPDF(const Real x, const Real mu);

    //Real ExpPow(const Real a, const Real b);
    //Real ExpPowPDF(const Real x, const Real a, const Real b);

    //Real Cauchy(const Real a);
    //Real CauchyPDF(const Real x, const Real a);

    //Real Chisq(const Real nu);
    //Real ChisqPDF(const Real x, const Real nu);

    //void Dirichlet(const sizeT K, const Real alpha[], Real theta[]);
    //Real DirichletPDF(const sizeT K, const Real alpha[], const Real theta[]);
    //Real Dirichlet_lnpdf(const sizeT K, const Real alpha[], const Real theta[]);

    //Real Erlang(const Real a, const Real n);
    //Real ErlangPDF(const Real x, const Real a, const Real n);

    //Real Fdist(const Real nu1, const Real nu2);
    //Real FdistPDF(const Real x, const Real nu1, const Real nu2);

    //Real Flat(const Real a, const Real b);
    //Real FlatPDF(Real x, const Real a, const Real b);

    //Real Gamma(const Real a, const Real b);
    //Real GammaInt(const unsigned int a);
    //Real GammaPDF(const Real x, const Real a, const Real b);
    //Real GammaMT(const Real a, const Real b);
    //Real GammaKnuth(const Real a, const Real b);

    Real Gaussian();
    Real Gaussian(const Real mean, const Real stdev);
    Real GaussianRatioMethod();
    Real GaussianRatioMethod(const Real mean, const Real stdev);
    Real GaussianZiggurat();
    Real GaussianZiggurat(const Real mean, const Real stdev);
    Real GaussianPDF(const Real x);
    Real GaussianPDF(const Real x, const Real stdev);
    Real GaussianCDF_P(const Real x);
    Real GaussianCDF_P(const Real x, const Real stdev);
    Real GaussianCDF_Q(const Real x);
    Real GaussianCDF_Q(const Real x, const Real stdev);
    Real GaussianCDF_Pinv(const Real P);
    Real GaussianCDF_Pinv(const Real P, const Real stdev);
    Real GaussianCDF_Qinv(const Real Q);
    Real GaussianCDF_Qinv(const Real Q, const Real stdev);

    Real GaussianTail(const Real a);
    Real GaussianTail(const Real a, const Real stdev);
    Real GaussianTailPDF(const Real x, const Real a);
    Real GaussianTailPDF(const Real x, const Real a, const Real stdev);

    //void BivariateGaussian(Real stdev_x, Real stdev_y, Real rho, Real *x, Real *y);
    //Real BivariateGaussianPDF(const Real x, const Real y, const Real stdev_x, const Real stdev_y, const Real rho);

    //Real Landau();
    //Real LandauPDF(const Real x);

    //unsigned int Geometric(const Real p);
    //Real GeometricPDF(const unsigned int k, const Real p);

    //unsigned int Hypergeometric(unsigned int n1, unsigned int n2, unsigned int t);
    //Real HypergeometricPDF(const unsigned int k, const unsigned int n1, const unsigned int n2, unsigned int t);

    //Real Gumbel1(const Real a, const Real b);
    //Real Gumbel1PDF(const Real x, const Real a, const Real b);

    //Real Gumbel2(const Real a, const Real b);
    //Real Gumbel2PDF(const Real x, const Real a, const Real b);

    //Real Logistic(const Real a);
    //Real LogisticPDF(const Real x, const Real a);

    //Real LogNormal(const Real zeta, const Real stdev);
    //Real LogNormalPDF(const Real x, const Real zeta, const Real stdev);

    //unsigned int Logarithmic(const Real p);
    //Real LogarithmicPDF(const unsigned int k, const Real p);

    //void Multinomial(const sizeT K,
    //                      const unsigned int N, const Real p[],
    //                      unsigned int n[] );
    //Real MultinomialPDF(const sizeT K,
    //                            const Real p[], const unsigned int n[] );
    //Real MultinomialLnPDF(const sizeT K,
    //                       const Real p[], const unsigned int n[] );

    //unsigned int NegativeBinomial(Real p, Real n);
    //Real NegativeBinomialPDF(const unsigned int k, const Real p, Real n);

    //unsigned int pascal(Real p, unsigned int n);
    //Real PascalPDF(const unsigned int k, const Real p, unsigned int n);

    //Real Pareto(Real a, const Real b);
    //Real ParetoPDF(const Real x, const Real a, const Real b);

    //unsigned int Poisson(Real mu);
    //void PoissonArray(sizeT n, unsigned int array[],
    //                        Real mu);
    //Real PoissonPDF(const unsigned int k, const Real mu);

    //Real Rayleigh(const Real stdev);
    //Real RayleighPDF(const Real x, const Real stdev);

    //Real RayleighTail(const Real a, const Real stdev);
    //Real RayleighTailPDF(const Real x, const Real a, const Real stdev);

    //Real TDist(const Real nu);
    //Real TDistPDF(const Real x, const Real nu);

    Real Uniform();
    Real UniformPositive();
    Real Uniform(const Real a, const Real b);
    
    //Real Laplace(const Real a);
    //Real LaplacePDF(const Real x, const Real a);

    //Real Levy(const Real c, const Real alpha);
    //Real LevySkew(const Real c, const Real alpha, const Real beta);

    //Real Weibull(const Real a, const Real b);
    //Real WeibullPDF(const Real x, const Real a, const Real b);

    // Set and/or Re-set the seed for the random number generator
    void Seed(unsigned long int s);
    void ClockSeed();

    // Methods to select the generator type
    const gsl_rng_type* GetGenerator(const std::string &myGeneratorString);
    const gsl_rng_type* GetGenerator(const Integer myGeneratorID);
    std::string GetGeneratorText(const Integer id) const;
    Integer GetGeneratorID(const std::string &str) const;

    enum GENERATOR_TYPE_REPS
    {
	MT19937_ID = 0,
	RANLXS0_ID,
	RANLXS1_ID,
	RANLXS2_ID,
	RANLXD1_ID,
	RANLXD2_ID,
	RANLUX_ID,
	RANLUX389_ID,
        CMRG_ID,
        MRG_ID,
        TAUS_ID,
        TAUS2_ID,
        GFSR4_ID,
	EndGeneratorTypeReps
    };

private:

    const gsl_rng_type *T;
    gsl_rng *r;
    long int idum;

    static const std::string GENERATOR_TEXT[EndGeneratorTypeReps];

};

#endif	/* _RandomNumber_hpp */

