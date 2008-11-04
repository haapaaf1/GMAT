//$Header$
//------------------------------------------------------------------------------
//                              PropCovar
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/08/28
//
/**
 *
 * Implements the covariance used in propagation. 
 *
 */
//------------------------------------------------------------------------------

#ifndef PropCovar_hpp
#define PropCovar_hpp

#include "gmatdefs.hpp"
#include <lapackpp.h>

using namespace la;

class GMAT_API PropCovar
{
public:

   PropCovar(const Integer dim = 6);
   virtual ~PropCovar();
   PropCovar(const PropCovar& ps);
   PropCovar&        operator=(const PropCovar& ps);
       
   // Access methods
   LaGenMatDouble    GetCovariance();
   Real              GetEpoch() const;
   Real              SetEpoch(const Real ep);

   void	SetCovariance(const Rvector6 &diagCovar);
   void	SetCovariance(const Real &c11, const Real &c22, const Real &c33,
		      const Real &c44, const Real &c55, const Real &c66);
   void	SetCovariance(const LaGenMatDouble &cov);
   void	SetCovariance(const Rvector &cov, Integer &m, Integer &n);
   
 
protected:

    /// Raw epoch data for the covariance
   Real              epoch;
   /// Matrix used for the covariance data
   LaGenMatDouble   covariance;
   
};

#endif // PropCovar_hpp
