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
   
protected:

    /// Raw epoch data for the covariance
   Real              epoch;
   /// Matrix used for the covariance data
   LaGenMatDouble   covariance;
   
};

#endif // PropCovar_hpp
