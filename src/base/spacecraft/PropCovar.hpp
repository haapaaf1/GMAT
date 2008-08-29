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

class GMAT_API PropCovar
{
public:

   PropCovar(const Integer dim = 6);
   virtual ~PropCovar();
   PropCovar(const PropCovar& ps);
   PropCovar&        operator=(const PropCovar& ps);
 
   // Matrix manipulations
   Real&             operator[](const Integer row, const Integer col);
   Real              operator[](const Integer row, const Integer col) const;
   
   // Sizing manipulation
   void              SetSize(const Integer size);
   
   // Access methods
   Integer           GetSize() const;
   Real*             GetCovariance();
   bool              SetCovariance(const Real *data, const Integer size);
   Real              GetEpoch() const;
   Real              SetEpoch(const Real ep);
   
 
protected:
   /// Matrix used for the covariance data
   Real              *covariance;
   /// Diagonal dimension of the covariance matrix
   Integer           dimension;
   /// Raw epoch data for the covariance
   Real              epoch;
};

#endif // PropCovar_hpp
