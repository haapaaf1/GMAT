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

#include "PropCovar.hpp"

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// PropCovar(const Integer dim)
//------------------------------------------------------------------------------
/**
 * Default constructor.
 *
 * @param <dim>   Size of the requested covariance vector.
 */
//------------------------------------------------------------------------------
PropCovar::PropCovar(const Integer dim) :
   epoch          (21545.0),
   covariance     (dim,dim) 
{
}


//------------------------------------------------------------------------------
// ~PropCovar()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
PropCovar::~PropCovar()
{
}


//------------------------------------------------------------------------------
// PropCovar(const PropCovar& ps)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <ps>  The PropCovar that gets copied here.
 * Only the upper triangular part of the covariance matrix is copied.
 */
//------------------------------------------------------------------------------
PropCovar::PropCovar(const PropCovar& ps) :
   epoch (ps.epoch), covariance(ps.covariance)
{
}


//------------------------------------------------------------------------------
// PropCovar& operator=(const PropCovar& ps)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * @param <ps>  The PropCovar that gets copied here.
 * Only the upper triangular part of the covariance matrix is copied.
 *
 * @return this PropCovar, with data matching the input covariance.
 */
//------------------------------------------------------------------------------
PropCovar& PropCovar::operator=(const PropCovar& ps)
{
   if (this == &ps)
      return *this;
      
   covariance.copy(ps.covariance);
   epoch = ps.epoch;
   
   return *this;
}

// Accessors

//------------------------------------------------------------------------------
// Real* GetCovariance()
//------------------------------------------------------------------------------
/**
 * Accesses the covariance vector.
 *
 * @return The covariance vector.
 */
//------------------------------------------------------------------------------
LaGenMatDouble PropCovar::GetCovariance()
{
   return covariance;
}

//------------------------------------------------------------------------------
// Real GetEpoch()
//------------------------------------------------------------------------------
/**
 * Accessor for the current epoch of the object, in A.1 Modified Julian format.
 *
 * @return The A.1 epoch.
 *
 * @todo The epoch probably should be TAI throughout GMAT.
 */
//------------------------------------------------------------------------------
Real PropCovar::GetEpoch() const
{
   return epoch;
}

//------------------------------------------------------------------------------
// Real SetEpoch(const Real ep)
//------------------------------------------------------------------------------
/**
 * Accessor used to set epoch (in A.1 Modified Julian format) of the object.
 *
 * @param <ep> The new A.1 epoch.
 *
 * @return The updated A.1 epoch.
 *
 * @todo The epoch probably should be TAI throughout GMAT.
 */
//------------------------------------------------------------------------------
Real PropCovar::SetEpoch(const Real ep)
{
   return epoch = ep;
}


//---------------------------------------------------------------------------
//  void SetSCovariance(const LaGenMatDouble &cov)
//---------------------------------------------------------------------------
/**
 * Set the covariance matrix to a desired value.
 * 
 * @param <cov> LaGenMatDouble matrix containing desired covariance data
 *
 */
//---------------------------------------------------------------------------
void PropCovar::SetCovariance(const LaGenMatDouble &cov)
{
    covariance.copy(cov);
}

//---------------------------------------------------------------------------
//  void SetSCovariance(const Rvector6 &diagCovar)
//---------------------------------------------------------------------------
/**
 * Set the elements to covariance of specified cartesian or orbit element states.
 * 
 * @param <diagCovar> Diagonal vector corresponding to variance of states
 *
 */
//---------------------------------------------------------------------------
void PropCovar::SetCovariance(const Rvector6 &diagCovar)
{     
   Rvector cov(36) = 0;
   cov(0) = diagCovar.Get(0);
   cov(7) = diagCovar.Get(1);
   cov(14) = diagCovar.Get(2);
   cov(21) = diagCovar.Get(3);
   cov(28) = diagCovar.Get(4);
   cov(35) = diagCovar.Get(5);
   
   LaGenMatDouble ctemp(cov,6,6);
         
   SetCovariance(ctemp);
}

//------------------------------------------------------------------------------
//  void SetCovariance(const Real &c11, const Real &c22, const Real &c33,
//		       const Real &c44, const Real &c55, const Real &c66)
//------------------------------------------------------------------------------
/**
 * Set the elements of a covariance matrix.
 * 
 * @param <c11>  First diagonal covariance element
 * @param <c22>  Second diagonal covariance element
 * @param <c33>  Third diagonal covariance element
 * @param <c44>  Fourth diagonal covariance element
 * @param <c55>  Fifth diagonal covariance element
 * @param <c66>  Sixth diagonal covariance element  
 */
//------------------------------------------------------------------------------
void SetCovariance(const Real &c11, const Real &c22, const Real &c33,
		   const Real &c44, const Real &c55, const Real &c66)
{
   Rvector cov(36) = 0;
   cov(0) = c11;
   cov(7) = c22;
   cov(14) = c33;
   cov(21) = c44;
   cov(28) = c55;
   cov(35) = c66;
   
   LaGenMatDouble ctemp(cov,6,6);
         
   SetCovariance(ctemp);
      
}

//------------------------------------------------------------------------------
//  void SetCovariance(Rvector &cov)
//------------------------------------------------------------------------------
/**
 * Set the elements of a covariance matrix.
 * 
 * @param <cov>  Rvector containing desired elements of the covariance matrix 
 * @param <m> Number of rows in desired covariance matrix
 * @param <n> Number of columns in desired covariance matrix
 */
//------------------------------------------------------------------------------
void SetCovariance(Rvector &cov, Integer &m, Integer &n)
{
   LaGenMatDouble ctemp(cov,m,n);
   SetCovariance(ctemp);
}
