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

//------------------------------------------------------------------------------
// LaGenMatDouble copy(LaGenMatDouble mat)
//------------------------------------------------------------------------------
/**
 * Unhides the LaGenMatDouble copy method.
 *
 * @return The covariance matrix to copy.
 */
//------------------------------------------------------------------------------
LaGenMatDouble PropCovar::copy(LaGenMatDouble mat)
{
   covariance.copy(mat);
}

