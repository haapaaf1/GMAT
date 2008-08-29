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
#include "SpaceObjectException.hpp"


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
   dimension      (dim),
   epoch          (21545.0)
{
   covariance = new Real[dimension][dimension];
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
   delete [] covariance;
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
   dimension      (ps.dimension),
   epoch          (ps.epoch)
{
   covariance = new Real[dimension][dimension];
   for (Integer i = 0; i < dimension; ++i) {
	for (Integer j = i; j < dimension; ++j)
	    covariance[i][j] = ps.covariance[i][j];
   }
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
      
   if (covariance)
      delete [] covariance;
   dimension = ps.dimension;
   covariance = new Real[dimension][dimension];
   for (Integer i = 0; i < dimension; ++i) {
	for (Integer j = i; j < dimension; ++j)
	    covariance[i][j] = ps.covariance[i][j];
   }
   epoch = ps.epoch;
   
   return *this;
}

 
//------------------------------------------------------------------------------
// Real operator[](const Integer row, const Integer col)
//------------------------------------------------------------------------------
/**
 * Element access operator.
 *
 * This operator lets other code read and assign values to the elements of a
 * PropCovar as if it were a standard C/C++ matrix.
 *
 * @param <row>  Row of the PropCovar matrix that is being manipulated.
 * @param <col> Column of the PropCovar matrix that is being manipulated.
 *
 * @return The element's value at the end of the call.
 */
//------------------------------------------------------------------------------
Real& PropCovar::operator[](const Integer row, const Integer col)
{
   if (((row < 0 ) || (row >= dimension)) || ((col < row ) || (col >= dimension)))
      throw SpaceObjectException("PropCovar matrix index out of bounds");
   return covariance[row][col];
}


 
//------------------------------------------------------------------------------
// Real operator[](const Integer row, const Integer col)
//------------------------------------------------------------------------------
/**
 * Element access operator.
 *
 * This operator lets other code read and assign values to the elements of a
 * PropCovar as if it were a standard C/C++ matrix within const methods.
 *
 * @param <row>  Row of the PropCovar matrix that is being manipulated.
 * @param <col> Column of the PropCovar matrix that is being manipulated.
 *
 * @return The element's value at the end of the call.
 */
//------------------------------------------------------------------------------
Real& PropCovar::operator[](const Integer row, const Integer col) const
{
   if (((row < 0 ) || (row >= dimension)) || ((col < row ) || (col >= dimension)))
      throw SpaceObjectException("PropCovar matrix index out of bounds");
   return covariance[row][el2];
}


//------------------------------------------------------------------------------
// void SetSize(const Integer size)
//------------------------------------------------------------------------------
/**
 * matrix size manipulator.
 *
 * This method changes the size of the PropCovar matrix.  The elements of the 
 * old matrix are copied into the new matrix; if the new size is larger than the
 * old matrix, only the elements up to the old size are filled.  If the new
 * matrix is smaller, only the elements at the start of the old matrix are
 * copied into the new one.
 *
 * @param <size> Size of the new PropCovar (must be greater than 0).
 *
 * @return The element's value at the end of the call.
 */
//------------------------------------------------------------------------------
void PropCovar::SetSize(const Integer size)
{
   if (size != dimension)
   {
      if (size <= 0)
         throw SpaceObjectException(
            "PropCovar resize requested for an unphysical covariance size.");
      Real *newcovariance = new Real[size][size];
      Integer copySize = ((size > dimension) ? dimension : size);
      memcpy(newcovariance, covariance, copySize * copySize * sizeof(Real));
      delete [] covariance;
      covariance = newcovariance;
      dimension = size;
   }
}


// Accessors

//------------------------------------------------------------------------------
// Integer GetDimension() const
//------------------------------------------------------------------------------
/**
 * Finds the size of the PropCovar vector.
 *
 * @return The size of the vector.
 */
//------------------------------------------------------------------------------
Integer PropCovar::GetSize() const
{
   return dimension;
}


//------------------------------------------------------------------------------
// Real* GetCovariance()
//------------------------------------------------------------------------------
/**
 * Accesses the covariance vector.
 *
 * @return The covariance vector.
 */
//------------------------------------------------------------------------------
Real* PropCovar::GetCovariance()
{
   return covariance;
}


//------------------------------------------------------------------------------
// bool PropCovar::SetCovariance(const Real *data, const Integer size)
//------------------------------------------------------------------------------
/**
 * Sets the covariance elements to match an input matrix.
 *
 * This method copies the elements of the input matrix into the  first size 
 * elements of the PropCovar vector.
 *
 * @param <data> The data that gets copied into the covariance vector.
 * @param <size> Number of elements that get copied (must be greater than 0).
 *
 * @return true if the elements were filled successfully.
 */
//------------------------------------------------------------------------------
bool PropCovar::SetCovariance(const Real *data, const Integer size)
{
   if (size <= dimension) {
      if (size <= 0)
         throw SpaceObjectException(
            "PropCovar attempting to fill an unphysical number of elements.");
      memcpy(covariance, data, size*size*sizeof(Real));
      return true;
   }
   return false;
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
