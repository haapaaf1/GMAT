//$Header$
//------------------------------------------------------------------------------
//                              GroundPropState
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/29
//
//
/**
 * Implements the state used in propagation of ground station objects. 
 */
//------------------------------------------------------------------------------


#include "GroundPropState.hpp"
#include "GroundStationObjectException.hpp"


//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// GroundPropState(const Integer dim)
//------------------------------------------------------------------------------
/**
 * Default constructor.
 *
 * @param <dim>   Size of the requested state vector.
 */
//------------------------------------------------------------------------------
GroundPropState::GroundPropState(const Integer dim) :
   dimension      (dim),
   epoch          (21545.0)
{
   groundstate = new Real[dimension];
}


//------------------------------------------------------------------------------
// ~GroundPropState()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
GroundPropState::~GroundPropState()
{
   delete [] groundstate;
}


//------------------------------------------------------------------------------
// GroundPropState(const GroundPropState& ps)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <ps>  The GroundPropState that gets copied here.
 */
//------------------------------------------------------------------------------
GroundPropState::GroundPropState(const GroundPropState& gps) :
   dimension      (gps.dimension),
   epoch          (gps.epoch)
{
   groundstate = new Real[dimension];
   for (Integer i = 0; i < dimension; ++i)
      groundstate[i] = gps.state[i];
}


//------------------------------------------------------------------------------
// GroundPropState& operator=(const GroundPropState& ps)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * @param <ps>  The GroundPropState that gets copied here.
 *
 * @return this GroundPropState, with data matching the input state.
 */
//------------------------------------------------------------------------------
GroundPropState& GroundPropState::operator=(const GroundPropState& gps)
{
   if (this == &gps)
      return *this;
      
   if (groundstate)
      delete [] groundstate;
   dimension = gps.dimension;
   groundstate = new Real[dimension];
   for (Integer i = 0; i < dimension; ++i)
      groundstate[i] = gps.groundstate[i];
   epoch = gps.epoch;
   
   return *this;
}

 
//------------------------------------------------------------------------------
// Real operator[](const Integer el)
//------------------------------------------------------------------------------
/**
 * Element access operator.
 *
 * This operator lets other code read and assign values to the elements of a
 * GroundPropState as if it were a standard C/C++ array.
 *
 * @param <el>  Index of the GroundPropState element that is being manipulated.
 *
 * @return The element's value at the end of the call.
 */
//------------------------------------------------------------------------------
Real& GroundPropState::operator[](const Integer el)
{
   if ((el < 0 ) || (el >= dimension))
      throw GroundStationObjectException("GroundPropState array index out of bounds");
   return groundstate[el];
}


//------------------------------------------------------------------------------
// Real operator[](const Integer el) const
//------------------------------------------------------------------------------
/**
 * Element access operator.
 *
 * This operator lets other code read values to the elements of a GroundPropState
 * as if it were a standard C/C++ array from within const methods.
 *
 * @param <el>  Index of the GroundPropState element that is being manipulated.
 *
 * @return The element's value at the end of the call.
 */
//------------------------------------------------------------------------------
Real GroundPropState::operator[](const Integer el) const
{
   if ((el < 0 ) || (el >= dimension))
      throw GroundStationObjectException("GroundPropState array index out of bounds");
   return groundstate[el];
}

// Accessors

//------------------------------------------------------------------------------
// Integer GetDimension() const
//------------------------------------------------------------------------------
/**
 * Finds the size of the GroundPropState vector.
 *
 * @return The size of the vector.
 */
//------------------------------------------------------------------------------
Integer GroundPropState::GetSize() const
{
   return dimension;
}


//------------------------------------------------------------------------------
// Real* GetState()
//------------------------------------------------------------------------------
/**
 * Accesses the state vector.
 *
 * @return The state vector.
 */
//------------------------------------------------------------------------------
Real* GroundPropState::GetState()
{
   return groundstate;
}


//------------------------------------------------------------------------------
// bool GroundPropState::SetState(const Real *data, const Integer size)
//------------------------------------------------------------------------------
/**
 * Sets the state elements to match an input array.
 *
 * This method copies the elements of the input array into the  first size 
 * elements of the GroundPropState vector.
 *
 * @param <data> The data that gets copied into the state vector.
 * @param <size> Number of elements that get copied (must be greater than 0).
 *
 * @return true if the elements were filled successfully.
 */
//------------------------------------------------------------------------------
bool GroundPropState::SetState(const Real *data, const Integer size)
{
   if (size <= dimension) {
      if (size <= 0)
         throw GroundStationObjectException(
            "GroundPropState attempting to fill an unphysical number of elements.");
      memcpy(groundstate, data, size*sizeof(Real));
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
Real GroundPropState::GetEpoch() const
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
Real GroundPropState::SetEpoch(const Real ep)
{
   return epoch = ep;
}
