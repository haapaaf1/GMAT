//$Id$
//------------------------------------------------------------------------------
//                                  GmatState
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/12/15
//
/**
 * Implementation of the GmatState class.  This is the class for state data used 
 * in GMAT's propagators and solvers.
 */
//------------------------------------------------------------------------------

#include "GmatState.hpp"
#include "GmatBaseException.hpp"

#include <strstream>


//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// GmatState(Integer size)
//------------------------------------------------------------------------------
/**
 * Default constructor for the GmatState.
 *
 * @param size Size of the requested state vector.
 */
//------------------------------------------------------------------------------
GmatState::GmatState(Integer size) :
   theEpoch          (21545.0),
   stateSize         (size)
{
   if (stateSize == 0)
      theData = NULL;
   else
   {
      theData = new Real[stateSize];
      Zero();
   }
}

//------------------------------------------------------------------------------
// ~GmatState()
//------------------------------------------------------------------------------
/**
 * Destructor for the GmatState.
 */
//------------------------------------------------------------------------------
GmatState::~GmatState()
{
   if (theData != NULL)
      delete [] theData;
}

//------------------------------------------------------------------------------
// GmatState(const GmatState& gs)
//------------------------------------------------------------------------------
/**
 * Copy constructor for the GmatState.
 * 
 * @param gs The state that is copied here
 */
//------------------------------------------------------------------------------
GmatState::GmatState(const GmatState& gs) :
   theEpoch       (gs.theEpoch),
   stateSize      (gs.stateSize)
{
   if (stateSize == 0)
      theData = NULL;
   else
   {
      theData = new Real[stateSize];
      for (Integer i = 0; i < stateSize; ++i)
         theData[i] = gs.theData[i];
   }
}

//------------------------------------------------------------------------------
// GmatState& operator=(const GmatState& gs)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the GmatState.
 * 
 * @param gs The state that is copied here
 */
//------------------------------------------------------------------------------
GmatState& GmatState::operator=(const GmatState& gs)
{
   if (this != &gs)
   {
      theEpoch  = gs.theEpoch;
      stateSize = gs.stateSize;
      
      if (theData != NULL)
         delete [] theData;

      if (stateSize == 0)
         theData = NULL;
      else
      {
         theData = new Real[stateSize];
         for (Integer i = 0; i < stateSize; ++i)
            theData[i] = gs.theData[i];
      }
   }

   return *this;
}


//------------------------------------------------------------------------------
//       OPERATOR OVERLOADS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Real& operator[](const Integer index)
//------------------------------------------------------------------------------
/**
 * Array element accessor used to set element values
 * 
 * @param index The index to the desired element
 */
//------------------------------------------------------------------------------
Real& GmatState::operator[](const Integer index)
{
   if ((index < 0 ) || (index >= stateSize))
      throw GmatBaseException("GmatState array index out of bounds");
   
   return theData[index];
}


//------------------------------------------------------------------------------
// Real operator[](const Integer index) const
//------------------------------------------------------------------------------
/**
 * Array element accessor used to retrieve element values
 * 
 * @param index The index to the desired element
 */
//------------------------------------------------------------------------------
Real GmatState::operator[](const Integer index) const
{
   if ((index < 0 ) || (index >= stateSize))
      throw GmatBaseException("GmatState array index out of bounds");
   
   return theData[index];
}


//------------------------------------------------------------------------------
//       PUBLIC METHODS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// void SetSize(const Integer size)
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
void GmatState::SetSize(const Integer size)
{
   if (size > 0)
      Resize(size);
   else
      throw GmatBaseException("State resizing to a value less than or equal "
            "to zero is not allowed");
}


//------------------------------------------------------------------------------
// Integer GetSize() const
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
Integer GmatState::GetSize() const
{
   return stateSize;
}


//------------------------------------------------------------------------------
// Real* GetState()
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
Real* GmatState::GetState()
{
   return theData;
}


//------------------------------------------------------------------------------
// bool SetState(const Real *data, const Integer size, const Integer start)

//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
bool GmatState::SetState(const Real *data, const Integer size, 
                           const Integer start)
{
   if (start < 0)
      throw GmatBaseException(
            "Cannot set state data -- starting index is out of range");
   if (start + size > stateSize)
      throw GmatBaseException(
            "Cannot set state data -- data span is out of range");
   
   memcpy(&(theData[start]), data, size*sizeof(Real));
   
   return true;
}


//------------------------------------------------------------------------------
// GmatEpoch GetEpoch() const
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
GmatEpoch GmatState::GetEpoch() const
{
   return theEpoch;
}


//------------------------------------------------------------------------------
// GmatEpoch SetEpoch(const GmatEpoch ep)
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
GmatEpoch GmatState::SetEpoch(const GmatEpoch ep)
{
   theEpoch = ep;
   return theEpoch;
}


//------------------------------------------------------------------------------
// void Resize(Integer newSize, bool withCopy)
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
void GmatState::Resize(Integer newSize, bool withCopy)
{
   if (newSize == stateSize)
   {
      // If size does not change, do not zero reagardless of the withCopy 
      //setting and just return
      return;
   }
   
   if (newSize <= 0)
      throw GmatBaseException("GmatState Resize requested an invalid size");
   
   Real *newData = new Real[newSize];
   Integer start = 0;
   
   if (withCopy)
   {
      // copy as much of the current state as possible into the new state 
      Integer size = (newSize > stateSize ? stateSize : newSize);
      memcpy(newData, theData, size*sizeof(Real));
      start = size;
   }
   
   stateSize = newSize;
   delete [] theData;
   theData = newData;

   // Zero the unset entries in the state data
   Zero(start, newSize - start);
}


//------------------------------------------------------------------------------
//       PROTECTED METHODS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// void Zero(Integer begin, UnsignedInt length)
//------------------------------------------------------------------------------
/**
 */
//------------------------------------------------------------------------------
void GmatState::Zero(Integer begin, UnsignedInt length)
{
   if ((begin < 0) || ((Integer)(begin + length) > stateSize))
   {
      std::strstream errmsg;
      errmsg << "GmatState request to zero " << length
             << " elements starting at element " << begin 
             << " exceeds the state size, which is " << stateSize;
      throw GmatBaseException(errmsg.str());
   }
   
   for (Integer i = begin; i < (Integer)(begin + length); ++i)
      theData[i] = 0.0;
}
