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


//------------------------------------------------------------------------------
// GmatState()
//------------------------------------------------------------------------------
/**
 * Default constructor for the GmatState.
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
      for (Integer i = 0; i < stateSize; ++i)
         theData[i] = 0.0;
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
