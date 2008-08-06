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
/**
 * Defines the state used in propagation of ground station objects.
 */
//------------------------------------------------------------------------------


#ifndef GroundPropState_hpp
#define GroundPropState_hpp

#include "gmatdefs.hpp"

class GMAT_API GroundPropState
{
public:

   GroundPropState(const Integer dim = 6);
   virtual ~GroundPropState();
   GroundPropState(const GroundPropState& ps);
   GroundPropState&        operator=(const GroundPropState& ps);
 
   // Array manipulations
   Real&             operator[](const Integer el);
   Real              operator[](const Integer el) const;
      
   // Access methods
   Integer           GetSize() const;
   Real*             GetState();
   bool              SetState(const Real *data, const Integer size);
   Real              GetEpoch() const;
   Real              SetEpoch(const Real ep);
   
 
protected:
   /// Array used for the state data
   Real              *groundstate;
   /// Size of the state vector
   Integer           dimension;
   /// Raw epoch data for the state
   Real              epoch;
};

#endif // GroundPropState_hpp
