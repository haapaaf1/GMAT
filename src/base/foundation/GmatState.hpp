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
 * Definition of the GmatState class.  This is the class for state data used in 
 * GMAT's propagators and solvers.
 */
//------------------------------------------------------------------------------

#ifndef GmatState_hpp
#define GmatState_hpp

#include "gmatdefs.hpp"
#include "StateVectorIds.hpp"

/**
 * A GmatState is a vector on numbers and an associated epoch.  
 * 
 * This class defines the core data set used in GMAT's propagation and solver 
 * subsystems when state data at a defined epoch is needed.
 */
class GmatState
{
public:
	GmatState(Integer size = 0);
	virtual ~GmatState();
   GmatState(const GmatState& gs);
   GmatState& operator=(const GmatState& gs);
   
   // Operator overloads
   Real&             operator[](const Integer index);
   Real              operator[](const Integer index) const;

   // Sizing manipulation
   void              SetSize(const Integer size);
   
   // Access methods
   Integer           GetSize() const;
   Real*             GetState();
   bool              SetState(const Real *data, const Integer size, 
                              const Integer start = 0);
   GmatEpoch         GetEpoch() const;
   GmatEpoch         SetEpoch(const GmatEpoch ep);
   
   bool              SetElementProperties(const Integer index, const Integer id, 
                           const std::string &textId);
   const StringArray&
                     GetElementDescriptions();

protected:
   /// The epoch of the state data
   GmatEpoch         theEpoch;
   /// The state data
   Real              *theData;
   Integer           *dataIDs;
   StringArray       dataTypes;
   
   /// Length of the state vector
   Integer           stateSize;
   
   void              Resize(Integer newSize, bool withCopy = true);
   void              Zero(Integer begin = 0, UnsignedInt length = 0);
};

#endif /*GmatState_hpp*/
