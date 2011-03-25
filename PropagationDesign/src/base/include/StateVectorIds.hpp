//$Id$
//------------------------------------------------------------------------------
//                           PropagationStateManager
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
 * IDs and definitions used in the propagation enhancement prototype.
 * 
 * These entries should be merged into gmatdefs when the code is integrated into 
 * the development branch.
 */
//------------------------------------------------------------------------------

#ifndef StateVectorIds_hpp
#define StateVectorIds_hpp

#include <vector>

/// GMAT's epoch representation; eventually a struct holding MJ day & sec of day
typedef double GmatEpoch;
typedef std::vector<Integer> IntegerArray;

namespace Gmat
{

   enum StateElementId {
      UNKNOWN_STATE = -1,
      CARTESIAN_STATE = 3700,          // Integrable state representations
      EQUINOCTIAL_STATE,
      ORBIT_STATE_TRANSITION_MATRIX,   // 6x6 STM for the orbit
      MASS_FLOW,                       // m dot
      PREDEFINED_STATE_MAX,
      USER_DEFINED_BEGIN = 3800,
      USER_DEFINED_END = 3999          // Allow up to 200 dynamic entries
   };

};

#endif /*StateVectorIds_hpp*/
