#ifndef StateVectorIds_hpp
#define StateVectorIds_hpp

namespace Gmat
{

   enum StateElementId {
      CARTESIAN_STATE = 3700,          // Integrable state representations
      EQUINOCTIAL_STATE,
      STATE_TRANSITION_MATRIX,         // STM
      MASS_FLOW,                       // m dot
      PREDEFINED_STATE_MAX,
      USER_DEFINED_BEGIN = 3800,
      USER_DEFINED_END = 3999          // Allow up to 200 dynamic entries
   };

};

#endif /*STATEVECTORIDS_HPP_*/
