//$Id: $
//------------------------------------------------------------------------------
//                         BodyFixedStateConverter
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy Shoan, GSFC/GSSB
// Created: 2010.08.24
//
/**
 * Definition of the namespace containing methods to convert between
 * celestial-body-fixed (CBF) state representations.
 *
 * @Note  Cartesian states are (x,y,z)
 *        Spherical and Spherical-Ellipsoid states are (latitude, longitude, height)
 */
//------------------------------------------------------------------------------


#ifndef BodyFixedStateConverter_hpp
#define BodyFixedStateConverter_hpp

#include "CelestialBody.hpp"

class GMAT_API InvalidStateRepresentationException : public BaseException
{
   public:
   InvalidStateRepresentationException(const std::string &message =
      "BodyFixedStateConverter: Conversion to invalid state representation requested: ")
      : BaseException(message) {};
};

namespace BodyFixedStateConverterUtil
{
   static const Integer     NUM_STATE_REPRESENTATIONS = 3;
   static const std::string BODY_FIXED_STATE_REPRESENTATION_TEXT[NUM_STATE_REPRESENTATIONS] =
   {
      "Cartesian",
      "Spherical",
      "Ellipsoid"
      // future representations here
   };

   Rvector3 Convert(const Rvector3    &origValue, const std::string &fromType, const std::string &toType,
                    const Real        flattening, const Real        meanRadius);

   Rvector3 Convert(const Rvector3    &origValue,
                    const std::string &fromType,  const std::string &fromHorizon,
                    const std::string &toType,    const std::string &toHorizon,
                    const Real        flattening, const Real        meanRadius);

   Rvector3 CartesianToSpherical(const Rvector3 &cart, const Real flattening, const Real meanRadius);

   Rvector3 SphericalToCartesian(const Rvector3 &spherical, const Real flattening, const Real meanRadius);

   Rvector3 SphericalEllipsoidToCartesian(const Rvector3 &sphEll, const Real flattening, const Real meanRadius);

   Rvector3 CartesianToSphericalEllipsoid(const Rvector3 &cart, const Real flattening, const Real meanRadius);

   Rvector3 SphericalToSphericalEllipsoid(const Rvector3 &spherical, const Real flattening, const Real meanRadius);

   Rvector3 SphericalEllipsoidToSpherical(const Rvector3 &sphEll, const Real flattening, const Real meanRadius);

   bool        IsValidateStateRepresentation(const std::string &rep);
   StringArray GetValidRepresentations();

}

#endif /* BodyFixedStateConverter_hpp */
