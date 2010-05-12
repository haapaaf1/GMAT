//$Id$
//------------------------------------------------------------------------------
//                            BodyFixedPoint
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy C. Shoan, NASA/GSFC (moved from GroundStation code, 
//         original author: Darrel J. Conway, Thinking Systems, Inc.)
// Created: 2008.09.18
// Modified: 
//    2010.03.15 Thomas Grubb 
//      - Changed visiblity of PARAMETER_TEXT, PARAMETER_TYPE, and enum from
//        protected to public
//      - Added LOCATION_UNITS_x labels to enum
//      - Overrode Copy method
//
/**
 * Defines the BodyFixedPoint class used to model body-fixed space points.
 */
//------------------------------------------------------------------------------

#ifndef BodyFixedPoint_hpp
#define BodyFixedPoint_hpp

#include "SpacePoint.hpp"
#include "LatLonHgt.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"


class GMAT_API BodyFixedPoint : public SpacePoint
{
public:
   BodyFixedPoint(const std::string &itsType, const std::string &itsName);
   virtual ~BodyFixedPoint();
   BodyFixedPoint(const BodyFixedPoint& bfp);
   BodyFixedPoint& operator=(const BodyFixedPoint& bfp);

   // Parameter access methods - overridden from GmatBase 
   virtual void            Copy(const GmatBase* orig);
   virtual std::string     GetParameterText(const Integer id) const;     
   virtual Integer         GetParameterID(const std::string &str) const; 
   virtual Gmat::ParameterType
                           GetParameterType(const Integer id) const;
   virtual std::string     GetParameterTypeString(const Integer id) const;

   virtual bool            IsParameterReadOnly(const Integer id) const;
   virtual bool            IsParameterReadOnly(const std::string &label) const;
   
   virtual Gmat::ObjectType
                           GetPropertyObjectType(const Integer id) const;
   virtual const StringArray&
                           GetPropertyEnumStrings(const Integer id) const;
   
   virtual std::string     GetStringParameter(const Integer id) const;
   virtual bool            SetStringParameter(const Integer id, 
                                              const std::string &value);
   virtual std::string     GetStringParameter(const std::string &label) const;
   virtual bool            SetStringParameter(const std::string &label, 
                                              const std::string &value);
   
   virtual std::string     GetStringParameter(const Integer id, 
                                              const Integer index) const;
   virtual bool            SetStringParameter(const Integer id, 
                                              const std::string &value, 
                                              const Integer index);
   virtual std::string     GetStringParameter(const std::string &label, 
                                              const Integer index) const;
   virtual bool            SetStringParameter(const std::string &label, 
                                              const std::string &value, 
                                              const Integer index);

   virtual Real            GetRealParameter(const Integer id) const;
   virtual Real            SetRealParameter(const Integer id,
                                         const Real value);
//   virtual Real            GetRealParameter(const Integer id,
//                                         const Integer index) const;
//   virtual Real            GetRealParameter(const Integer id, const Integer row,
//                                         const Integer col) const;
//   virtual Real            SetRealParameter(const Integer id,
//                                         const Real value,
//                                         const Integer index);
//   virtual Real            SetRealParameter(const Integer id, const Real value,
//                                         const Integer row, const Integer col);
   virtual Real            GetRealParameter(const std::string &label) const;
   virtual Real            SetRealParameter(const std::string &label,
                                         const Real value);
//   virtual Real            GetRealParameter(const std::string &label,
//                                         const Integer index) const;
//   virtual Real            SetRealParameter(const std::string &label,
//                                         const Real value,
//                                         const Integer index);
//   virtual Real            GetRealParameter(const std::string &label, 
//                                         const Integer row, 
//                                         const Integer col) const;
//   virtual Real            SetRealParameter(const std::string &label,
//                                         const Real value, const Integer row,
//                                         const Integer col);

   virtual GmatBase*       GetRefObject(const Gmat::ObjectType type,
                                        const std::string &name);
   virtual bool            SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                        const std::string &name = "");
   virtual GmatBase*       GetRefObject(const Gmat::ObjectType type,
                                        const std::string &name, 
                                        const Integer index);
   virtual bool            SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                        const std::string &name, 
                                        const Integer index);

   virtual bool            HasRefObjectTypeArray();
   virtual std::string     GetRefObjectName(const Gmat::ObjectType type) const;
   virtual const StringArray&
                           GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual const ObjectTypeArray&
                           GetRefObjectTypeArray();
   
   virtual const Rvector6  GetMJ2000State(const A1Mjd &atTime);
   virtual const Rvector3  GetMJ2000Position(const A1Mjd &atTime);
   virtual const Rvector3  GetMJ2000Velocity(const A1Mjd &atTime);
   
   virtual const Rvector3  GetBodyFixedLocation(const A1Mjd &atTime) const;
   virtual CoordinateSystem* 
                           GetBodyFixedCoordinateSystem() const;

   virtual void            SetSolarSystem(SolarSystem *ss);
   
protected:
   /// The point is attached to this body
   std::string       cBodyName;
   /// The point is attached to this body
   SpacePoint        *theBody;
   /// Strings that identify the location parameters
   StringArray       locationLabels;
   /// Strings that identify the location units
   StringArray       locationUnits;
   /// Type of coordinate system
   std::string       stateType;
   /// For geographic states, the horizon type
   std::string       horizon;
   /// The location, in the stateType-horizon system
   Real              location[3];
   /// The Cartesian body-fixed location
   Real              bfLocation[3];
   /// A solar system pointer (not sure if this is needed)
   SolarSystem       *solarSystem;
   /// name of the BodyFixed coordinate system
   std::string       bfcsName;
   /// The body-fixed coordinate system that the BFP lives on
   CoordinateSystem  *bfcs;
   /// name of the MJ2000 coordinate system
   std::string       mj2kcsName;
   /// A MJ2000 coordinate system
   CoordinateSystem  *mj2kcs;

   /// Converter helper
   CoordinateConverter ccvtr;

   
   /// Conversion code used to transform from lat-long-height to body fixed
   LatLonHgt      llh;
   
   Rvector3       j2000Pos;
   Rvector3       j2000Vel;
   Rvector6       j2000PosVel;
   
public:
   /// Published parameters for body-fixed points
   enum
   {
      CENTRAL_BODY = SpacePointParamCount,
      STATE_TYPE,          // Cartesian or Geographical
      HORIZON_REFERENCE,   // Sphere or Ellipsoid
      LOCATION_1,          // X or Latitude
      LOCATION_2,          // Y or Longitude
      LOCATION_3,          // Z or Height
      LOCATION_LABEL_1,
      LOCATION_LABEL_2,
      LOCATION_LABEL_3,
      LOCATION_UNITS_1,
      LOCATION_UNITS_2,
      LOCATION_UNITS_3,
      BodyFixedPointParamCount
   };
   
   /// burn parameter labels
   static const std::string 
      PARAMETER_TEXT[BodyFixedPointParamCount - SpacePointParamCount];
   /// burn parameter types
   static const Gmat::ParameterType 
      PARAMETER_TYPE[BodyFixedPointParamCount - SpacePointParamCount];

};

#endif /*BodyFixedPoint_hpp*/
