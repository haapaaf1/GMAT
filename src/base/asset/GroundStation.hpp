//$Id$
//------------------------------------------------------------------------------
//                            GroundStation
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/08/01
//
/**
 * Defines the Groundstation class used to model ground based tracking stations.
 */
//------------------------------------------------------------------------------

#ifndef GroundStation_hpp
#define GroundStation_hpp

#include "SpacePoint.hpp"
#include "LatLonHgt.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"


class GroundStation : public SpacePoint
{
public:
   GroundStation(const std::string &itsName);
   virtual ~GroundStation();
   GroundStation(const GroundStation& gs);
   GroundStation& operator=(const GroundStation& gs);

   // all classes derived from GmatBase must supply this Clone method
   virtual GmatBase*       Clone() const;

   // Parameter access methods - overridden from GmatBase 
   virtual std::string     GetParameterText(const Integer id) const;     
   virtual Integer         GetParameterID(const std::string &str) const; 
   virtual Gmat::ParameterType
                           GetParameterType(const Integer id) const;
   virtual std::string     GetParameterTypeString(const Integer id) const;

   virtual bool            IsParameterReadOnly(const Integer id) const;
   virtual bool            IsParameterReadOnly(const std::string &label) const;
   
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

   virtual std::string     GetRefObjectName(const Gmat::ObjectType type) const;
   virtual const StringArray&
                           GetRefObjectNameArray(const Gmat::ObjectType type);
   
   virtual const Rvector6  GetMJ2000State(const A1Mjd &atTime);
   virtual const Rvector3  GetMJ2000Position(const A1Mjd &atTime);
   virtual const Rvector3  GetMJ2000Velocity(const A1Mjd &atTime);

   virtual bool            Initialize();
   virtual void            SetSolarSystem(SolarSystem *ss);
   
protected:
   /// The ground station is attached to this body
   std::string       cBodyName;
   /// The ground station is attached to this body
   SpacePoint        *theBody;
   /// Strings that identify the location parameters
   StringArray       locationLabels;
   /// Type of coordinate system
   std::string       stateType;
   /// For geographic states, the horizon type
   std::string       horizon;
   /// The location, in the stateType-horizon system
   Real              location[3];
   /// The Cartesian body-fized location
   Real              bfLocation[3];
   /// A solar system pointer (not sure if this is needed)
   SolarSystem       *solarSystem;
   /// The body-fixed coordinate system that the GS lives on
   CoordinateSystem  *bfcs;
   /// A MJ2000 coordinate system
   CoordinateSystem  *mj2kcs;

   /// Converter helper
   CoordinateConverter ccvtr;

   
   /// Conversion code used to transform from lat-long-height to body fixed
   LatLonHgt      llh;
   
   Rvector3       j2000Pos;
   Rvector3       j2000Vel;
   Rvector6       j2000PosVel;
   
   /// Published parameters for burns
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
      GroundStationParamCount
   };
   
   /// burn parameter labels
   static const std::string 
      PARAMETER_TEXT[GroundStationParamCount - SpacePointParamCount];
   /// burn parameter types
   static const Gmat::ParameterType 
      PARAMETER_TYPE[GroundStationParamCount - SpacePointParamCount];

   // Override GetGenString to handle the changeable names for the parameters
   virtual const std::string&  
                        GetGeneratingString(
                           Gmat::WriteMode mode = Gmat::SCRIPTING,
                           const std::string &prefix = "",
                           const std::string &useName = "");
   virtual void         WriteParameters(Gmat::WriteMode mode, 
                           std::string &prefix, std::stringstream &stream);
};

#endif /*GroundStation_hpp*/
