//$Header$
//------------------------------------------------------------------------------
//                             Sensor
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/11/30
//
/**
 *
 * Implements Sensor base class to provide for modeling of sensors. This code
 * is based in part on the Thruster class in that it draws all the coordinate
 * system Get/Set methods from there.
 *
 */
//------------------------------------------------------------------------------

#ifndef _SENSOR_HPP
#define	_SENSOR_HPP


#include "Hardware.hpp"
#include "CoordinateSystem.hpp"
#include "CelestialBody.hpp"
#include "Spacecraft.hpp"

// Declare forward reference since Spacecraft owns Sensor
class Spacecraft;

class GMAT_API Sensor : public Hardware
{
public:

   static const Integer COEFFICIENT_COUNT = 16;
   static const Integer AXES_COUNT = 4;

   Sensor(std::string nomme);
   virtual ~Sensor();
   Sensor(const Sensor& th);
   Sensor& operator=(const Sensor& mySensor);

   // required method for all subclasses
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* inst);

   // Parameter access methods - overridden from GmatBase
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   virtual bool         IsParameterReadOnly(const Integer id) const;

   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);

   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;

   // Ref. object access methods - overridden from GmatBase
   virtual bool         SetRefObject(GmatBase *obj,
                                     const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   virtual ObjectArray& GetRefObjectArray(const std::string& typeString);


   virtual void         SetSolarSystem(SolarSystem *ss);
   virtual bool         Initialize();

protected:

   /// Solar system used to find the J2000 body, etc.
   SolarSystem                *solarSystem;
   /// Local Coordinate system
   CoordinateSystem           *localCoordSystem;
   /// Coordinate system
   CoordinateSystem           *coordSystem;
   /// Origin object pointer if coordinate system is set to Local
   CelestialBody              *localOrigin;
   /// J2000body pointer
   CelestialBody              *j2000Body;
   /// Secondary Spacecraft object if coordinate system is set to Local
   Spacecraft                 *spacecraft;
   /// Coordinate system name
   std::string                coordSystemName;
   /// Origin name if coordinate system is set to Local
   std::string                localOriginName;
   /// Axes name if coordinate system is set to Local
   std::string                localAxesName;
   /// Name of the J2000 body
   std::string                j2000BodyName;
   /// Name of the Spacecraft that has sensor
   std::string                satName;
   /// Sensor direction projected into the inertial coordinate system
   Real                       inertialDirection[3];
   /// Flag indicating if local coordinate system is used
   bool                       usingLocalCoordSys;
   /// Flag used to determine if the configuration needs updating
   bool                       initialized;

   /// Available local axes labels
   static  StringArray        localAxesLabels;

   /// Published parameters for sensors
   enum
   {
      THRUSTER_FIRING = HardwareParamCount,
      COORDINATE_SYSTEM,
      ORIGIN,
      AXES,
      DUTY_CYCLE,
      THRUST_SCALE_FACTOR,
      DECREMENT_MASS,
      TANK,
      GRAVITATIONAL_ACCELERATION,
      C1,    C2,    C3,    C4,    C5,    C6,    C7,    C8,
      C9,   C10,   C11,   C12,   C13,   C14,   C15,   C16,
      K1,    K2,    K3,    K4,    K5,    K6,    K7,    K8,
      K9,   K10,   K11,   K12,   K13,   K14,   K15,   K16,
      C_UNITS,
      K_UNITS,
      SensorParamCount
   };

   /// Sensor parameter labels
   static const std::string
                        PARAMETER_TEXT[SensorParamCount - HardwareParamCount];
   /// Sensor parameter types
   static const Gmat::ParameterType
                        PARAMETER_TYPE[SensorParamCount - HardwareParamCount];

   bool                 SetSpacecraft(Spacecraft *sat);

   CoordinateSystem*    CreateLocalCoordinateSystem();
   void                 ConvertDirectionToInertial(Real *dv, Real *dvInertial,
                                                   Real epoch);
   void                 ComputeInertialDirection(Real epoch);

};

#endif	/* _SENSOR_HPP */

