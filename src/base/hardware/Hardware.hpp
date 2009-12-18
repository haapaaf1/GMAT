//$Header$
//------------------------------------------------------------------------------
//                               Hardware
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2004/11/08
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under MOMS Task
// Order 124.
//
/**
 * Class definition for the Hardware base class.
 */
//------------------------------------------------------------------------------


#ifndef HARDWARE_HPP
#define HARDWARE_HPP

#include "GmatBase.hpp"
#include "HardwareException.hpp"
#include "CoordinateSystem.hpp"
#include "CelestialBody.hpp"
#include "Spacecraft.hpp"

class Spacecraft;

/**
 * Base class used for spacecraft hardware.
 * 
 * This class is the base class for spacecraft fuel tanks, thrusters, and other
 * hardware elements that can be added to a spacecraft in GMAT.  It contains
 * data structures that locate the center of the element in the spacecraft's 
 * body coordinate system (BCS) and that orient the elements in the same system.
 * 
 * @note The current builds of GMAT do not model torques or moments of inertia, 
 * so the parameter access for those pieces is commented out.
 */
class GMAT_API Hardware : public GmatBase 
{
public:

   static const Integer AXES_COUNT = 4;

   Hardware(Gmat::ObjectType typeId, const std::string &typeStr,
            const std::string &nomme = "");
   virtual ~Hardware();
   Hardware(const Hardware& hw);
   Hardware&               operator=(const Hardware& hw);
   
   // Parameter access methods - overridden from GmatBase

   virtual bool         IsParameterReadOnly(const Integer id) const;

   virtual std::string        GetParameterText(const Integer id) const;
   virtual Integer            GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                              GetParameterType(const Integer id) const;
   virtual std::string        GetParameterTypeString(const Integer id) const;
   
   
   virtual Real               GetRealParameter(const Integer id) const;
   virtual Real               SetRealParameter(const Integer id,
                                               const Real value);
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;

   // for enumerated strings
   virtual const StringArray&
                        GetPropertyEnumStrings(const Integer id) const;
   virtual const StringArray&
                        GetPropertyEnumStrings(const std::string &label) const;

   // Ref. object access methods - overridden from GmatBase
   virtual std::string  GetRefObjectName(const Gmat::ObjectType type) const;
   virtual const ObjectTypeArray&
                        GetRefObjectTypeArray();
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         SetRefObject(GmatBase *obj,
                                     const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   virtual ObjectArray& GetRefObjectArray(const std::string& typeString);

   virtual void         SetSolarSystem(SolarSystem *ss);

   virtual bool         Initialize();
   
protected:
   /// Location of center of the hardware element on the spacecraft, in meters.
   Real                    location[3];
   /// Principle direction for hardware element on the spacecraft.
   Real                    direction[3];
   /// Secondary direction, to complete the orientation.
   Real                    secondDirection[3];

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
   /// Name of the Spacecraft that has hardware
   std::string                satName;
   /// Principal direction projected into the inertial coordinate system
   Real                       inertialDirection[3];
   /// Available local axes labels
   static  StringArray        localAxesLabels;
   /// Flag indicating if local coordinate system is used
   bool                       usingLocalCoordSys;
   /// Flag indicating if axes is MJ2000Eq
   bool                       isMJ2000EqAxes;
   /// Flag indicating if axes is SpacecrftBody
   bool                       isSpacecraftBodyAxes;
   

   /// Enumeration defining user accessible parameters for Hardware elements.
   enum
   {
      DIRECTION_X = GmatBaseParamCount,
      DIRECTION_Y,
      DIRECTION_Z,
// These will be needed when detailed attitude is added to GMAT:
//      SECOND_DIRECTION_X,
//      SECOND_DIRECTION_Y,
//      SECOND_DIRECTION_Z,
//      BCS_X,
//      BCS_Y,
//      BCS_Z,
      COORDINATE_SYSTEM,
      ORIGIN,
      AXES,
      HardwareParamCount
   };
   
   /// Hardware Parameter labels
   static const std::string 
                        PARAMETER_TEXT[HardwareParamCount - GmatBaseParamCount];
   /// Hardware Parameter types
   static const Gmat::ParameterType 
                        PARAMETER_TYPE[HardwareParamCount - GmatBaseParamCount];

   bool                 SetSpacecraft(Spacecraft *sat);

   CoordinateSystem*    CreateLocalCoordinateSystem();
   void                 ConvertDirectionToInertial(Real *dv, Real *dvInertial,
                                                   Real epoch);
   void                 ComputeInertialDirection(Real epoch);
   void                 WriteDeprecatedMessage(const std::string &oldProp,
                                               const std::string &newProp) const;

};

#endif // HARDWARE_HPP
