//$Header$
//------------------------------------------------------------------------------
//                             Antenna
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
// Based in part upon elements of the Thruster class.
//
/**
 *
 * Implements the Antenna base class to provide for modeling of antenna
 * performance. An antenna is owned by a parent object and other hardware
 * on the parent object are assigned to use it.
 *
 */
//------------------------------------------------------------------------------

#ifndef _ANTENNA_HPP
#define	_ANTENNA_HPP

#include "Hardware.hpp"
#include "CoordinateSystem.hpp"
#include "CelestialBody.hpp"
#include "Spacecraft.hpp"
#include "GroundStation.hpp"

// Declare forward reference since Spacecraft ownes Antenna
class Spacecraft;
class GroundStation;

class GMAT_API Antenna : public Hardware
{
public:

    Antenna(std::string nomme);
    virtual ~Antenna();
    Antenna(const Antenna &ant);
    Antenna&            operator=(const Antenna &ant);

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

    virtual Real         GetRealParameter(const Integer id) const;
    virtual Real         SetRealParameter(const Integer id,
                                          const Real value);
    virtual std::string  GetStringParameter(const Integer id) const;
    virtual bool         SetStringParameter(const Integer id,
                                            const std::string &value);
    virtual bool         GetBooleanParameter(const Integer id) const;
    virtual bool         SetBooleanParameter(const Integer id,
                                             const bool value);

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
    /// Secondary Spacecraft object if coordinate system is set to Local
    GroundStation              *groundStation;
    /// Coordinate system name
    std::string                coordSystemName;
    /// Origin name if coordinate system is set to Local
    std::string                localOriginName;
    /// Axes name if coordinate system is set to Local
    std::string                localAxesName;
    /// Name of the J2000 body
    std::string                j2000BodyName;
    /// Name of the Spacecraft that has Antenna
    std::string                satName;
    /// Name of the GroundStation that has Antenna
    std::string                stationName;
    /// Flag used to turn Antenna on or off
    bool                       antennaActive;
    /// Pointing direction projected into the inertial coordinate system
    Real                       inertialDirection[3];
    /// Flag indicating if local coordinate system is used
    bool                       usingLocalCoordSys;
    /// Flag used to determine if the configuration needs updating
    bool                       initialized;

    /// Available local axes labels
    static  StringArray        localAxesLabels;

    // Antenna ID for identification purposes
    Integer                     antennaID;
    // Minimum and maximum range for this antenna
    Real                        maxRange;
    Real                        minRange;
    // Minimum and maximum range rate for this antenna
    Real                        maxRangeRate;
    Real                        minRangeRate;
    // The bore sight vector indication where the antenna is pointed in
    // the local coordinate frame
    Real                        boreSightVector[3];
    // Minimum and maximum co-elevation for this antenna
    Real                        maxCoElevation;
    Real                        minCoElevation;

   /// Published parameters for Antennas
   enum
   {
      ANTENNA_ACTIVE = HardwareParamCount,
      COORDINATE_SYSTEM,
      ORIGIN,
      AXES,
      ANTENNA_ID,
      MAX_RANGE,
      MIN_RANGE,
      MAX_RANGERATE,
      MIN_RANGERATE,
      BORESITEVECTOR_1,
      BORESITEVECTOR_2,
      BORESITEVECTOR_3,
      MIN_COELEVATION,
      MAX_COELEVATION,
      AntennaParamCount
   };

   /// Antenna parameter labels
   static const std::string
                        PARAMETER_TEXT[AntennaParamCount - HardwareParamCount];
   /// Antenna parameter types
   static const Gmat::ParameterType
                        PARAMETER_TYPE[AntennaParamCount - HardwareParamCount];

   bool                 SetSpacecraft(Spacecraft *sat);
   bool                 SetGroundStation(GroundStation *station);

   CoordinateSystem*    CreateLocalCoordinateSystem();
   void                 ConvertDirectionToInertial(Real *dv, Real *dvInertial,
                                                   Real epoch);
   void                 ComputeInertialDirection(Real epoch);

};

#endif	/* _ANTENNA_HPP */

