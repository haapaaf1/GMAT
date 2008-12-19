//$Id$
// *** File Name : gmatdefs.h
// *** Created   : May 20, 2003
// **************************************************************************
// ***  Developed By  :  Thinking Systems, Inc. (www.thinksysinc.com)     ***
// ***  For:  Flight Dynamics Analysis Branch (Code 595)                  ***
// ***  Under Contract:  P.O.  GSFC S-67573-G                             ***
// ***                                                                    ***
// ***  Header Version: July 12, 2002                                     ***
// **************************************************************************
// Module Type               : ANSI C++ Source
// Development Environment   : Visual C++ 7.0
// Modification History      : 5/20/2003 - D. Conway, Thinking Systems, Inc.
//                             Original delivery
//
// **Legal**
//
//                           : 2003/09/16 - W. Shoan/GSFC/583
//                             added ObjectTypes enum type in namespace Gmat
//                           : 11/9/2003 - D. Conway, Thinking Systems, Inc.
//                             Added OBJECT_TYPE for member objects
// **************************************************************************

#ifndef GMATDEFS_HPP
#define GMATDEFS_HPP


#include <string>               // For std::string
#include <vector>               // For std::vector
#include <map>                  // For std::map


#ifdef _WIN32  // Windows

   #ifdef _MSC_VER  // Microsoft Visual C++

      #define WIN32_LEAN_AND_MEAN  // Exclude rarely-used stuff from Windows headers
      // Windows Header File entry point:
      #include <windows.h>

      #define  _USE_MATH_DEFINES  // makes Msoft define pi

   #endif  // End Microsoft C++ specific block

   #ifdef _DYNAMICLINK  // Only used for Windows DLLs
      #ifdef GMAT_EXPORTS
         #define GMAT_API __declspec(dllexport)
      #else
         #define GMAT_API __declspec(dllimport)
      #endif
   #endif

#endif //  End of OS nits

#ifndef GMAT_API
   #define GMAT_API
#endif


typedef double          Real;              // 8 byte float
typedef int             Integer;           // 4 byte signed integer
typedef unsigned char   Byte;              // 1 byte
typedef unsigned int    UnsignedInt;       // 4 byte unsigned integer

typedef std::vector<Real>        RealArray;
typedef std::vector<Integer>     IntegerArray;
typedef std::vector<UnsignedInt> UnsignedIntArray;
typedef std::vector<std::string> StringArray;

class GmatBase;                            // Forward reference for ObjectArray
class ElementWrapper;
typedef std::vector<GmatBase*> ObjectArray;
typedef std::map<std::string, GmatBase*> ObjectMap;
typedef std::map<std::string, ElementWrapper*> WrapperMap;

typedef struct geoparms
{
   Real xtemp;  /// minimum global exospheric temperature (degrees K)
   Real tkp;    /// geomagnetic index

} GEOPARMS;


namespace Gmat
{
   /**
    * The list of object types
    *
    * This list needs to be synchronized with the GmatBase::OBJECT_TYPE_STRING
    * list found in base/Foundation/GmatBase.cpp
    */
   enum ObjectType
   {
      SPACECRAFT= 1001,
      FORMATION,
      SPACEOBJECT,
      GROUND_STATION,
      BURN,
      IMPULSIVE_BURN,
      FINITE_BURN,
      COMMAND,
      PROPAGATOR,
      FORCE_MODEL,
      PHYSICAL_MODEL,
      TRANSIENT_FORCE,
      INTERPOLATOR,
      SOLAR_SYSTEM,
      SPACE_POINT,
      CELESTIAL_BODY,
      CALCULATED_POINT,
      LIBRATION_POINT,
      BARYCENTER,
      ATMOSPHERE,
      PARAMETER,
      STOP_CONDITION,
      SOLVER,
      SUBSCRIBER,
      PROP_SETUP,
      FUNCTION,
      FUEL_TANK,
      THRUSTER,
      HARDWARE,            // Tanks, Thrusters, Antennae, Sensors, etc.
      COORDINATE_SYSTEM,
      AXIS_SYSTEM,
      ATTITUDE,
      MATH_NODE,
      MATH_TREE,
      ESTIMATOR,
      MEASUREMENT_MODEL,
      BODY_FIXED_POINT,
      DATA_FILE,
      UNKNOWN_OBJECT
   };


   /**
    * The list of data types
    *
    * This list needs to be synchronized with the GmatBase::PARAM_TYPE_STRING
    * list found in base/Foundation/GmatBase.cpp
    */
   enum ParameterType
   {
      INTEGER_TYPE,
      UNSIGNED_INT_TYPE,
      UNSIGNED_INTARRAY_TYPE,
      REAL_TYPE,
      REAL_ELEMENT_TYPE,
      STRING_TYPE,
      STRINGARRAY_TYPE,
      BOOLEAN_TYPE,
      RVECTOR_TYPE,
      RMATRIX_TYPE,
      TIME_TYPE,
      OBJECT_TYPE,
      OBJECTARRAY_TYPE,
      ON_OFF_TYPE,
      ENUMERATION_TYPE,
      TypeCount,
      UNKNOWN_PARAMETER_TYPE = -1
   };

   enum WrapperDataType
   {
      NUMBER,
      STRING,          // a raw text string
      STRING_OBJECT,   // name of a String Object
      OBJECT_PROPERTY,
      VARIABLE,
      ARRAY,
      ARRAY_ELEMENT,
      PARAMETER_OBJECT,
      OBJECT,
      BOOLEAN,
      INTEGER,
      ON_OFF,
      UNKNOWN_WRAPPER_TYPE = -2
   };


   enum RunState
   {
      IDLE = 10000,
      RUNNING,
      PAUSED,
      TARGETING,
      OPTIMIZING,
      SOLVING,
      SOLVEDPASS,
      WAITING
   };

   enum WriteMode
   {
      SCRIPTING,
      SHOW_SCRIPT,
      OWNED_OBJECT,
      MATLAB_STRUCT,
      EPHEM_HEADER,
      NO_COMMENTS,
   };

   /**
    * The list of SI Prefixes
    *
    * This list needs to be synchronized with the GmatBase::SI_PREFIXES_STRING,
    * GmatBase::SI_PREFIXES_SYMBOL, and GmatBase::SI_PREFIXES_FACTOR
    * lists found in base/Foundation/GmatBase.cpp
    */
   enum SIPrefixes
   {
      YOTTA,
      ZETTA,
      EXA,
      PETA,
      TERA,
      GIGA,
      MEGA,
      KILO,
      HECTO,
      DEKA,
      DECI,
      CENTI,
      MILLI,
      MICRO,
      NANO,
      PICO,
      FEMTO,
      ATTO,
      ZEPTO,
      YOCTO,
      UNKNOWN_SI_PREFIX
   };

   /**
    * The list of angle units
    *
    * This list needs to be synchronized with the GmatBase::ANGLE_UNITS_STRING
    * and GmatBase::ANGLE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum AngleUnits
   {
      RADIANS,
      STERADIANS,
      DEGREES,
      UNKNOWN_ANGLE_UNIT
   };

   /**
    * The list of area units
    *
    * This list needs to be synchronized with the GmatBase::AREA_UNITS_STRING
    * and the GmatBase::AREA_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum AreaUnits
   {
      SQUARE_INCH,
      SQUARE_FOOT,
      SQUARE_YARD,
      SQUARE_ROD,
      SQUARE_MILE,
      ACRE,
      SQUARE_METER,
      ARE,
      HECTARE,
      UNKNOWN_AREA_UNIT
   };

   /**
    * The list of distance units
    *
    * This list needs to be synchronized with the GmatBase::DISTANCE_UNITS_STRING
    * and the GmatBase::DISTANCE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum DistanceUnits
   {
      METER,
      LEAGUE,
      MILE,
      NAUTICAL_MILE,
      FURLONG,
      ROD,
      FATHOM,
      YARD,
      FOOT,
      INCH,
      ASTRONOMICAL_UNIT,
      LIGHT_YEAR,
      PARSEC,
      ANGSTROM,
      UNKNOWN_DISTANCE_UNIT
   };

   /**
    * The list of electric current units
    *
    * This list needs to be synchronized with the GmatBase::ELECTRIC_UNITS_STRING
    * and GmatBase::ELECTRIC_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum ElectricUnits
   {
      ELECTRIC_CURRENT,
      ELECTRIC_CHARGE,
      SI_ELECTRIC_CHARGE,
      ELECTRIC_CAPACITANCE,
      ELECTRIC_CONDUCTANCE,
      ELECTRIC_INDUCTANCE,
      ELECTRIC_POTENTIAL_DIFFERENCE,
      ELECTRIC_RESISTANCE,
      UNKNOWN_ELECTRIC_UNIT
   };

   /**
    * The list of energy units
    *
    * This list needs to be synchronized with the GmatBase::ENERGY_UNITS_STRING
    * and the GmatBase::ENERGY_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum EnergyUnits
   {
      WAVENUMBER,
      JOULES,
      JOULES_PER_MOLE,
      CALORIES,
      CALORIES_PER_MOLE,
      NANOMETER,
      HERTZ,
      ELECTRON_VOLT,
      UNKNOWN_ENERGY_UNIT
   };

   /**
    * The list of force units
    *
    * This list needs to be synchronized with the GmatBase::FORCE_UNITS_STRING
    * and the GmatBase::FORCE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum ForceUnits
   {
      DYNE,
      KILOGRAM_FORCE,
      KIP,
      NEWTON,
      PLANCK_FORCE,
      POUND_FORCE,
      POUNDAL,
      STHENE,
      UNKNOWN_FORCE_UNIT
   };

   /**
    * The list of luminance units
    *
    * This list needs to be synchronized with the GmatBase::LUMINANCE_UNITS_STRING
    * and GmatBase::LUMINANCE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    *
    * NOTE: 1 nit = 1 candela per square meter
    */
   enum LuminanceUnits
   {
      LAMBERT,
      FOOT_LAMBERT,
      CANDELA_PER_SQUARE_METER,
      NIT,
      UNKNOWN_LUMINANCE_UNIT
   };

   enum IlluminanceUnits
   {
       FOOT_CANDLE,
       LUX,
       PHOT,
       UNKNOWN_ILLUMINANCE_UNIT
   };

   enum LuminousFlux
   {
       LUMEN,
       RAYLEIGH,
       UNKNOWN_LUMINOUSFLUX_UNIT
   };

   enum LuminousEnergy
   {
       TALBOT,
       UNKNOWN_LUMINOUSENERGY_UNIT
   };

   enum LuminousIntensity
   {
       CANDELA,
       CANDELPOWER,
       UNKNOWN_LUMINOUSINTENSITY_UNIT
   };

   /**
    * The list of radiance units
    *
    * This list needs to be synchronized with the GmatBase::RADIANCE_UNITS_STRING
    * and GmatBase::RADIANCE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum RadianceUnits
   {
      RADIANT_ENERGY,
      RADIANT_FLUX,
      RADIANT_INTENSITY,
      RADIANCE,
      IRRADIANCE,
      RADIANT_EXITANCE,
      RADIOSITY,
      SPECTRAL_RADIANCE,
      SPECTRAL_IRRADIANCE,
      UNKNOWN_RADIANCE_UNIT
   };



   /**
    * The list of mass units
    *
    * This list needs to be synchronized with the GmatBase::MASS_UNITS_STRING
    * and GmatBase::MASS_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum MassUnits
   {
      METRIC_TON,
      GRAM,
      CARAT,
      LONGTON,
      SHORTON,
      POUND,
      OUNCE,
      DRAM,
      GRAIN,
      UNKNOWN_MASS_UNIT
   };

   /**
    * The list of magnetism units
    *
    * This list needs to be synchronized with the GmatBase::MAGNETISM_UNITS_STRING
    * and GmatBase::MAGNETISM_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum MagnetismUnits
   {
      GAUSS,
      TESLA,
      OERSTED,
      UNKNOWN_MAGNETISM_UNIT
   };


   /**
    * The list of pressure units
    *
    * This list needs to be synchronized with the GmatBase::PRESSURE_UNITS_STRING
    * and GmatBase::PRESSURE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum PressureUnits
   {
      POUNDS_PER_SQUARE_INCH,
      ATMOSPHERIC_PRESSURE,
      TORR,
      BAR,
      PASCAL,
      UNKNOWN_PRESSURE_UNIT
   };

   /**
    * The list of temperature units
    *
    * This list needs to be synchronized with the GmatBase::TEMPERATURE_UNITS_STRING
    * and GmatBase::TEMPERATURE_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum TemperatureUnits
   {
      FAHRENHEIT,
      CELSIUS,
      KELVIN,
      RANKINE,
      UNKNOWN_TEMPERATURE_UNIT
   };

   /**
    * The list of volume units
    *
    * This list needs to be synchronized with the GmatBase::VOLUME_UNITS_STRING
    * and GmatBase::VOLUME_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum VolumeUnits
   {
      FLUID_OUNCE,
      PINT,
      QUART,
      GALLON,
      CUBIC_INCHES,
      CUBIC_FEET,
      CUBIC_YARD,
      LITER,
      CUBIC_METER,
      UNKNOWN_VOLUME_UNIT
   };

   /**
    * The list of time units
    *
    * This list needs to be synchronized with the GmatBase::TIME_UNITS_STRING
    * and GmatBase::TIME_UNITS_SYMBOL list found in base/Foundation/GmatBase.cpp
    */
   enum TimeUnits
   {
      JULIAN_DATE,
      MODIFIED_JULIAN_DATE,
      GODDARD_MJD,
      SOLAR_TIME,
      SIDEREAL_TIME,
      UNIVERSAL_TIME,
      INTERNATIONAL_ATOMIC_TIME,
      APPARENT_SOLAR_TIME,
      MEAN_SOLAR_TIME,
      UT0,
      UT1,
      UT2,
      UTC,
      LOCAL_TIME,
      GREENWICH_MEAN_SIDEREAL_TIME,
      LOCAL_SIDEREAL_TIME,
      MEAN_SIDEREAL_TIME,
      APPARENT_SIDEREAL_TIME,
      TERRESTRIAL_TIME,
      BARYCENTRIC_DYNAMICAL_TIME,
      GEOCENTRIC_COORDINATE_TIME,
      BARYCENTRIC_COORDINATE_TIME,
      GPS_TIME,
      YEAR,
      MONTH,
      DAY,
      DAY_OF_YEAR,
      HOUR,
      MINUTE,
      SECOND,
      UNKNOWN_TIME_UNIT
   };

   enum USTimeZones
   {
       US_ATLANTIC_STANDARD,
       US_EASTERN_STANDARD,
       US_CENTRAL_STANDARD,
       US_MOUNTAIN_STANDARD,
       US_PACIFIC_STANDARD,
       US_ALASKAN_STANDARD,
       US_HAWAII_ALEUTIAN_STANDARD,
       US_ATLANTIC_DAYLIGHT_SAVINGS,
       US_EASTERN_DAYLIGHT_SAVINGS,
       US_CENTRAL_DAYLIGHT_SAVINGS,
       US_MOUNTAIN_DAYLIGHT_SAVINGS,
       US_PACIFIC_DAYLIGHT_SAVINGS,
       US_ALASKAN_DAYLIGHT_SAVINGS,
       EndUSTimeZones
   };

   enum CanadaTimeZones
   {
       CA_NEWFOUNDLAND_STANDARD,
       CA_ATLANTIC_STANDARD,
       CA_EASTERN_STANDARD,
       CA_CENTRAL_STANDARD,
       CA_MOUNTAIN_STANDARD,
       CA_PACIFIC_STANDARD,
       CA_YUKON_STANDARD,
       CA_NEWFOUNDLAND_DAYLIGHT_SAVINGS,
       CA_ATLANTIC_DAYLIGHT_SAVINGS,
       CA_EASTERN_DAYLIGHT_SAVINGS,
       CA_CENTRAL_DAYLIGHT_SAVINGS,
       CA_MOUNTAIN_DAYLIGHT_SAVINGS,
       CA_PACIFIC_DAYLIGHT_SAVINGS,
       EndCanadaTimeZones
   };

   enum MilitaryTimeZones
   {
       MTZ_ZULU,
       MTZ_ALPHA,
       MTZ_BRAVO,
       MTZ_CHARLIE,
       MTZ_DELTA,
       MTZ_ECHO,
       MTZ_FOXTROT,
       MTZ_GOLF,
       MTZ_HOTEL,
       MTZ_INDIA,
       MTZ_KILO,
       MTZ_LIMA,
       MTZ_MIKE,
       MTZ_NOVEMBER,
       MTZ_OSCAR,
       MTZ_PAPA,
       MTZ_QUEBEC,
       MTZ_ROMEO,
       MTZ_SIERRA,
       MTZ_TANGO,
       MTZ_UNIFORM,
       MTZ_VICTOR,
       MTZ_WHISKY,
       MTZ_XRAY,
       MTZ_YANKEE,
       EndMilitaryTimeZones
   };
   
}

typedef std::vector<Gmat::ObjectType> ObjectTypeArray;
typedef std::vector<Gmat::WrapperDataType> WrapperTypeArray;

#endif //GMATDEFS_HPP
