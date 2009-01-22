//$Id: GmatUnits.hpp 6169 2008-12-19 03:33:43Z aero9600 $
//------------------------------------------------------------------------------
//                                  GmatUnits
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/12/15
//
/**
 * Definition of units for all GMAT parameters
 */
//------------------------------------------------------------------------------


#ifndef _GMATUNITS_HPP
#define	_GMATUNITS_HPP

#include "GmatBase.hpp"

class GMAT_API GmatUnits : public GmatBase
{

public:
    
   /**
    * The list of SI Prefixes
    *
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
    */
   enum ElectricUnits
   {
      MY_ELECTRIC_CURRENT,
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

   enum SIBaseUnits
   {
       LENGTH,
       MASS,
       TIME,
       ELECTRIC_CURRENT,
       TEMPERATURE,
       AMOUNT_OF_SUBSTANCE,
       LUMINOUS_INTENSITY,
       UNKNOWN_BASE_UNIT
   };

    /// String Mappings for units

    //---------------------------------
    //  static data
    //---------------------------------

    static const std::string SI_BASE_UNITS_STRING[UNKNOWN_BASE_UNIT+1];
    static const std::string SI_BASE_UNITS_SYMBOL[UNKNOWN_BASE_UNIT+1];
    static const Real SI_PREFIXES_FACTOR[UNKNOWN_SI_PREFIX+1];
    static const std::string SI_PREFIXES_SYMBOL[UNKNOWN_SI_PREFIX+1];
    static const std::string SI_PREFIXES_STRING[UNKNOWN_SI_PREFIX+1];
    static const std::string ANGLE_UNITS_SYMBOL[UNKNOWN_ANGLE_UNIT+1];
    static const std::string ANGLE_UNITS_STRING[UNKNOWN_ANGLE_UNIT+1];
    static const std::string AREA_UNITS_SYMBOL[UNKNOWN_AREA_UNIT+1];
    static const std::string AREA_UNITS_STRING[UNKNOWN_AREA_UNIT+1];
    static const std::string DISTANCE_UNITS_SYMBOL[UNKNOWN_DISTANCE_UNIT+1];
    static const std::string DISTANCE_UNITS_STRING[UNKNOWN_DISTANCE_UNIT+1];
    static const std::string ELECTRIC_UNITS_SYMBOL[UNKNOWN_ELECTRIC_UNIT+1];
    static const std::string ELECTRIC_UNITS_STRING[UNKNOWN_ELECTRIC_UNIT+1];
    static const std::string ENERGY_UNITS_SYMBOL[UNKNOWN_ENERGY_UNIT+1];
    static const std::string ENERGY_UNITS_STRING[UNKNOWN_ENERGY_UNIT+1];
    static const std::string FORCE_UNITS_SYMBOL[UNKNOWN_FORCE_UNIT+1];
    static const std::string FORCE_UNITS_STRING[UNKNOWN_FORCE_UNIT+1];
    static const std::string LUMINANCE_UNITS_SYMBOL[UNKNOWN_LUMINANCE_UNIT+1];
    static const std::string LUMINANCE_UNITS_STRING[UNKNOWN_LUMINANCE_UNIT+1];
    static const std::string ILLUMINANCE_UNITS_SYMBOL[UNKNOWN_ILLUMINANCE_UNIT+1];
    static const std::string ILLUMINANCE_UNITS_STRING[UNKNOWN_ILLUMINANCE_UNIT+1];
    static const std::string LUMINOUS_FLUX_UNITS_SYMBOL[UNKNOWN_LUMINOUSFLUX_UNIT+1];
    static const std::string LUMINOUS_FLUX_UNITS_STRING[UNKNOWN_LUMINOUSFLUX_UNIT+1];
    static const std::string LUMINOUS_ENERGY_UNITS_SYMBOL[UNKNOWN_LUMINOUSENERGY_UNIT+1];
    static const std::string LUMINOUS_ENERGY_UNITS_STRING[UNKNOWN_LUMINOUSENERGY_UNIT+1];
    static const std::string LUMINOUS_INTENSITY_UNITS_SYMBOL[UNKNOWN_LUMINOUSINTENSITY_UNIT+1];
    static const std::string LUMINOUS_INTENSITY_UNITS_STRING[UNKNOWN_LUMINOUSINTENSITY_UNIT+1];
    static const std::string MASS_UNITS_SYMBOL[UNKNOWN_MASS_UNIT+1];
    static const std::string MASS_UNITS_STRING[UNKNOWN_MASS_UNIT+1];
    static const std::string PRESSURE_UNITS_SYMBOL[UNKNOWN_PRESSURE_UNIT+1];
    static const std::string PRESSURE_UNITS_STRING[UNKNOWN_PRESSURE_UNIT+1];
    /*
    static const std::string RADIANCE_UNITS_SYMBOL[UNKNOWN_RADIANCE_UNIT+1];
    static const std::string RADIANCE_UNITS_STRING[UNKNOWN_RADIANCE_UNIT+1];
    */
    static const std::string TEMPERATURE_UNITS_SYMBOL[UNKNOWN_TEMPERATURE_UNIT+1];
    static const std::string TEMPERATURE_UNITS_STRING[UNKNOWN_TEMPERATURE_UNIT+1];
    static const std::string VOLUME_UNITS_SYMBOL[UNKNOWN_VOLUME_UNIT+1];
    static const std::string VOLUME_UNITS_STRING[UNKNOWN_VOLUME_UNIT+1];
    static const std::string TIME_UNITS_SYMBOL[UNKNOWN_TIME_UNIT+1];
    static const std::string TIME_UNITS_STRING[UNKNOWN_TIME_UNIT+1];
    static const std::string MILITARY_TIME_ZONE_CODES[EndMilitaryTimeZones+1];
    static const Real MILITARY_TIME_UTC_OFFSETS[EndMilitaryTimeZones+1];
    static const std::string US_TIME_ZONE_CODES[EndUSTimeZones+1];
    static const Real US_TIME_UTC_OFFSETS[EndUSTimeZones+1];
    static const std::string CANADA_TIME_ZONE_CODES[EndCanadaTimeZones+1];
    static const Real CANADA_TIME_UTC_OFFSETS[EndCanadaTimeZones+1];

};



#endif	/* _GMATUNITS_HPP */

