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

class GmatUnits
{

public:

    // The usual suspects
    GmatUnits(const std::string &myUnits);
    GmatUnits(const StringArray &numerator, const StringArray &denominator);
    ~GmatUnits();
    GmatUnits(const GmatUnits &units);
    const GmatUnits& operator=(const GmatUnits &units);

    
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
      NOPREFIX,
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
      AU_SQUARE_INCH,
      AU_SQUARE_FOOT,
      AU_SQUARE_YARD,
      AU_SQUARE_ROD,
      AU_SQUARE_MILE,
      AU_ACRE,
      AU_SQUARE_METER,
      AU_ARE,
      AU_HECTARE,
      UNKNOWN_AREA_UNIT
   };

   /**
    * The list of distance units
    *
    */
   enum DistanceUnits
   {
      DU_METER,
      DU_LEAGUE,
      DU_MILE,
      DU_NAUTICAL_MILE,
      DU_FURLONG,
      DU_ROD,
      DU_FATHOM,
      DU_YARD,
      DU_FOOT,
      DU_INCH,
      DU_ASTRONOMICAL_UNIT,
      DU_LIGHT_YEAR,
      DU_PARSEC,
      DU_ANGSTROM,
      UNKNOWN_DISTANCE_UNIT
   };

   /**
    * The list of electric current units
    *
    */
   enum ElectricUnits
   {
      EU_CURRENT,
      EU_CHARGE,
      EU_SI_CHARGE,
      EU_CAPACITANCE,
      EU_CONDUCTANCE,
      EU_INDUCTANCE,
      EU_POTENTIAL_DIFFERENCE,
      EU_RESISTANCE,
      UNKNOWN_ELECTRIC_UNIT
   };

   /**
    * The list of energy units
    *
    */
   enum EnergyUnits
   {
      ENU_WAVENUMBER,
      ENU_JOULES,
      ENU_JOULES_PER_MOLE,
      ENU_CALORIES,
      ENU_CALORIES_PER_MOLE,
      ENU_NANOMETER,
      ENU_HERTZ,
      ENU_ELECTRON_VOLT,
      UNKNOWN_ENERGY_UNIT
   };

   /**
    * The list of force units
    *
    */
   enum ForceUnits
   {
      FU_NEWTON,
      FU_DYNE,
      FU_KILOGRAM_FORCE,
      FU_POUND_FORCE,
      FU_POUNDAL,
      FU_KIP,
      FU_PLANCK_FORCE,
      FU_STHENE,
      UNKNOWN_FORCE_UNIT
   };

   /**
    * The list of luminance units
    *
    * NOTE: 1 nit = 1 candela per square meter
    */
   enum LuminanceUnits
   {
      LU_LAMBERT,
      LU_FOOT_LAMBERT,
      LU_CANDELA_PER_SQUARE_METER,
      LU_NIT,
      UNKNOWN_LUMINANCE_UNIT
   };

   enum IlluminanceUnits
   {
       IU_FOOT_CANDLE,
       IU_LUX,
       IU_PHOT,
       UNKNOWN_ILLUMINANCE_UNIT
   };

   enum LuminousFluxUnits
   {
       LFU_LUMEN,
       LFU_RAYLEIGH,
       UNKNOWN_LUMINOUSFLUX_UNIT
   };

   enum LuminousEnergyUnits
   {
       LEU_TALBOT,
       UNKNOWN_LUMINOUSENERGY_UNIT
   };

   enum LuminousIntensityUnits
   {
       LIU_CANDELA,
       LIU_CANDELPOWER,
       UNKNOWN_LUMINOUSINTENSITY_UNIT
   };

   /**
    * The list of radiance units
    *
    */
   enum RadianceUnits
   {
      RU_RADIANT_ENERGY,
      RU_RADIANT_FLUX,
      RU_RADIANT_INTENSITY,
      RU_RADIANCE,
      RU_IRRADIANCE,
      RU_RADIANT_EXITANCE,
      RU_RADIOSITY,
      RU_SPECTRAL_RADIANCE,
      RU_SPECTRAL_IRRADIANCE,
      UNKNOWN_RADIANCE_UNIT
   };



   /**
    * The list of mass units
    *
    */
   enum MassUnits
   {
      MU_METRIC_TON,
      MU_GRAM,
      MU_CARAT,
      MU_LONGTON,
      MU_SHORTON,
      MU_POUND,
      MU_OUNCE,
      MU_DRAM,
      MU_GRAIN,
      UNKNOWN_MASS_UNIT
   };

   /**
    * The list of magnetism units
    *
    */
   enum MagnetismUnits
   {
      MAGU_GAUSS,
      MAGU_TESLA,
      MAGU_OERSTED,
      UNKNOWN_MAGNETISM_UNIT
   };


   /**
    * The list of pressure units
    *
    */
   enum PressureUnits
   {
      PU_POUNDS_PER_SQUARE_INCH,
      PU_ATMOSPHERIC_PRESSURE,
      PU_TORR,
      PU_BAR,
      PU_PASCAL,
      UNKNOWN_PRESSURE_UNIT
   };

   /**
    * The list of temperature units
    *
    */
   enum TemperatureUnits
   {
      TU_FAHRENHEIT,
      TU_CELSIUS,
      TU_KELVIN,
      TU_RANKINE,
      UNKNOWN_TEMPERATURE_UNIT
   };

   /**
    * The list of volume units
    *
    */
   enum VolumeUnits
   {
      VU_FLUID_OUNCE,
      VU_PINT,
      VU_QUART,
      VU_GALLON,
      VU_CUBIC_INCHES,
      VU_CUBIC_FEET,
      VU_CUBIC_YARD,
      VU_LITER,
      VU_CUBIC_METER,
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
       DIMENSIONLESS,
       UNKNOWN_BASE_UNIT
   };

   enum NamedDerivedUnits
   {
       FREQUENCY,
       ANGLE,
       SOLID_ANGLE,
       FORCE,
       PRESSURE,
       ENERGY,
       POWER,
       ELECTRIC_CHARGE,
       VOLTAGE,
       CAPACITANCE,
       RESISTANCE,
       CONDUCTANCE,
       MAGNETIC_FLUX,
       MAGNETIC_FIELD,
       INDUCTANCE,
       LUMINOUS_FLUX,
       ILLUMINANCE,
       RADIOACTIVITY,
       ABSORBED_DOSE,
       EQUIVALENT_DOSE,
       CATALYTIC_ACTIVITY,
       EndNamedDerivedUnits
   };

   enum OtherDerivedUnits
   {
       AREA,
       VOLUME,
       VELOCITY,
       VOLUMETRIC_FLOW,
       ACCELERATION,
       JERK,
       SNAP,
       ANGULAR_VELOCITY,
       MOMENTUM,
       ANGULAR_MOMENTUM,
       TORQUE,
       WAVENUMBER,
       DENSITY,
       SPECIFIC_VOLUME,
       CONCENTRATION,
       MOLAR_VOLUME,
       HEAT_CAPACITY,
       MOLAR_HEAT_CAPACITY,
       SPECIFIC_HEAT_CAPACITY,
       MOLAR_ENERGY,
       SPECIFIC_ENERGY,
       ENERGY_DENSITY,
       SURFACE_TENSION,
       IRRADIANCE,
       THERMAL_CONDUCTIVITY,
       KINEMATIC_VISCOSITY,
       DYNAMIC_VISCOSITY,
       ELECTRIC_CHARGE_DENSITY,
       ELECTRIC_CURRENT_DESNTIY,
       CONDUCTIVITY,
       MOLAR_CONDUCTIVITY,
       PERMITTIVITY,
       PERMEABILITY,
       ELECTRIC_FIELD_STRENGTH,
       MAGNETIC_FIELD_STRENGTH,
       LUMINANCE,
       EXPOSURE,
       ABSORBED_DOSE_RATE,
       RESISTIVITY,
       EndOtherDerivedUnits
   };

   std::string GetUnitType() const;
   void SetUnitType(const std::string type);

   std::string GetDisplayUnits() const;
   void SetDisplayUnits(const std::string text);

   Real GetConversionFactor() const;
   void SetConversionFactor(Real factor);

protected:

    /// Variables in the unit class

    std::string unitType;
    std::string displayUnits;
    Real conversionFactor;
    StringArray numeratorUnits[7];
    StringArray denominatorUnits[7];
    IntegerArray numeratorPowers[7];
    IntegerArray denominatorPowers[7];
    RealArray numeratorPrefix[7];
    RealArray denominatorPrefix[7];

    /// String Mappings for units

    //---------------------------------
    //  static data
    //---------------------------------

    static const std::string SI_BASE_UNITS_STRING[UNKNOWN_BASE_UNIT+1];
    static const std::string SI_BASE_UNITS_SYMBOL[UNKNOWN_BASE_UNIT+1];
    static const Real SI_PREFIXES_FACTOR[UNKNOWN_SI_PREFIX+1];
    static const std::string SI_PREFIXES_SYMBOL[UNKNOWN_SI_PREFIX+1];
    static const std::string SI_PREFIXES_STRING[UNKNOWN_SI_PREFIX+1];

    static const Real ANGLE_UNITS_CONVERSION[UNKNOWN_ANGLE_UNIT+1];
    static const std::string ANGLE_UNITS_SYMBOL[UNKNOWN_ANGLE_UNIT+1];
    static const std::string ANGLE_UNITS_STRING[UNKNOWN_ANGLE_UNIT+1];

    static const Real AREA_UNITS_CONVERSION[UNKNOWN_AREA_UNIT+1];
    static const std::string AREA_UNITS_SYMBOL[UNKNOWN_AREA_UNIT+1];
    static const std::string AREA_UNITS_STRING[UNKNOWN_AREA_UNIT+1];

    static const Real DISTANCE_UNITS_CONVERSION[UNKNOWN_DISTANCE_UNIT+1];
    static const std::string DISTANCE_UNITS_SYMBOL[UNKNOWN_DISTANCE_UNIT+1];
    static const std::string DISTANCE_UNITS_STRING[UNKNOWN_DISTANCE_UNIT+1];

    static const Real ELECTRIC_UNITS_CONVERSION[UNKNOWN_ELECTRIC_UNIT+1];
    static const std::string ELECTRIC_UNITS_SYMBOL[UNKNOWN_ELECTRIC_UNIT+1];
    static const std::string ELECTRIC_UNITS_STRING[UNKNOWN_ELECTRIC_UNIT+1];

    static const Real ENERGY_UNITS_CONVERSION[UNKNOWN_ENERGY_UNIT+1];
    static const std::string ENERGY_UNITS_SYMBOL[UNKNOWN_ENERGY_UNIT+1];
    static const std::string ENERGY_UNITS_STRING[UNKNOWN_ENERGY_UNIT+1];

    //static const Real FORCE_UNITS_CONVERSION[UNKNOWN_FORCE_UNIT+1][UNKNOWN_FORCE_UNIT+1];
    static const std::string FORCE_UNITS_SYMBOL[UNKNOWN_FORCE_UNIT+1];
    static const std::string FORCE_UNITS_STRING[UNKNOWN_FORCE_UNIT+1];

    static const Real LUMINANCE_UNITS_CONVERSION[UNKNOWN_LUMINANCE_UNIT+1];
    static const std::string LUMINANCE_UNITS_SYMBOL[UNKNOWN_LUMINANCE_UNIT+1];
    static const std::string LUMINANCE_UNITS_STRING[UNKNOWN_LUMINANCE_UNIT+1];

    static const Real ILLUMINANCE_UNITS_CONVERSION[UNKNOWN_ILLUMINANCE_UNIT+1];
    static const std::string ILLUMINANCE_UNITS_SYMBOL[UNKNOWN_ILLUMINANCE_UNIT+1];
    static const std::string ILLUMINANCE_UNITS_STRING[UNKNOWN_ILLUMINANCE_UNIT+1];

    static const Real LUMINOUS_FLUX_UNITS_CONVERSION[UNKNOWN_LUMINOUSFLUX_UNIT+1];
    static const std::string LUMINOUS_FLUX_UNITS_SYMBOL[UNKNOWN_LUMINOUSFLUX_UNIT+1];
    static const std::string LUMINOUS_FLUX_UNITS_STRING[UNKNOWN_LUMINOUSFLUX_UNIT+1];

    static const Real LUMINOUS_ENERGY_UNITS_CONVERSION[UNKNOWN_LUMINOUSENERGY_UNIT+1];
    static const std::string LUMINOUS_ENERGY_UNITS_SYMBOL[UNKNOWN_LUMINOUSENERGY_UNIT+1];
    static const std::string LUMINOUS_ENERGY_UNITS_STRING[UNKNOWN_LUMINOUSENERGY_UNIT+1];

    static const Real LUMINOUS_INTENSITY_UNITS_CONVERSION[UNKNOWN_LUMINOUSINTENSITY_UNIT+1];
    static const std::string LUMINOUS_INTENSITY_UNITS_SYMBOL[UNKNOWN_LUMINOUSINTENSITY_UNIT+1];
    static const std::string LUMINOUS_INTENSITY_UNITS_STRING[UNKNOWN_LUMINOUSINTENSITY_UNIT+1];

    static const Real MASS_UNITS_CONVERSION[UNKNOWN_MASS_UNIT+1];
    static const std::string MASS_UNITS_SYMBOL[UNKNOWN_MASS_UNIT+1];
    static const std::string MASS_UNITS_STRING[UNKNOWN_MASS_UNIT+1];

    static const Real PRESSURE_UNITS_CONVERSION[UNKNOWN_PRESSURE_UNIT+1];
    static const std::string PRESSURE_UNITS_SYMBOL[UNKNOWN_PRESSURE_UNIT+1];
    static const std::string PRESSURE_UNITS_STRING[UNKNOWN_PRESSURE_UNIT+1];

    /*
    static const Real RADIANCE_UNITS_CONVERSION[UNKNOWN_RADIANCE_UNIT+1];
    static const std::string RADIANCE_UNITS_SYMBOL[UNKNOWN_RADIANCE_UNIT+1];
    static const std::string RADIANCE_UNITS_STRING[UNKNOWN_RADIANCE_UNIT+1];
    */

    static const Real TEMPERATURE_UNITS_CONVERSION[UNKNOWN_TEMPERATURE_UNIT+1];
    static const std::string TEMPERATURE_UNITS_SYMBOL[UNKNOWN_TEMPERATURE_UNIT+1];
    static const std::string TEMPERATURE_UNITS_STRING[UNKNOWN_TEMPERATURE_UNIT+1];

    static const Real VOLUME_UNITS_CONVERSION[UNKNOWN_VOLUME_UNIT+1];
    static const std::string VOLUME_UNITS_SYMBOL[UNKNOWN_VOLUME_UNIT+1];
    static const std::string VOLUME_UNITS_STRING[UNKNOWN_VOLUME_UNIT+1];

    static const Real TIME_UNITS_CONVERSION[UNKNOWN_TIME_UNIT+1];
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

