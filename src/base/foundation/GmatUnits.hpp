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

//#include "gmatdefs.hpp"

namespace GmatUnits
{
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
       UNKONWN_BASE_UNIT
   };

/// String Mappings for units

//---------------------------------
//  static data
//---------------------------------

static const std::string SI_BASE_UNITS_STRING[GmatUnits::UNKNOWN_BASE_UNIT+1] =
{
    "Length", "Mass", "Time", "ElectricCurrent", "Temperature",
    "AmountOfSubstance", "LuminousIntensity", "UnknownBaseUnit"

};

static const std::string SI_BASE_UNITS_SYMBOL[GmatUnits::UNKNOWN_BASE_UNIT+1] =
{
    "L","M","T","A","K","MOL","C",""
};


static const Real SI_PREFIXES_FACTOR[GmatUnits::UNKNOWN_SI_PREFIX+1] =
{
  1e24,     1e21,   1e18,   1e15,   1e12,   1e9,    1e6,    1000.0,
  100.0,    10.0,   0.1,    0.01,   0.001,  1e-6,   1e-9,   1e-12,
  1e-15,    1e-18,  1e-21,  1e-24,  1.0
};

static const std::string
SI_PREFIXES_SYMBOL[GmatUnits::UNKNOWN_SI_PREFIX+1] =
{
    "Y",    "Z",        "E",    "P",    "T",    "G",
    "M",    "k",        "h",    "da",   "d",    "c",
    "m",    "mu",       "n",    "p",    "f",    "a",
    "z",    "y",        "UnkownSIPrefix"
};

static const std::string
SI_PREFIXES_STRING[GmatUnits::UNKNOWN_SI_PREFIX+1] =
{
    "yotta",    "zetta",    "exa",      "peta",     "tera",     "giga",
    "mega",     "kilo",     "hecto",    "deka",     "deci",     "centi",
    "milli",    "micro",    "nano",     "pico",     "femto",    "atto",
    "zepto",    "yocto",    "UnkownSIPrefix"
};

// Useful Unicode characters that we can't use right now
// Lookup additional characters at:
// http:://www.fileformat.info/info/unicode/char/search.htm
// Superscript 2 - \u00B2
// Superscript 3 - \u00B3
// Angstrom symbol - \u212B
// Greek letter lowercase mu - \u03BC
// Greek letter capital Omega - \u03A9


static const std::string
ANGLE_UNITS_SYMBOL[GmatUnits::UNKNOWN_ANGLE_UNIT+1] =
{
    "rad", "sr", "deg", ""
};

static const std::string
ANGLE_UNITS_STRING[GmatUnits::UNKNOWN_ANGLE_UNIT+1] =
{
    "Radians", "Steradians", "Degrees", ""
};

static const std::string
AREA_UNITS_SYMBOL[GmatUnits::UNKNOWN_AREA_UNIT+1] =
{
    "sq in",        "sq ft",       "sq yd",         "sq rd",
    "sq mi",        "acre",        "sq m",          "a",
    "ha",           "UnknownAreaUnit"
};

static const std::string
AREA_UNITS_STRING[GmatUnits::UNKNOWN_AREA_UNIT+1] =
{
    "Square Inches",    "Square Feet",  "Square Yard",  "Square Rod",
    "Square Mile",      "Acre",         "Square Meter", "Are",
    "Hectare",          "UnknownAreaUnit"
};

static const std::string
DISTANCE_UNITS_SYMBOL[GmatUnits::UNKNOWN_DISTANCE_UNIT+1] =
{
   "m",     "league",   "mi",       "naut mi",
   "fur",   "rd",       "fath",     "yd",
   "ft",    "in",       "ua",       "ly",
   "par",   "Angstrom",   "UnknownDistanceUnit"
};

static const std::string
DISTANCE_UNITS_STRING[GmatUnits::UNKNOWN_DISTANCE_UNIT+1] =
{
   "Meters",     "Leagues",   "Miles",       "Nautical Miles",
   "Furlongs",   "Rods",      "Fathoms",     "Yards",
   "Feet",      "Inches",       "Astronomical Units",       "Light Years",
   "Parsecs",   "Angstrom",   "UnknownDistanceUnit"
};

static const std::string
ELECTRIC_UNITS_SYMBOL[GmatUnits::UNKNOWN_ELECTRIC_UNIT+1] =
{
    "A", "Ah", "C", "F", "S", "H", "V", "Omega", ""
};

static const std::string
ELECTRIC_UNITS_STRING[GmatUnits::UNKNOWN_ELECTRIC_UNIT+1] =
{
    "Ampere","Aphere-hour", "Coulomb", "Farad", "Siemens", "Henry",
    "Volt", "Ohm", "UnknownElectricUnit"
};

static const std::string
ENERGY_UNITS_SYMBOL[GmatUnits::UNKNOWN_ENERGY_UNIT+1] =
{
   "cm\u207B\u2070",  "J",    "J/mol",    "cal",  "cal/mol",  "nm",
   "Hz",  "eV", "UnknownEngeryUnit"
};

static const std::string
ENERGY_UNITS_STRING[GmatUnits::UNKNOWN_ENERGY_UNIT+1] =
{
   "Wavenumber",  "Joules",    "Joules per mol",    "Calories",
   "Calories per mol", "Nanometers",  "Hertz",  "Electron Volts",
   "UnknownEngeryUnit"
};

/*
 1 pound-force 	≡ 0.45359237 kg × 9.80665 m/s²
= 4.4482216152605 N (exactly)

           newton	dyne            kilopond	 pound-force	poundal
1 N	≡ 1 kg·m/s²	= 105 dyn	≈ 0.10197 kp	≈ 0.22481 lbf	≈ 7.2330 pdl
1 dyn	= 10−5 N	≡ 1 g·cm/s²	≈ 1.0197×10−6 kp ≈ 2.2481×10−6 lbf	≈ 7.2330×10−5 pdl
1 kp	= 9.80665 N	= 980665 dyn	≡ gn·(1 kg)	≈ 2.2046 lbf	≈ 70.932 pdl
1 lbf	≈ 4.448222 N	≈ 444822 dyn	≈ 0.45359 kp	≡ gn·(1 lb)	≈ 32.174 pdl
1 pdl	≈ 0.138255 N	≈ 13825 dyn	≈ 0.014098 kp	≈ 0.031081 lbf	≡ 1 lb·ft/s²

 */

static const std::string
FORCE_UNITS_SYMBOL[GmatUnits::UNKNOWN_FORCE_UNIT+1] =
{
    "dyn", "kp", "kip", "N", "Planck-force", "lbf", "pdl",  "sn",
    "UnknownForceUnit"
};

static const std::string
FORCE_UNITS_STRING[GmatUnits::UNKNOWN_FORCE_UNIT+1] =
{
    "Dyne", "Kilopond", "Kip", "Newton", "Planck-force",
    "Pound-force", "Poundal", "Sthene",  "UnknownForceUnit"
};

static const std::string
LUMINANCE_UNITS_SYMBOL[GmatUnits::UNKNOWN_LUMINANCE_UNIT+1] =
{
    "L", "fL", "cd/m^2", "nit", ""
};

static const std::string
LUMINANCE_UNITS_STRING[GmatUnits::UNKNOWN_LUMINANCE_UNIT+1] =
{
    "Lambert", "Foot-lambert", "Candela per square meter", "Nit",
    "UnknownLuminanceUnit"
};

static const std::string
ILLUMINANCE_UNITS_SYMBOL[GmatUnits::UNKNOWN_ILLUMINANCE_UNIT+1] =
{
    "fc", "lx", "ph", ""
};

static const std::string
ILLUMINANCE_UNITS_STRING[GmatUnits::UNKNOWN_ILLUMINANCE_UNIT+1] =
{
    "Foot Candle", "Lux", "Phot", "UnknownIlluminanceUnit"
};

static const std::string
LUMINOUS_FLUX_UNITS_SYMBOL[GmatUnits::UNKNOWN_LUMINOUSFLUX_UNIT+1] =
{
    "lm", "R", ""
};

static const std::string
LUMINOUS_FLUX_UNITS_STRING[GmatUnits::UNKNOWN_LUMINOUSFLUX_UNIT+1] =
{
    "Lumen", "Rayleigh", "UnknownLuminousFluxUnit"
};

static const std::string
LUMINOUS_ENERGY_UNITS_SYMBOL[GmatUnits::UNKNOWN_LUMINOUSENERGY_UNIT+1] =
{
       "T", ""
};

static const std::string
LUMINOUS_ENERGY_UNITS_STRING[GmatUnits::UNKNOWN_LUMINOUSENERGY_UNIT+1] =
{
       "Talbot", "UnknownLuminousEnergyUnit"
};

static const std::string
LUMINOUS_INTENSITY_UNITS_SYMBOL[GmatUnits::UNKNOWN_LUMINOUSINTENSITY_UNIT+1] =
{
    "cd", "cp", ""
};

static const std::string
LUMINOUS_INTENSITY_UNITS_STRING[GmatUnits::UNKNOWN_LUMINOUSINTENSITY_UNIT+1] =
{
       "Candela", "Candle Power", "UnknownLuminousIntensityUnit"
};

static const std::string
MASS_UNITS_SYMBOL[GmatUnits::UNKNOWN_MASS_UNIT+1] =
{
    "t",    "g",    "c",    "longTon",  "shortTon", "lb",   "oz",
    "dr",   "grain",    "UnknownMassUnit"
};

static const std::string
MASS_UNITS_STRING[GmatUnits::UNKNOWN_MASS_UNIT+1] =
{
    "Metric Tons",    "Grams",   "Carats",    "Long Tons",  "Short Tons",
    "Pounds",  "Ounces",  "Drams",     "Grains",     "UnknownMassUnit"
};

static const std::string
PRESSURE_UNITS_SYMBOL[GmatUnits::UNKNOWN_PRESSURE_UNIT+1] =
{
    "psi",  "atm",  "torr", "bar",  "Pa",   "UnknownPressureUnit"
};

static const std::string
PRESSURE_UNITS_STRING[GmatUnits::UNKNOWN_PRESSURE_UNIT+1] =
{
    "Pounds per square inch",  "Atmospheres",  "Torr", "Bar",
    "Pascals",   "UnknownPressureUnit"
};
/*
static const std::string
RADIANCE_UNITS_SYMBOL[GmatUnits::UNKNOWN_RADIANCE_UNIT+1] =
{
    "J",  "W",  "W/sr", "W/sr m^2",  "",   "UnknownPressureUnit"
};

static const std::string
RADIANCE_UNITS_STRING[GmatUnits::UNKNOWN_RADIANCE_UNIT+1] =
{
    "Pounds per square inch",  "Atmospheres",  "Torr", "Bar",
    "Pascals",   "UnknownPressureUnit"
};
*/
static const std::string
TEMPERATURE_UNITS_SYMBOL[GmatUnits::UNKNOWN_TEMPERATURE_UNIT+1] =
{
    "F",    "C",    "Kelvin",   "Rankine",  "UnknownTemperatureUnit"
};

static const std::string
TEMPERATURE_UNITS_STRING[GmatUnits::UNKNOWN_TEMPERATURE_UNIT+1] =
{
    "Fahrenheit", "Celsius", "Kelvin",   "Rankine",  "UnknownTemperatureUnit"
};

static const std::string
VOLUME_UNITS_SYMBOL[GmatUnits::UNKNOWN_VOLUME_UNIT+1] =
{
    "oz",   "pt",   "qt",   "gal",  "cub in",   "cub ft",   "cub yd",
    "l",  "cub m", "UnknownVolumeUnit"
};

static const std::string
VOLUME_UNITS_STRING[GmatUnits::UNKNOWN_VOLUME_UNIT+1] =
{
    "Ounces",   "Pints",   "Quarts",   "Gallons",  "Cubic Inches",
    "Cubic Feet", "Cubic Yards", "Liters",  "Cubic Meter", "UnknownVolumeUnit"
};

static const std::string
TIME_UNITS_SYMBOL[GmatUnits::UNKNOWN_TIME_UNIT+1] =
{
    "JD",   "MJD",  "GMJD", "ST",   "UT",   "TAI",  "GAST", "MST",
    "UT0",  "UT1",  "UT2",  "UTC",  "LT",   "GMST", "LST",  "MST",
    "AST",  "TT",   "TDB",  "TCG",  "TCB",  "GPS",  "Year", "Month",
    "Day",  "DOY",  "Hour", "Minute",   "Second",  ""
};

static const std::string
TIME_UNITS_STRING[GmatUnits::UNKNOWN_TIME_UNIT+1] =
{
    "Julian Date",  "Modified Julian Date", "Goddard Modified Julian Date",
    "Solare Time",  "Sidereal Time",        "Universal Time",
    "Internation Atomic Time",  "Apparent Solar Time",
    "Mean Solar Time",  "UT0",  "UT1",  "UT2",  "UTC",
    "Local Time",   "Greenwich Mean Sidereal Time",
    "Local Sidereal Time",  "Mean Sidereal Time",
    "Apparent Sidereal Time",   "Terrestrial Time",
    "Barycentric Dynamical Time",   "Geoncentric Coordinate Time",
    "Barycentric Coordinate Time",  "GPS Time",
    "Year", "Month",    "Day",  "Day of Year",
    "Hour", "Minute",   "Second", "UnknownTimeUnit"
};

static const std::string
MILITARY_TIME_ZONE_CODES[GmatUnits::EndMilitaryTimeZones+1] =
{
    "Z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L",
    "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", ""
};

static const Real
MILITARY_TIME_UTC_OFFSETS[GmatUnits::EndMilitaryTimeZones+1] =
{
    0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0,
    -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0, -9, -10.0, -11.0, -12.0, 0.0
};

static const std::string
US_TIME_ZONE_CODES[GmatUnits::EndUSTimeZones+1] =
{
    "AST", "EST", "CST", "MST", "PST", "AKST", "HST",
    "ADT", "EDT", "CDT", "MDT", "PDT", "AKDT", ""
};

static const Real
US_TIME_UTC_OFFSETS[GmatUnits::EndUSTimeZones+1] =
{
    -4.0, -5.0, -6.0, -7.0, -8.0, -9.0, -10.0,
    -3.0, -4.0, -5.0, -6.0, -7.0, -8.0,  0.0
};

static const std::string
CANADA_TIME_ZONE_CODES[GmatUnits::EndCanadaTimeZones+1] =
{
    "NST", "AST", "EST", "CST", "MST", "PST", "YST",
    "NDT", "ADT", "EDT", "CDT", "MDT", "PDT", ""
};

static const Real
CANADA_TIME_UTC_OFFSETS[GmatUnits::EndCanadaTimeZones+1] =
{
    -3.5, -4.0, -5.0, -6.0, -7.0, -8.0, -8.0,
    -2.5, -3.0, -4.0, -5.0, -6.0, -7.0,  0.0
};

//---------------------------------
//  public methods
//---------------------------------



};



#endif	/* _GMATUNITS_HPP */

