#include "GmatUnits.hpp"

//---------------------------------
//   data
//---------------------------------

 const std::string GmatUnits::SI_BASE_UNITS_STRING[UNKNOWN_BASE_UNIT+1] =
{
    "Length", "Mass", "Time", "ElectricCurrent", "Temperature",
    "AmountOfSubstance", "LuminousIntensity", "UnknownBaseUnit"

};

 const std::string GmatUnits::SI_BASE_UNITS_SYMBOL[UNKNOWN_BASE_UNIT+1] =
{
    "L","M","T","A","K","MOL","C",""
};


 const Real GmatUnits::SI_PREFIXES_FACTOR[UNKNOWN_SI_PREFIX+1] =
{
  1e24,     1e21,   1e18,   1e15,   1e12,   1e9,    1e6,    1000.0,
  100.0,    10.0,   0.1,    0.01,   0.001,  1e-6,   1e-9,   1e-12,
  1e-15,    1e-18,  1e-21,  1e-24,  1.0
};

 const std::string
GmatUnits::SI_PREFIXES_SYMBOL[UNKNOWN_SI_PREFIX+1] =
{
    "Y",    "Z",        "E",    "P",    "T",    "G",
    "M",    "k",        "h",    "da",   "d",    "c",
    "m",    "mu",       "n",    "p",    "f",    "a",
    "z",    "y",        "UnkownSIPrefix"
};

 const std::string
GmatUnits::SI_PREFIXES_STRING[UNKNOWN_SI_PREFIX+1] =
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


 const std::string
GmatUnits::ANGLE_UNITS_SYMBOL[UNKNOWN_ANGLE_UNIT+1] =
{
    "rad", "sr", "deg", ""
};

 const std::string
GmatUnits::ANGLE_UNITS_STRING[UNKNOWN_ANGLE_UNIT+1] =
{
    "Radians", "Steradians", "Degrees", ""
};

 const std::string
GmatUnits::AREA_UNITS_SYMBOL[UNKNOWN_AREA_UNIT+1] =
{
    "sq in",        "sq ft",       "sq yd",         "sq rd",
    "sq mi",        "acre",        "sq m",          "a",
    "ha",           "UnknownAreaUnit"
};

 const std::string
GmatUnits::AREA_UNITS_STRING[UNKNOWN_AREA_UNIT+1] =
{
    "Square Inches",    "Square Feet",  "Square Yard",  "Square Rod",
    "Square Mile",      "Acre",         "Square Meter", "Are",
    "Hectare",          "UnknownAreaUnit"
};

 const std::string
GmatUnits::DISTANCE_UNITS_SYMBOL[UNKNOWN_DISTANCE_UNIT+1] =
{
   "m",     "league",   "mi",       "naut mi",
   "fur",   "rd",       "fath",     "yd",
   "ft",    "in",       "ua",       "ly",
   "par",   "Angstrom",   "UnknownDistanceUnit"
};

 const std::string
GmatUnits::DISTANCE_UNITS_STRING[UNKNOWN_DISTANCE_UNIT+1] =
{
   "Meters",     "Leagues",   "Miles",       "Nautical Miles",
   "Furlongs",   "Rods",      "Fathoms",     "Yards",
   "Feet",      "Inches",       "Astronomical Units",       "Light Years",
   "Parsecs",   "Angstrom",   "UnknownDistanceUnit"
};

 const std::string
GmatUnits::ELECTRIC_UNITS_SYMBOL[UNKNOWN_ELECTRIC_UNIT+1] =
{
    "A", "Ah", "C", "F", "S", "H", "V", "Omega", ""
};

 const std::string
GmatUnits::ELECTRIC_UNITS_STRING[UNKNOWN_ELECTRIC_UNIT+1] =
{
    "Ampere","Aphere-hour", "Coulomb", "Farad", "Siemens", "Henry",
    "Volt", "Ohm", "UnknownElectricUnit"
};

 const std::string
GmatUnits::ENERGY_UNITS_SYMBOL[UNKNOWN_ENERGY_UNIT+1] =
{
   "cm\u207B\u2070",  "J",    "J/mol",    "cal",  "cal/mol",  "nm",
   "Hz",  "eV", "UnknownEngeryUnit"
};

 const std::string
GmatUnits::ENERGY_UNITS_STRING[UNKNOWN_ENERGY_UNIT+1] =
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

 const std::string
GmatUnits::FORCE_UNITS_SYMBOL[UNKNOWN_FORCE_UNIT+1] =
{
    "dyn", "kp", "kip", "N", "Planck-force", "lbf", "pdl",  "sn",
    "UnknownForceUnit"
};

 const std::string
GmatUnits::FORCE_UNITS_STRING[UNKNOWN_FORCE_UNIT+1] =
{
    "Dyne", "Kilopond", "Kip", "Newton", "Planck-force",
    "Pound-force", "Poundal", "Sthene",  "UnknownForceUnit"
};

 const std::string
GmatUnits::LUMINANCE_UNITS_SYMBOL[UNKNOWN_LUMINANCE_UNIT+1] =
{
    "L", "fL", "cd/m^2", "nit", ""
};

 const std::string
GmatUnits::LUMINANCE_UNITS_STRING[UNKNOWN_LUMINANCE_UNIT+1] =
{
    "Lambert", "Foot-lambert", "Candela per square meter", "Nit",
    "UnknownLuminanceUnit"
};

 const std::string
GmatUnits::ILLUMINANCE_UNITS_SYMBOL[UNKNOWN_ILLUMINANCE_UNIT+1] =
{
    "fc", "lx", "ph", ""
};

 const std::string
GmatUnits::ILLUMINANCE_UNITS_STRING[UNKNOWN_ILLUMINANCE_UNIT+1] =
{
    "Foot Candle", "Lux", "Phot", "UnknownIlluminanceUnit"
};

 const std::string
GmatUnits::LUMINOUS_FLUX_UNITS_SYMBOL[UNKNOWN_LUMINOUSFLUX_UNIT+1] =
{
    "lm", "R", ""
};

 const std::string
GmatUnits::LUMINOUS_FLUX_UNITS_STRING[UNKNOWN_LUMINOUSFLUX_UNIT+1] =
{
    "Lumen", "Rayleigh", "UnknownLuminousFluxUnit"
};

 const std::string
GmatUnits::LUMINOUS_ENERGY_UNITS_SYMBOL[UNKNOWN_LUMINOUSENERGY_UNIT+1] =
{
       "T", ""
};

 const std::string
GmatUnits::LUMINOUS_ENERGY_UNITS_STRING[UNKNOWN_LUMINOUSENERGY_UNIT+1] =
{
       "Talbot", "UnknownLuminousEnergyUnit"
};

 const std::string
GmatUnits::LUMINOUS_INTENSITY_UNITS_SYMBOL[UNKNOWN_LUMINOUSINTENSITY_UNIT+1] =
{
    "cd", "cp", ""
};

 const std::string
GmatUnits::LUMINOUS_INTENSITY_UNITS_STRING[UNKNOWN_LUMINOUSINTENSITY_UNIT+1] =
{
       "Candela", "Candle Power", "UnknownLuminousIntensityUnit"
};

 const std::string
GmatUnits::MASS_UNITS_SYMBOL[UNKNOWN_MASS_UNIT+1] =
{
    "t",    "g",    "c",    "longTon",  "shortTon", "lb",   "oz",
    "dr",   "grain",    "UnknownMassUnit"
};

 const std::string
GmatUnits::MASS_UNITS_STRING[UNKNOWN_MASS_UNIT+1] =
{
    "Metric Tons",    "Grams",   "Carats",    "Long Tons",  "Short Tons",
    "Pounds",  "Ounces",  "Drams",     "Grains",     "UnknownMassUnit"
};

 const std::string
GmatUnits::PRESSURE_UNITS_SYMBOL[UNKNOWN_PRESSURE_UNIT+1] =
{
    "psi",  "atm",  "torr", "bar",  "Pa",   "UnknownPressureUnit"
};

 const std::string
GmatUnits::PRESSURE_UNITS_STRING[UNKNOWN_PRESSURE_UNIT+1] =
{
    "Pounds per square inch",  "Atmospheres",  "Torr", "Bar",
    "Pascals",   "UnknownPressureUnit"
};
/*
 const std::string
GmatUnits::RADIANCE_UNITS_SYMBOL[UNKNOWN_RADIANCE_UNIT+1] =
{
    "J",  "W",  "W/sr", "W/sr m^2",  "",   "UnknownPressureUnit"
};

 const std::string
GmatUnits::RADIANCE_UNITS_STRING[UNKNOWN_RADIANCE_UNIT+1] =
{
    "Pounds per square inch",  "Atmospheres",  "Torr", "Bar",
    "Pascals",   "UnknownPressureUnit"
};
*/
 const std::string
GmatUnits::TEMPERATURE_UNITS_SYMBOL[UNKNOWN_TEMPERATURE_UNIT+1] =
{
    "F",    "C",    "Kelvin",   "Rankine",  "UnknownTemperatureUnit"
};

 const std::string
GmatUnits::TEMPERATURE_UNITS_STRING[UNKNOWN_TEMPERATURE_UNIT+1] =
{
    "Fahrenheit", "Celsius", "Kelvin",   "Rankine",  "UnknownTemperatureUnit"
};

 const std::string
GmatUnits::VOLUME_UNITS_SYMBOL[UNKNOWN_VOLUME_UNIT+1] =
{
    "oz",   "pt",   "qt",   "gal",  "cub in",   "cub ft",   "cub yd",
    "l",  "cub m", "UnknownVolumeUnit"
};

 const std::string
GmatUnits::VOLUME_UNITS_STRING[UNKNOWN_VOLUME_UNIT+1] =
{
    "Ounces",   "Pints",   "Quarts",   "Gallons",  "Cubic Inches",
    "Cubic Feet", "Cubic Yards", "Liters",  "Cubic Meter", "UnknownVolumeUnit"
};

 const std::string
GmatUnits::TIME_UNITS_SYMBOL[UNKNOWN_TIME_UNIT+1] =
{
    "JD",   "MJD",  "GMJD", "ST",   "UT",   "TAI",  "GAST", "MST",
    "UT0",  "UT1",  "UT2",  "UTC",  "LT",   "GMST", "LST",  "MST",
    "AST",  "TT",   "TDB",  "TCG",  "TCB",  "GPS",  "Year", "Month",
    "Day",  "DOY",  "Hour", "Minute",   "Second",  ""
};

 const std::string
GmatUnits::TIME_UNITS_STRING[UNKNOWN_TIME_UNIT+1] =
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

 const std::string
GmatUnits::MILITARY_TIME_ZONE_CODES[EndMilitaryTimeZones+1] =
{
    "Z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L",
    "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", ""
};

 const Real
GmatUnits::MILITARY_TIME_UTC_OFFSETS[EndMilitaryTimeZones+1] =
{
    0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0,
    -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0, -9, -10.0, -11.0, -12.0, 0.0
};

 const std::string
GmatUnits::US_TIME_ZONE_CODES[EndUSTimeZones+1] =
{
    "AST", "EST", "CST", "MST", "PST", "AKST", "HST",
    "ADT", "EDT", "CDT", "MDT", "PDT", "AKDT", ""
};

 const Real
GmatUnits::US_TIME_UTC_OFFSETS[EndUSTimeZones+1] =
{
    -4.0, -5.0, -6.0, -7.0, -8.0, -9.0, -10.0,
    -3.0, -4.0, -5.0, -6.0, -7.0, -8.0,  0.0
};

 const std::string
GmatUnits::CANADA_TIME_ZONE_CODES[EndCanadaTimeZones+1] =
{
    "NST", "AST", "EST", "CST", "MST", "PST", "YST",
    "NDT", "ADT", "EDT", "CDT", "MDT", "PDT", ""
};

 const Real
GmatUnits::CANADA_TIME_UTC_OFFSETS[EndCanadaTimeZones+1] =
{
    -3.5, -4.0, -5.0, -6.0, -7.0, -8.0, -8.0,
    -2.5, -3.0, -4.0, -5.0, -6.0, -7.0,  0.0
};
