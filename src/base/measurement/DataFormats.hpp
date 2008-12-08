//$Header$
//------------------------------------------------------------------------------
//                             DataFormats
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/22
//
/**
 *
 * Defines the various data formats for standard measurement data types.
 *
 * This class allows other classes to know what to expect when requesting
 * data from a file reader.
 *
 */
//------------------------------------------------------------------------------

#ifndef DataFormats_hpp
#define	DataFormats_hpp

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"

namespace DataFormats
{
 
    // See the following website for a complete description of the SLR
    // data format. The comments below are taken directly from the ILRS website.
    // http://ilrs.gsfc.nasa.gov/products_formats_procedures/normal_point/np_format.html    
    struct slr_header {
	
	// SLR Type
	// 99999 - Standard observation data
	// 88888 - Engineering/Simulated observation data
	
	Integer slrType;
	
	// ILRS Satellite Identifier - 7 digit number based on COSPAR
	// Note: COSPAR ID to ILRS Satellite Identification Algorithm
	// COSPAR ID Format: (YYYY-XXXA)

	// YYYY is the four digit year when the launch vehicle was put in orbit
	// XXX is the sequential launch vehicle number for that year 
	// A is the alpha numeric sequence number within a launch
	// Example: LAGEOS-1 COSPAR ID is 1976-039A
	// Explanation: LAGEOS-1 launch vehicle wasplaced in orbit in 1976; 
	// was the 39th launch in that year; and LAGEOS-1 was the first object
	// injected into orbit from this launch.
	//  
	// ILRS Satellite Identification Format: (YYXXXAA), 
	// based on the COSPAR ID
	//  
	// Where YY is the two digit year when the launch vehicle was put in orbit
	// Where XXX is the sequential launch vehicle number for that year 
	// AA is the numeric sequence number within a launch
	// Example: LAGEOS-1 ILRS Satellite ID is 7603901
	std::string ilrsSatnum;

	// Year, Day of Year of century
	Integer year;
	Integer dayOfYear;

	// Crustal Dynamics Project PAD ID - a 4 digit monument identification
	Integer cdpPadID;

	// Crustal Dynamics Project 2-digit system number
	Integer cdpSysNum;

	// Crustal Dynamics Project 2-digit occupany sequence number
	Integer cdpOccupancySequenceNum;

	// Wavelength of the laser in nanometers. The original spec provides
	// an integer value with the following convention.
	// 3000 - 9999: units are 0.1 nanometer
	// 1000 - 2999: units are 1.0 nanometer
	// For the station generating the data, the rule is:
	// Wavelength in rate 0.3000 - 0.9999 microns: unit 0.1 nanometer
	// Wavelength in rate 1.000 - 2.999 microns: unit 1.0 nanometer
	// We have converted the units appropriately to nanometers.
	Real wavelength;

	// Calibration system delay (two-way value in picoseconds)
	Integer calSysDelay;

	// Calibration delay shift (two-way value in picoseconds)
	Integer calDelayShift;

	// Root Mean Square (RMS) of raw system delay values from the mean. 
	// Two-way value in picoseconds. If pre- and post- pass calibrations 
	// are made,use the mean of the two RMS values, or the RMS of 
	// the combined data set.
	Integer rmsSysDelay;

	// Normal Point window indicator (an integer from 0 to 9)
	// 0: not a normal point
	// 1: 5-second normal point (GFZ-1)
	// 2: LLR normal point
	// 3: 15-second normal point (TOPEX)
	// 4: 20-second normal point
	// 5: 30-second normal point
	// 6: 1-minute normal point
	// 7: 2-minute normal point (LAGEOS)
	// 8: 3-minute normal point
	// 9: 5-minute normal point (ETALON)
	Integer normalPointWindowIndicator;

	// Epoch time scale indicator
	// 3: UTC (USNO)
	// 4: UTC (GPS)
	// 7: UTC (BIPM) (BIH prior to 1988)
	Integer epochTimeScaleIndicator;
	
	// System calibration method and delay shift indicator. 
	// Indicates the type of calibration and the type of 
	// calibration shift given in columns 33-38
	//		Pre- to Post-Pass	Minimum to Maximum
	//		Calibration Shift	Calibration Shift
	//  External cal	0		    5
	//  Internal cal	1		    6
	//  Burst cal		2		    7
	//  Some other cal	3		    8
	//  Not used		4		    9	
	Integer sysCalMethodIndicator;
	
	// System CHange indicator (SCH). A flag to increment for every 
	// major change to the system (hardware or software). After the 
	// value '9' return to '0', and then continue incrementing. The 
	// station and data centers should keep a log in a standard format 
	// of the value used, the date of the change, and a description 
	// of the change.
	Integer schIndicator;
	
	// System Configuration Indicator (SCI). A flag used to indicate 
	// alternative modes of operation for a system (e.g., choice of 
	// alternative timers or detectors, or use of a different mode of 
	// operation for high satellites). Each value of the flag indicates 
	// a particular configuration, which is described in a log file held 
	// at the station and at the data centers. If only a single 
	// configuration is used then use a fixed value. If a new 
	// configuration is introduced then use the next higher flag value. 
	// If value exceeds '9' then return to '0', overwriting a previous 
	// configuration flag (it is not likely that a station will have 10 
	// current possible configurations).
	Integer sciIndicator;
	
	// Pass RMS from the mean of raw range values minus the trend function, 
	// for accepted ranges (two-way value in picoseconds).
	Integer passRMS;
	
	// Data quality assessment indicator
	// For LLR data:
	// 0: Undefined or no comment.
	// 1: Clear, easily filtered data, with little or no noise.
	// 2: Clear data with some noise; filtering is slightly compromised by 
	//    noise level.
	// 3: Clear data with a significant amount of noise, or weak data with 
	//    little noise. Data are certainly present, but filtering is difficult.
	// 4: Un-clear data; data appear marginally to be present, but are very 
	//    difficult to separate from noise during filtering. Signal to 
	//    noise ratio can be less than 1:1.
	// 5: No data apparent.
	Integer dataQualAssessmentIndicator;

	// Format revision number indicator.
	// Value '1' for the 1997 revision. 
	// Implied value '0' or 'space' for original 1990 release. 
	// Revision 2 and above, use byte 49 in data record to indicate 
	// power of ten with which to multiply the number stored in 
	// bytes 44-47 of data record.
	Integer formatRevisionNum;
	
    };

    struct slr_obtype {

        // Iterator Pointer to the header record
        std::vector<slr_header*>::const_iterator headerVectorIndex;

	
	// Time of day of laser firing, from 0 hours UTC in units of seconds
	// Value is given module 86400 if pass crosses 24 hours UTC
	// Note that the data spec provides this in units of 0.1 microseconds
	// but this integer is too large to store efficiently. Therefore,
	// we must convert to a real valued time in units of seconds.
	Real timeOfLaserFiring;
	
	// Two-way time-of-flight corrected for system delay, in seconds. 
	// Not corrected for atmospheric delay, nor to the center-of-mass 
	// of the satellite.
	// NOe that the data spec provides this in picoseconds
	// but this integer is too large to store efficiently. Therefore,
	// we must convert to a real valued time in units of seconds.
	Real twoWayTimeOfFlight;
	
	// Bin RMS from the mean of raw range values minus the trend function, 
	// for accepted ranges. Two-way value in picoseconds. If point is 
	// a single raw data point, then use pass RMS.
	Integer binRMSRange;
	
	// Surface pressure, in units of millibar. The original spec provides
	// an integer in units of 0.1 millibar but we convert it to a real
	// valued number.
	Real surfacePressure;
	
	// Surface temperature in units of degrees Kelvin.  The original 
	// spec provides an integer in units of 0.1 degrees Kelvin but we 
	// convert it to a real valued number.	
	Real surfaceTemp;
	
	// Relative humidity at surface in percent	
	Integer relativeHumidity;
	
	// Number of raw ranges (after editing) compressed into the normal 
	// point. In September 1999, the Jaguar Team concluded "That ILRS 
	// make NO RESTRICTION on the minimum number of returns used to 
	// generate Normal Points."
	Integer numRawRanges;

	// A flag to indicate the data release:
	// 0: first release of data
	// 1: first replacement release of the data,
	// 2: second replacement release, etc.
	Integer dataReleaseFlag;
		
	// For SLR data: not used before revision 2. Revision 2 and above, 
	// indicates power of ten with which to multiply number stored in 
	// bytes 44-47 in order to provide a very close approximation to the 
	// total number of returns for high yield systems (kHz systems).
	// For LLR data: integer seconds of the two-way time of flight 
	// (columns 13-24 contain the fractional part).
	Integer rawRangeFactor;
	
	// For SLR data: not used
	// For LLR data: normal point window indicator. Indicates the time span of the normal point (can be variable from point to point).
	// 1: <= 5 minutes
	// 2: 10 minutes
	// 3: 15 minutes
	// 4: 20 minutes
	// 5: 25 minutes
	// 6: 30 minutes
	// 7: 35 minutes
	// 8: 40 minutes
	// 9: >= 50 minutes
	Integer normalPointWindowIndicator2;
	
	// For SLR data: not used
	// For LLR data: signal to noise ratio, unitless
	Real  signalToNoiseRatio;

	//
	// The following are only used in Sample Engineering Data Records:
	// burstCalSysDelay, signalStrength, angleOriginIndicator, az, el
	//
	
	// Internal burst calibration system delay.
        Integer burstCalSysDelay;
	
	// Relative signal strength for the return (unit of measure determined by individual stations).
	Integer signalStrength;
	
	// Angle origin indicator - source of Az,E; angle values:
	// 0: Unknown
	// 1: Computed (from range)
	// 2: Command angles - predicted angles with refraction correction and crew biases, if any, applied
	// 3: Measured angles - encoder readings with mount model corrections removed to give actual azimuth and elevation as affected by refraction	
        Integer angleOriginIndicator;
	
	// Azimuth angle in units of degrees, 
	// using local reference system (north 0, east = 90)
        Real az;
	
	// Elevation angle in units of degree,
	// using local reference system (zenith = 90)
        Real el;

	
    };
    
    // The description and background info for the TLE variables come 
    // from Tom Kelecy's website http://www.celestrak.com
    struct tle_obtype {
	
	// The NORAD satellite Catalog number is a unique idenitifier
	// assigned by NORAD for each earth-orbiting satellite. This
	// number must be identical on each line of a TLE to be valid.
	Integer satnum;
	
        // Typeically "U" but may be other characters as well
	std::string securityClassification;
	
	// The International Designator is an additional unique identifier
	// assigned by the World Data Center-A for Rockets and Satellites (WDC-A-R&S)
	// in accordance with international treaty (1975 Convention on 
	// Registration of Objects Launched into Outer Space). The WDC-A-R&S 
	// works together with NORAD and NASA's National Space Science Data 
	// Center (NSSDC) in maintaining this registry. Although there have 
	// been some changes in format since it was first used back in the late 
	// 1950s (see "Space Surveillance" in Satellite Times Volume 4 Number 1), 
	// the International Designator indicates the year of the launch 
	// (field 1.4 only gives the last two digits), the launch of that year 
	// (field 1.5), and the piece of that launch (field 1.6) for each 
	// object. These three fields can be left blank, but all must be 
	// present if any is. there are some significant differences between 
	// NORAD's Catalog Number and the International Designator. For example, 
	// NORAD assigns a catalog number based upon when the object was first 
	// observed, whereas the International Designator is always tied to 
	// the original launch.
	std::string intlDesignator;
	//Integer intlLaunchYear;
	//Integer intlLaunchNum;
	//std::string intlLaunchPiece;
	
	// The next two fields (Year and DayOfYear) together define the 
	// reference time for the element set and are jointly referred to as 
	// the epoch. The epoch defines the time to which all of the time-varying 
	// fields in the element set are referenced. How is the epoch time 
	// format interpreted? This question is best answered by using an 
	// example. An epoch of 98001.00000000 corresponds to 0000 UT on 
	// 1998 January 01—in other words, midnight between 1997 December 31 
	// and 1998 January 01. An epoch of 98000.00000000 would actually 
	// correspond to the beginning of 1997 December 31—strange as that might 
	// seem. Note that the epoch day starts at UT midnight (not noon) and 
	// that all times are measured mean solar rather than sidereal time units.
	Integer epochYear;
	Real epochDayOfYear;
	
	// The first derivative of the mean motion divided by two, 
	// in units of revolutions per day
	Real ndotby2;
	
	// The second derivative of the mean motion divided by six, 
	// in units of revolutions per day
	Real nddotby6;
	
	// B* (BSTAR), which is an SGP4-type drag coefficient. In aerodynamic 
	// theory, every object has a ballistic coefficient, B, that is the 
	// product of its coefficient of drag, CD, and its cross-sectional area,
	// A, divided by its mass, m.
	//                         B = CD A/m 
	// The ballistic coefficient represents how susceptible an object is to 
	// drag—the higher the number, the more susceptible. B* is an adjusted 
	// value of B using the reference value of atmospheric density, ρo.
	//		           B* = B ρo/2
	// B* has units of (earth radii)-1
	Real bstar;
	
	// The ephemeris type (i.e., orbital model) used to generate the data. 
	// Spacetrack Report Number 3 suggests the following assignments: 
	//	1=SGP,
	//	2=SGP4, 
	//	3=SDP4, 
	//	4=SGP8, 
	//	5=SDP8. 
	// However, this value is used for internal analysis only—all 
	// distributed element sets have a value of zero and are generated 
	// using the SGP4/SDP4 orbital model (as appropriate).
	Integer ephemerisType;
	
	// the element set number. Normally, this number is incremented each 
	// time a new element set is generated. In practice, however, this 
	// doesn't always happen. When operations switch between the primary 
	// and backup Space Control Centers, sometimes the element set numbers 
	// get out of sync, with some numbers being reused and others skipped. 
	// Unfortunately, this makes it difficult to tell if you have all the 
	// element sets for a particular object.
	Integer elementNum;
	
	// The inclination measured in degrees [0 - 180]
	Real inclination;
	
	// The right ascension of the ascending node measured in degrees [0-360]
	Real raan;
	
	// The orbit eccentricity, a unitless quantity
	Real eccentricity;
	
	// The argument of perigee measured in degrees [0-360]
	Real argPerigee;
	
	// The mean anomaly measured in degrees [0-360]
	Real meanAnomaly;
	
	// The mean motion measure in revolutions per day
	Real meanMotion;
	
	// The revolution number at epoch
	Integer revolutionNum;
	
    };

    // The B3 observation type specification
    struct b3_obtype {      
    
        // Possible obtype values and their meaning
        // 0 - Range rate only
        // 1 - Azimuth and elevation
        // 2 - Range, azimuth and elevation
        // 3 - Range, azimuth, elevation, and range rate
        // 4 - Range, azimuth, eelcation, and range rate 
        //    (extra measurements for azimuth rate, elevation rate, etc are ignored)
        // 5 - Right Ascension and Declination
        // 6 - Range only
        // 8 - Azimuth, elevation, sometimes range and ECF position of the sensor
        // 9 - Right ascension, declination, sometimes range and 
        //     ECF position of the sensor
        //
        // The variables below will only be specified according to the b3Type
        // otherwise they will be NULL valued.

        Integer b3Type;
        
        // Typeically "U" but may be other characters as well
        std::string securityClassification;
        
        // SSSSS integer satellite ID number corresonding to the SSC number
        Integer satelliteID;
        
        // sss integer sensor ID
        Integer sensorID;
        
        // YY year (assumes 20YY if Y <= 50, 19YY if YY > 50)
        Integer year;
        
        // DDD Day of year where January 1st is day 1.
        Integer dayOfYear;
        
        // Hours, minutes, and seconds of observation
        Integer hour;
        Integer minute;
        Real seconds;
        
        // Elevation, declination are specified in degrees.
        Real elevation;
        Real declination;
	
	// Right Ascension specified in hours
        Real rightAscension;
        
        // Azimuth is specified in degrees and is always positive.
        Real azimuth;
        
        //  Range in km
        Real range;
        
        // Slant range rate in km/s
        Real rangeRate;
        
        // Earth Centered Fixed (ECF) sensor position in kilometers
        Real ecf_X;
        Real ecf_Y;
        Real ecf_Z;
    
    };


}



#endif	/* _Dataformats_hpp */

