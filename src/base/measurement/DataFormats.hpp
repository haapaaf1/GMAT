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
 
    // See the following website for a complete description of the SP3c
    // data format. Comments below are taken directly from the format
    // description on the website.
    // http://igscb.jpl.nasa.gov/components/formats.html
    // The SP3c data format has space to add additional parameters in the
    // future. At this time, the C++ structure defined below does not contain
    // variables to store the blank data. If the SP3c format does change in the
    // future, then this structure will need to be updated to handle the
    // additional information.
    struct sp3c_header
    {
        // The Position Record Flag, P, in line one indicates that no
        // velocities are included.  The Velocity Record Flag, V, in
        // line one indicates that at each epoch, for each satellite,
        // an additional satellite velocity and clock rate-of-change
        // has been computed. Here, if V is present, we set velFlag to true,
        // otherwise velFlag = false indicates the position flag was set.
        bool velFlag;

        // These two flags are not part of the original data format but
        // are created to make it easy to test for the presence of optional
        // position and velocity correlation records.
        bool epFlag, evFlag;

        // Start Year, Month, Day of Month, Hour, Minute, Second
        // This is the Gregorian date and time of day of the first
        // epoch of the orbit.
        Integer startYear, startMonth, startDay, startHour, startMinute;
        Real startSeconds;

        // Number of Epochs (up to 10 million)
        long int numEpochs;

        // Data Used Descriptor (primarily used by the agency
        // that generated the data. A suggested convention is listed here:
        // u  -- differenced carrier phase
        // du -- change in u with time
        // s  -- 2-receiver/1-satellite carrier phase
        // ds -- change in s with time
        // d  -- 2-receiver/2-satellite carrier phase
        // dd -- change in d with time
        // U  -- undifferenced code phase
        // dU -- change in U with time
        // S  -- 2-receiver/1-satellite code phase
        // dS -- change in S with time
        // D  -- 2-receiver/2-satellite code phase
        // dD -- change in D with time
        // + -- type separator
        // Combinations such as "u+U" seem reasonable. Complex combinations
        // of data types could use "mixed" where mixed would be explained on
        // the comment lines.
        std::string dataUsed;

        // Coordinate System
        // Documentation does not clarify this variable but it appears to
        // be ITRyy where yy is a year (e.g. ITR97, ITR00, etc)
        std::string coordSystem;

        // Orbit Type (only 4 currently defined)
        // FIT - fitted
        // EXT - extrapolated or predicted
        // BCT - broadcast
        // HLM - fitted after applying a Helmert transformation
        std::string orbitType;

        // Agency
        std::string agency;

        // GPS Week, seconds of week ( 0.0 <= secondsOfWeek < 604800.0 )
        Integer gpsWeek;
        Real secondsOfWeek;

        // Epoch Interval ( 0.0 < epoch interval < 100000.0 ) in seconds
        Real epochInterval;

        // The Starting Mod Julian Day, fractional day
        // 44244 represents GPS zero time - Jan 6, 1980
        // 0.0 <= fractionOfDay < 1.0
        Integer modJulianDay;
        Real fractionOfDay;

        // Number of satellites in this file
        Integer numSats;

        // Satellite ID's in this file which may be listed in any order but
        // should be in alpha/numerical order of use for ease of viewing
        // The value 0 should only appear after all ID's have been listed.
        // Each identifier will consist of a letter followed by a 2-digit
        // integer between 01 and 99.  For example, "Gnn" for GPS satellites,
        // "Rnn" for GLONASS satellites, "Lnn" for Low-Earth Orbiting (LEO)
        // satellites, and "Enn" for Galileo satellites.  Other letters will
        // be allowed for other types of satellites.  Lower numbered satellites
        // must always have a preceding zero (e.g., "G09" not "G 9").  The
        // letter, which represents the Satellite System Indicator, must always
        // be present (i.e.," 09" is no longer a valid satellite identifier).
        // This is a significant change from SP3-a and needs to be noted when
        // software is updated to read the new SP3-c format.  A list of
        // identifiers created for LEO satellites can be viewed at
        // http://cddis.gsfc.nasa.gov/sp3c_satlist.html
        StringArray satIdList;

        // Satellite accuracy
        // The value 0 is interpreted as accuracy unknown. A satellite's accuracy
        // exponent appears in the same slot on lines 8-12 as the identifier
        // on lines 3-7.  The accuracy is computed from the exponent as in the
        // following example.  If the accuracy exponent is 13, the accuracy
        // is 2**13 mm or ~ 8 m. The quoted orbital error should represent one
        // standard deviation and be based on the orbital error in the entire
        // file for the respective satellite. This may lead to some distortion
        // when orbit files are joined together, or when a file contains both
        //observed and predicted data.
        IntegerArray satAccuracyList;

        // File Type
        // The currently defined values are: "G " for GPS only files, "M "
        // for mixed files, "R " for GLONASS only files, "L " for LEO only
        // files, and "E " for Galileo only files. No default values are
        // implied; either "G ", "M ", "R ", "L ", or "E " is required.
        Integer fileType;

        // Time System
        // This field specifies the time system used in each SP3-c file: use
        // "GPS" to identify GPS Time, "GLO" to identify the GLONASS UTC time
        // system, "GAL" to identify Galileo system time, "TAI" to identify
        // International Atomic Time, or "UTC" to identify Coordinated Universal
        // Time.  No default value is implied; either "GPS", "GLO", "GAL", "TAI,
        // or "UTC" must be specified.
        Integer timeSystem;

        // Base value for pos/vel standard deviation (base^n)
        // where n is provided in the individual data record
        // mm or 10^-4 mm/sec
        Real basePosVelStdDev;

        // Base value for clk/rate standard deviation (base^n)
        // where n is provided in the individual data record
        // psec or 10^-4 psec/sec
        Real baseClkRateStdDev;

        // Comment lines
        StringArray comments;

    };

    struct sp3c_position
    {
        // Vehicle ID
        std::string vehicleID;

        // X,Y,Z coordinates (km)
        // The positional values are preceise to 1 mm. Bad or absent position
        // values are to be set to 0.0.
        Real x, y, z;

        // Clock value (micro seconds)
        // The clock values are accurate to 1 picosecond. Bad or absent clock
        // values should be set to 999999.999999 with the six integer 9's
        // being required and the fractional 9's being optional.
        Real clockValue;

        // Clock rate-of-change value (10^-4 microseconds/second)
        // These values are precise to 10^-16 seconds/second. Bad or absent
        // values are set to 999999.99999 with the six integer 9's
        // being required and the fractional 9's being optional.

        // X,Y,Z standard deviations (base^n mm)
        Real stdDevX, stdDevY, stdDevZ;

        // Clock standard deviation (base^n pico seconds)
        Real stdDevClock;

        // Clock event flag
        // This flag is true when a discontinuity in the satellite clock
        // correction is present sometime between the previous and the current
        // epoch or at the current epoch.
        bool clockEventFlag;

        // Clock prediction flag
        // This flag is true when the clock correction is predicted and false
        // when the clock correction is observed.
        bool clockPredictionFlag;

        // Maneuver flag
        // This flag is true if a maneuver occured sometime between the previous
        // and the current epoch or at the current epoch. As an example,
        // if a certain maneuver lasted 50 minutes (a satellite changing orbital
        // planes) then these M-flags could conceivably appear at five separate
        // 15-minute orbit epochs.  If the maneuver started at 11h 14m and
        // lasted to 12h 04m, M-flags would appear for the epochs 11:15, 11:30,
        // 11:45, 12:00 and 12:15.  A maneuver is loosely defined as any planned
        // or humanly-detectable thruster firing that changes the orbit of a
        // satellite.  When this flag is false it means either no maneuver
        // occurred, or it is unknown whether any maneuver occurred.
        bool maneuverFlag;

        // Orbit prediction flag
        // When this flag is true, it means that the satellite position at this
        // epoch is predicted. When this flag is false, it means the satellite
        // position at this epoch is observed.
        bool orbitPredictFlag;

    };

    struct sp3c_velocity
    {
        // Vehicle ID
        std::string vehicleID;

        // VX, VY, VZ velocity components (decimeters/second)
        // These values are precise to 10^-4 mm/second. Bad or absent velocity
        // values are to be set to 0.0.
        Real vx, vy, vz;

        // Clock rate-of-change value (10^-4 microseconds/second)
        // These values are precise to 10^-16 seconds/second. Bad or absent
        // values are set to 999999.99999 with the six integer 9's
        // being required and the fractional 9's being optional.
        Real clockRateOfChange;

        // VX,VY,VZ standard deviations (base^n 10^-4 mm/sec)
        Real stdDevVX, stdDevVY, stdDevVZ;

        // Clock rate standard deviation (base^n 10^-4 picoseconds/sec)
        Real stdDevClockRate;

    };

    struct sp3c_posClockCorrelation
    {

        // X,Y,Z Std Dev (10^-4 mm)
        // The standard deviations in this record are given to greater
        // resolution than the approximate values given in the Position and
        // Clock Record. A value of 9999 would mean that a standard deviation
        // was too large to be represented. If a standard deviation is unknown,
        // its field is left blank.
        Real highResolutionStdDevX;
        Real highResolutionStdDevY;
        Real highResolutionStdDevZ;

        // Clock Std Dev (10^-4 picoseconds)
        // A value of 9999999 would mean that the standard deviation was too
        // large to be represented.
        Real highResolutionStdDevClock;

        // XVY, XVZ, XC, YVZ, YC, ZC correlation
        // The correlation values are between -0.999999 and +0.9999999
        Real xYCorrelation;
        Real xZCorrelation;
        Real xCCorrelation;
        Real yZCorrelation;
        Real yCCorrelation;
        Real zCCorrelation;

    };

    struct sp3c_velClockRateCorrelation
    {

        // VX,VY,VZ Std Dev (10^-4 mm/sec)
        // The standard deviations in this record are given to greater
        // resolution than the approximate values given in the Velcoity and
        // Clock Rate Record. A value of 9999 would mean that a standard deviation
        // was too large to be represented. If a standard deviation is unknown,
        // its field is left blank.
        Real highResolutionStdDevVX;
        Real highResolutionStdDevVY;
        Real highResolutionStdDevVZ;

        // Clock Rate Std Dev (10^-4 picoseconds/sec)
        Real highResolutionStdDevClockRate;

        // VXVY, VXVZ, VXC, VYVZ, VYC, VZC correlation
        // The correlation values are between -0.999999 and +0.9999999
        Real vxVYCorrelation;
        Real vxVZCorrelation;
        Real vxCCorrelation;
        Real vyVZCorrelation;
        Real vyCCorrelation;
        Real vzCCorrelation;

    };

    struct sp3c_obtype
    {

        // Iterator Pointer to the header record
        std::vector<sp3c_header*>::iterator headerVectorIndex;

        // Start Year, Month, Day of Month, Hour, Minute, Second
        // This is the Gregorian date and time of day of the first
        // epoch of the orbit.
        Integer year;
        Integer month;
        Integer day;
        Integer hour;
        Integer minute;
        Real seconds;
        
        std::vector<sp3c_position*> position;
        std::vector<sp3c_posClockCorrelation*> posClockCorrelation;
        std::vector<sp3c_velocity*> velocity;
        std::vector<sp3c_velClockRateCorrelation*> velClockRateCorrelation;
        
    };

    enum SP3c_DATA_REPS
    {
        SP3c_POSVELFLAG_ID,
        SP3c_STARTYEAR_ID,
        SP3c_STARTMONTH_ID,
        SP3c_STARTDAY_ID,
        SP3c_STARTHOUR_ID,
        SP3c_STARTMINUTE_ID,
        SP3c_STARTSECOND_ID,
        SP3c_NUMEPOCHS_ID,
        SP3c_DATAUSED_ID,
        SP3c_COORDSYS_ID,
        SP3c_ORBITTYPE_ID,
        SP3c_AGENCY_ID,
        SP3c_GPSWEEK_ID,
        SP3c_SECONDSOFWEEK_ID,
        SP3c_EPOCHINTERVAL_ID,
        SP3c_MODJULIANDAY_ID,
        SP3c_FRACTIONOFDAY_ID,
        SP3c_NUMSATS_ID,
        SP3c_SATIDLIST_ID,
        SP3c_SATACCURACYLIST_ID,
        SP3c_FILETYPE_ID,
        SP3c_TIMESYSTEM_ID,
        SP3c_BASEPOSVELSTDDEV_ID,
        SP3c_BASECLKRATESTDDEV_ID,
        SP3c_COMMENTS_ID,
        SP3c_VEHICLEID_ID,
        SP3c_X_ID,
        SP3c_Y_ID,
        SP3c_Z_ID,
        SP3c_VX_ID,
        SP3c_VY_ID,
        SP3c_VZ_ID,
        SP3c_CLOCKVALUE_ID,
        SP3c_STDDEV_X_ID,
        SP3c_STDDEV_Y_ID,
        SP3c_STDDEV_Z_ID,
        SP3c_STDDEV_VX_ID,
        SP3c_STDDEV_VY_ID,
        SP3c_STDDEV_VZ_ID,
        SP3c_STDDEV_CLOCK_ID,
        SP3c_STDDEV_CLOCKRATE_ID,
        SP3c_CLOCKEVENTFLAG_ID,
        SP3c_CLOCKPREDICTIONFLAG_ID,
        SP3c_MANEUVERFLAG_ID,
        SP3c_ORBITPREDICTFLAG_ID,
        SP3c_XY_CORRELATION_ID,
        SP3c_XZ_CORRELATION_ID,
        SP3c_XC_CORRELATION_ID,
        SP3c_YZ_CORRELATION_ID,
        SP3c_YC_CORRELATION_ID,
        SP3c_ZC_CORRELATION_ID,
        SP3c_VXVY_CORRELATION_ID,
        SP3c_VXVZ_CORRELATION_ID,
        SP3c_VXC_CORRELATION_ID,
        SP3c_VYVZ_CORRELATION_ID,
        SP3c_VYC_CORRELATION_ID,
        SP3c_VZC_CORRELATION_ID,
        SP3c_HIGHRESOLUTION_STDDEV_X_ID,
        SP3c_HIGHRESOLUTION_STDDEV_Y_ID,
        SP3c_HIGHRESOLUTION_STDDEV_Z_ID,
        SP3c_HIGHRESOLUTION_STDDEV_VX_ID,
        SP3c_HIGHRESOLUTION_STDDEV_VY_ID,
        SP3c_HIGHRESOLUTION_STDDEV_VZ_ID,
        SP3c_HIGHRESOLUTION_STDDEV_CLOCK_ID,
        SP3c_HIGHRESOLUTION_STDDEV_CLOCKRATE_ID,
        SP3c_YEAR_ID,
        SP3c_MONTH_ID,
        SP3c_DAY_ID,
        SP3c_HOUR_ID,
        SP3c_MINUTE_ID,
        SP3c_SECOND_ID,
        EndSP3cDataReps
    };

    static const Gmat::ParameterType SP3c_PARAMETER_TYPE[EndSP3cDataReps] =
    {
        Gmat::BOOLEAN_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::STRING_TYPE,
        Gmat::STRING_TYPE,
        Gmat::STRING_TYPE,
        Gmat::STRING_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::STRINGARRAY_TYPE,
        Gmat::INTARRAY_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::STRINGARRAY_TYPE,
        Gmat::STRING_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
    };

    static const std::string SP3c_FILEFORMAT_DESCRIPTIONS[EndSP3cDataReps] =
    {
        "PosVelFlag",
        "StartYear",
        "StartMonth",
        "StartHour",
        "StartMinute",
        "StartSecond",
        "NumEpochs",
        "DataUsed",
        "CoordSys",
        "OrbitType",
        "Agency",
        "GpsWeek",
        "SecondsOfWeek",
        "EpochInterval",
        "ModJulianDay",
        "FractionOfDay",
        "NumSats",
        "SatIdList",
        "satAccuracyList",
        "FileType",
        "TimeSystem",
        "BasePosVelStdDev",
        "BaseClkRateStdDev",
        "Comments",
        "VehicleID",
        "X",
        "Y",
        "Z",
        "VX",
        "VY",
        "VZ",
        "ClockValue",
        "StdDevX",
        "StdDevY",
        "StdDevZ",
        "StdDevVX",
        "StdDevVY",
        "StdDevVZ",
        "StdDevClock",
        "StdDevClockRate",
        "ClockEventFlag",
        "ClockPredictionFlag",
        "ManeuverFlag",
        "OrbitPredictFlag",
        "XYCorrelation",
        "XZCorrelation",
        "XCCorrelation",
        "YZCorrelation",
        "YCCorrelation",
        "ZCCorrelation",
        "VXVYCorrelation",
        "VXVZCorrelation",
        "VXCCorrelation",
        "VYVZCorrelation",
        "VYCCorrelation",
        "VZCCorrelation",
        "HighResolutionStdDevX",
        "HighResolutionStdDevY",
        "HighResolutionStdDevZ",
        "HighResolutionStdDevVX",
        "HighResolutionStdDevVY",
        "HighResolutionStdDevVZ",
        "HighResolutionStdDevClock",
        "HighResolutionStdDevClockRate",
        "Year",
        "Month",
        "Hour",
        "Minute",
        "Second"
    };

    static const std::string SP3c_UNIT_DESCRIPTIONS[EndSP3cDataReps] =
    {
        "",
        "years",
        "months",
        "hrs",
        "min",
        "sec",
        "",
        "",
        "",
        "",
        "",
        "",
        "sec",
        "sec",
        "",
        "FractionOfDay",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "km",
        "km",
        "km",
        "dm/sec",
        "dm/sec",
        "dm/sec",
        "microsec",
        "mm",
        "mm",
        "mm",
        "mm/sec",
        "mm/sec",
        "mm/sec",
        "picosec",
        "picosec/sec",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "years",
        "months",
        "hrs",
        "min",
        "sec"
    };

    // See the following website for a complete description of the SLR
    // data format. The comments below are taken directly from the ILRS website.
    // http://ilrs.gsfc.nasa.gov/products_formats_procedures/normal_point/np_format.html    
    struct slr_header
    {
	
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

    struct slr_obtype
    {

        // Iterator Pointer to the header record
        std::vector<slr_header*>::iterator headerVectorIndex;

	
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

    enum SLR_DATA_REPS
    {
	SLR_TYPE_ID,
	SLR_ILRSSATNUM_ID,
	SLR_YEAR_ID,
	SLR_DAYOFYEAR_ID,
	SLR_CDPPADID_ID,
	SLR_CDPSYSNUM_ID,
	SLR_CDPOCCUPANCYSEQUENCENUM_ID,
	SLR_WAVELENGTH_ID,
	SLR_CALSYSDELAY_ID,
	SLR_CALDELAYSHIFT_ID,
	SLR_RMSSYSDELAY_ID,
	SLR_NORMALPOINTWINDOWINDICATOR_ID,
	SLR_EPOCHTIMESCALEINDICATOR_ID,
	SLR_SYSCALMETHODINDICATOR_ID,
	SLR_SCHINDICATOR_ID,
	SLR_SCIINDICATOR_ID,
	SLR_PASSRMS_ID,
	SLR_DATAQUALASSESSMENTINDICATOR_ID,
	SLR_FORMATREVISIONNUM_ID,
	SLR_TIMEOFLASERFIRING_ID,
	SLR_TWOWAYTIMEOFFLIGHT_ID,
	SLR_BINRMSRANGE_ID,
	SLR_SURFACEPRESSURE_ID,
	SLR_SURFACETEMP_ID,
	SLR_RELATIVEHUMIDITY_ID,
	SLR_NUMRAWRANGES_ID,
	SLR_DATARELEASEFLAG_ID,
	SLR_RAWRANGEFACTOR_ID,
	SLR_NORMALPOINTWINDOWINDICATOR2_ID,
	SLR_SIGNALTONOISERATIO_ID,
        SLR_BURSTCALSYSDELAY_ID,
	SLR_SIGNALSTRENGTH_ID,
        SLR_ANGLEORIGININDICATOR_ID,
        SLR_AZIMUTH_ID,
        SLR_ELEVATION_ID,
        EndSLRDataReps
    };

    static const std::string SLR_FILEFORMAT_DESCRIPTIONS[EndSLRDataReps] =
    {
	"SlrType",
	"IlrsSatnum",
	"Year",
	"DayOfYear",
	"CdpPadID",
	"CdpSysNum",
	"CdpOccupancySequenceNum",
	"Wavelength",
	"CalSysDelay",
	"CalDelayShift",
	"RmsSysDelay",
	"NormalPointWindowIndicator",
	"EpochTimeScaleIndicator",
	"SysCalMethodIndicator",
	"SchIndicator",
	"SciIndicator",
	"PassRMS",
	"DataQualAssessmentIndicator",
	"FormatRevisionNum",
	"TimeOfLaserFiring",
	"TwoWayTimeOfFlight",
	"BinRMSRange",
	"SurfacePressure",
	"SurfaceTemp",
	"RelativeHumidity",
	"NumRawRanges",
	"DataReleaseFlag",
	"RawRangeFactor",
	"NormalPointWindowIndicator2",
	"SignalToNoiseRatio",
        "BurstCalSysDelay",
	"SignalStrength",
        "AngleOriginIndicator",
        "Azimuth",
        "Elevation"
    };

    static const std::string SLR_UNIT_DESCRIPTIONS[EndSLRDataReps] =
    {
	"",
	"",
	"years",
	"DayOfYear",
	"",
	"",
	"",
	"nm",
	"picosec",
	"picosec",
	"picosec",
	"",
	"",
	"",
	"",
	"",
	"picosec",
	"",
	"",
	"sec",
	"sec",
	"picosec",
	"millibar",
	"Kelvin",
	"%",
	"",
	"",
	"",
	"",
	"",
        "",
	"",
        "",
        "deg",
        "deg"
    };

    static const Gmat::ParameterType SLR_PARAMETER_TYPE[EndSLRDataReps] =
    {
	Gmat::INTEGER_TYPE,
	Gmat::STRING_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE
    };

    // The description and background info for the TLE variables come 
    // from Tom Kelecy's website http://www.celestrak.com
    struct tle_obtype
    {
	
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

    enum TLE_DATA_REPS
    {
	TLE_SATNUM_ID,
	TLE_SECURITYCLASSIFICATION_ID,
	TLE_INTLDESIGNATOR_ID,
	TLE_EPOCHYEAR_ID,
	TLE_EPOCHDAYOFYEAR_ID,
	TLE_NDOTBY2_ID,
	TLE_NDDOTBY6_ID,
	TLE_BSTAR_ID,
	TLE_EPHEMERISTYPE_ID,
	TLE_ELEMENTNUM_ID,
	TLE_INCLINATION_ID,
	TLE_RAAN_ID,
	TLE_ECCENTRICITY_ID,
	TLE_ARGPERIGEE_ID,
	TLE_MEANANOMALY_ID,
	TLE_MEANMOTION_ID,
	TLE_REVOLUTIONNUM_ID,
        EndTLEDataReps
    };

    static const std::string TLE_FILEFORMAT_DESCRIPTIONS[EndTLEDataReps] =
    {
	"Satnum",
	"SecurityClassification",
	"IntlDesignator",
	"EpochYear",
	"EpochDayOfYear",
	"Ndotby2",
	"Nddotby6",
	"Bstar",
	"EphemerisType",
	"ElementNum",
	"Inclination",
	"Raan",
	"Eccentricity",
	"ArgPerigee",
	"MeanAnomaly",
	"MeanMotion",
	"RevolutionNum"
    };

    static const std::string TLE_UNIT_DESCRIPTIONS[EndTLEDataReps] =
    {
	"",
	"",
	"",
	"years",
	"DayOfYear",
	"rev/sec^2",
	"rad/sec^3",
	"1/EarthRadii",
	"",
	"",
	"deg",
	"deg",
	"",
	"deg",
	"deg",
	"rev/day",
	""
    };

    static const Gmat::ParameterType TLE_PARAMETER_TYPE[EndTLEDataReps] =
    {
	Gmat::INTEGER_TYPE,
	Gmat::STRING_TYPE,
	Gmat::STRING_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE
    };

    // The B3 observation type specification
    struct b3_obtype
    {
    
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

    enum B3_DATA_REPS
    {
        B3_TYPE_ID,
        B3_SECURITYCLASSIFICATION_ID,
        B3_SATELLITE_ID,
        B3_SENSORID_ID,
        B3_YEAR_ID,
        B3_DAYOFYEAR_ID,
        B3_HOUR_ID,
        B3_MINUTE_ID,
        B3_SECONDS_ID,
        B3_ELEVATION_ID,
        B3_DECLINATION_ID,
        B3_RIGHTASCENSION_ID,
        B3_AZIMUTH_ID,
        B3_RANGE_ID,
        B3_RANGERATE_ID,
        B3_ECFX_ID,
        B3_ECFY_ID,
        B3_ECFZ_ID,
        EndB3DataReps
    };

    static const std::string B3_FILEFORMAT_DESCRIPTIONS[EndB3DataReps] =
    {
        "B3Type",
        "SecurityClassification",
        "SatelliteID",
        "SensorID",
        "Year",
        "DayOfYear",
        "Hour",
        "Minute",
        "Seconds",
        "Elevation",
        "Declination",
        "RightAscension",
        "Azimuth",
        "Range",
        "RangeRate",
        "Ecf_X",
        "Ecf_Y",
        "Ecf_Z"
    };

    static const std::string B3_UNIT_DESCRIPTIONS[EndB3DataReps] =
    {
        "",
        "",
        "",
        "",
        "year",
        "DayOfYear",
        "hrs",
        "min",
        "sec",
        "deg",
        "deg",
        "deg",
        "deg",
        "km",
        "km/sec",
        "km",
        "km",
        "km"
    };


    static const Gmat::ParameterType B3_PARAMETER_TYPE[EndB3DataReps] =
    {
        Gmat::INTEGER_TYPE,
        Gmat::STRING_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
    };


    struct rinex_gpsob_header
    {
        std::string rinexVersion;
        std::string fileType;
        std::string satSystem;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        std::string antennaMarkerName;
        std::string antennaMarkerNumber;
        std::string observerName;
        std::string agencyName;
        std::string receiverNumber;
        std::string receiverType;
        std::string receiverVersion;
        std::string antennaNumber;
        std::string antennaType;
        Real antennaHeight;
        Real antennaEccentricitiesEast;
        Real antennaEccentricitiesNorth;
        Integer defaultWavelengthFactorL1;
        Integer defaultWavelengthFactorL2;
        IntegerArray specificWavelengthFactorL1;
        IntegerArray specificWavelengthFactorL2;
        StringArray satPRNForSpecificWaveFactors;
        Integer numObsTypes;
        StringArray obsTypeCodes;
        Real observationInterval;
        Integer firstObYear, firstObMonth, firstObDay;
        Integer firstObHour, firstObMinute;
        Real firstObSeconds;
        std::string timeSystem;
        Integer lastObYear, lastObMonth, lastObDay;
        Integer lastObHour, lastObMinute;
        Real lastObSeconds;
        bool receiverClockOffsetApplied;
        Integer numLeapSecondsSince6JAN1980;
        Integer numSatsInFile;
        // This needs to be fixed... numObs should be a mxn matrix
        StringArray satPRNlist;
        IntegerArray numObs;

    };

    struct rinex_gpsob_obtype
    {
        std::vector<rinex_gpsob_header*>::iterator headerVectorIndex;

        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Integer epochFlag;
        Integer numSatsThisEpoch;
        StringArray satPRNlist;
        Real receiverClockOffset;
        RealArray observation;
        IntegerArray lli;
        IntegerArray signalStrength;

    };

    struct rinex_gpsnav_header
    {

        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        RealArray ionAlpha;
        RealArray ionBeta;
        Real A0, A1;
        Real T;
        Integer W;
        Integer leapSeconds;

    };

    struct rinex_gpsnav_obtype
    {

        Integer satPRN;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Real svClockBias, svClockDrift, svClockDriftRate;
        Real iodeIssueOfData, crs, deltaN, M0;
        Real cuc, e, cus, sqrtA;
        Real toe, cic, capOmega, cis;
        Real i0, crc, omega, capOmegaDot;
        Real iDot,codesOnL2Channel,gpsWeek,l2PDataFlag;
        Real svAccuracy, svHealth, tgd, iodcIssueOfData;
        Real transmissionTimeOfMessage, fitInterval;

    };

    struct rinex_met_header
    {

        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        std::string markerName, markerNumber;
        Integer numObsTypes;
        StringArray obsTypes;
        std::string sensorModel, sensorType;
        Real sensorAccuracy;
        std::string sensorObType;
        Real sensorGeocentricX, sensorGeocentricY, sensorGeocentricZ;
        Real sensorEllipsoidalHeight;
        std::string sensorObType2;

    };

    struct rinex_met_obtype
    {

        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        RealArray metData;

    };

    struct rinex_glonassnav_header
    {

        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        Integer refYear, refMonth, refDay;
        Real sysTimeCorrection;
        Real numLeapSecondsSince6JAN1980;

    };

    struct rinex_glonassnav_obtype
    {

        Integer satNum;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Real svClockBias,svRelativeFrequencyBias;
        Real messageTimeFrame;
        Real x, xDot, xAcc, health;
        Real y, yDot, yAcc, frequencyNum;
        Real z, zDot, zAcc, ageOfOperInfo;

    };

    struct rinex_geonav_header
    {

        std::string rinexVersion;
        std::string fileType;
        std::string programName;
        std::string runByAgency;
        std::string dateOfFileCreation;
        StringArray comments;
        Integer refSysTimeCorrYear, refSysTimeCorrMonth, refSysTimeCorrDay;
        Real geoTimeToUTC;
        Real A0, A1;
        Real T;
        Integer W,U;
        std::string S;
        Integer numLeapSecondsSince6JAN1980;


    };

    struct rinex_geonav_obtype
    {

        Integer satNum;
        Integer epochYear,epochMonth,epochDay,epochHour,epochMinute;
        Real epochSeconds;
        Real svClockBias,svRelativeFrequencyBias;
        Real transmissionTimeofMessage;
        Real x, xDot, xAcc, health;
        Real y, yDot, yAcc, accuracyCode;
        Real z, zDot, zAcc, IODN;

    };

    enum RINEX_2_11_DATA_REPS
    {
        RINEX_2_11_TYPE_ID,
        RINEX_2_11_SECURITYCLASSIFICATION_ID,
        RINEX_2_11_SATELLITE_ID,
        RINEX_2_11_SENSORID_ID,
        RINEX_2_11_YEAR_ID,
        RINEX_2_11_DAYOFYEAR_ID,
        RINEX_2_11_HOUR_ID,
        RINEX_2_11_MINUTE_ID,
        RINEX_2_11_SECONDS_ID,
        RINEX_2_11_ELEVATION_ID,
        RINEX_2_11_DECLINATION_ID,
        RINEX_2_11_RIGHTASCENSION_ID,
        RINEX_2_11_AZIMUTH_ID,
        RINEX_2_11_RANGE_ID,
        RINEX_2_11_RANGERATE_ID,
        RINEX_2_11_ECFX_ID,
        RINEX_2_11_ECFY_ID,
        RINEX_2_11_ECFZ_ID,
        EndRinex211DataReps
    };

    static const std::string RINEX_2_11_FILEFORMAT_DESCRIPTIONS[EndRinex211DataReps] =
    {
        "B3Type",
        "SecurityClassification",
        "SatelliteID",
        "SensorID",
        "Year",
        "DayOfYear",
        "Hour",
        "Minute",
        "Seconds",
        "Elevation",
        "Declination",
        "RightAscension",
        "Azimuth",
        "Range",
        "RangeRate",
        "Ecf_X",
        "Ecf_Y",
        "Ecf_Z"
    };

    static const std::string RINEX_2_11_UNIT_DESCRIPTIONS[EndRinex211DataReps] =
    {
        "",
        "",
        "",
        "",
        "year",
        "DayOfYear",
        "hrs",
        "min",
        "sec",
        "deg",
        "deg",
        "deg",
        "deg",
        "km",
        "km/sec",
        "km",
        "km",
        "km"
    };


    static const Gmat::ParameterType RINEX_2_11_PARAMETER_TYPE[EndRinex211DataReps] =
    {

    };

}

#endif	/* _Dataformats_hpp */


