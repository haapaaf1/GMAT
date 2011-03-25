/* 
 * File:   SP3cFormatDescription.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 5:03 AM
 */

#ifndef _SP3CFORMATDESCRIPTION_HPP
#define	_SP3CFORMATDESCRIPTION_HPP

#include "Obtype.hpp"

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

class SP3cObtype : public Obtype
{
    
public :

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

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);    
    
    // Measurement Data Access functions
    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
    Integer     GetFileTypeID(const std::string &str) const;
    Integer     GetTimeSystemID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    bool     GetBoolDataParameter(const Integer id) const;
    bool     GetBoolDataParameter(const std::string &label) const;
    Real     GetRealDataParameter(const Integer id) const;
    Real     GetRealDataParameter(const std::string &label) const;
    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
    IntegerArray     GetIntegerArrayDataParameter(const Integer id) const;
    IntegerArray     GetIntegerArrayDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    enum DATATYPE_REPS
    {
	GPSONLY_ID = EndDataTypeReps,
        MIXED_ID,
        GLONASSONLY_ID,
	LEOONLY_ID,
	GALILEOONLY_ID,
	EndSP3cTypeReps
    };

    enum TIMESYSTEM_REPS
    {
	GPSTIME_ID = EndTimeReps,
        GLONASSUTC_ID,
        GALILEOSYSTEMTIME_ID,
	TAI_ID,
	UTC_ID,
	EndSP3cTimeReps
    };

protected:

    static const std::string SP3c_DATATYPE_DESCRIPTIONS[EndSP3cTypeReps];
    static const std::string SP3c_TIME_DESCRIPTIONS[EndSP3cTimeReps];
    
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

#endif	/* _SP3CFORMATDESCRIPTION_HPP */

