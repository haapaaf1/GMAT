/* 
 * File:   B3FormatDesciption.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 5:00 AM
 */

#ifndef _B3OBTYPE_HPP
#define	_B3OBTYPE_HPP

#include "ObType.hpp"

// The B3 observation type specification
class B3ObType : public ObType
{
    
public :
    	
    // default constructor
    B3ObType();
    // copy constructor
    B3ObType(const B3ObType &b3Ob);
    // operator =
    const B3ObType& operator=(const B3ObType &b3Ob);
    // destructor
    virtual ~B3ObType();

    GmatBase *Clone() const;

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real    GetRealDataParameter(const Integer id) const;
    Real    GetRealDataParameter(const std::string &label) const;
    Integer    GetIntegerDataParameter(const Integer id) const;
    Integer    GetIntegerDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
        
    bool        IsParameterRequired(const Integer id) const;
    bool        IsParameterRequired(const std::string &label) const;
    
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
    
    enum B3_DATATYPE_REPS
    {
	RANGERATEONLY_ID = 0,
        AZEL_ID,
	RAZEL_ID,
	RAZELRR_ID,
	RAZELRR2_ID,
	RADEC_ID,
	RANGEONLY_ID,
	AZELSENSORPOS_ID = 8,
	RADECSENSORPOS_ID,
	EndB3TypeReps    
    };
    
    enum B3_TIMESYSTEM_REPS
    {
	UTC_ID,
	EndB3TimeReps
    };

    friend std::ostream& operator<< (std::ostream &output, const B3ObType *myB3);
    
    // Declare DataFile a friend class so that we have access
    // directly to variables instead of having to use Get/Set
    friend class B3DataFile;    

protected:

    static const std::string B3_DATATYPE_DESCRIPTIONS[EndB3TypeReps];
    static const std::string B3_TIMESYSTEM_DESCRIPTIONS[EndB3TimeReps];
    static const bool B3_IS_REQUIRED[EndB3DataReps];
    static const Gmat::ParameterType B3_PARAMETER_TYPE[EndB3DataReps];
    static const std::string B3_UNIT_DESCRIPTIONS[EndB3DataReps];
    static const std::string B3_FILEFORMAT_DESCRIPTIONS[EndB3DataReps];

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
    //    ECF position of the sensor
    //
    // The variables below will only be specified according to the b3Type
    // otherwise they will be NULL valued.

    Integer b3Type;
    
    // Typeically "U" but may be other characters as well
    std::string securityClassification;
    
    // SSSSS integer satellite ID number corresonding to the SSC number
    Integer satID;
    
    // sss integer sensor ID
    Integer sensID;
    
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

#endif	/* _B3OBTYPE_HPP */

