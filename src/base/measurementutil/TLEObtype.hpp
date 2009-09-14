/* 
 * File:   TLEFormatDescription.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 5:00 AM
 */

#ifndef _TLEFORMATDESCRIPTION_HPP
#define	_TLEFORMATDESCRIPTION_HPP

#include "Obtype.hpp"

// The description and background info for the TLE variables come 
// from Tom Kelecy's website http://www.celestrak.com
class TLEObtype : public Obtype
{

public :

    // default constructor
    TLEObtype();
    // copy constructor
    TLEObtype(const TLEObtype &tleOb);
    // operator =
    const TLEObtype& operator=(const TLEObtype &tleOb);
    // destructor
    virtual ~TLEObtype();

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
   
    enum DATATYPE_REPS
    {
	BSTAR_ID,
	INCLINATION_ID,
	RAAN_ID,
	ECCENTRICITY_ID,
	ARGPERIGEE_ID,
	MEANANOMALY_ID,
	MEANMOTION_ID,
	EndTLETypeReps    
    };
    
    enum TIMESYSTEM_REPS
    {
	UT_ID,
	EndTLETimeReps
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
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real     GetRealDataParameter(const Integer id) const;
    Real     GetRealDataParameter(const std::string &label) const;
    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    friend std::ostream& operator<< (std::ostream &output, const TLEObtype *myTLE);
    
    // Declare DataFile a friend class so that we have access
    // directly to variables instead of having to use Get/Set
    friend class ProcessTLEDataFile;      
    
protected:
    
    static const std::string DATATYPE_DESCRIPTIONS[EndTLETypeReps];
    static const std::string TIMESYSTEM_DESCRIPTIONS[EndTLETimeReps];
    
    static const bool TLE_IS_REQUIRED[EndTLEDataReps];
    static const Gmat::ParameterType TLE_PARAMETER_TYPE[EndTLEDataReps];
    static const std::string TLE_UNIT_DESCRIPTIONS[EndTLEDataReps];
    static const std::string TLE_FILEFORMAT_DESCRIPTIONS[EndTLEDataReps];    

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
    // 1998 January 01‚Äîin other words, midnight between 1997 December 31 
    // and 1998 January 01. An epoch of 98000.00000000 would actually 
    // correspond to the beginning of 1997 December 31‚Äîstrange as that might 
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
    // drag‚Äîthe higher the number, the more susceptible. B* is an adjusted 
    // value of B using the reference value of atmospheric density, œÅo.
    //		           B* = B œÅo/2
    // B* has units of (earth radii)-1
    Real bstar;

    // The ephemeris type (i.e., orbital model) used to generate the data. 
    // Spacetrack Report Number 3 suggests the following assignments: 
    //	1=SGP,
    //	2=SGP4, 
    //	3=SDP4, 
    //	4=SGP8, 
    //	5=SDP8. 
    // However, this value is used for internal analysis only‚Äîall 
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

#endif	/* _TLEFORMATDESCRIPTION_HPP */

