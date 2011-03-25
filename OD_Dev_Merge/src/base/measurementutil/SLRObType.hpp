//$Header$
//------------------------------------------------------------------------------
//                             SLRObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/09/03
//
/**
 *
 * This class specifies the SLR observation data type.
 * See the following website for a complete description of the SLR
 * data format. The comments below are taken directly from the ILRS website.
 * http://ilrs.gsfc.nasa.gov/products_formats_procedures/normal_point/np_format.html
 * 
 */
//------------------------------------------------------------------------------


#ifndef _SLROBTYPE_HPP
#define _SLROBTYPE_HPP

#include "ObType.hpp"
#include "SLRHeader.hpp"

class SLRObType : public ObType
{
    
public :

    // default constructor
    SLRObType();
    // copy constructor
    SLRObType(const SLRObType &slrOb);
    // operator =
    const SLRObType& operator=(const SLRObType &slrOb);
    // destructor
    virtual ~SLRObType();

    GmatBase *Clone() const;
    
    enum SLR_DATA_REPS
    {
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

    // Data access functions
    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real     GetRealDataParameter(const Integer id) const;
    Real     GetRealDataParameter(const std::string &label) const;
    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;

    bool SetDataParameter(const Integer id, const Integer &value);
    bool SetDataParameter(const std::string &label, const Integer &value);
    bool SetDataParameter(const Integer id, const Real &value);
    bool SetDataParameter(const std::string &label, const Real &value);

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);
    
    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    enum DATATYPE_REPS
    {
	TWOWAYTIMEOFFLIGHT_ID,
	EndSLRTypeReps    
    };
    
    enum TIMESYSTEM_REPS
    {
	UTC_ID,
	GPS_ID,
	BIPM_ID,
	BIH_ID,
	EndSLRTimeReps
    };
    
    friend std::ostream& operator<< (std::ostream &output, const SLRObType *mySLR);

    // Declare DataFile a friend class so that we have access
    // directly to variables instead of having to use Get/Set
    friend class SLRHeader;
    friend class SLRDataFile;
          
protected:
    
    static const std::string SLR_DATATYPE_DESCRIPTIONS[EndSLRTypeReps];
    static const std::string SLR_TIMESYSTEM_DESCRIPTIONS[EndSLRTimeReps];

    static const bool SLR_IS_REQUIRED[EndSLRDataReps];
    static const Gmat::ParameterType SLR_PARAMETER_TYPE[EndSLRDataReps];
    static const std::string SLR_UNIT_DESCRIPTIONS[EndSLRDataReps];
    static const std::string SLR_FILEFORMAT_DESCRIPTIONS[EndSLRDataReps];

    // Pointer to the header record
    SLRHeader *slrHeader;
            
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

#endif    /* _SLROBTYPE_HPP */

