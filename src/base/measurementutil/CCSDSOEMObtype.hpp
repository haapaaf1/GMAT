/* 
 * File:   CCSDSOEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSOEMOBTYPE_HPP
#define	_CCSDSOEMOBTYPE_HPP

#include "CCSDSObtype.hpp"
    
class CCSDSOEMObtype : public CCSDSObtype
{

public:

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
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

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
        
    bool IsParameterRequired(const Integer id) const;

    enum OEM_TYPE_REPS
    {
	GPSONLY_ID = EndCCSDSTypeReps,
        MIXED_ID,
        GLONASSONLY_ID,
	LEOONLY_ID,
	GALILEOONLY_ID,
	EndCCSDSOEMTypeReps
    };

    enum OEM_TIME_REPS
    {
	GPSTIME_ID = EndCCSDSTimeReps,
        GLONASSUTC_ID,
        GALILEOSYSTEMTIME_ID,
	TAI_ID,
	UTC_ID,
	EndCCSDSOEMTimeReps
    };

    struct ccsds_oem_metadata
    {
	std::string objectName;
	std::string internationalDesignator;
	std::string refFrameOrigin;
	std::string refFrame;
	std::string timeSystem;
	std::string startTime;
	std::string stopTime;
	std::string useableStartTime;
	std::string useableStopTime;
	std::string interpolationMethod;
	Integer interpolationDegree;
	StringArray metadataComments;
    };

    friend class ProcessCCSDSOEMDataFile;

protected:

    static const std::string CCSDS_TYPE_DESCRIPTIONS[EndCCSDSOEMTypeReps-EndCCSDSTypeReps];
    static const std::string CCSDS_TIME_DESCRIPTIONS[EndCCSDSOEMTimeReps-EndCCSDSTimeReps];
   
    // Iterator Pointer to the metadata record
    ccsds_oem_metadata* ccsdsOEMMetaData;
};   

#endif	/* _CCSDSOEMOBTYPE_HPP */

