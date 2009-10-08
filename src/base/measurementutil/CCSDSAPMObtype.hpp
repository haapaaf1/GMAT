/* 
 * File:   CCSDSAPMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSAPMOBTYPE_HPP
#define	_CCSDSAPMOBTYPE_HPP

#include "CCSDSObtype.hpp"

class CCSDSAPMMetaData
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAPMMetaData *myMetaData);

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string timeSystem;
    StringArray comments;
};

class CCSDSAPMObType : public CCSDSObType
{
    
public:

    CCSDSAPMObType();
    CCSDSAPMObType(const CCSDSAPMObType &APM);
    const CCSDSAPMObType& CCSDSAPMObType::operator=(const CCSDSAPMObType &APM);
    ~CCSDSAPMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAPMObType *myAPM);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    bool IsParameterRequired(const Integer id) const;

    enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        TT_ID,
        GPS_ID,
	TDB_ID,
        TCB_ID,
	EndCCSDSAPMTimeReps
    };

    enum CCSDS_DATA_REPS
    {
        CCSDS_APM_OBJECTNAME_ID = EndCCSDSDataReps,
        CCSDS_APM_OBJECTID_ID,
        CCSDS_APM_CENTERNAME_ID,
	CCSDS_APM_TIMESYSTEM_ID,
        CCSDS_APM_METADATACOMMENTS_ID,
        EndCCSDSAPMDataReps
    };

    friend class ProcessCCSDSAPMDataFile;

protected:

    static const std::string CCSDS_TIME_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps];
    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSAPMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_APM_KEYWORDS[EndCCSDSAPMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAPMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps - EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSAPMDataReps - EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSAPMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAPMDataReps - EndCCSDSDataReps];

    // Pointer to the metadata record
    CCSDSAPMMetaData *ccsdsAPMMetaData;
    
};

#endif	/* _CCSDSAPMOBTYPE_HPP */

