/* 
 * File:   CCSDSAEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:23 AM
 */

#ifndef _CCSDSAEMOBTYPE_HPP
#define	_CCSDSAEMOBTYPE_HPP

#include "CCSDSObtype.hpp"

class CCSDSAEMMetaData
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMMetaData *myMetaData);

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string frameA;
    std::string frameB;
    std::string direction;
    std::string timeSystem;
    std::string startEpoch;
    std::string stopEpoch;
    std::string useableStartEpoch;
    std::string useableStopEpoch;
    std::string attitudeType;
    std::string quaternionType;
    std::string eulerRotationSequence;
    std::string rateFrame;
    std::string interpolationMethod;
    Integer interpolationDegree;
    StringArray comments;
};

class CCSDSAEMObType : public CCSDSObType
{

public:

    CCSDSAEMObType();
    CCSDSAEMObType(const CCSDSAEMObType &AEM);
    const CCSDSAEMObType& CCSDSAEMObType::operator=(const CCSDSAEMObType &AEM);
    ~CCSDSAEMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMObType *myAEM);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
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
	EndCCSDSAEMTimeReps
    };

    enum CCSDS_DATA_REPS
    {
        CCSDS_AEM_OBJECTNAME_ID = EndCCSDSDataReps,
        CCSDS_AEM_OBJECTID_ID,
        CCSDS_AEM_CENTERNAME_ID,
        CCSDS_AEM_REFFRAMEA_ID,
        CCSDS_AEM_REFFRAMEB_ID,
        CCSDS_AEM_ATTITUDEDIR_ID,
        CCSDS_AEM_TIMESYSTEM_ID,
	CCSDS_AEM_STARTEPOCH_ID,
	CCSDS_AEM_USEABLE_STARTEPOCH_ID,
	CCSDS_AEM_USEABLE_STOPEPOCH_ID,
	CCSDS_AEM_STOPEPOCH_ID,
        CCSDS_AEM_ATTITUDETYPE_ID,
        CCSDS_AEM_QUATERNIONTYPE_ID,
        CCSDS_AEM_EULERROTSEQ_ID,
        CCSDS_AEM_RATEFRAME_ID,
        CCSDS_AEM_INTERPOLATION_ID,
        CCSDS_AEM_INTERPOLATIONDEGREE_ID,
        CCSDS_AEM_METADATACOMMENTS_ID,
        EndCCSDSAEMDataReps
    };

    friend class ProcessCCSDSAEMDataFile;

protected:

    static const std::string CCSDS_TIME_DESCRIPTIONS[EndCCSDSAEMTimeReps-EndCCSDSTimeReps];
    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSAEMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_AEM_KEYWORDS[EndCCSDSAEMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAEMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps - EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSAEMDataReps - EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSAEMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAEMDataReps - EndCCSDSDataReps];

    // Pointer to the metadata record
    CCSDSAEMMetaData* ccsdsAEMMetaData;
};

#endif	/* _CCSDSAEMOBTYPE_HPP */

