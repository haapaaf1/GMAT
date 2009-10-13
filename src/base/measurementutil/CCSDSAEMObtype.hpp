/* 
 * File:   CCSDSAEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:23 AM
 */

#ifndef _CCSDSAEMOBTYPE_HPP
#define	_CCSDSAEMOBTYPE_HPP

#include "CCSDSObtype.hpp"


class CCSDSAEMQuaternion : public CCSDSQuaternion
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMQuaternion *myCCSDSAEMQuaternion);

};

class CCSDSAEMEulerAngle : public CCSDSEulerAngle
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMEulerAngle *myCCSDSAEMEulerAngle);
};

// The CCSDS spin stabilized attitude specification.
class CCSDSAEMSpinStabilized : public CCSDSSpinStabilized
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                              const CCSDSAEMSpinStabilized *myCCSDSAEMSpinStabilized);
};

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
    Integer direction;
    std::string timeSystem;
    std::string startEpoch;
    std::string stopEpoch;
    std::string useableStartEpoch;
    std::string useableStopEpoch;
    Integer attitudeType;
    Integer quaternionType;
    std::string eulerRotationSequence;
    Integer rateFrame;
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
    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
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
	CCSDS_AEM_QUATERNION_TYPE_ID,
	CCSDS_AEM_QUATERNION_EPOCH_ID,
	CCSDS_AEM_QUATERNION_FRAMEA_ID,
	CCSDS_AEM_QUATERNION_FRAMEB_ID,
	CCSDS_AEM_QUATERNION_DIRECTION_ID,
	CCSDS_AEM_QUATERNION_Q1_ID,
	CCSDS_AEM_QUATERNION_Q2_ID,
	CCSDS_AEM_QUATERNION_Q3_ID,
	CCSDS_AEM_QUATERNION_QC_ID,
	CCSDS_AEM_QUATERNION_Q1DOT_ID,
        CCSDS_AEM_QUATERNION_Q2DOT_ID,
        CCSDS_AEM_QUATERNION_Q3DOT_ID,
        CCSDS_AEM_QUATERNION_QCDOT_ID,
        CCSDS_AEM_QUATERNION_XRATE_ID,
        CCSDS_AEM_QUATERNION_YRATE_ID,
        CCSDS_AEM_QUATERNION_ZRATE_ID,
        CCSDS_AEM_QUATERNION_COMMENTS_ID,
        CCSDS_AEM_EULERANGLE_TYPE_ID,
	CCSDS_AEM_EULERANGLE_EPOCH_ID,
	CCSDS_AEM_EULERANGLE_FRAMEA_ID,
	CCSDS_AEM_EULERANGLE_FRAMEB_ID,
	CCSDS_AEM_EULERANGLE_DIRECTION_ID,
        CCSDS_AEM_EULERANGLE_ROTATIONSEQUENCE_ID,
        CCSDS_AEM_EULERANGLE_RATEFRAME_ID,
        CCSDS_AEM_EULERANGLE_XANGLE_ID,
        CCSDS_AEM_EULERANGLE_YANGLE_ID,
        CCSDS_AEM_EULERANGLE_ZANGLE_ID,
        CCSDS_AEM_EULERANGLE_XRATE_ID,
        CCSDS_AEM_EULERANGLE_YRATE_ID,
        CCSDS_AEM_EULERANGLE_ZRATE_ID,
        CCSDS_AEM_EULERANGLE_COMMENTS_ID,
        CCSDS_AEM_SPINSTABILIZED_ATTITUDETYPE_ID,
	CCSDS_AEM_SPINSTABILIZED_EPOCH_ID,
	CCSDS_AEM_SPINSTABILIZED_FRAMEA_ID,
	CCSDS_AEM_SPINSTABILIZED_FRAMEB_ID,
	CCSDS_AEM_SPINSTABILIZED_DIRECTION_ID,
	CCSDS_AEM_SPINSTABILIZED_SPINALPHA_ID,
	CCSDS_AEM_SPINSTABILIZED_SPINDELTA_ID,
	CCSDS_AEM_SPINSTABILIZED_SPINANGLE_ID,
	CCSDS_AEM_SPINSTABILIZED_SPINANGLEVEOCITY_ID,
	CCSDS_AEM_SPINSTABILIZED_NUTATION_ID,
	CCSDS_AEM_SPINSTABILIZED_NUTATIONPERIOD_ID,
	CCSDS_AEM_SPINSTABILIZED_NUTATIONPHASE_ID,
	CCSDS_AEM_SPINSTABILIZED_COMMENTS_ID,
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
    CCSDSAEMQuaternion *ccsdsAEMQuaternion;
    CCSDSAEMEulerAngle *ccsdsAEMEulerAngle;
    CCSDSAEMSpinStabilized *ccsdsAEMSpinStabilized;

};

#endif	/* _CCSDSAEMOBTYPE_HPP */

