/* 
 * File:   CCSDSAPMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSAPMOBTYPE_HPP
#define	_CCSDSAPMOBTYPE_HPP

#include "CCSDSObtype.hpp"

class CCSDSAPMSpacecraftInertia
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                     const CCSDSAPMSpacecraftInertia *myCCSDSspacecraftInertia);

    std::string inertiaRefFrame;
    Real i11, i22, i33, i12, i13, i23;
    StringArray comments;
};

class CCSDSAPMQuaternion : public CCSDSQuaternion
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAPMQuaternion *myCCSDSAPMQuaternion);

};

class CCSDSAPMEulerAngle : public CCSDSEulerAngle
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAPMEulerAngle *myCCSDSAPMEulerAngle);
};

// The CCSDS spin stabilized attitude specification.
class CCSDSAPMSpinStabilized : public CCSDSSpinStabilized
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSAPMSpinStabilized *myCCSDSAPMSpinStabilized);
};

class CCSDSAPMAttitudeManeuver
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                       const CCSDSAPMAttitudeManeuver *myCCSDSAttitudemaneuver);

    std::string epochStart;
    Real duration;
    std::string refFrame;
    Real tor1, tor2, tor3;
    StringArray comments;
};

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
	EndCCSDSAPMTimeReps
    };

    enum CCSDS_DATA_REPS
    {
        CCSDS_APM_OBJECTNAME_ID = EndCCSDSDataReps,
        CCSDS_APM_OBJECTID_ID,
        CCSDS_APM_CENTERNAME_ID,
	CCSDS_APM_TIMESYSTEM_ID,
        CCSDS_APM_METADATACOMMENTS_ID,
	CCSDS_APM_QUATERNION_TYPE_ID,
	CCSDS_APM_QUATERNION_EPOCH_ID,
	CCSDS_APM_QUATERNION_FRAMEA_ID,
	CCSDS_APM_QUATERNION_FRAMEB_ID,
	CCSDS_APM_QUATERNION_DIRECTION_ID,
	CCSDS_APM_QUATERNION_Q1_ID,
	CCSDS_APM_QUATERNION_Q2_ID,
	CCSDS_APM_QUATERNION_Q3_ID,
	CCSDS_APM_QUATERNION_QC_ID,
	CCSDS_APM_QUATERNION_Q1DOT_ID,
        CCSDS_APM_QUATERNION_Q2DOT_ID,
        CCSDS_APM_QUATERNION_Q3DOT_ID,
        CCSDS_APM_QUATERNION_QCDOT_ID,
        CCSDS_APM_QUATERNION_XRATE_ID,
        CCSDS_APM_QUATERNION_YRATE_ID,
        CCSDS_APM_QUATERNION_ZRATE_ID,
        CCSDS_APM_QUATERNION_COMMENTS_ID,
        CCSDS_APM_EULERANGLE_TYPE_ID,
	CCSDS_APM_EULERANGLE_EPOCH_ID,
	CCSDS_APM_EULERANGLE_FRAMEA_ID,
	CCSDS_APM_EULERANGLE_FRAMEB_ID,
	CCSDS_APM_EULERANGLE_DIRECTION_ID,
        CCSDS_APM_EULERANGLE_ROTATIONSEQUENCE_ID,
        CCSDS_APM_EULERANGLE_RATEFRAME_ID,
        CCSDS_APM_EULERANGLE_XANGLE_ID,
        CCSDS_APM_EULERANGLE_YANGLE_ID,
        CCSDS_APM_EULERANGLE_ZANGLE_ID,
        CCSDS_APM_EULERANGLE_XRATE_ID,
        CCSDS_APM_EULERANGLE_YRATE_ID,
        CCSDS_APM_EULERANGLE_ZRATE_ID,
        CCSDS_APM_EULERANGLE_COMMENTS_ID,
        CCSDS_APM_SPINSTABILIZED_ATTITUDETYPE_ID,
	CCSDS_APM_SPINSTABILIZED_EPOCH_ID,
	CCSDS_APM_SPINSTABILIZED_FRAMEA_ID,
	CCSDS_APM_SPINSTABILIZED_FRAMEB_ID,
	CCSDS_APM_SPINSTABILIZED_DIRECTION_ID,
	CCSDS_APM_SPINSTABILIZED_SPINALPHA_ID,
	CCSDS_APM_SPINSTABILIZED_SPINDELTA_ID,
	CCSDS_APM_SPINSTABILIZED_SPINANGLE_ID,
	CCSDS_APM_SPINSTABILIZED_SPINANGLEVEOCITY_ID,
	CCSDS_APM_SPINSTABILIZED_NUTATION_ID,
	CCSDS_APM_SPINSTABILIZED_NUTATIONPERIOD_ID,
	CCSDS_APM_SPINSTABILIZED_NUTATIONPHASE_ID,
	CCSDS_APM_SPINSTABILIZED_COMMENTS_ID,
	CCSDS_APM_SPACECRAFTINERTIA_INERTIAREFFRAME_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I11_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I22_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I33_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I12_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I13_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I23_ID,
	CCSDS_APM_SPACECRAFTINERTIA_COMMENTS_ID,
        CCSDS_APM_ATTITUDEMANUEVER_EPOCHSTART_ID,
        CCSDS_APM_ATTITUDEMANUEVER_DURATION_ID,
        CCSDS_APM_ATTITUDEMANUEVER_REFFRAME_ID,
        CCSDS_APM_ATTITUDEMANUEVER_TOR1_ID,
        CCSDS_APM_ATTITUDEMANUEVER_TOR2_ID,
        CCSDS_APM_ATTITUDEMANUEVER_TOR3_ID,
        CCSDS_APM_ATTITUDEMANUEVER_COMMENTS_ID,
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

    // Pointer to the data records
    CCSDSAPMMetaData *ccsdsAPMMetaData;
    CCSDSAPMQuaternion *ccsdsAPMQuaternion;
    CCSDSAPMEulerAngle *ccsdsAPMEulerAngle;
    CCSDSAPMSpinStabilized *ccsdsAPMSpinStabilized;
    CCSDSAPMSpacecraftInertia *ccsdsAPMSpacecraftInertia;
    std::vector<CCSDSAPMAttitudeManeuver*> ccsdsAPMAttitudeManeuvers;
    std::vector<CCSDSAPMAttitudeManeuver*>::const_iterator i_ccsdsAPMAttitudeManeuvers;




};

#endif	/* _CCSDSAPMOBTYPE_HPP */

