/* 
 * File:   CCSDSQuaternion.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:03 AM
 */

#ifndef _CCSDSQUATERNION_HPP
#define	_CCSDSQUATERNION_HPP

#include "CCSDSData.hpp"

class CCSDSQuaternion : public CCSDSData
{

public:

    CCSDSQuaternion();
    CCSDSQuaternion(const CCSDSQuaternion &myQ);
    const CCSDSQuaternion& CCSDSQuaternion::operator=(const CCSDSQuaternion &myQ);
    ~CCSDSQuaternion();

    std::string GetQuaternionTypeText(const Integer id) const;
    Integer    GetQuaternionTypeID(const std::string &str) const;

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Integer	GetIntegerDataParameter(const Integer id) const;
    Integer	GetIntegerDataParameter(const std::string &label) const;
    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberQuaternionParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_QUATERNION_TYPE
    {
        CCSDS_QUATERNION_FIRST_ID = 0,
        CCSDS_QUATERNION_LAST_ID,
        EndCCSDSQuaternionTypeReps
    };
    
    enum CCSDS_DATA_REPS
    {
	CCSDS_QUATERNION_TIMETAG_ID,
	CCSDS_QUATERNION_FRAMEA_ID,
	CCSDS_QUATERNION_FRAMEB_ID,
	CCSDS_QUATERNION_DIRECTION_ID,
	CCSDS_QUATERNION_Q1_ID,
	CCSDS_QUATERNION_Q2_ID,
	CCSDS_QUATERNION_Q3_ID,
	CCSDS_QUATERNION_QC_ID,
	CCSDS_QUATERNION_Q1DOT_ID,
        CCSDS_QUATERNION_Q2DOT_ID,
        CCSDS_QUATERNION_Q3DOT_ID,
        CCSDS_QUATERNION_QCDOT_ID,
        CCSDS_QUATERNION_XRATE_ID,
        CCSDS_QUATERNION_YRATE_ID,
        CCSDS_QUATERNION_ZRATE_ID,
        CCSDS_QUATERNION_COMMENTS_ID,
        EndCCSDSQuaternionDataReps
    };

protected:

    static const std::string CCSDS_QUATERNION_TYPE[EndCCSDSQuaternionTypeReps];
    static const std::string CCSDS_QUATERNION_KEYWORDS[EndCCSDSQuaternionDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSQuaternionDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSQuaternionDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSQuaternionDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSQuaternionDataReps];

    Integer attitudeType;
    Integer quaternionType;
    std::string timeTag;
    std::string frameA;
    std::string frameB;
    Integer direction;
    Real q1, q2, q3, qC;
    Real q1Dot, q2Dot, q3Dot, qCDot;
    Real xRate, yRate, zRate;
    StringArray comments;
};

#endif	/* _CCSDSQUATERNION_HPP */

