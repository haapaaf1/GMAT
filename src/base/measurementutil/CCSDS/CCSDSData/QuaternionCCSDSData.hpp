//$Header$
//------------------------------------------------------------------------------
//                             QuaternionCCSDSData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the base class for the Quaternion data construct
 *  that is used by the CCSDS Attitude Parameter and Ephemeris Message formats.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSQUATERNION_HPP
#define	_CCSDSQUATERNION_HPP

#include "CCSDSData.hpp"

class QuaternionCCSDSData : public CCSDSData
{

public:

    QuaternionCCSDSData();
    QuaternionCCSDSData(const QuaternionCCSDSData &myQ);
    const QuaternionCCSDSData& QuaternionCCSDSData::operator=(const QuaternionCCSDSData &myQ);
    virtual ~QuaternionCCSDSData();

    std::string GetQuaternionTypeText(const Integer id) const;
    Integer     GetQuaternionTypeID(const std::string &str) const;

    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
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

    bool SetDataParameter(const Integer id, const Integer &value);
    bool SetDataParameter(const std::string &label, const Integer &value);
    bool SetDataParameter(const Integer id, const Real &value);
    bool SetDataParameter(const std::string &label, const Real &value);
    bool SetDataParameter(const Integer id, const std::string &value);
    bool SetDataParameter(const std::string &label, const std::string &value);
    bool SetDataParameter(const Integer id, const StringArray &value);
    bool SetDataParameter(const std::string &label, const StringArray &value);

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_QUATERNION_TYPE
    {
        CCSDS_QUATERNION_FIRST_ID = 0,
        CCSDS_QUATERNION_LAST_ID,
        EndQuaternionCCSDSDataTypeReps
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
        EndQuaternionCCSDSDataReps
    };

    friend class AEMCCSDSDataFile;
    friend class APMCCSDSDataFile;

protected:

    static const std::string CCSDS_QUATERNION_TYPE[EndQuaternionCCSDSDataTypeReps];
    static const std::string CCSDS_QUATERNION_KEYWORDS[EndQuaternionCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndQuaternionCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndQuaternionCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndQuaternionCCSDSDataReps];

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

