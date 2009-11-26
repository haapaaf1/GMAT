//$Header$
//------------------------------------------------------------------------------
//                             EulerAngleCCSDSData
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
 * This class specifies the base class for the Euler Angle data construct
 *  that is used by the CCSDS Attitude Parameter and Ephemeris Message formats.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSEULERANGLE_HPP
#define	_CCSDSEULERANGLE_HPP

#include "CCSDSData.hpp"

class EulerAngleCCSDSData : public CCSDSData
{

public:

    EulerAngleCCSDSData();
    EulerAngleCCSDSData(const EulerAngleCCSDSData &ea);
    const EulerAngleCCSDSData& EulerAngleCCSDSData::operator=(const EulerAngleCCSDSData &ea);
    virtual ~EulerAngleCCSDSData();

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    std::string GetEulerSequenceText(const Integer id) const;
    Integer     GetEulerSequenceID(const std::string &str) const;

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

    enum CCSDS_DATA_REPS
    {
	CCSDS_EULERANGLE_FRAMEA_ID = 0,
	CCSDS_EULERANGLE_FRAMEB_ID,
	CCSDS_EULERANGLE_DIRECTION_ID,
        CCSDS_EULERANGLE_ROTATIONSEQUENCE_ID,
        CCSDS_EULERANGLE_RATEFRAME_ID,
        CCSDS_EULERANGLE_XANGLE_ID,
        CCSDS_EULERANGLE_YANGLE_ID,
        CCSDS_EULERANGLE_ZANGLE_ID,
        CCSDS_EULERANGLE_XRATE_ID,
        CCSDS_EULERANGLE_YRATE_ID,
        CCSDS_EULERANGLE_ZRATE_ID,
        CCSDS_EULERANGLE_COMMENTS_ID,
        EndEulerAngleCCSDSDataDataReps,
        CCSDS_EULERANGLE_ANGLE1_ID,
        CCSDS_EULERANGLE_ANGLE2_ID,
        CCSDS_EULERANGLE_ANGLE3_ID,
        CCSDS_EULERANGLE_RATE1_ID,
        CCSDS_EULERANGLE_RATE2_ID,
        CCSDS_EULERANGLE_RATE3_ID,
        EndEulerAngleDataReps
    };

    enum CCSDS_EULER_SEQ_LIST
    {
        CCSDS_EULERANGLE_123,
        CCSDS_EULERANGLE_132,
        CCSDS_EULERANGLE_213,
        CCSDS_EULERANGLE_231,
        CCSDS_EULERANGLE_312,
        CCSDS_EULERANGLE_321,
        CCSDS_EULERANGLE_121,
        CCSDS_EULERANGLE_131,
        CCSDS_EULERANGLE_212,
        CCSDS_EULERANGLE_232,
        CCSDS_EULERANGLE_313,
        CCSDS_EULERANGLE_323,
        EndCCSDSEulerSeqList
    };

    friend class AEMCCSDSDataFile;
    friend class APMCCSDSDataFile;
    
protected:

    static const std::string CCSDS_EULERSEQUENCE_LIST[EndCCSDSEulerSeqList];
    static const std::string CCSDS_EULERANGLE_KEYWORDS[EndEulerAngleCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndEulerAngleCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndEulerAngleCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndEulerAngleDataReps];

    Integer eulerAngleType;
    std::string timeTag;
    std::string frameA;
    std::string frameB;
    Integer direction;
    Integer rotationSequence;
    Integer rateFrame;
    Real angle1, angle2, angle3;
    Real angleRate1, angleRate2, angleRate3;
    StringArray comments;
};

#endif /* _CCSDSEULERANGLE_HPP */