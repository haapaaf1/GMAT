/*
 * File:   CCSDSEulerAngle.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:02 AM
 */

#ifndef _CCSDSEULERANGLE_HPP
#define	_CCSDSEULERANGLE_HPP

#include "CCSDSData.hpp"

class CCSDSEulerAngle : public CCSDSData
{

public:

    CCSDSEulerAngle();
    CCSDSEulerAngle(const CCSDSEulerAngle &ea);
    const CCSDSEulerAngle& CCSDSEulerAngle::operator=(const CCSDSEulerAngle &ea);
    ~CCSDSEulerAngle();

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    virtual Integer	GetIntegerDataParameter(const Integer id) const;
    virtual Integer	GetIntegerDataParameter(const std::string &label) const;
    virtual Real	GetRealDataParameter(const Integer id) const;
    virtual Real	GetRealDataParameter(const std::string &label) const;
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;
    virtual StringArray GetStringArrayDataParameter(const Integer id) const;
    virtual StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberEulerAngleParameters();
    bool Validate() const;

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
        EndCCSDSEulerAngleDataReps
    };

    friend class ProcessCCSDSAEMDataFile;
    friend class ProcessCCSDSAPMDataFile;
    
protected:

    static const std::string CCSDS_EULERANGLE_KEYWORDS[EndCCSDSEulerAngleDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSEulerAngleDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSEulerAngleDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSEulerAngleDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSEulerAngleDataReps];

    Integer eulerAngleType;
    std::string timeTag;
    std::string frameA;
    std::string frameB;
    Integer direction;
    std::string rotationSequence;
    Integer rateFrame;
    Real xAngle, yAngle, zAngle;
    Real xRate, yRate, zRate;
    StringArray comments;
};

#endif /* _CCSDSEULERANGLE_HPP */