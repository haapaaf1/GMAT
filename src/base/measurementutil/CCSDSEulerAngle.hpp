/*
 * File:   CCSDSEulerAngle.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:02 AM
 */

#ifndef _CCSDSEULERANGLE_HPP
#define	_CCSDSEULERANGLE_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>

class CCSDSEulerAngle
{

public:

    CCSDSEulerAngle();
    CCSDSEulerAngle(const CCSDSEulerAngle &ea);
    const CCSDSEulerAngle& CCSDSEulerAngle::operator=(const CCSDSEulerAngle &ea);
    ~CCSDSEulerAngle();

    friend std::string GetRateFrameText(const Integer id);
    friend Integer    GetRateFrameID(const std::string &str);

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
    bool CheckDataAvailability(const std::string str) const;
    bool IsParameterRequired(const Integer id) const;
    friend Integer CCSDSCountRequiredNumberDataParameters();

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_DATA_REPS
    {
        CCSDS_EULERANGLE_TYPE_ID,
	CCSDS_EULERANGLE_FRAMEA_ID,
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

    enum CCSDS_RATE_FRAME
    {
        CCSDS_RATE_FRAME_A_ID = 0,
        CCSDS_RATE_FRAME_B_ID,
        EndCCSDSRateFrameReps
    };
    
protected:

    static const std::string CCSDS_RATE_FRAME[EndCCSDSRateFrameReps];
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