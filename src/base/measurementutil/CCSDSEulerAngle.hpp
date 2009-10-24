/*
 * File:   CCSDSEulerAngle.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:02 AM
 */

#ifndef _CCSDSEULERANGLE_HPP
#define	_CCSDSEULERANGLE_HPP

class CCSDSEulerAngle : public CCSDSObType
{

public:

    CCSDSEulerAngle();
    CCSDSEulerAngle(const CCSDSEulerAngle &ea);
    const CCSDSEulerAngle& CCSDSEulerAngle::operator=(const CCSDSEulerAngle &ea);
    ~CCSDSEulerAngle();

    GmatBase *Clone() const;

    friend std::string GetRateFrameText(const Integer id);
    friend Integer    GetRateFrameID(const std::string &str);

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