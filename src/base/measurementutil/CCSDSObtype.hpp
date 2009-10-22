/* 
 * File:   CCSDSObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:29 AM
 */

#ifndef _CCSDSOBTYPE_HPP
#define	_CCSDSOBTYPE_HPP

#include "ObType.hpp"

class CCSDSData
{

public:

    Integer keywordID;
    std::string timeTag;
    Real measurement;
    StringArray comments;
};

// The CCSDS header specification that is common to all CCSDS formats
class CCSDSHeader
{

public:

    friend std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myCCSDSheader);

    std::string fileType;
    Real ccsdsVersion;
    std::string creationDate;
    std::string originator;
    StringArray comments;
    Integer dataType;
};

// The CCSDS quaternion specification. Some formats do not use
// all the parameters such as the rates.
class CCSDSQuaternion
{

public:

    Integer quaternionType;
    std::string epoch;
    std::string frameA;
    std::string frameB;
    std::string direction;
    Real q1, q2, q3, qC;
    Real q1Dot, q2Dot, q3Dot, qCDot;
    Real xRate, yRate, zRate;
    StringArray comments;
};

// The CCSDS Euler angle specification. Some formats do not use
// all the parameters such as the rates.
class CCSDSEulerAngle
{

public:
    
    Integer eulerAngleType;
    std::string epoch;
    std::string frameA;
    std::string frameB;
    std::string direction;
    std::string rotationSequence;
    std::string rateFrame;
    Real xAngle, yAngle, zAngle;
    Real xRate, yRate, zRate;
    StringArray comments;
};

// The CCSDS spin stabilized attitude specification.
class CCSDSSpinStabilized
{

public:

    Integer attitudeType;
    std::string epoch;
    std::string frameA;
    std::string frameB;
    std::string direction;
    Real spinAlpha;
    Real spinDelta;
    Real spinAngle;
    Real spinAngleVelocity;
    Real nutation;
    Real nutationPeriod;
    Real nutationPhase;
    StringArray comments;
};

class CCSDSStateVector
{

public:

    std::string epoch;
    Real x, y, z;
    Real xDot, yDot, zDot;
    StringArray comments;
};

class CCSDSObType : public ObType
{
    
public :
    
    CCSDSObType(const std::string &type, const std::string &name);
    CCSDSObType(const CCSDSObType &ob);
    const CCSDSObType& operator=(const CCSDSObType &ob);
    virtual ~CCSDSObType();

    GmatBase *Clone() const;

    friend std::string GetAttitudeTypeText(const Integer id);
    friend Integer    GetAttitudeTypeID(const std::string &str);
    friend std::string GetQuaternionTypeText(const Integer id);
    friend Integer    GetQuaternionTypeID(const std::string &str);
    friend std::string GetAttitudeDirText(const Integer id);
    friend Integer    GetAttitudeDirID(const std::string &str);
    friend std::string GetRateFrameText(const Integer id);
    friend Integer    GetRateFrameID(const std::string &str);
    
    virtual std::string GetHeaderDataParameterText(const Integer id) const;
    virtual Integer    GetHeaderDataParameterID(const std::string &str) const;
    virtual Gmat::ParameterType GetHeaderDataParameterType(const Integer id) const;
    virtual std::string GetHeaderDataParameterTypeString(const Integer id) const;
    virtual std::string GetHeaderDataUnits(const Integer id) const;

    virtual Real	GetRealDataParameter(const Integer id) const;
    virtual Real	GetRealDataParameter(const std::string &label) const;
    virtual Integer	GetIntegerDataParameter(const Integer id) const;
    virtual Integer	GetIntegerDataParameter(const std::string &label) const;
    virtual bool        GetBoolDataParameter(const Integer id) const;
    virtual bool        GetBoolDataParameter(const std::string &label) const;    
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;
    virtual StringArray GetStringArrayDataParameter(const Integer id) const;
    virtual StringArray GetStringArrayDataParameter(const std::string &label) const;

    virtual const std::string* GetKeywords() const;
    virtual const Integer GetKeywordID(const std::string str) const;
    virtual std::string GetUnits(const Integer &id) const;

    // Functions to verify data availability
    virtual bool CheckHeaderDataAvailability(const std::string str) const;
    
    virtual bool IsHeaderParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberHeaderDataParameters();

    virtual const std::string* GetDataTypes() const;
    virtual std::string GetDataTypeText(const Integer &id) const;
    virtual Integer GetDataTypeID(const std::string &label);

    virtual const std::string* GetTimeSystems() const;
    virtual std::string GetTimeSystemText(const Integer &id) const;
    virtual Integer GetTimeSystemID(const std::string &label);    

    enum CCSDS_DATATYPE_REPS
    {
        QUATERNION_ID,
        EULERANGLE_ID,
        SPINSTABILIZED_ID,
        STATEVECTOR_ID,
        KEPLERIANELEMENTS_ID,
        SPACECRAFTPARAMETERS_ID,
        SPACECRAFTINERTIA_ID,
        MANEUVER_ID,
        ATTITUDEMANEUVER_ID,
        GENERICDATA_ID,
	EndCCSDSTypeReps    
    };
    
    enum CCSDS_TIMESYSTEM_REPS
    {
	EndCCSDSTimeReps
    };
    
    enum CCSDS_HEADERDATA_REPS
    {
	CCSDS_VERSION_ID,
	CCSDS_CREATIONDATE_ID,
	CCSDS_ORIGINATOR_ID,
	CCSDS_HEADERCOMMENTS_ID,
	EndCCSDSHeaderDataReps
    };

    enum CCSDS_ATTITUDE_TYPE
    {
        CCSDS_QUATERNION_ID = 0,
        CCSDS_QUATERNION_DERIVATIVE_ID,
        CCSDS_QUATERNION_RATE_ID,
        CCSDS_EULER_ANGLE_ID,
        CCSDS_EULER_ANGLE_RATE_ID,
        CCSDS_SPIN_ID,
        CCSDS_SPIN_NUTATION_ID,
        EndCCSDSAttitudeTypeReps
    };

    enum CCSDS_RATE_FRAME
    {
        CCSDS_RATE_FRAME_A_ID = 0,
        CCSDS_RATE_FRAME_B_ID,
        EndCCSDSRateFrameReps
    };

    enum CCSDS_QUATERNION_TYPE
    {
        CCSDS_QUATERNION_FIRST_ID = 0,
        CCSDS_QUATERNION_LAST_ID,
        EndCCSDSQuaternionTypeReps
    };

    enum CCSDS_ATTITUDE_DIR
    {
        CCSDS_ATTITUDE_A2B_ID = 0,
        CCSDS_ATTITUDE_B2A_ID,
        EndCCSDSAttitudeDirReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
    friend class ProcessCCSDSOPMDataFile;
    friend class ProcessCCSDSOEMDataFile;
    friend class ProcessCCSDSAPMDataFile;
    friend class ProcessCCSDSAEMDataFile;
    
protected:

    static const std::string CCSDS_RATE_FRAME[EndCCSDSRateFrameReps];
    static const std::string CCSDS_ATTITUDE_DIR[EndCCSDSAttitudeDirReps];
    static const std::string CCSDS_ATTITUDE_TYPE[EndCCSDSAttitudeTypeReps];
    static const std::string CCSDS_QUATERNION_TYPE[EndCCSDSQuaternionTypeReps];
    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];
    static const std::string CCSDS_HEADER_KEYWORDS[EndCCSDSHeaderDataReps];
    static const bool CCSDS_HEADER_IS_REQUIRED[EndCCSDSHeaderDataReps];
    static const Gmat::ParameterType CCSDS_HEADER_PARAMETER_TYPE[EndCCSDSHeaderDataReps];
    static const std::string CCSDS_HEADER_UNIT_DESCRIPTIONS[EndCCSDSHeaderDataReps];
    static const std::string CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[EndCCSDSHeaderDataReps];

    // Pointer to the header record associated with this data point
    CCSDSHeader *ccsdsHeader;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
