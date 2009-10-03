/* 
 * File:   CCSDSObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:29 AM
 */

#ifndef _CCSDSOBTYPE_HPP
#define	_CCSDSOBTYPE_HPP

#include "ObType.hpp"

// The CCSDS header specification that is common to all CCSDS formats
class CCSDSHeader
{

public:

    friend std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myCCSDSheader);

    std::string fileType;
    Real ccsdsVersion;
    std::string creationDate;
    std::string originator;
    StringArray headerComments;
};

// The CCSDS quaternion specification. Some formats do not use
// all the parameters such as the rates.
class CCSDSQuaternion
{

public:

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSQuaternion *myCCSDSquaternion);

    std::string quarternionType;
    std::string frameA;
    std::string frameB;
    std::string direction;
    Real q1, q2, q3, qC;
    Real q1Dot, q2Dot, q3Dot, qcDot;
    Real xRate, yRate, zRate;
};

// The CCSDS Euler angle specification. Some formats do not use
// all the parameters such as the rates.
class CCSDSEulerAngle
{

public:

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSEulerAngle *myCCSDSeulerAngle);

    std::string eulerAngleType;
    std::string frameA;
    std::string frameB;
    std::string direction;
    std::string rotationSequence;
    std::string rateFrame;
    Real xAngle, yAngle, zAngle;
    Real xRate, yRate, zRate;
};

// The CCSDS spin stabilized attitude specification.
class CCSDSSpinStabilized
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                              const CCSDSSpinStabilized *myCCSDSspinStabilized);

    std::string attitudeType;
    StringArray comments;
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
};

class CCSDSStateVector
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSStateVector *myCCSDSstateVector);

    std::string epoch;
    Real X, Y, Z;
    Real xDot, yDot, zDot;
};

class CCSDSKeplerianElements
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSKeplerianElements *myCCSDSkeplerianElements);

    Real semiMajorAxis;
    Real eccentricity;
    Real inclination;
    Real raan;
    Real argumentOfPericenter;
    Real trueAnomaly;
    Real meanAnomaly;
    Real gravitationalCoefficient;
};

class CCSDSSpacecraftParameters
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                  const CCSDSSpacecraftParameters *myCCSDSspacecraftParameters);

    Real mass;
    Real solarRadiationArea;
    Real solarRadiationCoefficient;
    Real dragArea;
    Real dragCoefficient;
    std::string intertiaRefFrame;
    Real i11, i22, i33, i12, i13, i23;
};

class CCSDSManeuver
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSManeuver *myCCSDSmaneuver);

    std::string ignitionEpoch;
    Real duration;
    Real deltaMass;
    std::string refFrame;
    Real deltaV1, deltaV2, deltaV3;
};


class CCSDSAttitudeManeuver
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                          const CCSDSAttitudeManeuver *myCCSDSAttitudemaneuver);

    std::string epochStart;
    Real duration;
    std::string refFrame;
    Real tor1, tor2, tor3;
};

class CCSDSData
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSData *myCCSDSdata);

    Integer keywordID;
    std::string timeTag;
    Real measurement;
};

class CCSDSObType : public ObType
{
    
public :
    
    CCSDSObType(const std::string &type, const std::string &name);
    CCSDSObType(const CCSDSObType &ob);
    const CCSDSObType& operator=(const CCSDSObType &ob);
    virtual ~CCSDSObType();

    virtual std::string GetDataParameterText(const Integer id) const;
    virtual Integer    GetDataParameterID(const std::string &str) const;
    virtual Gmat::ParameterType GetDataParameterType(const Integer id) const;
    virtual std::string GetDataParameterTypeString(const Integer id) const;
    virtual std::string GetDataUnits(const Integer id) const;

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
    virtual bool CheckDataAvailability(const std::string str) const;
    
    virtual bool IsParameterRequired(const Integer id) const;

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
        MANEUVER_ID,
        GENERICDATA_ID,
	EndCCSDSTypeReps    
    };
    
    enum CCSDS_TIMESYSTEM_REPS
    {
	EndCCSDSTimeReps
    };
    
    enum CCSDS_DATA_REPS
    {
	CCSDS_VERSION_ID = 0,
	CCSDS_CREATIONDATE_ID,
	CCSDS_ORIGINATOR_ID,
	CCSDS_HEADERCOMMENTS_ID,
	EndCCSDSDataReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
//    friend class ProcessCCSDSOPMDataFile;
//    friend class ProcessCCSDSOEMDataFile;
//    friend class ProcessCCSDSAPMDataFile;
//    friend class ProcessCCSDSAEMDataFile;
    
protected:

    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps];
    static const std::string CCSDSObType::CCSDS_KEYWORDS[EndCCSDSDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSDataReps];

    // Pointer to the header record associated with this data point
    CCSDSHeader *ccsdsHeader;

    // Pointers to the various kinds of data that the CCSDS format supports
    // Only one of these pointers should be not null after reading in
    // a particular observation.

    CCSDSData *ccsdsData;
    CCSDSQuaternion *ccsdsQuaternion;
    CCSDSEulerAngle *ccsdsEulerAngle;
    CCSDSSpinStabilized *ccsdsSpinStabilized;
    CCSDSStateVector *ccsdsStateVector;
    CCSDSKeplerianElements *ccsdsKeplerianElements;
    CCSDSSpacecraftParameters *ccsdsSpacecraftParameters;
    CCSDSManeuver *ccsdsManeuver;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
