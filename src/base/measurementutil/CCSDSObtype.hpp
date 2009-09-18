/* 
 * File:   CCSDSObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:29 AM
 */

#ifndef _CCSDSOBTYPE_HPP
#define	_CCSDSOBTYPE_HPP

#include "Obtype.hpp"

class CCSDSObtype : public Obtype
{
    
public :
    
    CCSDSObtype();
    CCSDSObtype(const CCSDSObtype &ob);
    const CCSDSObtype& operator=(const CCSDSObtype &ob);
    virtual ~CCSDSObtype();
    	
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

    // The CCSDS header specification that is common to all CCSDS formats
    struct ccsds_header
    {
	Real ccsdsVersion;
	std::string creationDate;
	std::string originator;
	StringArray headerComments;
    };
    
    // The CCSDS quaternion specification. Some formats do not use
    // all the parameters such as the rates.
    struct ccsds_quaternion
    {
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
    struct ccsds_eulerAngle
    {
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
    struct ccsds_spinStabilized
    {
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

    struct ccsds_stateVector
    {
	std::string epoch;
	Real X, Y, Z;
	Real xDot, yDot, zDot;
    };
    
    struct ccsds_keplerianElements
    {
	Real semiMajorAxis;
	Real eccentricity;
	Real inclination;
	Real raan;
	Real argumentOfPericenter;
	Real trueAnomaly;
	Real meanAnomaly;
	Real gravitationalCoefficient;
    };
    
    struct ccsds_spacecraftParameters
    {
	Real mass;
	Real solarRadiationArea;
	Real solarRadiationCoefficient;
	Real dragArea;
	Real dragCoefficient;
	std::string intertiaRefFrame;
	Real i11, i22, i33, i12, i13, i23;    
    };
    
    struct ccsds_data
    {
	Integer keywordID;
	std::string timeTag;
	Real measurement;
    };        

    enum CCSDS_DATATYPE_REPS
    {
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
    static const std::string CCSDSObtype::CCSDS_KEYWORDS[EndCCSDSDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSDataReps];

    // Pointer to the header record associated with this data point
    ccsds_header *ccsdsHeader;

    // Pointers to the various kinds of data that the CCSDS format supports
    // Only one of these pointers should be not null after reading in
    // a particular observation.

    ccsds_data *ccsdsData;
    ccsds_quaternion *ccsdsQuaternion;
    ccsds_eulerAngle *ccsdsEulerAngle;
    ccsds_spinStabilized *ccsdsSpinStabilized;
    ccsds_stateVector *ccsdsStateVector;
    ccsds_keplerianElements *ccsdsKeplerianElements;
    ccsds_spacecraftParameters *ccsdsSpacecraftParameters;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
