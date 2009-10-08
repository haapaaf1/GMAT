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
    StringArray comments;
    Integer dataType;
};

// The CCSDS quaternion specification. Some formats do not use
// all the parameters such as the rates.
class CCSDSQuaternion
{

public:

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSQuaternion *myCCSDSquaternion);

    std::string quarternionType;
    std::string epoch;
    std::string frameA;
    std::string frameB;
    std::string direction;
    Real q1, q2, q3, qC;
    Real q1Dot, q2Dot, q3Dot, qCDot;
    StringArray comments;
};

// The CCSDS Euler angle specification. Some formats do not use
// all the parameters such as the rates.
class CCSDSEulerAngle
{

public:

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSEulerAngle *myCCSDSeulerAngle);

    std::string eulerAngleType;
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

    friend std::ostream& operator<< (std::ostream &output,
                              const CCSDSSpinStabilized *myCCSDSspinStabilized);

    std::string attitudeType;
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

    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSStateVector *myCCSDSstateVector);

    std::string epoch;
    Real x, y, z;
    Real xDot, yDot, zDot;
    StringArray comments;
};

class CCSDSKeplerianElements
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSKeplerianElements *myCCSDSkeplerianElements);

    std::string epoch;
    Real semiMajorAxis;
    Real eccentricity;
    Real inclination;
    Real raan;
    Real argumentOfPericenter;
    Real trueAnomaly;
    Real meanAnomaly;
    Real gravitationalCoefficient;
    StringArray comments;
};

class CCSDSSpacecraftParameters
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                  const CCSDSSpacecraftParameters *myCCSDSspacecraftParameters);

    std::string epoch;
    Real mass;
    Real solarRadiationArea;
    Real solarRadiationCoefficient;
    Real dragArea;
    Real dragCoefficient;
    std::string inertiaRefFrame;
    Real i11, i22, i33, i12, i13, i23;
    StringArray comments;
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
    StringArray comments;
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
    StringArray comments;
};

class CCSDSData
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSData *myCCSDSdata);

    Integer keywordID;
    std::string timeTag;
    Real measurement;
    StringArray comments;
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
        ATTITUDEMANEUVER_ID,
        GENERICDATA_ID,
	EndCCSDSTypeReps    
    };
    
    enum CCSDS_TIMESYSTEM_REPS
    {
	EndCCSDSTimeReps
    };
    
    enum CCSDS_DATA_REPS
    {
	CCSDS_VERSION_ID,
	CCSDS_CREATIONDATE_ID,
	CCSDS_ORIGINATOR_ID,
	CCSDS_HEADERCOMMENTS_ID,
	CCSDS_QUATERNION_TYPE_ID,
	CCSDS_QUATERNION_EPOCH_ID,
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
        CCSDS_QUATERNION_COMMENTS_ID,
        CCSDS_EULERANGLE_TYPE_ID,
	CCSDS_EULERANGLE_EPOCH_ID,
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
        CCSDS_SPINSTABILIZED_ATTITUDETYPE_ID,
	CCSDS_SPINSTABILIZED_EPOCH_ID,
	CCSDS_SPINSTABILIZED_FRAMEA_ID,
	CCSDS_SPINSTABILIZED_FRAMEB_ID,
	CCSDS_SPINSTABILIZED_DIRECTION_ID,
	CCSDS_SPINSTABILIZED_SPINALPHA_ID,
	CCSDS_SPINSTABILIZED_SPINDELTA_ID,
	CCSDS_SPINSTABILIZED_SPINANGLE_ID,
	CCSDS_SPINSTABILIZED_SPINANGLEVEOCITY_ID,
	CCSDS_SPINSTABILIZED_NUTATION_ID,
	CCSDS_SPINSTABILIZED_NUTATIONPERIOD_ID,
	CCSDS_SPINSTABILIZED_NUTATIONPHASE_ID,
	CCSDS_SPINSTABILIZED_COMMENTS_ID,
	CCSDS_STATEVECTOR_EPOCH_ID,
	CCSDS_STATEVECTOR_X_ID,
	CCSDS_STATEVECTOR_Y_ID,
	CCSDS_STATEVECTOR_Z_ID,
	CCSDS_STATEVECTOR_XDOT_ID,
        CCSDS_STATEVECTOR_YDOT_ID,
	CCSDS_STATEVECTOR_ZDOT_ID,
	CCSDS_STATEVECTOR_COMMENTS_ID,
	CCSDS_KEPLERIANELEMENTS_EPOCH_ID,
	CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID,
	CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID,
	CCSDS_KEPLERIANELEMENTS_INCLINATION_ID,
	CCSDS_KEPLERIANELEMENTS_RAAN_ID,
	CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID,
	CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID,
	CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID,
	CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID,
	CCSDS_KEPLERIANELEMENTS_COMMENTS_ID,
	CCSDS_SPACECRAFTPARAMETERS_EPOCH_ID,
	CCSDS_SPACECRAFTPARAMETERS_MASS_ID,
	CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID,
	CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID,
	CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID,
	CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID,
	CCSDS_SPACECRAFTPARAMETERS_INERTIAREFFRAME_ID,
	CCSDS_SPACECRAFTPARAMETERS_I11_ID,
	CCSDS_SPACECRAFTPARAMETERS_I22_ID,
	CCSDS_SPACECRAFTPARAMETERS_I33_ID,
	CCSDS_SPACECRAFTPARAMETERS_I12_ID,
	CCSDS_SPACECRAFTPARAMETERS_I13_ID,
	CCSDS_SPACECRAFTPARAMETERS_I23_ID,
	CCSDS_SPACECRAFTPARAMETERS_COMMENTS_ID,
        CCSDS_MANUEVER_IGNITIONEPOCH_ID,
        CCSDS_MANUEVER_DURATION_ID,
        CCSDS_MANUEVER_DELTAMASS_ID,
        CCSDS_MANUEVER_REFFRAME_ID,
        CCSDS_MANUEVER_DELTAV1_ID,
        CCSDS_MANUEVER_DELTAV2_ID,
        CCSDS_MANUEVER_DELTAV3_ID,
        CCSDS_MANUEVER_COMMENTS_ID,
        CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID,
        CCSDS_ATTITUDEMANUEVER_DURATION_ID,
        CCSDS_ATTITUDEMANUEVER_REFFRAME_ID,
        CCSDS_ATTITUDEMANUEVER_TOR1_ID,
        CCSDS_ATTITUDEMANUEVER_TOR2_ID,
        CCSDS_ATTITUDEMANUEVER_TOR3_ID,
        CCSDS_ATTITUDEMANUEVER_COMMENTS_ID,
        CCSDS_GENERALDATA_KEYWORD_ID,
        CCSDS_GENERALDATA_TIMETAG_ID,
        CCSDS_GENERALDATA_MEASUREMENT_ID,
        CCSDS_GENERALDATA_COMMENTS_ID,
	EndCCSDSDataReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
    friend class ProcessCCSDSOPMDataFile;
    friend class ProcessCCSDSOEMDataFile;
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
    CCSDSAttitudeManeuver *ccsdsAttitudeManeuver;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
