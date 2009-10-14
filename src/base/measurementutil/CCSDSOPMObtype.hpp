/* 
 * File:   CCSDSOPMObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:20 AM
 */

#ifndef _CCSDSOPMOBTYPE_HPP
#define	_CCSDSOPMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "Anomaly.hpp"

class CCSDSOPMSpacecraftParameters
{

public:

    CCSDSOPMSpacecraftParameters();
    CCSDSOPMSpacecraftParameters(const CCSDSOPMSpacecraftParameters &opmSP);
    const CCSDSOPMSpacecraftParameters& CCSDSOPMSpacecraftParameters::operator=
                                    (const CCSDSOPMSpacecraftParameters &opmSP);
    ~CCSDSOPMSpacecraftParameters();

    friend std::ostream& operator<< (std::ostream &output,
               const CCSDSOPMSpacecraftParameters *myCCSDSspacecraftParameters);

    Real mass;
    Real solarRadiationArea;
    Real solarRadiationCoefficient;
    Real dragArea;
    Real dragCoefficient;
    StringArray comments;
};

class CCSDSOPMKeplerianElements
{

public:

    CCSDSOPMKeplerianElements();
    CCSDSOPMKeplerianElements(const CCSDSOPMKeplerianElements &opmKE);
    const CCSDSOPMKeplerianElements& CCSDSOPMKeplerianElements::operator=
                                       (const CCSDSOPMKeplerianElements &opmKE);
    ~CCSDSOPMKeplerianElements();

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSOPMKeplerianElements *myCCSDSOPMKeplerianElements);

    Real semiMajorAxis;
    Real eccentricity;
    Real inclination;
    Real raan;
    Real argumentOfPericenter;
    Anomaly theAnomaly;
    Real gravitationalCoefficient;
    StringArray comments;
};

class CCSDSOPMStateVector : public CCSDSStateVector
{

public:

    CCSDSOPMStateVector();
    CCSDSOPMStateVector(const CCSDSOPMStateVector &opmSV);
    const CCSDSOPMStateVector& CCSDSOPMStateVector::operator=(const CCSDSOPMStateVector &opmSV);
    ~CCSDSOPMStateVector();

    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSOPMStateVector *myCCSDSOPMStateVector);
};

class CCSDSOPMManeuver
{

public:

    CCSDSOPMManeuver();
    CCSDSOPMManeuver(const CCSDSOPMManeuver &opmM);
    const CCSDSOPMManeuver& CCSDSOPMManeuver::operator=(const CCSDSOPMManeuver &opmM);
    ~CCSDSOPMManeuver();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOPMManeuver *myCCSDSOPMManeuver);

    std::string ignitionEpoch;
    Real duration;
    Real deltaMass;
    std::string refFrame;
    Real deltaV1, deltaV2, deltaV3;
    StringArray comments;
};

class CCSDSOPMMetaData
{
public:

    CCSDSOPMMetaData();
    CCSDSOPMMetaData(const CCSDSOPMMetaData &opmMD);
    const CCSDSOPMMetaData& CCSDSOPMMetaData::operator=(const CCSDSOPMMetaData &opmMD);
    ~CCSDSOPMMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOPMMetaData *myMetadata);
    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string refFrame;
    std::string timeSystem;
    StringArray comments;
};
    
class CCSDSOPMObType : public CCSDSObType
{

public :

    CCSDSOPMObType();
    CCSDSOPMObType(const CCSDSOPMObType &opm);
    const CCSDSOPMObType& CCSDSOPMObType::operator=(const CCSDSOPMObType &opm);
    ~CCSDSOPMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOPMObType *myOPM);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;
    
    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    bool IsParameterRequired(const Integer id) const;

    enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        TT_ID,
        GPS_ID,
	TDB_ID,
        TCB_ID,
	EndCCSDSOPMTimeReps
    };

    enum CCSDS_DATA_REPS
    {
        CCSDS_OPM_OBJECTNAME_ID = EndCCSDSDataReps,
        CCSDS_OPM_OBJECTID_ID,
        CCSDS_OPM_CENTERNAME_ID,
        CCSDS_OPM_REFFRAME_ID,
	CCSDS_OPM_TIMESYSTEM_ID,
        CCSDS_OPM_METADATACOMMENTS_ID,
	CCSDS_OPM_STATEVECTOR_EPOCH_ID,
	CCSDS_OPM_STATEVECTOR_X_ID,
	CCSDS_OPM_STATEVECTOR_Y_ID,
	CCSDS_OPM_STATEVECTOR_Z_ID,
	CCSDS_OPM_STATEVECTOR_XDOT_ID,
        CCSDS_OPM_STATEVECTOR_YDOT_ID,
	CCSDS_OPM_STATEVECTOR_ZDOT_ID,
	CCSDS_OPM_STATEVECTOR_COMMENTS_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_EPOCH_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_ECCENTRICITY_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_INCLINATION_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_RAAN_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_TRUEANOMALY_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_MEANANOMALY_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_COMMENTS_ID,
	CCSDS_OPM_SPACECRAFTPARAMETERS_MASS_ID,
	CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID,
	CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID,
	CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGAREA_ID,
	CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID,
	CCSDS_OPM_SPACECRAFTPARAMETERS_COMMENTS_ID,
        CCSDS_OPM_MANUEVER_IGNITIONEPOCH_ID,
        CCSDS_OPM_MANUEVER_DURATION_ID,
        CCSDS_OPM_MANUEVER_DELTAMASS_ID,
        CCSDS_OPM_MANUEVER_REFFRAME_ID,
        CCSDS_OPM_MANUEVER_DELTAV1_ID,
        CCSDS_OPM_MANUEVER_DELTAV2_ID,
        CCSDS_OPM_MANUEVER_DELTAV3_ID,
        CCSDS_OPM_MANUEVER_COMMENTS_ID,
        EndCCSDSOPMDataReps
    };

    friend class ProcessCCSDSOPMDataFile;

private:

    static const std::string CCSDS_OPM_KEYWORDS[EndCCSDSOPMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOPMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps - EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSOPMDataReps - EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSOPMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMDataReps - EndCCSDSDataReps];

    // Pointer to the data records
    CCSDSOPMMetaData *ccsdsOPMMetaData;
    CCSDSOPMStateVector *ccsdsOPMStateVector;
    CCSDSOPMKeplerianElements *ccsdsOPMKeplerianElements;
    CCSDSOPMSpacecraftParameters *ccsdsOPMSpacecraftParameters;
    std::vector<CCSDSOPMManeuver*> ccsdsOPMManeuvers;
    std::vector<CCSDSOPMManeuver*>::const_iterator i_ccsdsOPMManeuvers;
};

#endif	/* _CCSDSOPMOBTYPE_HPP */

