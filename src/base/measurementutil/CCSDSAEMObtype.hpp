/* 
 * File:   CCSDSAEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:23 AM
 */

#ifndef _CCSDSAEMOBTYPE_HPP
#define	_CCSDSAEMOBTYPE_HPP

#include "CCSDSObtype.hpp"
#include "CCSDSAEMMetaData.hpp"

class CCSDSAEMQuaternion : public CCSDSQuaternion
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMQuaternion *myCCSDSAEMQuaternion);

};

class CCSDSAEMEulerAngle : public CCSDSEulerAngle
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMEulerAngle *myCCSDSAEMEulerAngle);
};

// The CCSDS spin stabilized attitude specification.
class CCSDSAEMSpinStabilized : public CCSDSSpinStabilized
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                              const CCSDSAEMSpinStabilized *myCCSDSAEMSpinStabilized);
};

class CCSDSAEMObType : public CCSDSObType
{

public:

    CCSDSAEMObType();
    CCSDSAEMObType(const CCSDSAEMObType &AEM);
    const CCSDSAEMObType& CCSDSAEMObType::operator=(const CCSDSAEMObType &AEM);
    ~CCSDSAEMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMObType *myAEM);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
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
	EndCCSDSAEMTimeReps
    };

    friend class ProcessCCSDSAEMDataFile;

protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps - EndCCSDSTimeReps];

    // Pointer to the metadata record
    CCSDSAEMMetaData* ccsdsAEMMetaData;
    CCSDSAEMQuaternion *ccsdsAEMQuaternion;
    CCSDSAEMEulerAngle *ccsdsAEMEulerAngle;
    CCSDSAEMSpinStabilized *ccsdsAEMSpinStabilized;

};

#endif	/* _CCSDSAEMOBTYPE_HPP */

