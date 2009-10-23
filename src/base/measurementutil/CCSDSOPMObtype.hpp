/* 
 * File:   CCSDSOPMObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:20 AM
 */

#ifndef _CCSDSOPMOBTYPE_HPP
#define	_CCSDSOPMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "CCSDSOPMMetaData.hpp"

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

    friend class ProcessCCSDSOPMDataFile;

private:

    static const std::string CCSDS_OPM_KEYWORDS[EndCCSDSOPMDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOPMDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps - EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSOPMDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSOPMDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMDataReps];

    // Pointer to the data records
    CCSDSOPMMetaData *ccsdsOPMMetaData;
    CCSDSOPMStateVector *ccsdsOPMStateVector;
    CCSDSOPMKeplerianElements *ccsdsOPMKeplerianElements;
    CCSDSOPMSpacecraftParameters *ccsdsOPMSpacecraftParameters;
    std::vector<CCSDSOPMManeuver*> ccsdsOPMManeuvers;
    std::vector<CCSDSOPMManeuver*>::const_iterator i_ccsdsOPMManeuvers;
};

#endif	/* _CCSDSOPMOBTYPE_HPP */

