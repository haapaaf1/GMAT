/*
 * File:   CCSDSManeuver.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSMANEUVER_HPP
#define	_CCSDSMANEUVER_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>

class CCSDSManeuver
{

public:

    CCSDSManeuver();
    CCSDSManeuver(const CCSDSManeuver &man);
    const CCSDSManeuver& CCSDSManeuver::operator=(const CCSDSManeuver &man);
    ~CCSDSManeuver();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSManeuver *myCCSDSManeuver);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

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
        CCSDS_MANUEVER_IGNITIONEPOCH_ID,
        CCSDS_MANUEVER_DURATION_ID,
        CCSDS_MANUEVER_DELTAMASS_ID,
        CCSDS_MANUEVER_REFFRAME_ID,
        CCSDS_MANUEVER_DELTAV1_ID,
        CCSDS_MANUEVER_DELTAV2_ID,
        CCSDS_MANUEVER_DELTAV3_ID,
        CCSDS_MANUEVER_COMMENTS_ID,
        EndCCSDSManeuverDataReps
    };
    
protected:

    static const std::string CCSDS_MANEUVER_KEYWORDS[EndCCSDSManeuverDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSManeuverDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSManeuverDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSManeuverDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSManeuverDataReps];

    std::string ignitionEpoch;
    Real duration;
    Real deltaMass;
    std::string refFrame;
    Real deltaV1, deltaV2, deltaV3;
    StringArray comments;
};


#endif	/* _CCSDSMANEUVER_HPP */

