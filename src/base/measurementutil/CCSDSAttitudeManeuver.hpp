/*
 * File:   CCSDSAttitudeManeuver.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSATTITUDEMANEUVER_HPP
#define	_CCSDSATTITUDEMANEUVER_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "StringUtil.hpp"

class CCSDSAttitudeManeuver
{

public:

    CCSDSAttitudeManeuver();
    CCSDSAttitudeManeuver(const CCSDSAttitudeManeuver &am);
    const CCSDSAttitudeManeuver& CCSDSAttitudeManeuver::operator=(const CCSDSAttitudeManeuver &am);
    ~CCSDSAttitudeManeuver();

    friend std::ostream& operator<< (std::ostream &output,
                       const CCSDSAttitudeManeuver *myCCSDSAttitudemaneuver);

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
        CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID,
        CCSDS_ATTITUDEMANUEVER_DURATION_ID,
        CCSDS_ATTITUDEMANUEVER_REFFRAME_ID,
        CCSDS_ATTITUDEMANUEVER_TOR1_ID,
        CCSDS_ATTITUDEMANUEVER_TOR2_ID,
        CCSDS_ATTITUDEMANUEVER_TOR3_ID,
        CCSDS_ATTITUDEMANUEVER_COMMENTS_ID,
        EndCCSDSAttitudeManeuverDataReps
    };

protected:

    static const std::string CCSDS_ATTITUDEMANEUVER_KEYWORDS[EndCCSDSAttitudeManeuverDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAttitudeManeuverDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSAttitudeManeuverDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSAttitudeManeuverDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAttitudeManeuverDataReps];

    std::string epochStart;
    Real duration;
    std::string refFrame;
    Real tor1, tor2, tor3;
    StringArray comments;
    
};

#endif	/* _CCSDSATTITUDEMANEUVER_HPP */

