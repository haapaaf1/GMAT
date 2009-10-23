/*
 * File:   CCSDSAttitudeManeuver.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSATTITUDEMANEUVER_HPP
#define	_CCSDSATTITUDEMANEUVER_HPP

class CCSDSAttitudeManeuver
{

public:

    CCSDSAttitudeManeuver();
    CCSDSAttitudeManeuver(const CCSDSAttitudeManeuver &am);
    const CCSDSAttitudeManeuver& CCSDSAttitudeManeuver::operator=(const CCSDSAttitudeManeuver &am);
    ~CCSDSAttitudeManeuver();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                       const CCSDSAttitudeManeuver *myCCSDSAttitudemaneuver);

    enum CCSDS_DATA_REPS
    {
        CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID,
        CCSDS_ATTITUDEMANUEVER_DURATION_ID,
        CCSDS_ATTITUDEMANUEVER_REFFRAME_ID,
        CCSDS_ATTITUDEMANUEVER_TOR1_ID,
        CCSDS_ATTITUDEMANUEVER_TOR2_ID,
        CCSDS_ATTITUDEMANUEVER_TOR3_ID,
        CCSDS_ATTITUDEMANUEVER_COMMENTS_ID,
        EndCCSDSAPMDataReps
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

