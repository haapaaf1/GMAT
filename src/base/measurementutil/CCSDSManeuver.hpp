/*
 * File:   CCSDSManeuver.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSMANEUVER_HPP
#define	_CCSDSMANEUVER_HPP

class CCSDSManeuver
{

public:

    CCSDSManeuver();
    CCSDSManeuver(const CCSDSManeuver &man);
    const CCSDSManeuver& CCSDSManeuver::operator=(const CCSDSManeuver &man);
    ~CCSDSManeuver();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSManeuver *myCCSDSManeuver);


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

