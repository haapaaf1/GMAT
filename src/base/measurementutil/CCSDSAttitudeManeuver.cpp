#include "CCSDSAttitudeManeuver.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMObType::CCSDS_QUATERNION_KEYWORDS[EndCCSDSAttitudeManeuverDataReps] =
{
    "MAN_EPOCH_START",
    "MAN_DURATION",
    "MAN_REF_FRAME",
    "MAN_TOR1",
    "MAN_TOR2",
    "MAN_TOR3",
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAttitudeManeuverDataReps] =
{
    "",
    "s",
    "",
    "N m",
    "N m",
    "N m",
    ""
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAttitudeManeuverDataReps] =
{
    "Attitude Maneuver Epoch Start",
    "Attitude Maneuver Duration",
    "Attitude Maneuver Ref Frame",
    "Attitude Maneuver TOR1",
    "Attitude Maneuver TOR2",
    "Attitude Maneuver TOR3",
    "Attitude Maneuver Comments"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSAttitudeManeuverDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSAttitudeManeuverDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAttitudeManeuver *myAttitudeManeuver)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mymyManeuver>    CCSDS maneuver data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAttitudeManeuver *myAttitudeManeuver)
{
    using namespace std;

    output << "MAN_EPOCH_START = " << myAttitudeManeuver->epochStart << endl;
    output << "MAN_DURATION = " << myAttitudeManeuver->duration << endl;
    output << "MAN_REF_FRAME = " << myAttitudeManeuver->refFrame << endl;
    output << "MAN_TOR_1 = " << myAttitudeManeuver->tor1 << endl;
    output << "MAN_TOR_2 = " << myAttitudeManeuver->tor2 << endl;
    output << "MAN_TOR_3 = " << myAttitudeManeuver->tor3 << endl;

    return output;
}