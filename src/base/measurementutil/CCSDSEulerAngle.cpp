#include "CCSDSEulerAngle.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMObType::CCSDS_QUATERNION_KEYWORDS[EndCCSDSQuaternionDataReps] =
{
    "",
    "EULER_FRAME_A",
    "EULER_FRAME_B",
    "EULER_DIR",
    "EULER_ROT_SEQ",
    "RATE_FRAME",
    "X_ANGLE",
    "Y_ANGLE",
    "Z_ANGLE",
    "X_RATE",
    "Y_RATE",
    "Z_RATE",
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    "deg",
    "deg",
    "deg",
    "deg/s",
    "deg/s",
    "deg/s",
    ""
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
    "Euler Angle Type",
    "Euler Angle Frame A",
    "Euler Angle Frame B",
    "Euler Angle Direction",
    "Euler Angle Rotation Sequence",
    "Euler Angle Rate Frame",
    "Euler Angle X Angle",
    "Euler Angle Y Angle",
    "Euler Angle Z Angle",
    "Euler Angle X Rate",
    "Euler Angle Y Rate",
    "Euler Angle Z Rate",
    "Euler Angle Comments"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSQuaternionDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
};

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSQuaternionDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};
