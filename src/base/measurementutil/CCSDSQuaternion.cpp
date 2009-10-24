#include "CCSDSQuaternion.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSObType::CCSDS_QUATERNION_TYPE[EndCCSDSQuaternionTypeReps] =
{
    "FIRST",
    "LAST"
};

const std::string CCSDSAPMObType::CCSDS_QUATERNION_KEYWORDS[EndCCSDSQuaternionDataReps] =
{
    "",
    "EPOCH",
    "Q_FRAME_A",
    "Q_FRAME_B",
    "Q_DIR",
    "Q1",
    "Q2",
    "Q3",
    "QC",
    "Q1_DOT",
    "Q2_DOT",
    "Q3_DOT",
    "QC_DOT",
    "X_RATE",
    "Y_RATE",
    "Z_RATE",
    "COMMENT",
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
    "COMMENT",
    "",
    "SPIN_FRAME_A",
    "SPIN_FRAME_B",
    "SPIN_DIR",
    "SPIN_ALPHA",
    "SPIN_DELTA",
    "SPIN_ANGLE",
    "SPIN_ANGLE_VEL",
    "NUTATION",
    "NUTATION_PER",
    "NUTATION_PHASE",
    "COMMENT",
    "INERTIA_REF_FRAME",
    "I11",
    "I22",
    "I33",
    "I12",
    "I13",
    "I23",
    "COMMENT",
    "MAN_EPOCH_START",
    "MAN_DURATION",
    "MAN_REF_FRAME",
    "MAN_TOR1",
    "MAN_TOR2",
    "MAN_TOR3",
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "",
    "",
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
    "",
    "",
    "",
    "",
    "",
    "deg",
    "deg",
    "deg",
    "deg/s",
    "deg",
    "s",
    "deg",
    "",
    "",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "",
    "",
    "s",
    "",
    "N m",
    "N m",
    "N m",
    ""
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
    "Quaternion Type",
    "Quaternion Epoch",
    "Quaternion Frame A",
    "Quaternion Frame B",
    "Quaternion Direction",
    "Quaternion Q1",
    "Quaternion Q2",
    "Quaternion Q3",
    "Quaternion QC",
    "Quaternion Q1 Dot",
    "Quaternion Q2 Dot",
    "Quaternion Q3 Dot",
    "Quaternion QC Dot",
    "Quaternion X Rate",
    "Quaternion Y Rate",
    "Quaternion Z Rate",
    "Quaternion Comments",
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
    "Euler Angle Comments",
    "Spin Stabilized Attitude Type",
    "Spin Stabilized Frame A",
    "Spin Stabilized Frame B",
    "Spin Stabilized Direction",
    "Spin Stabilized Spin Alpha",
    "Spin Stabilized Spin Delta",
    "Spin Stabilized Spin Angle",
    "Spin Stabilized Spin Angle Velocity",
    "Spin Stabilized Nutation",
    "Spin Stabilized Nutation Period",
    "Spin Stabilized Nutation Phase",
    "Spin Stabilized Comments",
    "Spacecraft Inertia Ref Frame",
    "Spacecraft Inertia Component (1,1)",
    "Spacecraft Inertia Component (2,2)",
    "Spacecraft Inertia Component (3,3)",
    "Spacecraft Inertia Component (1,2)",
    "Spacecraft Inertia Component (1,3)",
    "Spacecraft Inertia Component (2,3)",
    "Spacecraft Inertia Comments",
    "Attitude Maneuver Epoch Start",
    "Attitude Maneuver Duration",
    "Attitude Maneuver Ref Frame",
    "Attitude Maneuver TOR1",
    "Attitude Maneuver TOR2",
    "Attitude Maneuver TOR3",
    "Attitude Maneuver Comments"
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
    true,
    true,
    true,
    true,
    true,
    false,
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
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    false
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
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
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
    Gmat::STRINGARRAY_TYPE,
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
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSQuaternion()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSQuaternion class
 */
//------------------------------------------------------------------------------
CCSDSQuaternion::CCSDSQuaternion() : CCSDSObType(),
    quaternionType(0),
    timeTag(std::string("")),
    frameA(std::string("")),
    frameB(std::string("")),
    direction(0),
    q1(0),
    q2(0),
    q3(0),
    qC(0),
    q1Dot(0),
    q2Dot(0),
    q3Dot(0),
    qCDot(0),
    xRate(0),
    yRate(0),
    zRate(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSQuaternion(const CCSDSQuaternion &myQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSQuaternion::CCSDSQuaternion(const CCSDSQuaternion &myQ) : CCSDSObType(myQ),
    quaternionType(myQ.quaternionType),
    timeTag(myQ.timeTag),
    frameA(myQ.frameA),
    frameB(myQ.frameB),
    direction(myQ.direction),
    q1(myQ.q1),
    q2(myQ.q2),
    q3(myQ.q3),
    qC(myQ.qC),
    q1Dot(myQ.q1Dot),
    q2Dot(myQ.q2Dot),
    q3Dot(myQ.q3Dot),
    qCDot(myQ.qCDot),
    xRate(myQ.xRate),
    yRate(myQ.yRate),
    zRate(myQ.zRate),
    comments(myQ.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSQuaternion& operator=
//                                   (const CCSDSQuaternion &myQ)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <myQ> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSQuaternion& CCSDSQuaternion::operator=(const CCSDSQuaternion &myQ)

{
    if (&myQ == this)
        return *this;

    CCSDSObType::operator=(myQ);

    quaternionType = myQ.quaternionType;
    timeTag = myQ.timeTag;
    frameA = myQ.frameA;
    frameB = myQ.frameB;
    direction = myQ.direction;
    q1 = myQ.q1;
    q2 = myQ.q2;
    q3 = myQ.q3;
    qC = myQ.qC;
    q1Dot = myQ.q1Dot;
    q2Dot = myQ.q2Dot;
    q3Dot = myQ.q3Dot;
    qCDot = myQ.qCDot;
    xRate = myQ.xRate;
    yRate = myQ.yRate;
    zRate = myQ.zRate;
    comments = myQ.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSQuaternion()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSQuaternion class
 */
//------------------------------------------------------------------------------
CCSDSQuaternion::~CCSDSQuaternion()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSQuaternion.
 *
 * @return clone of the CCSDSQuaternion.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSQuaternion::Clone() const
{
   GmatBase *clone = new CCSDSQuaternion(*this);
   return (clone);
}
