#include "CCSDSSpinStabilized.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMObType::CCSDS_QUATERNION_KEYWORDS[EndCCSDSQuaternionDataReps] =
{
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
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
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
    ""
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
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
    "Spin Stabilized Comments"
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
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpinStabilized::CCSDSSpinStabilized() : CCSDSObtype(),
{
}

//------------------------------------------------------------------------------
//  CCSDSSpinStabilized(const CCSDSSpinStabilized &ss)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpinStabilized::CCSDSSpinStabilized(const CCSDSSpinStabilized &ss) : CCSDSObtype(ss),
{
}

//---------------------------------------------------------------------------
//  CCSDSSpinStabilized& operator=(const CCSDSSpinStabilized &ss)
//---------------------------------------------------------------------------
/**
 * Asssgnment operator for ObType structures.
 *
 * @param <ss> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSSpinStabilized& CCSDSSpinStabilized::operator=(const CCSDSSpinStabilized &ss)
{
   if (&ss == this)
      return *this;

   CCSDSObtype::operator=(ss);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpinStabilized::~CCSDSSpinStabilized()
{
}
