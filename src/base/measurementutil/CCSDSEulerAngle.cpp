#include "CCSDSEulerAngle.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSEulerAngle::CCSDS_RATE_FRAME[EndCCSDSEulerAngleDataReps] =
{
    "REF_FRAME_A",
    "REF_FRAME_B"
};

const std::string CCSDSEulerAngle::CCSDS_EulerAngle_KEYWORDS[EndCCSDSEulerAngleDataReps] =
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

const std::string CCSDSEulerAngle::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSEulerAngleDataReps] =
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

const std::string CCSDSEulerAngle::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSEulerAngleDataReps] =
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

const bool CCSDSEulerAngle::CCSDS_IS_REQUIRED[EndCCSDSEulerAngleDataReps] =
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

const Gmat::ParameterType CCSDSEulerAngle::CCSDS_PARAMETER_TYPE[EndCCSDSEulerAngleDataReps] =
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

//------------------------------------------------------------------------------
//  CCSDSEulerAngle()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSEulerAngle class
 */
//------------------------------------------------------------------------------
CCSDSEulerAngle::CCSDSEulerAngle() : CCSDSObType(),
    eulerAngleType(0),
    timeTag(std::string("")),
    frameA(std::string("")),
    frameB(std::string("")),
    direction(0),
    rotationSequence(std::string("")),
    rateFrame(0),
    xAngle(0),
    yAngle(0),
    zAngle(0),
    xRate(0),
    yRate(0),
    zRate(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSEulerAngle(const CCSDSEulerAngle &ea)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSEulerAngle::CCSDSEulerAngle(const CCSDSEulerAngle &ea) : CCSDSObType(ea),
    eulerAngleType(ea.eulerAngleType),
    timeTag(ea.timeTag),
    frameA(ea.frameA),
    frameB(ea.frameB),
    direction(ea.direction),
    rotationSequence(ea.rotationSequence),
    rateFrame(ea.rateFrame),
    xAngle(ea.xAngle),
    yAngle(ea.yAngle),
    zAngle(ea.zAngle),
    xRate(ea.xRate),
    yRate(ea.yRate),
    zRate(ea.zRate),
    comments(ea.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSEulerAngle& operator=
//                                   (const CCSDSEulerAngle &ea)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <ea> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSEulerAngle& CCSDSEulerAngle::operator=(const CCSDSEulerAngle &ea)

{
    if (&ea == this)
        return *this;

    CCSDSObType::operator=(ea);

    eulerAngleType = ea.eulerAngleType;
    timeTag = ea.timeTag;
    frameA = ea.frameA;
    frameB = ea.frameB;
    direction = ea.direction;
    rotationSequence = ea.rotationSequence;
    rateFrame = ea.rateFrame;
    xAngle = ea.xAngle;
    yAngle = ea.yAngle;
    zAngle = ea.zAngle;
    xRate = ea.xRate;
    yRate = ea.yRate;
    zRate = ea.zRate;
    comments = ea.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSEulerAngle()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSEulerAngle class
 */
//------------------------------------------------------------------------------
CCSDSEulerAngle::~CCSDSEulerAngle()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSEulerAngle.
 *
 * @return clone of the CCSDSEulerAngle.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSEulerAngle::Clone() const
{
   GmatBase *clone = new CCSDSEulerAngle(*this);
   return (clone);
}

//------------------------------------------------------------------------------
//  std::string  GetRateFrameText(const Integer id)
//------------------------------------------------------------------------------
/**
 * Function to obtain the attitude type keyword for a specific ID
 *
 * @param <id> The rate frame type id
 * @return The rate frame type keyword
 *
 */
//------------------------------------------------------------------------------
std::string GetRateFrameText(const Integer id)
{
   if ((id >= 0) && (id < CCSDSEulerAngle::EndCCSDSRateFrameReps))
   {
      return CCSDSEulerAngle::CCSDS_RATE_FRAME[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetRateFrameID(const std::string &str)
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an rate frame keyword
 *
 * @param <str> The rate frame keyword
 * @return The rate frame id
 *
 */
//------------------------------------------------------------------------------
Integer GetRateFrameID(const std::string &str)
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < CCSDSEulerAngle::EndCCSDSRateFrameReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDSEulerAngle::CCSDS_RATE_FRAME[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}


