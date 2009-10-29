#include "CCSDSEulerAngle.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSEulerAngle::CCSDS_RATE_FRAME[EndCCSDSRateFrameReps] =
{
    "REF_FRAME_A",
    "REF_FRAME_B"
};

const std::string CCSDSEulerAngle::CCSDS_EULERANGLE_KEYWORDS[EndCCSDSEulerAngleDataReps] =
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
CCSDSEulerAngle::CCSDSEulerAngle() :
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
CCSDSEulerAngle::CCSDSEulerAngle(const CCSDSEulerAngle &ea) :
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
//  CCSDSEulerAngle& operator=(const CCSDSEulerAngle &ea)
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

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSEulerAngle::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSEulerAngleDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;;
}

//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSEulerAngle::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSEulerAngleDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSEulerAngle::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSEulerAngleDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSEulerAngle::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;;
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSEulerAngle::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_EULERANGLE_TYPE_ID:

	    return eulerAngleType;

	case CCSDS_EULERANGLE_DIRECTION_ID:

	    return direction;

        case CCSDS_EULERANGLE_RATEFRAME_ID:

	    return rateFrame;

     default:

        return GmatBase::INTEGER_PARAMETER_UNDEFINED;;

    }

}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSEulerAngle::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSEulerAngle::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_EULERANGLE_XANGLE_ID:

            return xAngle;

        case CCSDS_EULERANGLE_YANGLE_ID:

            return yAngle;

        case CCSDS_EULERANGLE_ZANGLE_ID:

            return zAngle;

        case CCSDS_EULERANGLE_XRATE_ID:

            return xRate;

        case CCSDS_EULERANGLE_YRATE_ID:

            return yRate;

        case CCSDS_EULERANGLE_ZRATE_ID:

            return zRate;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSEulerAngle::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSEulerAngle::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_EULERANGLE_FRAMEA_ID:

	    return frameA;

	case CCSDS_EULERANGLE_FRAMEB_ID:

	    return frameB;

        case CCSDS_EULERANGLE_ROTATIONSEQUENCE_ID:

	    return rotationSequence;

        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSEulerAngle::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSEulerAngle::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_EULERANGLE_COMMENTS_ID:

	    return comments;

        default:

            return GmatBase::STRINGARRAY_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSEulerAngle::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS APM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSEulerAngle::GetKeywords() const
{
   return CCSDS_EULERANGLE_KEYWORDS;
}

//------------------------------------------------------------------------------
//  const Integer GetKeywordID(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
const Integer CCSDSEulerAngle::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSEulerAngleDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_EULERANGLE_KEYWORDS[i]))
            return i;
    }

   return -1;

}

//------------------------------------------------------------------------------
//  std::string GetUnits(const Integer &id) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
std::string CCSDSEulerAngle::GetUnits(const Integer &id) const
{
    if (id > 0 && id <= EndCCSDSEulerAngleDataReps)
        return CCSDS_UNIT_DESCRIPTIONS[id];
    else
        return GmatBase::STRING_PARAMETER_UNDEFINED;
}


//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSEulerAngle::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSEulerAngleDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberEulerAngleParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberEulerAngleParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSEulerAngle::EndCCSDSEulerAngleDataReps; id++)
        if (CCSDSEulerAngle::CCSDS_IS_REQUIRED[id])
            num++;

    return num;
}

//------------------------------------------------------------------------------
//  bool CheckDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool CCSDSEulerAngle::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSEulerAngleDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

