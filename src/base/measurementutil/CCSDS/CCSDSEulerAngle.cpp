#include "CCSDSEulerAngle.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSEulerAngle::CCSDS_EULERANGLE_KEYWORDS[EndCCSDSEulerAngleDataReps] =
{
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

const Gmat::ParameterType CCSDSEulerAngle::CCSDS_PARAMETER_TYPE[EndCCSDSEulerAngleDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
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
CCSDSEulerAngle::CCSDSEulerAngle() : CCSDSData(),
    eulerAngleType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    timeTag(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameA(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameB(GmatBase::STRING_PARAMETER_UNDEFINED),
    direction(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    rotationSequence(GmatBase::STRING_PARAMETER_UNDEFINED),
    rateFrame(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    xAngle(GmatBase::REAL_PARAMETER_UNDEFINED),
    yAngle(GmatBase::REAL_PARAMETER_UNDEFINED),
    zAngle(GmatBase::REAL_PARAMETER_UNDEFINED),
    xRate(GmatBase::REAL_PARAMETER_UNDEFINED),
    yRate(GmatBase::REAL_PARAMETER_UNDEFINED),
    zRate(GmatBase::REAL_PARAMETER_UNDEFINED),
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
    CCSDSData(ea),
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

    CCSDSData::operator=(ea);

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

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;

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
    if (id >= 0 && id <= EndCCSDSEulerAngleDataReps)
        return CCSDS_UNIT_DESCRIPTIONS[id];
    else
        return GmatBase::STRING_PARAMETER_UNDEFINED;
}
