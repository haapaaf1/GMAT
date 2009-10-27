#include "CCSDSSpinStabilized.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSSpinStabilized::CCSDS_SPINSTABILIZED_KEYWORDS[EndCCSDSSpinStabilizedDataReps] =
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

const std::string CCSDSSpinStabilized::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSSpinStabilizedDataReps] =
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

const std::string CCSDSSpinStabilized::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSSpinStabilizedDataReps] =
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

const bool CCSDSSpinStabilized::CCSDS_IS_REQUIRED[EndCCSDSSpinStabilizedDataReps] =
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
    false
};

const Gmat::ParameterType CCSDSSpinStabilized::CCSDS_PARAMETER_TYPE[EndCCSDSSpinStabilizedDataReps] =
{
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
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
CCSDSSpinStabilized::CCSDSSpinStabilized() :
    attitudeType(0),
    frameA(std::string("")),
    frameB(std::string("")),
    direction(0),
    spinAlpha(0),
    spinDelta(0),
    spinAngle(0),
    spinAngleVelocity(0),
    nutation(0),
    nutationPeriod(0),
    nutationPhase(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSSpinStabilized(const CCSDSSpinStabilized &ss)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpinStabilized::CCSDSSpinStabilized(const CCSDSSpinStabilized &ss) :
    attitudeType(ss.attitudeType),
    frameA(ss.frameA),
    frameB(ss.frameB),
    direction(ss.direction),
    spinAlpha(ss.spinAlpha),
    spinDelta(ss.spinDelta),
    spinAngle(ss.spinAngle),
    spinAngleVelocity(ss.spinAngleVelocity),
    nutation(ss.nutation),
    nutationPeriod(ss.nutationPeriod),
    nutationPhase(ss.nutationPhase),
    comments(ss.comments)
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

    attitudeType = ss.attitudeType;
    //timeTag = ss.timeTag;
    frameA = ss.frameA;
    frameB = ss.frameB;
    direction = ss.direction;
    spinAlpha = ss.spinAlpha;
    spinDelta = ss.spinDelta;
    spinAngle = ss.spinAngle;
    spinAngleVelocity = ss.spinAngleVelocity;
    nutation = ss.nutation;
    nutationPeriod = ss.nutationPeriod;
    nutationPhase = ss.nutationPhase;
    comments = ss.comments;

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
std::string CCSDSSpinStabilized::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSSpinStabilizedDataReps))
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
Integer CCSDSSpinStabilized::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpinStabilizedDataReps; i++)
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
Gmat::ParameterType CCSDSSpinStabilized::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSSpinStabilizedDataReps))
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
std::string CCSDSSpinStabilized::GetDataParameterTypeString(const Integer id) const
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
Integer CCSDSSpinStabilized::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_SPINSTABILIZED_ATTITUDETYPE_ID:

	    return attitudeType;

	case CCSDS_SPINSTABILIZED_DIRECTION_ID:

	    return direction;

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
Integer CCSDSSpinStabilized::GetIntegerDataParameter(const std::string &label) const
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
Real CCSDSSpinStabilized::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPINSTABILIZED_SPINALPHA_ID:

            return spinAlpha;

	case CCSDS_SPINSTABILIZED_SPINDELTA_ID:

            return spinDelta;

	case CCSDS_SPINSTABILIZED_SPINANGLE_ID:

            return spinAngle;

	case CCSDS_SPINSTABILIZED_SPINANGLEVEOCITY_ID:

            return spinAngleVelocity;

	case CCSDS_SPINSTABILIZED_NUTATION_ID:

            return nutation;

	case CCSDS_SPINSTABILIZED_NUTATIONPERIOD_ID:

            return nutationPeriod;

	case CCSDS_SPINSTABILIZED_NUTATIONPHASE_ID:

            return nutationPhase;

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
Real CCSDSSpinStabilized::GetRealDataParameter(const std::string &label) const
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
std::string CCSDSSpinStabilized::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPINSTABILIZED_FRAMEA_ID:

	    return frameA;

	case CCSDS_SPINSTABILIZED_FRAMEB_ID:

	    return frameB;

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
std::string CCSDSSpinStabilized::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSSpinStabilized::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_SPINSTABILIZED_COMMENTS_ID:

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
StringArray CCSDSSpinStabilized::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSSpinStabilized::GetKeywords() const
{
   return CCSDS_SPINSTABILIZED_KEYWORDS;
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
const Integer CCSDSSpinStabilized::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpinStabilizedDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_SPINSTABILIZED_KEYWORDS[i]))
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
std::string CCSDSSpinStabilized::GetUnits(const Integer &id) const
{
    if (id > 0 && id <= EndCCSDSSpinStabilizedDataReps)
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
bool CCSDSSpinStabilized::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSSpinStabilizedDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
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
bool CCSDSSpinStabilized::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpinStabilizedDataReps; i++)
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
