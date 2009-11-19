#include "CCSDSEulerAngle.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSEulerAngle::CCSDS_EULERSEQUENCE_LIST[EndCCSDSEulerSeqList] =
{
     "123",
     "132",
     "213",
     "231",
     "312",
     "321",
     "121",
     "131",
     "212",
     "232",
     "313",
     "323"
};

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

const std::string CCSDSEulerAngle::CCSDS_FILEFORMAT_DESCRIPTIONS[EndEulerAngleDataReps] =
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
    "Euler Angle Comments",
    "",
    "Euler Angle 1",
    "Euler Angle 2",
    "Euler Angle 3",
    "Euler Rate 1",
    "Euler Rate 2",
    "Euler Rate 3"
};

const Gmat::ParameterType CCSDSEulerAngle::CCSDS_PARAMETER_TYPE[EndCCSDSEulerAngleDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
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
    rotationSequence(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    rateFrame(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    angle1(GmatBase::REAL_PARAMETER_UNDEFINED),
    angle2(GmatBase::REAL_PARAMETER_UNDEFINED),
    angle3(GmatBase::REAL_PARAMETER_UNDEFINED),
    angleRate1(GmatBase::REAL_PARAMETER_UNDEFINED),
    angleRate2(GmatBase::REAL_PARAMETER_UNDEFINED),
    angleRate3(GmatBase::REAL_PARAMETER_UNDEFINED),
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
    angle1(ea.angle1),
    angle2(ea.angle2),
    angle3(ea.angle3),
    angleRate1(ea.angleRate1),
    angleRate2(ea.angleRate2),
    angleRate3(ea.angleRate3),
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
    angle1 = ea.angle1;
    angle2 = ea.angle2;
    angle3 = ea.angle3;
    angleRate1 = ea.angleRate1;
    angleRate2 = ea.angleRate2;
    angleRate3 = ea.angleRate3;
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
//  std::string GetEulerSequenceText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the Euler angle sequence for a specific ID
 *
 * @param <id> The Euler angle sequence id
 * @return The Euler angle sequence
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSEulerAngle::GetEulerSequenceText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSEulerSeqList))
   {
      return CCSDS_EULERSEQUENCE_LIST[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetEulerSequenceID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an Euler angle sequence
 *
 * @param <str> The Euler angle sequence
 * @return The Euler angle sequence id
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSEulerAngle::GetEulerSequenceID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSEulerSeqList; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_EULERSEQUENCE_LIST[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

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

    for (Integer i = 0; i < EndEulerAngleDataReps; i++)
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

        case CCSDS_EULERANGLE_ROTATIONSEQUENCE_ID:

	    return rotationSequence;

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

        case CCSDS_EULERANGLE_ANGLE1_ID:

            return angle1;

        case CCSDS_EULERANGLE_ANGLE2_ID:

            return angle2;

        case CCSDS_EULERANGLE_ANGLE3_ID:

            return angle3;

        case CCSDS_EULERANGLE_RATE1_ID:

            return angleRate1;

        case CCSDS_EULERANGLE_RATE2_ID:

            return angleRate2;

        case CCSDS_EULERANGLE_RATE3_ID:

            return angleRate3;

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
