//$Header$
//------------------------------------------------------------------------------
//                             QuaternionCCSDSData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the base class for the Quaternion data construct
 *  that is used by the CCSDS Attitude Parameter and Ephemeris Message formats.
 *
 */
//------------------------------------------------------------------------------

#include "QuaternionCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string QuaternionCCSDSData::CCSDS_QUATERNION_TYPE[EndQuaternionCCSDSDataTypeReps] =
{
    "FIRST",
    "LAST"
};

const std::string QuaternionCCSDSData::CCSDS_QUATERNION_KEYWORDS[EndQuaternionCCSDSDataReps] =
{
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
    "COMMENT"
};

const std::string QuaternionCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndQuaternionCCSDSDataReps] =
{
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
    ""
};

const std::string QuaternionCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndQuaternionCCSDSDataReps] =
{
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
    "Quaternion Comments"
};

const Gmat::ParameterType QuaternionCCSDSData::CCSDS_PARAMETER_TYPE[EndQuaternionCCSDSDataReps] =
{
    Gmat::STRING_TYPE,
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
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};
  
//------------------------------------------------------------------------------
//  QuaternionCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the QuaternionCCSDSData class
 */
//------------------------------------------------------------------------------
QuaternionCCSDSData::QuaternionCCSDSData() : CCSDSData(),
    attitudeType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    quaternionType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    timeTag(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameA(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameB(GmatBase::STRING_PARAMETER_UNDEFINED),
    direction(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    q1(GmatBase::REAL_PARAMETER_UNDEFINED),
    q2(GmatBase::REAL_PARAMETER_UNDEFINED),
    q3(GmatBase::REAL_PARAMETER_UNDEFINED),
    qC(GmatBase::REAL_PARAMETER_UNDEFINED),
    q1Dot(GmatBase::REAL_PARAMETER_UNDEFINED),
    q2Dot(GmatBase::REAL_PARAMETER_UNDEFINED),
    q3Dot(GmatBase::REAL_PARAMETER_UNDEFINED),
    qCDot(GmatBase::REAL_PARAMETER_UNDEFINED),
    xRate(GmatBase::REAL_PARAMETER_UNDEFINED),
    yRate(GmatBase::REAL_PARAMETER_UNDEFINED),
    zRate(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  QuaternionCCSDSData(const QuaternionCCSDSData &myQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
QuaternionCCSDSData::QuaternionCCSDSData(const QuaternionCCSDSData &myQ) :
    CCSDSData(myQ),
    attitudeType(myQ.attitudeType),
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
//  QuaternionCCSDSData& operator=(const QuaternionCCSDSData &myQ)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <myQ> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const QuaternionCCSDSData& QuaternionCCSDSData::operator=(const QuaternionCCSDSData &myQ)

{
    if (&myQ == this)
        return *this;

    CCSDSData::operator=(myQ);

    attitudeType = myQ.attitudeType;
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
//  ~QuaternionCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the QuaternionCCSDSData class
 */
//------------------------------------------------------------------------------
QuaternionCCSDSData::~QuaternionCCSDSData()
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
std::string QuaternionCCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndQuaternionCCSDSDataReps))
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
Integer QuaternionCCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndQuaternionCCSDSDataReps; i++)
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
Gmat::ParameterType QuaternionCCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndQuaternionCCSDSDataReps))
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
std::string QuaternionCCSDSData::GetDataParameterTypeString(const Integer id) const
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
Integer QuaternionCCSDSData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_QUATERNION_DIRECTION_ID:

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
Integer QuaternionCCSDSData::GetIntegerDataParameter(const std::string &label) const
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
Real QuaternionCCSDSData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_QUATERNION_Q1_ID:

            return q1;

	case CCSDS_QUATERNION_Q2_ID:

            return q2;

	case CCSDS_QUATERNION_Q3_ID:

            return q3;

	case CCSDS_QUATERNION_QC_ID:

            return qC;

	case CCSDS_QUATERNION_Q1DOT_ID:

            return q1Dot;

        case CCSDS_QUATERNION_Q2DOT_ID:

            return q2Dot;

        case CCSDS_QUATERNION_Q3DOT_ID:

            return q3Dot;

        case CCSDS_QUATERNION_QCDOT_ID:

            return qCDot;

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
Real QuaternionCCSDSData::GetRealDataParameter(const std::string &label) const
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
std::string QuaternionCCSDSData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_QUATERNION_TIMETAG_ID:

	    return timeTag;

        case CCSDS_QUATERNION_FRAMEA_ID:

	    return frameA;

	case CCSDS_QUATERNION_FRAMEB_ID:

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
std::string QuaternionCCSDSData::GetStringDataParameter(const std::string &label) const
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
StringArray QuaternionCCSDSData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_QUATERNION_COMMENTS_ID:

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
StringArray QuaternionCCSDSData::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* QuaternionCCSDSData::GetKeywords() const
{
   return CCSDS_QUATERNION_KEYWORDS;
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
const Integer QuaternionCCSDSData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";
    for (Integer i = 0; i < EndQuaternionCCSDSDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_QUATERNION_KEYWORDS[i]))
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
std::string QuaternionCCSDSData::GetUnits(const Integer &id) const
{
    if (id >= 0 && id <= EndQuaternionCCSDSDataReps)
        return CCSDS_UNIT_DESCRIPTIONS[id];
    else
        return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetQuaternionTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the quaternion type keyword for a specific ID
 *
 * @param <id> The quaternion type id
 * @return The quaternion type keyword
 *
 */
//------------------------------------------------------------------------------
std::string QuaternionCCSDSData::GetQuaternionTypeText(const Integer id) const
{
   if ((id >= 0) && (id < EndQuaternionCCSDSDataTypeReps))
       return CCSDS_QUATERNION_TYPE[id];
   else
       return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetQuaternionTypeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an quaternion type keyword
 *
 * @param <str> The quaternion type keyword
 * @return The quaternion type id
 *
 */
//------------------------------------------------------------------------------
Integer QuaternionCCSDSData::GetQuaternionTypeID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndQuaternionCCSDSDataTypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_QUATERNION_TYPE[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}
