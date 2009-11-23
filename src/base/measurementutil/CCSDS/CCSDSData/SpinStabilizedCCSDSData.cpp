//$Header$
//------------------------------------------------------------------------------
//                             SpinStabilizedCCSDSData
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
 * This class specifies the base class for the Spin Stabilized data construct
 *  that is used by the CCSDS Attitude Parameter and Ephemeris Message formats.
 *
 */
//------------------------------------------------------------------------------

#include "SpinStabilizedCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string SpinStabilizedCCSDSData::CCSDS_SPINSTABILIZED_KEYWORDS[EndSpinStabilizedCCSDSDataDataReps] =
{
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

const std::string SpinStabilizedCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndSpinStabilizedCCSDSDataDataReps] =
{
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

const std::string SpinStabilizedCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndSpinStabilizedCCSDSDataDataReps] =
{
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

const Gmat::ParameterType SpinStabilizedCCSDSData::CCSDS_PARAMETER_TYPE[EndSpinStabilizedCCSDSDataDataReps] =
{
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
//  SpinStabilizedCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SpinStabilizedCCSDSData::SpinStabilizedCCSDSData() : CCSDSData(),
    attitudeType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    timeTag(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameA(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameB(GmatBase::STRING_PARAMETER_UNDEFINED),
    direction(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    spinAlpha(GmatBase::REAL_PARAMETER_UNDEFINED),
    spinDelta(GmatBase::REAL_PARAMETER_UNDEFINED),
    spinAngle(GmatBase::REAL_PARAMETER_UNDEFINED),
    spinAngleVelocity(GmatBase::REAL_PARAMETER_UNDEFINED),
    nutation(GmatBase::REAL_PARAMETER_UNDEFINED),
    nutationPeriod(GmatBase::REAL_PARAMETER_UNDEFINED),
    nutationPhase(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  SpinStabilizedCCSDSData(const SpinStabilizedCCSDSData &ss)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SpinStabilizedCCSDSData::SpinStabilizedCCSDSData(const SpinStabilizedCCSDSData &ss) :
    CCSDSData(ss),
    attitudeType(ss.attitudeType),
    timeTag(ss.timeTag),
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
//  SpinStabilizedCCSDSData& operator=(const SpinStabilizedCCSDSData &ss)
//---------------------------------------------------------------------------
/**
 * Asssgnment operator for ObType structures.
 *
 * @param <ss> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const SpinStabilizedCCSDSData& SpinStabilizedCCSDSData::operator=(const SpinStabilizedCCSDSData &ss)
{
   if (&ss == this)
      return *this;

   CCSDSData::operator=(ss);

   attitudeType = ss.attitudeType;
   timeTag = ss.timeTag;
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
//  ~SpinStabilizedCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
SpinStabilizedCCSDSData::~SpinStabilizedCCSDSData()
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
std::string SpinStabilizedCCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSpinStabilizedCCSDSDataDataReps))
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
Integer SpinStabilizedCCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSpinStabilizedCCSDSDataDataReps; i++)
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
Gmat::ParameterType SpinStabilizedCCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSpinStabilizedCCSDSDataDataReps))
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
std::string SpinStabilizedCCSDSData::GetDataParameterTypeString(const Integer id) const
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
Integer SpinStabilizedCCSDSData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        
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
Integer SpinStabilizedCCSDSData::GetIntegerDataParameter(const std::string &label) const
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
Real SpinStabilizedCCSDSData::GetRealDataParameter(const Integer id) const
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
Real SpinStabilizedCCSDSData::GetRealDataParameter(const std::string &label) const
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
std::string SpinStabilizedCCSDSData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPINSTABILIZED_TIMETAG_ID:

	    return timeTag;

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
std::string SpinStabilizedCCSDSData::GetStringDataParameter(const std::string &label) const
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
StringArray SpinStabilizedCCSDSData::GetStringArrayDataParameter(const Integer id) const
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
StringArray SpinStabilizedCCSDSData::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* SpinStabilizedCCSDSData::GetKeywords() const
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
const Integer SpinStabilizedCCSDSData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSpinStabilizedCCSDSDataDataReps; i++)
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
std::string SpinStabilizedCCSDSData::GetUnits(const Integer &id) const
{
    if (id >= 0 && id <= EndSpinStabilizedCCSDSDataDataReps)
        return CCSDS_UNIT_DESCRIPTIONS[id];
    else
        return GmatBase::STRING_PARAMETER_UNDEFINED;
}