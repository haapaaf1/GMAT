//$Header$
//------------------------------------------------------------------------------
//                             StateVectorCCSDSData
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
 * This class specifies the State Vector construct that is used by the
 * CCSDS Orbit Parameter and Ephemeris message formats.
 *
 */
//------------------------------------------------------------------------------

#include "StateVectorCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string StateVectorCCSDSData::CCSDS_STATEVECTOR_KEYWORDS[EndStateVectorCCSDSDataDataReps] =
{
    "EPOCH",
    "X",
    "Y",
    "Z",
    "X_DOT",
    "Y_DOT",
    "Z_DOT",
    "COMMENT"
};

const std::string StateVectorCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndStateVectorCCSDSDataDataReps] =
{
    "",
    "km",
    "km",
    "km",
    "km/s",
    "km/s",
    "km/s",
    ""
};

const std::string StateVectorCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndStateVectorCCSDSDataDataReps] =
{
    "Epoch",
    "X",
    "Y",
    "Z",
    "XDot",
    "YDot",
    "ZDot",
    "Comments"
};

const bool StateVectorCCSDSData::CCSDS_IS_REQUIRED[EndStateVectorCCSDSDataDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType StateVectorCCSDSData::CCSDS_PARAMETER_TYPE[EndStateVectorCCSDSDataDataReps] =
{
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
//  StateVectorCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVectorCCSDSData class
 */
//------------------------------------------------------------------------------
StateVectorCCSDSData::StateVectorCCSDSData() : CCSDSData(),
    timeTag(GmatBase::STRING_PARAMETER_UNDEFINED),
    x(GmatBase::REAL_PARAMETER_UNDEFINED),
    y(GmatBase::REAL_PARAMETER_UNDEFINED),
    z(GmatBase::REAL_PARAMETER_UNDEFINED),
    xDot(GmatBase::REAL_PARAMETER_UNDEFINED),
    yDot(GmatBase::REAL_PARAMETER_UNDEFINED),
    zDot(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  StateVectorCCSDSData(const StateVectorCCSDSData &sv)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
StateVectorCCSDSData::StateVectorCCSDSData(const StateVectorCCSDSData &sv) :
    CCSDSData(sv),
    timeTag(sv.timeTag),
    x(sv.x),
    y(sv.y),
    z(sv.z),
    xDot(sv.xDot),
    yDot(sv.yDot),
    zDot(sv.zDot),
    comments(sv.comments)
{
}

//---------------------------------------------------------------------------
//  StateVectorCCSDSData& operator=(const StateVectorCCSDSData &sv)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <sv> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const StateVectorCCSDSData& StateVectorCCSDSData::operator=(const StateVectorCCSDSData &sv)

{
    if (&sv == this)
        return *this;

    CCSDSData::operator=(sv);
    
    timeTag = sv.timeTag;
    x = sv.x;
    y = sv.y;
    z = sv.z;
    xDot = sv.xDot;
    yDot = sv.yDot;
    zDot = sv.zDot;
    comments = sv.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~StateVectorCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the StateVectorCCSDSData class
 */
//------------------------------------------------------------------------------
StateVectorCCSDSData::~StateVectorCCSDSData()
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
std::string StateVectorCCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndStateVectorCCSDSDataDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetUnits(const Integer &id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string StateVectorCCSDSData::GetUnits(const Integer &id) const
{
   if ((id >= 0) && (id < EndStateVectorCCSDSDataDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer StateVectorCCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndStateVectorCCSDSDataDataReps; i++)
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
Gmat::ParameterType StateVectorCCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndStateVectorCCSDSDataDataReps))
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
std::string StateVectorCCSDSData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real StateVectorCCSDSData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_STATEVECTOR_X_ID:

            return x;

	case CCSDS_STATEVECTOR_Y_ID:

            return y;

	case CCSDS_STATEVECTOR_Z_ID:

            return z;

	case CCSDS_STATEVECTOR_XDOT_ID:

            return xDot;

        case CCSDS_STATEVECTOR_YDOT_ID:

            return yDot;

	case CCSDS_STATEVECTOR_ZDOT_ID:

            return zDot;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real StateVectorCCSDSData::GetRealDataParameter(const std::string &label) const
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
std::string StateVectorCCSDSData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_STATEVECTOR_TIMETAG_ID:

	    return timeTag;

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
std::string StateVectorCCSDSData::GetStringDataParameter(const std::string &label) const
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
StringArray StateVectorCCSDSData::GetStringArrayDataParameter(const Integer id) const
{

    switch (id)
    {
        case CCSDS_STATEVECTOR_COMMENTS_ID:

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
StringArray StateVectorCCSDSData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS OEM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* StateVectorCCSDSData::GetKeywords() const
{
   return CCSDS_STATEVECTOR_KEYWORDS;
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
const Integer StateVectorCCSDSData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndStateVectorCCSDSDataDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_STATEVECTOR_KEYWORDS[i]))
            return i;
    }

   return -1;

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
bool StateVectorCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndStateVectorCCSDSDataDataReps)
	return CCSDS_IS_REQUIRED[id];
    else
	return false;
}

//---------------------------------------------------------------------------
//  Integer CountRequiredNumberStateVectorParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberStateVectorParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < StateVectorCCSDSData::EndStateVectorCCSDSDataDataReps; id++)
        if (StateVectorCCSDSData::CCSDS_IS_REQUIRED[id])
            num++;

    return num;
}
//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool StateVectorCCSDSData::Validate() const
{

    for (unsigned int i = 0; i < EndStateVectorCCSDSDataDataReps; i++ )
    {
        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::REAL_TYPE:
                    if (!IsParameterDefined(GetRealDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Real parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRING_TYPE:
                    if (!IsParameterDefined(GetStringDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    if (!IsParameterDefined(GetStringArrayDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                default:
                    return false;
                    break;
            }
        }
    }

    return true;
}


