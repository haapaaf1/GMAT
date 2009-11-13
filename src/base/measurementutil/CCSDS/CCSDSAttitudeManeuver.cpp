#include "CCSDSAttitudeManeuver.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAttitudeManeuver::CCSDS_ATTITUDEMANEUVER_KEYWORDS[EndCCSDSAttitudeManeuverDataReps] =
{
    "MAN_EPOCH_START",
    "MAN_DURATION",
    "MAN_REF_FRAME",
    "MAN_TOR1",
    "MAN_TOR2",
    "MAN_TOR3",
    "COMMENT"
};

const std::string CCSDSAttitudeManeuver::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAttitudeManeuverDataReps] =
{
    "",
    "s",
    "",
    "N m",
    "N m",
    "N m",
    ""
};

const std::string CCSDSAttitudeManeuver::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAttitudeManeuverDataReps] =
{
    "Attitude Maneuver Epoch Start",
    "Attitude Maneuver Duration",
    "Attitude Maneuver Ref Frame",
    "Attitude Maneuver TOR1",
    "Attitude Maneuver TOR2",
    "Attitude Maneuver TOR3",
    "Attitude Maneuver Comments"
};

const bool CCSDSAttitudeManeuver::CCSDS_IS_REQUIRED[EndCCSDSAttitudeManeuverDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAttitudeManeuver::CCSDS_PARAMETER_TYPE[EndCCSDSAttitudeManeuverDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSAttitudeManeuver class
 */
//------------------------------------------------------------------------------
CCSDSAttitudeManeuver::CCSDSAttitudeManeuver() : CCSDSData(),
    epochStart(std::string("")),
    duration(0),
    refFrame(std::string("")),
    tor1(0),
    tor2(0),
    tor3(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSAttitudeManeuver(const CCSDSAttitudeManeuver &am)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSAttitudeManeuver::CCSDSAttitudeManeuver(const CCSDSAttitudeManeuver &am) :
    CCSDSData(am),
    epochStart(am.epochStart),
    duration(am.duration),
    refFrame(am.refFrame),
    tor1(am.tor1),
    tor2(am.tor2),
    tor3(am.tor3),
    comments(am.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSAttitudeManeuver& operator=(const CCSDSAttitudeManeuver &ea)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <ea> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAttitudeManeuver& CCSDSAttitudeManeuver::operator=(const CCSDSAttitudeManeuver &am)

{
    if (&am == this)
        return *this;

    CCSDSData::operator=(am);

    epochStart = am.epochStart;
    duration = am.duration;
    refFrame = am.refFrame;
    tor1 = am.tor1;
    tor2 = am.tor2;
    tor3 = am.tor3;
    comments = am.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAttitudeManeuver()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSAttitudeManeuver class
 */
//------------------------------------------------------------------------------
CCSDSAttitudeManeuver::~CCSDSAttitudeManeuver()
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
std::string CCSDSAttitudeManeuver::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAttitudeManeuverDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
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
Integer CCSDSAttitudeManeuver::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAttitudeManeuverDataReps; i++)
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
Gmat::ParameterType CCSDSAttitudeManeuver::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAttitudeManeuverDataReps))
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
std::string CCSDSAttitudeManeuver::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSAttitudeManeuver::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_ATTITUDEMANUEVER_DURATION_ID:

            return duration;

        case CCSDS_ATTITUDEMANUEVER_TOR1_ID:

            return tor1;

        case CCSDS_ATTITUDEMANUEVER_TOR2_ID:

            return tor2;

        case CCSDS_ATTITUDEMANUEVER_TOR3_ID:

            return tor3;

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
Real CCSDSAttitudeManeuver::GetRealDataParameter(const std::string &label) const
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
std::string CCSDSAttitudeManeuver::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID:

	    return epochStart;

        case CCSDS_ATTITUDEMANUEVER_REFFRAME_ID:

	    return refFrame;

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
std::string CCSDSAttitudeManeuver::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAttitudeManeuver::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_ATTITUDEMANUEVER_COMMENTS_ID:

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
StringArray CCSDSAttitudeManeuver::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSAttitudeManeuver::GetKeywords() const
{
   return CCSDS_ATTITUDEMANEUVER_KEYWORDS;
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
const Integer CCSDSAttitudeManeuver::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAttitudeManeuverDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_ATTITUDEMANEUVER_KEYWORDS[i]))
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
std::string CCSDSAttitudeManeuver::GetUnits(const Integer &id) const
{
    if (id >= 0 && id <= EndCCSDSAttitudeManeuverDataReps)
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
bool CCSDSAttitudeManeuver::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSAttitudeManeuverDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAttitudeManeuverParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAttitudeManeuverParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSAttitudeManeuver::EndCCSDSAttitudeManeuverDataReps; id++)
        if (CCSDSAttitudeManeuver::CCSDS_IS_REQUIRED[id])
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
bool CCSDSAttitudeManeuver::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSAttitudeManeuverDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::REAL_TYPE:
                    {
                    Real rvalue = GetRealDataParameter(i);
                    if (&rvalue == NULL ||
                        rvalue == GmatBase::REAL_PARAMETER_UNDEFINED)
                        return false;
                    }
                    break;
                case Gmat::STRING_TYPE:
                    {
                    std::string svalue = GetStringDataParameter(i);
                    if (&svalue == NULL ||
                        svalue == GmatBase::STRING_PARAMETER_UNDEFINED)
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    {
                    StringArray savalue = GetStringArrayDataParameter(i);
                    if (&savalue == NULL ||
                        savalue == GmatBase::STRINGARRAY_PARAMETER_UNDEFINED)
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAttitudeManeuver *myAttitudeManeuver)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mymyManeuver>    CCSDS maneuver data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAttitudeManeuver *myAttitudeManeuver)
{
    using namespace std;

    output << "MAN_EPOCH_START = " << myAttitudeManeuver->epochStart << endl;
    output << "MAN_DURATION = " << myAttitudeManeuver->duration << endl;
    output << "MAN_REF_FRAME = " << myAttitudeManeuver->refFrame << endl;
    output << "MAN_TOR_1 = " << myAttitudeManeuver->tor1 << endl;
    output << "MAN_TOR_2 = " << myAttitudeManeuver->tor2 << endl;
    output << "MAN_TOR_3 = " << myAttitudeManeuver->tor3 << endl;

    return output;
}