//$Header$
//------------------------------------------------------------------------------
//                             AttitudeManeuverCCSDSData
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
 * This class specifies the attitude maneuver data construct used in the
 * CCSDS Attitude Parameter message format.
 *
 */
//------------------------------------------------------------------------------

#include "AttitudeManeuverCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string AttitudeManeuverCCSDSData::CCSDS_ATTITUDEMANEUVER_KEYWORDS[EndAttitudeManeuverCCSDSDataDataReps] =
{
    "MAN_EPOCH_START",
    "MAN_DURATION",
    "MAN_REF_FRAME",
    "MAN_TOR_1",
    "MAN_TOR_2",
    "MAN_TOR_3",
    "COMMENT"
};

const std::string AttitudeManeuverCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndAttitudeManeuverCCSDSDataDataReps] =
{
    "",
    "s",
    "",
    "N m",
    "N m",
    "N m",
    ""
};

const std::string AttitudeManeuverCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndAttitudeManeuverCCSDSDataDataReps] =
{
    "Attitude Maneuver Epoch Start",
    "Attitude Maneuver Duration",
    "Attitude Maneuver Ref Frame",
    "Attitude Maneuver TOR1",
    "Attitude Maneuver TOR2",
    "Attitude Maneuver TOR3",
    "Attitude Maneuver Comments"
};

const bool AttitudeManeuverCCSDSData::CCSDS_IS_REQUIRED[EndAttitudeManeuverCCSDSDataDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType AttitudeManeuverCCSDSData::CCSDS_PARAMETER_TYPE[EndAttitudeManeuverCCSDSDataDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  AttitudeManeuverCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the AttitudeManeuverCCSDSData class
 */
//------------------------------------------------------------------------------
AttitudeManeuverCCSDSData::AttitudeManeuverCCSDSData() : CCSDSData(),
    epochStart(GmatBase::STRING_PARAMETER_UNDEFINED),
    duration(GmatBase::REAL_PARAMETER_UNDEFINED),
    refFrame(GmatBase::STRING_PARAMETER_UNDEFINED),
    tor1(GmatBase::REAL_PARAMETER_UNDEFINED),
    tor2(GmatBase::REAL_PARAMETER_UNDEFINED),
    tor3(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  AttitudeManeuverCCSDSData(const AttitudeManeuverCCSDSData &am)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
AttitudeManeuverCCSDSData::AttitudeManeuverCCSDSData(const AttitudeManeuverCCSDSData &am) :
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
//  AttitudeManeuverCCSDSData& operator=(const AttitudeManeuverCCSDSData &ea)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <ea> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const AttitudeManeuverCCSDSData& AttitudeManeuverCCSDSData::operator=(const AttitudeManeuverCCSDSData &am)

{
    if (&am == this)
        return *this;

    AttitudeManeuverCCSDSData::operator=(am);

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
//  ~AttitudeManeuverCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the AttitudeManeuverCCSDSData class
 */
//------------------------------------------------------------------------------
AttitudeManeuverCCSDSData::~AttitudeManeuverCCSDSData()
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
std::string AttitudeManeuverCCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndAttitudeManeuverCCSDSDataDataReps))
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
Integer AttitudeManeuverCCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndAttitudeManeuverCCSDSDataDataReps; i++)
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
Gmat::ParameterType AttitudeManeuverCCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndAttitudeManeuverCCSDSDataDataReps))
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
std::string AttitudeManeuverCCSDSData::GetDataParameterTypeString(const Integer id) const
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
Real AttitudeManeuverCCSDSData::GetRealDataParameter(const Integer id) const
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
Real AttitudeManeuverCCSDSData::GetRealDataParameter(const std::string &label) const
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
std::string AttitudeManeuverCCSDSData::GetStringDataParameter(const Integer id) const
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
std::string AttitudeManeuverCCSDSData::GetStringDataParameter(const std::string &label) const
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
StringArray AttitudeManeuverCCSDSData::GetStringArrayDataParameter(const Integer id) const
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
StringArray AttitudeManeuverCCSDSData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Real &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool AttitudeManeuverCCSDSData::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

        case CCSDS_ATTITUDEMANUEVER_DURATION_ID:

            duration = value;
            return true;

        case CCSDS_ATTITUDEMANUEVER_TOR1_ID:

            tor1 = value;
            return true;

        case CCSDS_ATTITUDEMANUEVER_TOR2_ID:

            tor2 = value;
            return true;

        case CCSDS_ATTITUDEMANUEVER_TOR3_ID:

            tor3 = value;
            return true;

        default:

            return false;

    }

}


//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const Real &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <label> String label identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool AttitudeManeuverCCSDSData::SetDataParameter(const std::string &label, const Real &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const std::string &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a std::string parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool AttitudeManeuverCCSDSData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

        case CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID:

	    epochStart = value;
            return true;

        case CCSDS_ATTITUDEMANUEVER_REFFRAME_ID:

	    refFrame = value;
            return true;

        default:

            return false;

    }

}


//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const std::string &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a std::string parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool AttitudeManeuverCCSDSData::SetDataParameter(const std::string &label, const std::string &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const StringArray &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a StringArray parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool AttitudeManeuverCCSDSData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {

        case CCSDS_ATTITUDEMANUEVER_COMMENTS_ID:

	    comments = value;
            return true;

        default:

            return false;

    }

}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const StringArray &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a StringArray parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool AttitudeManeuverCCSDSData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
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
const std::string* AttitudeManeuverCCSDSData::GetKeywords() const
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
const Integer AttitudeManeuverCCSDSData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndAttitudeManeuverCCSDSDataDataReps; i++)
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
std::string AttitudeManeuverCCSDSData::GetUnits(const Integer &id) const
{
    if (id >= 0 && id <= EndAttitudeManeuverCCSDSDataDataReps)
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
bool AttitudeManeuverCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndAttitudeManeuverCCSDSDataDataReps)
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

    for (Integer id = 0; id < AttitudeManeuverCCSDSData::EndAttitudeManeuverCCSDSDataDataReps; id++)
        if (AttitudeManeuverCCSDSData::CCSDS_IS_REQUIRED[id])
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
bool AttitudeManeuverCCSDSData::Validate() const
{

    for (unsigned int i = 0; i < EndAttitudeManeuverCCSDSDataDataReps; i++ )
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const AttitudeManeuverCCSDSData *myAttitudeManeuver)
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
                          const AttitudeManeuverCCSDSData *myAttitudeManeuver)
{
    using namespace std;

    if (!myAttitudeManeuver->Validate()) return output;

    for (unsigned int i = 0; i < myAttitudeManeuver->comments.size(); i++ )
    {
        output << "COMMENT " << myAttitudeManeuver->comments[i] << endl;
    }

    output << "MAN_EPOCH_START = " << myAttitudeManeuver->epochStart << endl;
    output << "MAN_DURATION = " << myAttitudeManeuver->duration << endl;
    output << "MAN_REF_FRAME = " << myAttitudeManeuver->refFrame << endl;
    output << "MAN_TOR_1 = " << myAttitudeManeuver->tor1 << endl;
    output << "MAN_TOR_2 = " << myAttitudeManeuver->tor2 << endl;
    output << "MAN_TOR_3 = " << myAttitudeManeuver->tor3 << endl;

    return output;
}