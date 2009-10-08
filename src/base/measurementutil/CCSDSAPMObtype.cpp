#include "CCSDSAPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMObType::CCSDS_APM_KEYWORDS[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "TIME_SYSTEM",
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    "",
    "",
    "",
    "",
    ""
};

const std::string CCSDSAPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Time System",
    "Comments"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSAPMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::CCSDSAPMObType() : CCSDSObType("CCSDSAPMObType", ""),
	ccsdsAPMMetaData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMObType(const CCSDSAPMObType &APM)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::CCSDSAPMObType(const CCSDSAPMObType &APM) : CCSDSObType(APM),
	ccsdsAPMMetaData(APM.ccsdsAPMMetaData)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMObType& operator=(const CCSDSAPMObType &APM)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <APM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMObType& CCSDSAPMObType::operator=(const CCSDSAPMObType &APM)
{
   if (&APM == this)
      return *this;

    ccsdsAPMMetaData = APM.ccsdsAPMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::~CCSDSAPMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAPMDataFile.
 *
 * @return clone of the ProcessCCSDSAPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSAPMObType::Clone() const
{
   GmatBase *clone = new CCSDSAPMObType(*this);
   return (clone);
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
std::string CCSDSAPMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAPMDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataParameterText(id);
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAPMObType::GetDataUnits(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAPMDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataUnits(id);
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSAPMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAPMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return CCSDSObType::GetDataParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSAPMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAPMDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return CCSDSObType::GetDataParameterType(id);
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSAPMObType::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObType::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAPMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_APM_TIMESYSTEM_ID:

            return ccsdsAPMMetaData->timeSystem;

	case CCSDS_APM_CENTERNAME_ID:

            return ccsdsAPMMetaData->refFrameOrigin;

	case CCSDS_APM_OBJECTID_ID:

            return ccsdsAPMMetaData->internationalDesignator;

        case CCSDS_APM_OBJECTNAME_ID:

            return ccsdsAPMMetaData->objectName;

        default:

            return CCSDSObType::GetStringDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAPMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAPMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_APM_METADATACOMMENTS_ID:

	    return ccsdsAPMMetaData->comments;

        default:

            return CCSDSObType::GetStringArrayDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSAPMObType::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSAPMObType::GetKeywords() const
{
   return CCSDS_APM_KEYWORDS;
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
const Integer CCSDSAPMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAPMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_APM_KEYWORDS[i]))
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
std::string CCSDSAPMObType::GetUnits(const Integer &id) const
{
   return CCSDS_UNIT_DESCRIPTIONS[id];
}

//------------------------------------------------------------------------------
// const StringArray GetTimeSystems() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable time systems.
 *
 * @return String array of all time systems
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSAPMObType::GetTimeSystems() const
{
   return CCSDS_TIMESYSTEM_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetTimeSystemText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the time system text corresponding to a ID
 *
 * @param <id> Integer ID associated with the time system
 * @return The string description of the time system
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSAPMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSAPMTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return CCSDSObType::GetTimeSystemText(id);
}

//------------------------------------------------------------------------------
// Integer GetTimeSystemID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the time system ID
 *
 * @param <label> The string label associated with the time system
 * @return The integer time system ID
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSAPMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSAPMTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}

    }

    return CCSDSObType::GetTimeSystemID(label);

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
bool CCSDSAPMObType::IsParameterRequired(const Integer id) const
{
if (id > EndCCSDSDataReps && id <= EndCCSDSAPMDataReps)
    return CCSDS_IS_REQUIRED[id];
else
    return CCSDSObType::IsParameterRequired(id);
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
bool CCSDSAPMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAPMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return CCSDSObType::CheckDataAvailability(str);

}


//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAPMObType *myAPM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS APM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAPMObType *myAPM)
{
    switch (myAPM->ccsdsHeader->dataType)
    {
        case CCSDSObType::QUATERNION_ID:
            output << myAPM->ccsdsQuaternion;
            break;
        case CCSDSObType::EULERANGLE_ID:
            output << myAPM->ccsdsEulerAngle;
            break;
        case CCSDSObType::SPINSTABILIZED_ID:
            output << myAPM->ccsdsSpinStabilized;
            break;
        case CCSDSObType::SPACECRAFTPARAMETERS_ID:
            output << myAPM->ccsdsSpacecraftParameters;
            break;
        case CCSDSObType::ATTITUDEMANEUVER_ID:
            output << myAPM->ccsdsAttitudeManeuver;
            break;
        default:
            break;
    }

    return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAPMMetaData *myMetadata)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS APM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAPMMetaData *myMetadata)
{

   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

   output << "META_START" << std::endl;

   for (Integer i = 0; i < myMetadata->comments.size(); i++ )
   {
       output << "COMMENT " << myMetadata->comments[i] << std::endl;
   }
   output << "OBJECT_NAME = " << myMetadata->objectName << std::endl;
   output << "OBJECT_ID = " << myMetadata->internationalDesignator << std::endl;
   output << "CENTER_NAME = " << myMetadata->refFrameOrigin << std::endl;
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << std::endl;


   output << "META_STOP" << std::endl << std::endl;

   return output;
}