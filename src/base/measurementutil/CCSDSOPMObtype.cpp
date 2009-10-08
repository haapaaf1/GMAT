#include "CCSDSOPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOPMObType::CCSDS_OPM_KEYWORDS[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "REF_FRAME",
    "TIME_SYSTEM",
    "COMMENT"
};

const std::string CCSDSOPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    ""
};

const std::string CCSDSOPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

const std::string CCSDSOPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Reference Frame",
    "Time System",
    "Comments"
};

const bool CCSDSOPMObType::CCSDS_IS_REQUIRED[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSOPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSOPMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::CCSDSOPMObType() : CCSDSObType("CCSDSOPMObType", ""),
	ccsdsOPMMetaData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMObType(const CCSDSOPMObType &opm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::CCSDSOPMObType(const CCSDSOPMObType &opm) : CCSDSObType(opm),
	ccsdsOPMMetaData(opm.ccsdsOPMMetaData)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMObType& operator=(const CCSDSOPMObType &opm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <OPM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMObType& CCSDSOPMObType::operator=(const CCSDSOPMObType &opm)
{
   if (&opm == this)
      return *this;

    ccsdsOPMMetaData = opm.ccsdsOPMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::~CCSDSOPMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOPMDataFile.
 *
 * @return clone of the ProcessCCSDSOPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOPMObType::Clone() const
{
   GmatBase *clone = new CCSDSOPMObType(*this);
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
std::string CCSDSOPMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOPMDataReps))
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
std::string CCSDSOPMObType::GetDataUnits(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOPMDataReps))
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
Integer CCSDSOPMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOPMDataReps; i++)
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
Gmat::ParameterType CCSDSOPMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOPMDataReps))
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
std::string CCSDSOPMObType::GetDataParameterTypeString(const Integer id) const
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
std::string CCSDSOPMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_OPM_TIMESYSTEM_ID:

            return ccsdsOPMMetaData->timeSystem;

	case CCSDS_OPM_REFFRAME_ID:

            return ccsdsOPMMetaData->refFrame;

	case CCSDS_OPM_CENTERNAME_ID:

            return ccsdsOPMMetaData->refFrameOrigin;

	case CCSDS_OPM_OBJECTID_ID:

            return ccsdsOPMMetaData->internationalDesignator;

        case CCSDS_OPM_OBJECTNAME_ID:

            return ccsdsOPMMetaData->objectName;

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
std::string CCSDSOPMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSOPMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_OPM_METADATACOMMENTS_ID:

	    return ccsdsOPMMetaData->comments;

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
StringArray CCSDSOPMObType::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS OPM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSOPMObType::GetKeywords() const
{
   return CCSDS_OPM_KEYWORDS;
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
const Integer CCSDSOPMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOPMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_OPM_KEYWORDS[i]))
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
std::string CCSDSOPMObType::GetUnits(const Integer &id) const
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
const std::string* CCSDSOPMObType::GetTimeSystems() const
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
std::string CCSDSOPMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSOPMTimeReps))
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
Integer CCSDSOPMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSOPMTimeReps; i++)
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
bool CCSDSOPMObType::IsParameterRequired(const Integer id) const
{
if (id > EndCCSDSDataReps && id <= EndCCSDSOPMDataReps)
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
bool CCSDSOPMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOPMDataReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMObType *myOPM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OPM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOPMObType *myOPM)
{
    switch (myOPM->ccsdsHeader->dataType)
    {
        case CCSDSObType::STATEVECTOR_ID:
            output << myOPM->ccsdsStateVector;
            break;
        case CCSDSObType::KEPLERIANELEMENTS_ID:
            output << myOPM->ccsdsKeplerianElements;
            break;
        case CCSDSObType::SPACECRAFTPARAMETERS_ID:
            output << myOPM->ccsdsSpacecraftParameters;
            break;
        case CCSDSObType::MANEUVER_ID:
            output << myOPM->ccsdsManeuver;
            break;
        default:
            break;
    }

    return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMMetaData *myMetadata)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OPM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOPMMetaData *myMetadata)
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
   output << "REF_FRAME = " << myMetadata->refFrame << std::endl;
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << std::endl;


   output << "META_STOP" << std::endl << std::endl;

   return output;
}