#include "CCSDSOEMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOEMObType::CCSDS_OEM_KEYWORDS[EndCCSDSOEMDataReps-EndCCSDSDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "REF_FRAME",
    "TIME_SYSTEM",
    "START_TIME",
    "USEABLE_START_TIME",
    "USEABLE_STOP_TIME",
    "STOP_TIME",
    "INTERPOLATION",
    "INTERPOLATION_DEGREE",
    "COMMENT"
};

const std::string CCSDSOEMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOEMDataReps-EndCCSDSDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
};

const std::string CCSDSOEMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

const std::string CCSDSOEMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOEMDataReps-EndCCSDSDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Reference Frame",
    "Time System",
    "Start Epoch",
    "Useable Start Epoch",
    "Useable Stop Epoch",
    "Stop Epoch",
    "Interpolation Method",
    "Interpolation Degree",
    "Comments"
};

const bool CCSDSOEMObType::CCSDS_IS_REQUIRED[EndCCSDSOEMDataReps-EndCCSDSDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    true,
    false,
    false,
    false
};

const Gmat::ParameterType CCSDSOEMObType::CCSDS_PARAMETER_TYPE[EndCCSDSOEMDataReps-EndCCSDSDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSOEMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::CCSDSOEMObType() : CCSDSObType("CCSDSOEMObType", ""),
	ccsdsOEMMetaData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSOEMObType(const CCSDSOEMObType &oem)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::CCSDSOEMObType(const CCSDSOEMObType &oem) : CCSDSObType(oem),
	ccsdsOEMMetaData(oem.ccsdsOEMMetaData)
{
}

//---------------------------------------------------------------------------
//  CCSDSOEMObType& operator=(const CCSDSOEMObType &oem)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <oem> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOEMObType& CCSDSOEMObType::operator=(const CCSDSOEMObType &oem)
{
   if (&oem == this)
      return *this;

    ccsdsOEMMetaData = oem.ccsdsOEMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOEMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOEMObType::~CCSDSOEMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOEMDataFile.
 *
 * @return clone of the ProcessCCSDSOEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOEMObType::Clone() const
{
   GmatBase *clone = new CCSDSOEMObType(*this);
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
std::string CCSDSOEMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOEMDataReps))
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
std::string CCSDSOEMObType::GetDataUnits(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOEMDataReps))
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
Integer CCSDSOEMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOEMDataReps; i++)
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
Gmat::ParameterType CCSDSOEMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOEMDataReps))
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
std::string CCSDSOEMObType::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObType::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSOEMObType::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_OEM_INTERPOLATIONDEGREE_ID:

	    return ccsdsOEMMetaData->interpolationDegree;

        default:

            return CCSDSObType::GetIntegerDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSOEMObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSOEMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_OEM_TIMESYSTEM_ID:

            return ccsdsOEMMetaData->timeSystem;

	case CCSDS_OEM_REFFRAME_ID:

            return ccsdsOEMMetaData->refFrame;

	case CCSDS_OEM_CENTERNAME_ID:

            return ccsdsOEMMetaData->refFrameOrigin;

	case CCSDS_OEM_OBJECTID_ID:

            return ccsdsOEMMetaData->internationalDesignator;

        case CCSDS_OEM_OBJECTNAME_ID:

            return ccsdsOEMMetaData->objectName;

        case CCSDS_OEM_STARTEPOCH_ID:

            return ccsdsOEMMetaData->startEpoch;

        case CCSDS_OEM_STOPEPOCH_ID:

            return ccsdsOEMMetaData->stopEpoch;

        case CCSDS_OEM_USEABLE_STARTEPOCH_ID:

            return ccsdsOEMMetaData->useableStartEpoch;

        case CCSDS_OEM_USEABLE_STOPEPOCH_ID:

            return ccsdsOEMMetaData->useableStopEpoch;

        case CCSDS_OEM_INTERPOLATION_ID:

            return ccsdsOEMMetaData->interpolationMethod;

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
std::string CCSDSOEMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSOEMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_OEM_METADATACOMMENTS_ID:

	    return ccsdsOEMMetaData->comments;

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
StringArray CCSDSOEMObType::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSOEMObType::GetKeywords() const
{
   return CCSDS_OEM_KEYWORDS;
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
const Integer CCSDSOEMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOEMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_OEM_KEYWORDS[i]))
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
std::string CCSDSOEMObType::GetUnits(const Integer &id) const
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
const std::string* CCSDSOEMObType::GetTimeSystems() const
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
std::string CCSDSOEMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSOEMTimeReps))
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
Integer CCSDSOEMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSOEMTimeReps; i++)
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
bool CCSDSOEMObType::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSOEMDataReps)
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
bool CCSDSOEMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOEMDataReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSOEMObType *myOEM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOEMObType *myOEM)
{
    switch (myOEM->ccsdsHeader->dataType)
    {
        case CCSDSObType::STATEVECTOR_ID:
            output << myOEM->ccsdsStateVector;
            break;
        default:
            break;
    }

    return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSOEMMetaData *myMetadata)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOEMMetaData *myMetadata)
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
   output << "START_TIME = " << myMetadata->startEpoch << std::endl;
   output << "USEABLE_START_TIME = " << myMetadata->useableStartEpoch << std::endl;
   output << "USEABLE_STOP_TIME = " << myMetadata->useableStopEpoch << std::endl;
   output << "STOP_TIME = " << myMetadata->stopEpoch << std::endl;
   output << "INTERPOLATION = " << myMetadata->interpolationMethod << std::endl;
   output << "INTERPOLATION_DEGREE = " << myMetadata->interpolationDegree << std::endl;


   output << "META_STOP" << std::endl << std::endl;

   return output;
}
