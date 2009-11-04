#include "CCSDSAPMMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMMetaData::CCSDS_APM_METADATA_KEYWORDS[EndCCSDSAPMMetaDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "TIME_SYSTEM",
    "COMMENT"
};

const std::string CCSDSAPMMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSAPMMetaDataReps] =
{
    "",
    "",
    "",
    "",
    ""
};

const std::string CCSDSAPMMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSAPMMetaDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Time System",
    "Comments"
};

const bool CCSDSAPMMetaData::CCSDS_METADATA_IS_REQUIRED[EndCCSDSAPMMetaDataReps] =
{
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAPMMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSAPMMetaDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSAPMMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSAPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSAPMMetaData::CCSDSAPMMetaData() :
    objectName(std::string("")),
    internationalDesignator(std::string("")),
    refFrameOrigin(std::string("")),
    timeSystem(std::string("")),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMMetaData(const CCSDSAPMMetaData &apmMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSAPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSAPMMetaData::CCSDSAPMMetaData
               (const CCSDSAPMMetaData &apmMD) :
    objectName(apmMD.objectName),
    internationalDesignator(apmMD.internationalDesignator),
    refFrameOrigin(apmMD.refFrameOrigin),
    timeSystem(apmMD.timeSystem),
    comments(apmMD.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMMetaData& operator= (const CCSDSAPMMetaData &apmMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSAPMMetaData structures.
 *
 * @param <apmMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMMetaData& CCSDSAPMMetaData::operator=
                                     (const CCSDSAPMMetaData &apmMD)
{
    if (&apmMD == this)
        return *this;

    objectName = apmMD.objectName;
    internationalDesignator = apmMD.internationalDesignator;
    refFrameOrigin = apmMD.refFrameOrigin;
    timeSystem = apmMD.timeSystem;
    comments = apmMD.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSAPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSAPMMetaData::~CCSDSAPMMetaData()
{
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
const std::string* CCSDSAPMMetaData::GetKeywords() const
{
   return CCSDS_APM_METADATA_KEYWORDS;
}

//------------------------------------------------------------------------------
//  const Integer GetMetaDataKeywordID(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
const Integer CCSDSAPMMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAPMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_APM_METADATA_KEYWORDS[i]))
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
std::string CCSDSAPMMetaData::GetUnits(const Integer &id) const
{
   return CCSDS_METADATA_UNIT_DESCRIPTIONS[id];
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
bool CCSDSAPMMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSAPMMetaDataReps)
        return CCSDS_METADATA_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool CountRequiredNumberAPMMetaDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAPMMetaDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSAPMMetaData::EndCCSDSAPMMetaDataReps; id++)
        if (CCSDSAPMMetaData::CCSDS_METADATA_IS_REQUIRED[id])
            num++;

    return num;
}

//------------------------------------------------------------------------------
//  bool CheckMetaDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool CCSDSAPMMetaData::CheckMetaDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAPMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

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
std::string CCSDSAPMMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAPMMetaDataReps))
   {
      return CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[id];
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
Integer CCSDSAPMMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAPMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[i]))
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
Gmat::ParameterType CCSDSAPMMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAPMMetaDataReps))
      return CCSDS_METADATA_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSAPMMetaData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAPMMetaData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_APM_TIMESYSTEM_ID:

            return timeSystem;

	case CCSDS_APM_CENTERNAME_ID:

            return refFrameOrigin;

	case CCSDS_APM_OBJECTID_ID:

            return internationalDesignator;

        case CCSDS_APM_OBJECTNAME_ID:

            return objectName;

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
std::string CCSDSAPMMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAPMMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_APM_METADATACOMMENTS_ID:

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
StringArray CCSDSAPMMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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

   for (unsigned int i = 0; i < myMetadata->comments.size(); i++ )
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
