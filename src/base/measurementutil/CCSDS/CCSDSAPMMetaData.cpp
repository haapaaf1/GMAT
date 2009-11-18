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
CCSDSAPMMetaData::CCSDSAPMMetaData() : CCSDSMetaData(),
    objectName(GmatBase::STRING_PARAMETER_UNDEFINED),
    internationalDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrameOrigin(GmatBase::STRING_PARAMETER_UNDEFINED),
    timeSystem(GmatBase::STRING_PARAMETER_UNDEFINED),
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
CCSDSAPMMetaData::CCSDSAPMMetaData(const CCSDSAPMMetaData &apmMD) :
    CCSDSMetaData(apmMD),
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
const CCSDSAPMMetaData& CCSDSAPMMetaData::operator=(const CCSDSAPMMetaData &apmMD)
{
    if (&apmMD == this)
        return *this;

    CCSDSMetaData::operator=(apmMD);

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

//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSAPMMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSAPMMetaDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::BOOLEAN_TYPE:
                    if (!IsParameterDefined(GetBooleanDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Boolean parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::INTEGER_TYPE:
                    if (!IsParameterDefined(GetIntegerDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Integer parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
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
// std::ostream& operator<< (std::ostream &output, const CCSDSAPMMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetaData>    CCSDS APM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAPMMetaData *myMetaData)
{

    if(!myMetaData->Validate()) return output;
    
   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);
    using namespace std;

    for (unsigned int i = 0; i < myMetaData->comments.size(); i++ )
    {
        output << "COMMENT " << myMetaData->comments[i] << endl;
    }

    for (unsigned int i = 0; i < CCSDSAPMMetaData::EndCCSDSAPMMetaDataReps; i++)
    {

        switch (i)
        {

            case CCSDSAPMMetaData::CCSDS_APM_OBJECTNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->objectName);
                if (definedFlag)
                {
                    output << "OBJECT_NAME = " << myMetaData->objectName;
                    output << endl;
                }
            }

            break;

            case CCSDSAPMMetaData::CCSDS_APM_OBJECTID_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->internationalDesignator);
                if (definedFlag)
                {   
                    output << "OBJECT_ID = " << myMetaData->internationalDesignator;
                    output << endl;
                }
            }

            break;

            case CCSDSAPMMetaData::CCSDS_APM_CENTERNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrameOrigin);
                if (definedFlag)
                {
                    output << "CENTER_NAME = " << myMetaData->refFrameOrigin;
                    output << endl;
                }
            }

            break;

            case CCSDSAPMMetaData::CCSDS_APM_TIMESYSTEM_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->timeSystem);
                if (definedFlag)
                {
                    output << "TIME_SYSTEM = " << myMetaData->timeSystem;
                    output << endl;
                }
            }

            break;

            default:
                break;

        }
    }

    return output;
}
