//$Header$
//------------------------------------------------------------------------------
//                             APMCCSDSMetaData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/20
//
/**
 *
 * This class specifies the CCSDS Attitude Parameter MetaData class.
 *
 */
//------------------------------------------------------------------------------

#include "APMCCSDSMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string APMCCSDSMetaData::CCSDS_APM_METADATA_KEYWORDS[EndAPMCCSDSMetaDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "TIME_SYSTEM",
    "COMMENT"
};

const std::string APMCCSDSMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndAPMCCSDSMetaDataReps] =
{
    "",
    "",
    "",
    "",
    ""
};

const std::string APMCCSDSMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndAPMCCSDSMetaDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Time System",
    "Comments"
};

const bool APMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[EndAPMCCSDSMetaDataReps] =
{
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType APMCCSDSMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndAPMCCSDSMetaDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  APMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the APMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
APMCCSDSMetaData::APMCCSDSMetaData() : CCSDSMetaData(),
    objectName(GmatBase::STRING_PARAMETER_UNDEFINED),
    internationalDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrameOrigin(GmatBase::STRING_PARAMETER_UNDEFINED),
    timeSystem(GmatBase::STRING_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  APMCCSDSMetaData(const APMCCSDSMetaData &apmMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the APMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
APMCCSDSMetaData::APMCCSDSMetaData(const APMCCSDSMetaData &apmMD) :
    CCSDSMetaData(apmMD),
    objectName(apmMD.objectName),
    internationalDesignator(apmMD.internationalDesignator),
    refFrameOrigin(apmMD.refFrameOrigin),
    timeSystem(apmMD.timeSystem),
    comments(apmMD.comments)
{
}

//---------------------------------------------------------------------------
//  APMCCSDSMetaData& operator= (const APMCCSDSMetaData &apmMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for APMCCSDSMetaData structures.
 *
 * @param <apmMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const APMCCSDSMetaData& APMCCSDSMetaData::operator=(const APMCCSDSMetaData &apmMD)
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
//  ~APMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the APMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
APMCCSDSMetaData::~APMCCSDSMetaData()
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
const std::string* APMCCSDSMetaData::GetKeywords() const
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
const Integer APMCCSDSMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndAPMCCSDSMetaDataReps; i++)
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
std::string APMCCSDSMetaData::GetUnits(const Integer &id) const
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
bool APMCCSDSMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndAPMCCSDSMetaDataReps)
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

    for (Integer id = 0; id < APMCCSDSMetaData::EndAPMCCSDSMetaDataReps; id++)
        if (APMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[id])
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
std::string APMCCSDSMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndAPMCCSDSMetaDataReps))
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
Integer APMCCSDSMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndAPMCCSDSMetaDataReps; i++)
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
Gmat::ParameterType APMCCSDSMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndAPMCCSDSMetaDataReps))
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
std::string APMCCSDSMetaData::GetDataParameterTypeString(const Integer id) const
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
std::string APMCCSDSMetaData::GetStringDataParameter(const Integer id) const
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
std::string APMCCSDSMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray APMCCSDSMetaData::GetStringArrayDataParameter(const Integer id) const
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
StringArray APMCCSDSMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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
bool APMCCSDSMetaData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

	case CCSDS_APM_TIMESYSTEM_ID:

            timeSystem = value;

	case CCSDS_APM_CENTERNAME_ID:

            refFrameOrigin = value;

	case CCSDS_APM_OBJECTID_ID:

            internationalDesignator = value;

        case CCSDS_APM_OBJECTNAME_ID:

            objectName = value;

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
bool APMCCSDSMetaData::SetDataParameter(const std::string &label, const std::string &value)
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
bool APMCCSDSMetaData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {

        case CCSDS_APM_METADATACOMMENTS_ID:

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
bool APMCCSDSMetaData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
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
bool APMCCSDSMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndAPMCCSDSMetaDataReps; i++ )
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
// std::ostream& operator<< (std::ostream &output, const APMCCSDSMetaData *myMetaData)
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
std::ostream& operator<< (std::ostream &output, const APMCCSDSMetaData *myMetaData)
{

    if(!myMetaData->Validate()) return output;
    
   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);
    using namespace std;

    for (unsigned int i = 0; i < myMetaData->comments.size(); i++ )
    {
        output << "COMMENT " << myMetaData->comments[i] << endl;
    }

    for (unsigned int i = 0; i < APMCCSDSMetaData::EndAPMCCSDSMetaDataReps; i++)
    {

        switch (i)
        {

            case APMCCSDSMetaData::CCSDS_APM_OBJECTNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->objectName);
                if (definedFlag)
                {
                    output << "OBJECT_NAME = " << myMetaData->objectName;
                    output << endl;
                }
            }

            break;

            case APMCCSDSMetaData::CCSDS_APM_OBJECTID_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->internationalDesignator);
                if (definedFlag)
                {   
                    output << "OBJECT_ID = " << myMetaData->internationalDesignator;
                    output << endl;
                }
            }

            break;

            case APMCCSDSMetaData::CCSDS_APM_CENTERNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrameOrigin);
                if (definedFlag)
                {
                    output << "CENTER_NAME = " << myMetaData->refFrameOrigin;
                    output << endl;
                }
            }

            break;

            case APMCCSDSMetaData::CCSDS_APM_TIMESYSTEM_ID:
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
