//$Header$
//------------------------------------------------------------------------------
//                             OEMCCSDSMetaData
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
 * This class specifies the CCSDS Orbit Ephemeris MetaData class.
 *
 */
//------------------------------------------------------------------------------

#include "OEMCCSDSMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string OEMCCSDSMetaData::CCSDS_OEM_METADATA_KEYWORDS[EndOEMCCSDSMetaDataReps] =
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

const std::string OEMCCSDSMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndOEMCCSDSMetaDataReps] =
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
    ""
};

const std::string OEMCCSDSMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndOEMCCSDSMetaDataReps] =
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

const bool OEMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[EndOEMCCSDSMetaDataReps] =
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

const Gmat::ParameterType OEMCCSDSMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndOEMCCSDSMetaDataReps] =
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
//  OEMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the OEMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
OEMCCSDSMetaData::OEMCCSDSMetaData() : CCSDSMetaData(),
    objectName(GmatBase::STRING_PARAMETER_UNDEFINED),
    internationalDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrameOrigin(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrame(GmatBase::STRING_PARAMETER_UNDEFINED),
    timeSystem(GmatBase::STRING_PARAMETER_UNDEFINED),
    startEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    stopEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    useableStartEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    useableStopEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    interpolationMethod(GmatBase::STRING_PARAMETER_UNDEFINED),
    interpolationDegree(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  OEMCCSDSMetaData(const OEMCCSDSMetaData &oemMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the OEMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
OEMCCSDSMetaData::OEMCCSDSMetaData(const OEMCCSDSMetaData &oemMD) :
    CCSDSMetaData(oemMD),
    objectName(oemMD.objectName),
    internationalDesignator(oemMD.internationalDesignator),
    refFrameOrigin(oemMD.refFrameOrigin),
    refFrame(oemMD.refFrame),
    timeSystem(oemMD.timeSystem),
    startEpoch(oemMD.startEpoch),
    stopEpoch(oemMD.stopEpoch),
    useableStartEpoch(oemMD.useableStartEpoch),
    useableStopEpoch(oemMD.useableStopEpoch),
    interpolationMethod(oemMD.interpolationMethod),
    interpolationDegree(oemMD.interpolationDegree),
    comments(oemMD.comments)
{
}

//---------------------------------------------------------------------------
//  OEMCCSDSMetaData& operator= (const OEMCCSDSMetaData &oemMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for OEMCCSDSMetaData structures.
 *
 * @param <oemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const OEMCCSDSMetaData& OEMCCSDSMetaData::operator=
                                     (const OEMCCSDSMetaData &oemMD)
{
    if (&oemMD == this)
        return *this;

    CCSDSMetaData::operator=(oemMD);

    objectName = oemMD.objectName;
    internationalDesignator = oemMD.internationalDesignator;
    refFrameOrigin = oemMD.refFrameOrigin;
    refFrame = oemMD.refFrame;
    timeSystem = oemMD.timeSystem;
    startEpoch = oemMD.startEpoch;
    stopEpoch = oemMD.stopEpoch;
    useableStartEpoch = oemMD.useableStartEpoch;
    useableStopEpoch = oemMD.useableStopEpoch;
    interpolationMethod = oemMD.interpolationMethod;
    interpolationDegree = oemMD.interpolationDegree;
    comments = oemMD.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~OEMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the OEMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
OEMCCSDSMetaData::~OEMCCSDSMetaData()
{
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
const std::string* OEMCCSDSMetaData::GetKeywords() const
{
   return CCSDS_OEM_METADATA_KEYWORDS;
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
const Integer OEMCCSDSMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndOEMCCSDSMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_OEM_METADATA_KEYWORDS[i]))
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
std::string OEMCCSDSMetaData::GetUnits(const Integer &id) const
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
bool OEMCCSDSMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndOEMCCSDSMetaDataReps)
        return CCSDS_METADATA_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool CountRequiredNumberOEMMetaDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberOEMMetaDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < OEMCCSDSMetaData::EndOEMCCSDSMetaDataReps; id++)
        if (OEMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[id])
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
std::string OEMCCSDSMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndOEMCCSDSMetaDataReps))
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
Integer OEMCCSDSMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndOEMCCSDSMetaDataReps; i++)
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
Gmat::ParameterType OEMCCSDSMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndOEMCCSDSMetaDataReps))
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
std::string OEMCCSDSMetaData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer OEMCCSDSMetaData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_OEM_INTERPOLATIONDEGREE_ID:

	    return interpolationDegree;

        default:

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;
    }
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer OEMCCSDSMetaData::GetIntegerDataParameter(const std::string &label) const
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
std::string OEMCCSDSMetaData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_OEM_TIMESYSTEM_ID:

            return timeSystem;

	case CCSDS_OEM_REFFRAME_ID:

            return refFrame;

	case CCSDS_OEM_CENTERNAME_ID:

            return refFrameOrigin;

	case CCSDS_OEM_OBJECTID_ID:

            return internationalDesignator;

        case CCSDS_OEM_OBJECTNAME_ID:

            return objectName;

        case CCSDS_OEM_STARTEPOCH_ID:

            return startEpoch;

        case CCSDS_OEM_STOPEPOCH_ID:

            return stopEpoch;

        case CCSDS_OEM_USEABLE_STARTEPOCH_ID:

            return useableStartEpoch;

        case CCSDS_OEM_USEABLE_STOPEPOCH_ID:

            return useableStopEpoch;

        case CCSDS_OEM_INTERPOLATION_ID:

            return interpolationMethod;

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
std::string OEMCCSDSMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray OEMCCSDSMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_OEM_METADATACOMMENTS_ID:

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
StringArray OEMCCSDSMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Integer &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Integer parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool OEMCCSDSMetaData::SetDataParameter(const Integer id, const Integer &value)
{
    switch (id)
    {
	case CCSDS_OEM_INTERPOLATIONDEGREE_ID:

	    interpolationDegree = value;
            return true;
            
        default:

            return false;
    }
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const Integer &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Integer parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool OEMCCSDSMetaData::SetDataParameter(const std::string &label, const Integer &value)
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
bool OEMCCSDSMetaData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

	case CCSDS_OEM_TIMESYSTEM_ID:

            timeSystem = value;
            return true;

	case CCSDS_OEM_REFFRAME_ID:

            refFrame = value;
            return true;

	case CCSDS_OEM_CENTERNAME_ID:

            refFrameOrigin = value;
            return true;

	case CCSDS_OEM_OBJECTID_ID:

            internationalDesignator = value;
            return true;

        case CCSDS_OEM_OBJECTNAME_ID:

            objectName = value;
            return true;

        case CCSDS_OEM_STARTEPOCH_ID:

            startEpoch = value;
            return true;

        case CCSDS_OEM_STOPEPOCH_ID:

            stopEpoch = value;
            return true;

        case CCSDS_OEM_USEABLE_STARTEPOCH_ID:

            useableStartEpoch = value;
            return true;

        case CCSDS_OEM_USEABLE_STOPEPOCH_ID:

            useableStopEpoch = value;
            return true;

        case CCSDS_OEM_INTERPOLATION_ID:

            interpolationMethod = value;
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
bool OEMCCSDSMetaData::SetDataParameter(const std::string &label, const std::string &value)
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
bool OEMCCSDSMetaData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {
	case CCSDS_OEM_METADATACOMMENTS_ID:

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
bool OEMCCSDSMetaData::SetDataParameter(const std::string &label, const StringArray &value)
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
bool OEMCCSDSMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndOEMCCSDSMetaDataReps; i++ )
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
// std::ostream& operator<< (std::ostream &output, const OEMCCSDSMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetaData>    CCSDS OEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const OEMCCSDSMetaData *myMetaData)
{
    using namespace std;

    if(!myMetaData->Validate()) return output;
    
   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

   output << "META_START" << endl;

   for (unsigned int i = 0; i < myMetaData->comments.size(); i++ )
   {
       output << "COMMENT " << myMetaData->comments[i] << endl;
   }
   
    for (unsigned int i = 0; i < OEMCCSDSMetaData::EndOEMCCSDSMetaDataReps; i++)
    {

        switch (i)
        {

            case OEMCCSDSMetaData::CCSDS_OEM_OBJECTNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->objectName);
                if (definedFlag)
                {
                    output << "OBJECT_NAME = " << myMetaData->objectName;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_OBJECTID_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->internationalDesignator);
                if (definedFlag)
                {
                    output << "OBJECT_ID = " << myMetaData->internationalDesignator;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_CENTERNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrameOrigin);
                if (definedFlag)
                {
                    output << "CENTER_NAME = " << myMetaData->refFrameOrigin;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_REFFRAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrame);
                if (definedFlag)
                {
                    output << "REF_FRAME = " << myMetaData->refFrame;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_TIMESYSTEM_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->timeSystem);
                if (definedFlag)
                {
                    output << "TIME_SYSTEM = " << myMetaData->timeSystem;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_STARTEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->startEpoch);
                if (definedFlag)
                {
                    output << "START_TIME = " << myMetaData->startEpoch;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_USEABLE_STARTEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->useableStartEpoch);
                if (definedFlag)
                {
                    output << "USEABLE_START_TIME = " << myMetaData->useableStartEpoch;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_USEABLE_STOPEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->useableStopEpoch);
                if (definedFlag)
                {
                    output << "USEABLE_STOP_TIME = " << myMetaData->useableStopEpoch;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_STOPEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->stopEpoch);
                if (definedFlag)
                {
                    output << "STOP_TIME = " << myMetaData->stopEpoch;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_INTERPOLATION_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->interpolationMethod);
                if (definedFlag)
                {
                    output << "INTERPOLATION = " << myMetaData->interpolationMethod;
                    output << endl;
                }
            }

            break;

            case OEMCCSDSMetaData::CCSDS_OEM_INTERPOLATIONDEGREE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->interpolationDegree);
                if (definedFlag)
                {
                    output << "INTERPOLATION_DEGREE = " << myMetaData->interpolationDegree;
                    output << endl;
                }
            }

            break;

            default:
                break;
        }
    }

    output << "META_STOP" << endl;

   return output;
   
}
