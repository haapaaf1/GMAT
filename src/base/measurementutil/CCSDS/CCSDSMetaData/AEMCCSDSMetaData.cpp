//$Header$
//------------------------------------------------------------------------------
//                             AEMCCSDSMetaData
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
 * This class specifies the CCSDS Attitude Ephemeris MetaData class.
 *
 */
//------------------------------------------------------------------------------

#include "AEMCCSDSMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string AEMCCSDSMetaData::CCSDS_AEM_METADATA_KEYWORDS[EndAEMCCSDSMetaDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "REF_FRAME_A",
    "REF_FRAME_B",
    "ATTITUDE_DIR",
    "TIME_SYSTEM",
    "START_TIME",
    "USEABLE_START_TIME",
    "USEABLE_STOP_TIME",
    "STOP_TIME",
    "ATTITUDE_TYPE",
    "QUATERNION_TYPE",
    "EULER_ROT_SEQ",
    "RATE_FRAME",
    "INTERPOLATION_METHOD",
    "INTERPOLATION_DEGREE",
    "COMMENT"
};

const std::string AEMCCSDSMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndAEMCCSDSMetaDataReps] =
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
    "",
    "",
    "",
    "",
    "",
    "",
    ""
};

const std::string AEMCCSDSMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndAEMCCSDSMetaDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Reference Frame A",
    "Reference Frame B",
    "Attitude Rotation Direction",
    "Time System",
    "Start Epoch",
    "Useable Start Epoch",
    "Useable Stop Epoch",
    "Stop Epoch",
    "Attitude Type",
    "Quaternion Type",
    "Euler Rotation Sequence",
    "Rate Frame",
    "Interpolation Method",
    "Interpolation Degree",
    "Comments"
};

const bool AEMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[EndAEMCCSDSMetaDataReps] =
{
    true,
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    false
};

const Gmat::ParameterType AEMCCSDSMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndAEMCCSDSMetaDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  AEMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the AEMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
AEMCCSDSMetaData::AEMCCSDSMetaData() : CCSDSMetaData(),
    objectName(GmatBase::STRING_PARAMETER_UNDEFINED),
    internationalDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrameOrigin(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameA(GmatBase::STRING_PARAMETER_UNDEFINED),
    frameB(GmatBase::STRING_PARAMETER_UNDEFINED),
    direction(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    timeSystem(GmatBase::STRING_PARAMETER_UNDEFINED),
    startEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    stopEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    useableStartEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    useableStopEpoch(GmatBase::STRING_PARAMETER_UNDEFINED),
    attitudeType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    quaternionType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    eulerRotationSequence(GmatBase::STRING_PARAMETER_UNDEFINED),
    rateFrame(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    interpolationMethod(GmatBase::STRING_PARAMETER_UNDEFINED),
    interpolationDegree(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  AEMCCSDSMetaData(const AEMCCSDSMetaData &aemMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the AEMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
AEMCCSDSMetaData::AEMCCSDSMetaData(const AEMCCSDSMetaData &aemMD) :
    CCSDSMetaData(aemMD),
    objectName(aemMD.objectName),
    internationalDesignator(aemMD.internationalDesignator),
    refFrameOrigin(aemMD.refFrameOrigin),
    frameA(aemMD.frameA),
    frameB(aemMD.frameB),
    direction(aemMD.direction),
    timeSystem(aemMD.timeSystem),
    startEpoch(aemMD.startEpoch),
    stopEpoch(aemMD.stopEpoch),
    useableStartEpoch(aemMD.useableStartEpoch),
    useableStopEpoch(aemMD.useableStopEpoch),
    attitudeType(aemMD.attitudeType),
    quaternionType(aemMD.quaternionType),
    eulerRotationSequence(aemMD.eulerRotationSequence),
    rateFrame(aemMD.rateFrame),
    interpolationMethod(aemMD.interpolationMethod),
    interpolationDegree(aemMD.interpolationDegree),
    comments(aemMD.comments)
{
}

//---------------------------------------------------------------------------
//  AEMCCSDSMetaData& operator=(const AEMCCSDSMetaData &aemMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for AEMCCSDSMetaData structures.
 *
 * @param <aemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const AEMCCSDSMetaData& AEMCCSDSMetaData::operator=(const AEMCCSDSMetaData &aemMD)
{
    if (&aemMD == this)
        return *this;

    AEMCCSDSMetaData::operator=(aemMD);

    objectName = aemMD.objectName;
    internationalDesignator = aemMD.internationalDesignator;
    refFrameOrigin = aemMD.refFrameOrigin;
    frameA = aemMD.frameA;
    frameB = aemMD.frameB;
    direction = aemMD.direction;
    timeSystem = aemMD.timeSystem;
    startEpoch = aemMD.startEpoch;
    stopEpoch = aemMD.stopEpoch;
    useableStartEpoch = aemMD.useableStartEpoch;
    useableStopEpoch = aemMD.useableStopEpoch;
    attitudeType = aemMD.attitudeType;
    quaternionType = aemMD.quaternionType;
    eulerRotationSequence = aemMD.eulerRotationSequence;
    interpolationMethod = aemMD.interpolationMethod;
    interpolationDegree = aemMD.interpolationDegree;
    rateFrame = aemMD.rateFrame;
    comments = aemMD.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~AEMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the AEMCCSDSMetaData class
 */
//------------------------------------------------------------------------------
AEMCCSDSMetaData::~AEMCCSDSMetaData()
{
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS AEM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* AEMCCSDSMetaData::GetKeywords() const
{
   return CCSDS_AEM_METADATA_KEYWORDS;
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
const Integer AEMCCSDSMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndAEMCCSDSMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_AEM_METADATA_KEYWORDS[i]))
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
std::string AEMCCSDSMetaData::GetUnits(const Integer &id) const
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
bool AEMCCSDSMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndAEMCCSDSMetaDataReps)
        return CCSDS_METADATA_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool CountRequiredNumberAEMMetaDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAEMMetaDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < AEMCCSDSMetaData::EndAEMCCSDSMetaDataReps; id++)
        if (AEMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[id])
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
std::string AEMCCSDSMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndAEMCCSDSMetaDataReps))
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
Integer AEMCCSDSMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndAEMCCSDSMetaDataReps; i++)
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
Gmat::ParameterType AEMCCSDSMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndAEMCCSDSMetaDataReps))
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
std::string AEMCCSDSMetaData::GetDataParameterTypeString(const Integer id) const
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
Integer AEMCCSDSMetaData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_AEM_ATTITUDEDIR_ID:

            return direction;

        case CCSDS_AEM_RATEFRAME_ID:

            return rateFrame;

        case CCSDS_AEM_ATTITUDETYPE_ID:

            return attitudeType;

        case CCSDS_AEM_QUATERNIONTYPE_ID:

            return quaternionType;

	case CCSDS_AEM_INTERPOLATIONDEGREE_ID:

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
Integer AEMCCSDSMetaData::GetIntegerDataParameter(const std::string &label) const
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
std::string AEMCCSDSMetaData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_AEM_TIMESYSTEM_ID:

            return timeSystem;

	case CCSDS_AEM_REFFRAMEA_ID:

            return frameA;

	case CCSDS_AEM_REFFRAMEB_ID:

            return frameB;

	case CCSDS_AEM_CENTERNAME_ID:

            return refFrameOrigin;

	case CCSDS_AEM_OBJECTID_ID:

            return internationalDesignator;

        case CCSDS_AEM_OBJECTNAME_ID:

            return objectName;

        case CCSDS_AEM_STARTEPOCH_ID:

            return startEpoch;

        case CCSDS_AEM_STOPEPOCH_ID:

            return stopEpoch;

        case CCSDS_AEM_USEABLE_STARTEPOCH_ID:

            return useableStartEpoch;

        case CCSDS_AEM_USEABLE_STOPEPOCH_ID:

            return useableStopEpoch;

        case CCSDS_AEM_EULERROTSEQ_ID:

            return eulerRotationSequence;

        case CCSDS_AEM_INTERPOLATION_ID:

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
std::string AEMCCSDSMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray AEMCCSDSMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_AEM_METADATACOMMENTS_ID:

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
StringArray AEMCCSDSMetaData::GetStringArrayDataParameter(const std::string &label) const
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
bool AEMCCSDSMetaData::SetDataParameter(const Integer id, const Integer &value)
{
    switch (id)
    {

        case CCSDS_AEM_ATTITUDEDIR_ID:

            direction = value;
            return true;

        case CCSDS_AEM_RATEFRAME_ID:

            rateFrame = value;
            return true;

        case CCSDS_AEM_ATTITUDETYPE_ID:

            attitudeType = value;
            return true;

        case CCSDS_AEM_QUATERNIONTYPE_ID:

            quaternionType = value;
            return true;

	case CCSDS_AEM_INTERPOLATIONDEGREE_ID:

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
bool AEMCCSDSMetaData::SetDataParameter(const std::string &label, const Integer &value)
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
bool AEMCCSDSMetaData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

	case CCSDS_AEM_TIMESYSTEM_ID:

            timeSystem = value;
            return true;

	case CCSDS_AEM_REFFRAMEA_ID:

            frameA = value;
            return true;

	case CCSDS_AEM_REFFRAMEB_ID:

            frameB = value;
            return true;

	case CCSDS_AEM_CENTERNAME_ID:

            refFrameOrigin = value;
            return true;

	case CCSDS_AEM_OBJECTID_ID:

            internationalDesignator = value;
            return true;

        case CCSDS_AEM_OBJECTNAME_ID:

            objectName = value;
            return true;

        case CCSDS_AEM_STARTEPOCH_ID:

            startEpoch = value;
            return true;

        case CCSDS_AEM_STOPEPOCH_ID:

            stopEpoch = value;
            return true;

        case CCSDS_AEM_USEABLE_STARTEPOCH_ID:

            useableStartEpoch = value;
            return true;

        case CCSDS_AEM_USEABLE_STOPEPOCH_ID:

            useableStopEpoch = value;
            return true;

        case CCSDS_AEM_EULERROTSEQ_ID:

            eulerRotationSequence = value;
            return true;

        case CCSDS_AEM_INTERPOLATION_ID:

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
bool AEMCCSDSMetaData::SetDataParameter(const std::string &label, const std::string &value)
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
bool AEMCCSDSMetaData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {

        case CCSDS_AEM_METADATACOMMENTS_ID:

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
bool AEMCCSDSMetaData::SetDataParameter(const std::string &label, const StringArray &value)
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
bool AEMCCSDSMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndAEMCCSDSMetaDataReps; i++ )
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
// std::ostream& operator<< (std::ostream &output, const AEMCCSDSMetaData *myMetadata)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS AEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const AEMCCSDSMetaData *myMetaData)
{
    using namespace std;

    if(!myMetaData->Validate()) return output;

    //output.setf(std::ios::showpoint);
    //output.setf(std::ios::scientific);

    output << "META_START" << endl;

    for (unsigned int i = 0; i < myMetaData->comments.size(); i++)
        output << "COMMENT " << myMetaData->comments[i] << endl;

    for (unsigned int i = 0; i < AEMCCSDSMetaData::EndAEMCCSDSMetaDataReps; i++ )
    {

        switch (i)
        {

            case AEMCCSDSMetaData::CCSDS_AEM_OBJECTNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->objectName);
                if (definedFlag)
                {
                    output << "OBJECT_NAME = " << myMetaData->objectName;
                    output << endl;
                }
            }

            break;

            case AEMCCSDSMetaData::CCSDS_AEM_OBJECTID_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->internationalDesignator);
                if (definedFlag)
                {
                    output << "OBJECT_ID = " << myMetaData->internationalDesignator;
                    output << endl;
                }
            }

            break;

            case AEMCCSDSMetaData::CCSDS_AEM_CENTERNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrameOrigin);
                if (definedFlag)
                {
                    output << "CENTER_NAME = " << myMetaData->refFrameOrigin << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_TIMESYSTEM_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->timeSystem);
                if (definedFlag)
                {
                    output << "TIME_SYSTEM = " << myMetaData->timeSystem << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_STARTEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->startEpoch);
                if (definedFlag)
                {
                    output << "START_TIME = " << myMetaData->startEpoch << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_STOPEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->stopEpoch);
                if (definedFlag)
                {
                    output << "STOP_TIME = " << myMetaData->stopEpoch << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_USEABLE_STARTEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->useableStartEpoch);
                if (definedFlag)
                {
                    output << "USEABLE_START_TIME = " << myMetaData->useableStartEpoch << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_USEABLE_STOPEPOCH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->useableStopEpoch);
                if (definedFlag)
                {
                    output << "USEABLE_STOP_TIME = " << myMetaData->useableStopEpoch << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_INTERPOLATION_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->interpolationMethod);
                if (definedFlag)
                {
                    output << "INTERPOLATION_METHOD = " << myMetaData->interpolationMethod << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_INTERPOLATIONDEGREE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->interpolationDegree);
                if (definedFlag)
                {
                    output << "INTERPOLATION_DEGREE = " << myMetaData->interpolationDegree << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_REFFRAMEA_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->frameA);
                if (definedFlag)
                {
                    output << "REF_FRAME_A = " << myMetaData->frameA << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_REFFRAMEB_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->frameB);
                if (definedFlag)
                {
                    output << "REF_FRAME_B = " << myMetaData->frameB << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_ATTITUDEDIR_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->direction);
                if (definedFlag)
                {
                    std::string directionText = myMetaData->GetAttitudeDirText(myMetaData->direction);
                    output << "ATTITUDE_DIR = " << directionText << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_ATTITUDETYPE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->attitudeType);
                if (definedFlag)
                {
                    std::string attitudeTypeText = myMetaData->GetAttitudeTypeText(myMetaData->attitudeType);
                    output << "ATTITUDE_TYPE = " << attitudeTypeText << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_QUATERNIONTYPE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->quaternionType);
                if (definedFlag)
                {
                    std::string quaternionTypeText = myMetaData->GetQuaternionTypeText(myMetaData->quaternionType);
                    output << "QUATERNION_TYPE = " << quaternionTypeText << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_EULERROTSEQ_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->eulerRotationSequence);
                if (definedFlag)
                {
                    output << "EULER_ROT_SEQ = " << myMetaData->eulerRotationSequence << endl;
                }
            }
            break;

            case AEMCCSDSMetaData::CCSDS_AEM_RATEFRAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->rateFrame);
                if (definedFlag)
                {
                    std::string rateFrameText = myMetaData->GetRateFrameText(myMetaData->rateFrame);
                    output << "RATE_FRAME = " << rateFrameText << endl;
                }
            }
            break;

            default:

                break;

        }
    }

    output << "META_STOP" << endl;
    output << endl;
   
    return output;
}



