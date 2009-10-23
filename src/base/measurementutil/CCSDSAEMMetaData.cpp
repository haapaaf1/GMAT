#include "CCSDSAEMMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAEMMetaData::CCSDS_AEM_METADATA_KEYWORDS[EndCCSDSAEMMetaDataReps] =
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
    "INTERPOLATION",
    "INTERPOLATION_DEGREE",
    "COMMENT"
};

const std::string CCSDSAEMMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSAEMMetaDataReps] =
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

const std::string CCSDSAEMMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSAEMMetaDataReps] =
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

const bool CCSDSAEMMetaData::CCSDS_METADATA_IS_REQUIRED[EndCCSDSAEMMetaDataReps] =
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

const Gmat::ParameterType CCSDSAEMMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSAEMMetaDataReps] =
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
//  CCSDSAEMMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSAEMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSAEMMetaData::CCSDSAEMMetaData() : CCSDSObType("CCSDSAEMMetaData", ""),
    objectName(std::string("")),
    internationalDesignator(std::string("")),
    refFrameOrigin(std::string("")),
    frameA(std::string("")),
    frameB(std::string("")),
    direction(0),
    timeSystem(std::string("")),
    startEpoch(std::string("")),
    stopEpoch(std::string("")),
    useableStartEpoch(std::string("")),
    useableStopEpoch(std::string("")),
    attitudeType(0),
    quaternionType(0),
    eulerRotationSequence(std::string("")),
    rateFrame(0),
    interpolationMethod(std::string("")),
    interpolationDegree(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMMetaData(const CCSDSAEMMetaData &aemMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSAEMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSAEMMetaData::CCSDSAEMMetaData
               (const CCSDSAEMMetaData &aemMD) : CCSDSObType(aemMD),
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
//  CCSDSAEMMetaData& operator= (const CCSDSAEMMetaData &aemMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSAEMMetaData structures.
 *
 * @param <aemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMMetaData& CCSDSAEMMetaData::operator=
                                     (const CCSDSAEMMetaData &aemMD)
{
    if (&aemMD == this)
        return *this;

    CCSDSObType::operator=(aemMD);

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
//  ~CCSDSAEMMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSAEMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSAEMMetaData::~CCSDSAEMMetaData()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAEMDataFile.
 *
 * @return clone of the ProcessCCSDSAEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSAEMMetaData::Clone() const
{
   GmatBase *clone = new CCSDSAEMMetaData(*this);
   return (clone);
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
const std::string* CCSDSAEMMetaData::GetKeywords() const
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
const Integer CCSDSAEMMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAEMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_AEM_METADATA_KEYWORDS[i]))
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
std::string CCSDSAEMMetaData::GetUnits(const Integer &id) const
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
bool CCSDSAEMMetaData::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSAEMMetaDataReps)
        return CCSDS_METADATA_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool CCSDSAEMCountRequiredNumberMetaDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CCSDSAEMCountRequiredNumberMetaDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSAEMMetaData::EndCCSDSAEMMetaDataReps; id++)
        if (CCSDSAEMMetaData::CCSDS_METADATA_IS_REQUIRED[id])
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
bool CCSDSAEMMetaData::CheckMetaDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAEMMetaDataReps; i++)
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
std::string CCSDSAEMMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAEMMetaDataReps))
   {
      return CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataParameterText(id);
}

//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSAEMMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAEMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[i]))
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
Gmat::ParameterType CCSDSAEMMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAEMMetaDataReps))
      return CCSDS_METADATA_PARAMETER_TYPE[id];

   return CCSDSObType::GetDataParameterType(id);
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSAEMMetaData::GetDataParameterTypeString(const Integer id) const
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
Integer CCSDSAEMMetaData::GetIntegerDataParameter(const Integer id) const
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
Integer CCSDSAEMMetaData::GetIntegerDataParameter(const std::string &label) const
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
std::string CCSDSAEMMetaData::GetStringDataParameter(const Integer id) const
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
std::string CCSDSAEMMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAEMMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_AEM_METADATACOMMENTS_ID:

	    return comments;

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
StringArray CCSDSAEMMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAEMMetaData *myMetadata)
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
std::ostream& operator<< (std::ostream &output, const CCSDSAEMMetaData *myMetadata)
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
   output << "REF_FRAME_A = " << myMetadata->frameA << std::endl;
   output << "REF_FRAME_B = " << myMetadata->frameB << std::endl;
   output << "ATTITUDE_DIR = " << myMetadata->direction << std::endl;
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << std::endl;
   output << "START_TIME = " << myMetadata->startEpoch << std::endl;
   output << "USEABLE_START_TIME = " << myMetadata->useableStartEpoch << std::endl;
   output << "USEABLE_STOP_TIME = " << myMetadata->useableStopEpoch << std::endl;
   output << "STOP_TIME = " << myMetadata->stopEpoch << std::endl;
   output << "ATTITUDE_TYPE = " << myMetadata->attitudeType << std::endl;
   output << "QUATERNION_TYPE = " << myMetadata->quaternionType << std::endl;
   output << "EULER_ROT_SEQ = " << myMetadata->eulerRotationSequence << std::endl;
   output << "RATE_FRAME = " << myMetadata->rateFrame << std::endl;
   output << "INTERPOLATION = " << myMetadata->interpolationMethod << std::endl;
   output << "INTERPOLATION_DEGREE = " << myMetadata->interpolationDegree << std::endl;

   output << "META_STOP" << std::endl << std::endl;

   return output;
}



