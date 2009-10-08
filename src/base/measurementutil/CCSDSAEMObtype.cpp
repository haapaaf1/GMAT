#include "CCSDSAEMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAEMObType::CCSDS_AEM_KEYWORDS[EndCCSDSAEMDataReps-EndCCSDSDataReps] =
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

const std::string CCSDSAEMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAEMDataReps-EndCCSDSDataReps] =
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

const std::string CCSDSAEMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

const std::string CCSDSAEMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAEMDataReps-EndCCSDSDataReps] =
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

const bool CCSDSAEMObType::CCSDS_IS_REQUIRED[EndCCSDSAEMDataReps-EndCCSDSDataReps] =
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
    false
};

const Gmat::ParameterType CCSDSAEMObType::CCSDS_PARAMETER_TYPE[EndCCSDSAEMDataReps-EndCCSDSDataReps] =
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
//  CCSDSAEMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::CCSDSAEMObType() : CCSDSObType("CCSDSAEMObType", ""),
	ccsdsAEMMetaData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMObType(const CCSDSAEMObType &AEM)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::CCSDSAEMObType(const CCSDSAEMObType &AEM) : CCSDSObType(AEM),
	ccsdsAEMMetaData(AEM.ccsdsAEMMetaData)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMObType& operator=(const CCSDSAEMObType &AEM)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <AEM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMObType& CCSDSAEMObType::operator=(const CCSDSAEMObType &AEM)
{
   if (&AEM == this)
      return *this;

    ccsdsAEMMetaData = AEM.ccsdsAEMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::~CCSDSAEMObType()
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
GmatBase* CCSDSAEMObType::Clone() const
{
   GmatBase *clone = new CCSDSAEMObType(*this);
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
std::string CCSDSAEMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAEMDataReps))
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
std::string CCSDSAEMObType::GetDataUnits(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAEMDataReps))
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
Integer CCSDSAEMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAEMDataReps; i++)
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
Gmat::ParameterType CCSDSAEMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAEMDataReps))
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
std::string CCSDSAEMObType::GetDataParameterTypeString(const Integer id) const
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
Integer CCSDSAEMObType::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_AEM_INTERPOLATIONDEGREE_ID:

	    return ccsdsAEMMetaData->interpolationDegree;

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
Integer CCSDSAEMObType::GetIntegerDataParameter(const std::string &label) const
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
std::string CCSDSAEMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_AEM_TIMESYSTEM_ID:

            return ccsdsAEMMetaData->timeSystem;

	case CCSDS_AEM_REFFRAMEA_ID:

            return ccsdsAEMMetaData->frameA;

	case CCSDS_AEM_REFFRAMEB_ID:

            return ccsdsAEMMetaData->frameB;

	case CCSDS_AEM_CENTERNAME_ID:

            return ccsdsAEMMetaData->refFrameOrigin;

	case CCSDS_AEM_OBJECTID_ID:

            return ccsdsAEMMetaData->internationalDesignator;

        case CCSDS_AEM_OBJECTNAME_ID:

            return ccsdsAEMMetaData->objectName;

        case CCSDS_AEM_STARTEPOCH_ID:

            return ccsdsAEMMetaData->startEpoch;

        case CCSDS_AEM_STOPEPOCH_ID:

            return ccsdsAEMMetaData->stopEpoch;

        case CCSDS_AEM_USEABLE_STARTEPOCH_ID:

            return ccsdsAEMMetaData->useableStartEpoch;

        case CCSDS_AEM_USEABLE_STOPEPOCH_ID:

            return ccsdsAEMMetaData->useableStopEpoch;

        case CCSDS_AEM_ATTITUDEDIR_ID:

            return ccsdsAEMMetaData->direction;

        case CCSDS_AEM_ATTITUDETYPE_ID:

            return ccsdsAEMMetaData->attitudeType;

        case CCSDS_AEM_QUATERNIONTYPE_ID:

            return ccsdsAEMMetaData->quaternionType;

        case CCSDS_AEM_EULERROTSEQ_ID:

            return ccsdsAEMMetaData->eulerRotationSequence;

        case CCSDS_AEM_RATEFRAME_ID:

            return ccsdsAEMMetaData->rateFrame;

        case CCSDS_AEM_INTERPOLATION_ID:

            return ccsdsAEMMetaData->interpolationMethod;

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
std::string CCSDSAEMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAEMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_AEM_METADATACOMMENTS_ID:

	    return ccsdsAEMMetaData->comments;

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
StringArray CCSDSAEMObType::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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
const std::string* CCSDSAEMObType::GetKeywords() const
{
   return CCSDS_AEM_KEYWORDS;
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
const Integer CCSDSAEMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAEMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_AEM_KEYWORDS[i]))
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
std::string CCSDSAEMObType::GetUnits(const Integer &id) const
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
const std::string* CCSDSAEMObType::GetTimeSystems() const
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
std::string CCSDSAEMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSAEMTimeReps))
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
Integer CCSDSAEMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSAEMTimeReps; i++)
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
bool CCSDSAEMObType::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSAEMDataReps)
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
bool CCSDSAEMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAEMDataReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSAEMObType *myAEM)
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
std::ostream& operator<< (std::ostream &output, const CCSDSAEMObType *myAEM)
{
    switch (myAEM->ccsdsHeader->dataType)
    {
        case CCSDSObType::QUATERNION_ID:
            output << myAEM->ccsdsQuaternion;
            break;
        case CCSDSObType::EULERANGLE_ID:
            output << myAEM->ccsdsEulerAngle;
            break;
        case CCSDSObType::SPINSTABILIZED_ID:
            output << myAEM->ccsdsSpinStabilized;
            break;
        default:
            break;
    }

    return output;
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

   for (Integer i = 0; i < myMetadata->comments.size(); i++ )
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
