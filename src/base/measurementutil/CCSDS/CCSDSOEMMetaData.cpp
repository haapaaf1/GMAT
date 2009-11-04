#include "CCSDSOEMMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOEMMetaData::CCSDS_OEM_METADATA_KEYWORDS[EndCCSDSOEMMetaDataReps] =
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

const std::string CCSDSOEMMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSOEMMetaDataReps] =
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

const std::string CCSDSOEMMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSOEMMetaDataReps] =
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

const bool CCSDSOEMMetaData::CCSDS_METADATA_IS_REQUIRED[EndCCSDSOEMMetaDataReps] =
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

const Gmat::ParameterType CCSDSOEMMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSOEMMetaDataReps] =
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
//  CCSDSOEMMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOEMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOEMMetaData::CCSDSOEMMetaData() :
    objectName(std::string("")),
    internationalDesignator(std::string("")),
    refFrameOrigin(std::string("")),
    refFrame(std::string("")),
    timeSystem(std::string("")),
    startEpoch(std::string("")),
    stopEpoch(std::string("")),
    useableStartEpoch(std::string("")),
    useableStopEpoch(std::string("")),
    interpolationMethod(std::string("")),
    interpolationDegree(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSOEMMetaData(const CCSDSOEMMetaData &oemMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOEMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOEMMetaData::CCSDSOEMMetaData
               (const CCSDSOEMMetaData &oemMD) :
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
//  CCSDSOEMMetaData& operator= (const CCSDSOEMMetaData &oemMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSOEMMetaData structures.
 *
 * @param <oemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOEMMetaData& CCSDSOEMMetaData::operator=
                                     (const CCSDSOEMMetaData &oemMD)
{
    if (&oemMD == this)
        return *this;

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
//  ~CCSDSOEMMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOEMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOEMMetaData::~CCSDSOEMMetaData()
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
const std::string* CCSDSOEMMetaData::GetKeywords() const
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
const Integer CCSDSOEMMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOEMMetaDataReps; i++)
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
std::string CCSDSOEMMetaData::GetUnits(const Integer &id) const
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
bool CCSDSOEMMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSOEMMetaDataReps)
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

    for (Integer id = 0; id < CCSDSOEMMetaData::EndCCSDSOEMMetaDataReps; id++)
        if (CCSDSOEMMetaData::CCSDS_METADATA_IS_REQUIRED[id])
            num++;

    return num;
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
bool CCSDSOEMMetaData::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOEMMetaDataReps; i++)
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
std::string CCSDSOEMMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOEMMetaDataReps))
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
Integer CCSDSOEMMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOEMMetaDataReps; i++)
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
Gmat::ParameterType CCSDSOEMMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOEMMetaDataReps))
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
std::string CCSDSOEMMetaData::GetDataParameterTypeString(const Integer id) const
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
Integer CCSDSOEMMetaData::GetIntegerDataParameter(const Integer id) const
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
Integer CCSDSOEMMetaData::GetIntegerDataParameter(const std::string &label) const
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
std::string CCSDSOEMMetaData::GetStringDataParameter(const Integer id) const
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
std::string CCSDSOEMMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSOEMMetaData::GetStringArrayDataParameter(const Integer id) const
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
StringArray CCSDSOEMMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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

   for (unsigned int i = 0; i < myMetadata->comments.size(); i++ )
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
