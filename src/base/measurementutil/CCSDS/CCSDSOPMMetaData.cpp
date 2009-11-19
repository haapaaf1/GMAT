//$Header$
//------------------------------------------------------------------------------
//                             CCSDSOPMMetaData
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
 * This class specifies the CCSDS Orbit Parameter MetaData class.
 *
 */
//------------------------------------------------------------------------------

#include "CCSDSOPMMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOPMMetaData::CCSDS_OPM_METADATA_KEYWORDS[EndCCSDSOPMMetaDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "REF_FRAME",
    "TIME_SYSTEM",
    "COMMENT"
};

const std::string CCSDSOPMMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSOPMMetaDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    ""
};

const std::string CCSDSOPMMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMMetaDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Reference Frame",
    "Time System",
    "Comments"
};

const bool CCSDSOPMMetaData::CCSDS_METADATA_IS_REQUIRED[EndCCSDSOPMMetaDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSOPMMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSOPMMetaDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSOPMMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOPMMetaData::CCSDSOPMMetaData() : CCSDSMetaData(),
    objectName(GmatBase::STRING_PARAMETER_UNDEFINED),
    internationalDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrameOrigin(GmatBase::STRING_PARAMETER_UNDEFINED),
    refFrame(GmatBase::STRING_PARAMETER_UNDEFINED),
    timeSystem(GmatBase::STRING_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMMetaData(const CCSDSOPMMetaData &opmMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOPMMetaData::CCSDSOPMMetaData(const CCSDSOPMMetaData &opmMD) :
    CCSDSMetaData(opmMD),
    objectName(opmMD.objectName),
    internationalDesignator(opmMD.internationalDesignator),
    refFrameOrigin(opmMD.refFrameOrigin),
    refFrame(opmMD.refFrame),
    timeSystem(opmMD.timeSystem),
    comments(opmMD.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMMetaData& operator= (const CCSDSOPMMetaData &opmMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSOPMMetaData structures.
 *
 * @param <opmMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMMetaData& CCSDSOPMMetaData::operator=(const CCSDSOPMMetaData &opmMD)
{
    if (&opmMD == this)
        return *this;

    CCSDSMetaData::operator=(opmMD);

    objectName = opmMD.objectName;
    internationalDesignator = opmMD.internationalDesignator;
    refFrameOrigin = opmMD.refFrameOrigin;
    refFrame = opmMD.refFrame;
    timeSystem = opmMD.timeSystem;
    comments = opmMD.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOPMMetaData::~CCSDSOPMMetaData()
{
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
const std::string* CCSDSOPMMetaData::GetKeywords() const
{
   return CCSDS_OPM_METADATA_KEYWORDS;
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
const Integer CCSDSOPMMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOPMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_OPM_METADATA_KEYWORDS[i]))
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
std::string CCSDSOPMMetaData::GetUnits(const Integer &id) const
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
bool CCSDSOPMMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSOPMMetaDataReps)
        return CCSDS_METADATA_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool CountRequiredNumberOPMMetaDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberOPMMetaDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSOPMMetaData::EndCCSDSOPMMetaDataReps; id++)
        if (CCSDSOPMMetaData::CCSDS_METADATA_IS_REQUIRED[id])
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
std::string CCSDSOPMMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOPMMetaDataReps))
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
Integer CCSDSOPMMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOPMMetaDataReps; i++)
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
Gmat::ParameterType CCSDSOPMMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOPMMetaDataReps))
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
std::string CCSDSOPMMetaData::GetDataParameterTypeString(const Integer id) const
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
std::string CCSDSOPMMetaData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_OPM_TIMESYSTEM_ID:

            return timeSystem;

	case CCSDS_OPM_REFFRAME_ID:

            return refFrame;

	case CCSDS_OPM_CENTERNAME_ID:

            return refFrameOrigin;

	case CCSDS_OPM_OBJECTID_ID:

            return internationalDesignator;

        case CCSDS_OPM_OBJECTNAME_ID:

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
std::string CCSDSOPMMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSOPMMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_OPM_METADATACOMMENTS_ID:

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
StringArray CCSDSOPMMetaData::GetStringArrayDataParameter(const std::string &label) const
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
bool CCSDSOPMMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSOPMMetaDataReps; i++ )
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
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetaData>    CCSDS OPM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOPMMetaData *myMetaData)
{

    using namespace std;

    if(!myMetaData->Validate()) return output;

   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

   for (unsigned int i = 0; i < myMetaData->comments.size(); i++ )
   {
       output << "COMMENT " << myMetaData->comments[i] << endl;
   }

    for (unsigned int i = 0; i < CCSDSOPMMetaData::EndCCSDSOPMMetaDataReps; i++)
    {

        switch (i)
        {

            case CCSDSOPMMetaData::CCSDS_OPM_OBJECTNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->objectName);
                if (definedFlag)
                {
                    output << "OBJECT_NAME = " << myMetaData->objectName;
                    output << endl;
                }
            }

            break;

            case CCSDSOPMMetaData::CCSDS_OPM_OBJECTID_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->internationalDesignator);
                if (definedFlag)
                {
                    output << "OBJECT_ID = " << myMetaData->internationalDesignator;
                    output << endl;
                }
            }

            break;

            case CCSDSOPMMetaData::CCSDS_OPM_CENTERNAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrameOrigin);
                if (definedFlag)
                {
                    output << "CENTER_NAME = " << myMetaData->refFrameOrigin;
                    output << endl;
                }
            }

            break;

            case CCSDSOPMMetaData::CCSDS_OPM_REFFRAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->refFrame);
                if (definedFlag)
                {
                    output << "REF_FRAME = " << myMetaData->refFrame;
                    output << endl;
                }
            }

            break;

            case CCSDSOPMMetaData::CCSDS_OPM_TIMESYSTEM_ID:
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
