#include "CCSDSHeader.hpp"

//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string CCSDSHeader::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps] =
{
    "Quaternion",
    "Euler Angle",
    "Spin Stabilized",
    "State Vector",
    "Keplerian Elements",
    "Spacecraft Parameters",
    "Spacecraft Inertia",
    "Maneuver",
    "Attitude Maneuver",
    "Generic Data Type"
};

const std::string CCSDSHeader::CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[EndCCSDSHeaderDataReps] =
{
    "CCSDS Version",
    "Creation Date",
    "Originator",
    "Header Comments"
};

const std::string CCSDSHeader::CCSDS_HEADER_KEYWORDS[EndCCSDSHeaderDataReps] =
{
    "CCSDS_VERSION",
    "CREATION_DATE",
    "ORIGINATOR",
    "COMMENT"
};

const bool CCSDSHeader::CCSDS_HEADER_IS_REQUIRED[EndCCSDSHeaderDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSHeader::CCSDS_HEADER_PARAMETER_TYPE[EndCCSDSHeaderDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE
};

const std::string CCSDSHeader::CCSDS_HEADER_UNIT_DESCRIPTIONS[EndCCSDSHeaderDataReps] =
{
    "",
    "",
    "",
    ""
};

//------------------------------------------------------------------------------
//  CCSDSHeader()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSHeader class
 */
//------------------------------------------------------------------------------
CCSDSHeader::CCSDSHeader() //: ObType(),
    fileType(std::string("")),
    ccsdsVersion(std::string("")),
    creationDate(std::string("")),
    originator(std::string("")),
    comments(),
    dataType(0)

{
}

//------------------------------------------------------------------------------
//  CCSDSHeader(const CCSDSHeader &header)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSHeader::CCSDSHeader(const CCSDSHeader &header) //: ObType(header).
    fileType(header.fileType),
    ccsdsVersion(header.ccsdsVersion),
    creationDate(header.creationDate),
    originator(header.originator),
    comments(header.comments),
    dataType(header.dataType)
{
}

//---------------------------------------------------------------------------
//  CCSDSHeader& operator=(const CCSDSHeader &header)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <header> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSHeader& CCSDSHeader::operator=(const CCSDSHeader &header)

{
    if (&header == this)
        return *this;

    //ObType::operator=(header);

    fileType = header.fileType;
    ccsdsVersion = header.ccsdsVersion;
    creationDate = header.creationDate;
    originator = header.originator;
    comments = header.comments;
    dataType = header.dataType;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSHeader()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSHeader class
 */
//------------------------------------------------------------------------------
CCSDSHeader::~CCSDSHeader()
{
}

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSHeader::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSHeaderDataReps))
   {
      return CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSHeader::GetUnits(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSHeaderDataReps))
   {
      return CCSDS_HEADER_UNIT_DESCRIPTIONS[id];
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
Integer CCSDSHeader::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSHeaderDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[i]))
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
Gmat::ParameterType CCSDSHeader::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSHeaderDataReps))
      return CCSDS_HEADER_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSHeader::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}


//---------------------------------------------------------------------------
//  bool CountRequiredNumberHeaderDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberHeaderDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSHeader::EndCCSDSHeaderDataReps; id++)
        if (CCSDSHeader::CCSDS_HEADER_IS_REQUIRED[id])
            num++;

    return num;
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
bool CCSDSHeader::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndCCSDSHeaderDataReps)
    return CCSDS_HEADER_IS_REQUIRED[id];
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
bool CCSDSHeader::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSHeaderDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS TDM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSHeader::GetKeywords() const
{
   return CCSDS_HEADER_KEYWORDS;
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
const Integer CCSDSHeader::GetKeywordID(const std::string str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSHeaderDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_HEADER_KEYWORDS[i]))
            return i;
    }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// const std::string* GetDataTypes() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable data types.
 *
 * @return String array of all data types.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSHeader::GetDataTypes() const
{
   return CCSDS_DATATYPE_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetDataTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type text corresponding to a ID
 *
 * @param <id> Integer ID associated with the data type
 * @return The string description of the data type
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSHeader::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTypeReps))
   {
      return CCSDS_DATATYPE_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetDataTypeID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type ID
 *
 * @param <label> The string label associated with the data type
 * @return The integer data type ID
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSHeader::GetDataTypeID(const std::string &label)
{
    std::string regex = "^" + label + "$";

    for (Integer i = 0; i < EndCCSDSHeaderDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_DATATYPE_DESCRIPTIONS[i]))
            return i;
    }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myHeader)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myHeader>    CCSDS header data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myHeader)
{
    using namespace std;

    output << "CCSDS_" << myHeader->fileType << "_VERS = " << myHeader->ccsdsVersion << endl;
    for (unsigned int i = 0; i < myHeader->comments.size(); i++)
    {
        output << "COMMENT " << myHeader->comments[i] << endl;
    }
    output << "CREATION_DATE = " << myHeader->creationDate << endl;
    output << "ORIGINATOR = " << myHeader->originator << endl;

    return output;
}
