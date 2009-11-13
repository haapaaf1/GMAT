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
    "Tracking Data Type"
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
CCSDSHeader::CCSDSHeader() : //ObType(),
    fileType(GmatBase::STRING_PARAMETER_UNDEFINED),
    ccsdsVersion(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    creationDate(GmatBase::STRING_PARAMETER_UNDEFINED),
    originator(GmatBase::STRING_PARAMETER_UNDEFINED),
    dataType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSHeader(const CCSDSHeader &header)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSHeader::CCSDSHeader(const CCSDSHeader &header) : //ObType(header).
    fileType(header.fileType),
    ccsdsVersion(header.ccsdsVersion),
    creationDate(header.creationDate),
    originator(header.originator),
    dataType(header.dataType),
    comments(header.comments)
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
    dataType = header.dataType;
    comments = header.comments;

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
//  bool IsParameterDefined(const Integer id, bool value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSHeader::IsParameterDefined(const Integer id, bool value) const
{
    if (GetDataParameterType(id) == Gmat::BOOLEAN_TYPE)
    {
        if (&value == NULL)
            return false;
        else
            return true;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, StringArray value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSHeader::IsParameterDefined(const Integer id, StringArray value) const
{
    if (id >= 0 && id <= EndCCSDSHeaderDataReps)
    {

        if (GetDataParameterType(id) == Gmat::STRINGARRAY_TYPE)
        {
            if (&value == NULL || value == GmatBase::STRINGARRAY_PARAMETER_UNDEFINED)
                return false;
            else
                return true;
        }
        else
            return false;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, std::string value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSHeader::IsParameterDefined(const Integer id, std::string value) const
{
    if (id >= 0 && id <= EndCCSDSHeaderDataReps)
    {

        if (GetDataParameterType(id) == Gmat::STRING_TYPE)
        {
            if (&value == NULL || value == GmatBase::STRING_PARAMETER_UNDEFINED)
                return false;
            else
                return true;
        }
        else
            return false;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, Real value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSHeader::IsParameterDefined(const Integer id, Real value) const
{
    if (id >= 0 && id <= EndCCSDSHeaderDataReps)
    {
        if (GetDataParameterType(id) == Gmat::REAL_TYPE)
        {
            if (&value == NULL || value == GmatBase::REAL_PARAMETER_UNDEFINED)
                return false;
            else
                return true;
        }
        else
            return false;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, Integer value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSHeader::IsParameterDefined(const Integer id, Integer value) const
{
    if (id >= 0 && id <= EndCCSDSHeaderDataReps)
    {

        if (GetDataParameterType(id) == Gmat::INTEGER_TYPE)
        {
            if (&value == NULL || value == GmatBase::INTEGER_PARAMETER_UNDEFINED)
                return false;
            else
                return true;
        }
        else
            return false;
    }
    else
        return false;
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
bool CCSDSHeader::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSHeaderDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::INTEGER_TYPE:
                    {
                    Integer ivalue = GetIntegerDataParameter(i);
                    if (&ivalue == NULL ||
                        ivalue == GmatBase::INTEGER_PARAMETER_UNDEFINED)
                        return false;
                    }
                    break;
                case Gmat::REAL_TYPE:
                    {
                    Real rvalue = GetRealDataParameter(i);
                    if (&rvalue == NULL ||
                        rvalue == GmatBase::REAL_PARAMETER_UNDEFINED)
                        return false;
                    }
                    break;
                case Gmat::STRING_TYPE:
                    {
                    std::string svalue = GetStringDataParameter(i);
                    if (&svalue == NULL ||
                        svalue == GmatBase::STRING_PARAMETER_UNDEFINED)
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    {
                    StringArray savalue = GetStringArrayDataParameter(i);
                    if (&savalue == NULL ||
                        savalue == GmatBase::STRINGARRAY_PARAMETER_UNDEFINED)
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

//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> ID of the parameter.
 *
 * @return true if the parameter is read only, false (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSHeader::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSHeaderDataReps)
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
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Real CCSDSHeader::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_VERSION_ID:

            return ccsdsVersion;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSHeader::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSHeader::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSHeader::GetIntegerDataParameter(const std::string &label) const
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
std::string CCSDSHeader::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_CREATIONDATE_ID:

            return creationDate;

	case CCSDS_ORIGINATOR_ID:

            return originator;

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
std::string CCSDSHeader::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSHeader::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_HEADERCOMMENTS_ID:

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
StringArray CCSDSHeader::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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

    output.setf(ios::fixed, ios_base::floatfield);
    output.precision(2);
    output << "CCSDS_" << myHeader->fileType << "_VERS = " << myHeader->ccsdsVersion << endl;
    output << "CREATION_DATE = " << myHeader->creationDate << endl;
    output << "ORIGINATOR = " << myHeader->originator << endl;
    output << endl;
    unsigned int i;
    for (i = 0; i < myHeader->comments.size(); i++)
    {
        output << "COMMENT " << myHeader->comments[i] << endl;
    }
    if (i > 0) output << endl;

    output.unsetf(ios_base::floatfield);
    output.precision(5);

    return output;
}
