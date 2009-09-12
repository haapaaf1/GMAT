#include "CCSDSObtype.hpp";
//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

const std::string CCSDSObtype::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "CCSDS Version",
    "Creation Date",
    "Originator",
    "HeaderComments"
};

const bool CCSDSObtype::CCSDS_IS_REQUIRED[EndCCSDSDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSObtype::CCSDS_PARAMETER_TYPE[EndCCSDSDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE
};

const std::string CCSDSObtype::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "",
    "",
    "",
    ""
};

//------------------------------------------------------------------------------
//  CCSDSObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObtype::CCSDSObtype() : Obtype(),
    ccsdsHeader(NULL)
{
}

//------------------------------------------------------------------------------
//  ~CCSDSObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObtype::~CCSDSObtype()
{
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer CCSDSObtype::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";
    
    for (Integer i = 0; i < EndCCSDSDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSObtype::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see Obtype
 */
//---------------------------------------------------------------------------
std::string CCSDSObtype::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//---------------------------------------------------------------------------
Integer CCSDSObtype::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer CCSDSObtype::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool CCSDSObtype::GetBoolDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool CCSDSObtype::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
StringArray CCSDSObtype::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_HEADERCOMMENTS_ID:

	    return ccsdsHeader->headerComments;

        default:

            return GmatBase::STRINGARRAY_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
StringArray CCSDSObtype::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_CREATIONDATE_ID:

	    return ccsdsHeader->creationDate;

	case CCSDS_ORIGINATOR_ID:

	    return ccsdsHeader->originator;
	    
        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSObtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real CCSDSObtype::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

       case CCSDS_VERSION_ID:

            return ccsdsHeader->ccsdsVersion;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real CCSDSObtype::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
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
const std::string* CCSDSObtype::GetDataTypes() const
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
std::string CCSDSObtype::GetDataTypeText(const Integer &id) const
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
Integer CCSDSObtype::GetDataTypeID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
const std::string* CCSDSObtype::GetTimeSystems() const
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
std::string CCSDSObtype::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer CCSDSObtype::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = 0; i < EndCCSDSTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}
    
    }
      
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
 
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
bool CCSDSObtype::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndCCSDSDataReps)
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
bool CCSDSObtype::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}
