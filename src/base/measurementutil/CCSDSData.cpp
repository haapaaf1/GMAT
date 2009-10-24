#include "CCSDSData.hpp"
//---------------------------------
//  static data
//---------------------------------

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSGenericDataReps] =
{
    "Keyword",
    "Epoch",
    "Measurement",
    "Comment"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSGenericDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSGenericDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::CCSDSData() : CCSDSObType()
{
}

//------------------------------------------------------------------------------
//  CCSDSData(const CCSDSData &data) : CCSDSData(data)
//------------------------------------------------------------------------------
/**
 * Constructor for the generic CCSDS Data class
 */
//------------------------------------------------------------------------------
CCSDSData::CCSDSData(const CCSDSData &data) : CCSDSObType(data)
{
}

//---------------------------------------------------------------------------
//  CCSDSData& operator=(const CCSDSData &data)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Data structures.
 *
 * @param <data> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSData& CCSDSData::operator=(const CCSDSData &data)

{
    if (&data == this)
        return *this;

    CCSDSObType::operator=(data);

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::~CCSDSData()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSData.
 *
 * @return clone of the CCSDSData.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSData::Clone() const
{
   GmatBase *clone = new CCSDSData(*this);
   return (clone);
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
bool CCSDSData::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSGenericDataReps)
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
bool CCSDSData::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSGenericDataReps; i++)
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
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSGenericDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
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
Integer CCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSGenericDataReps; i++)
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
Gmat::ParameterType CCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSGenericDataReps))
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
std::string CCSDSData::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObType::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_GENERICDATA_KEYWORD_ID:

            return keyword;

	case CCSDS_GENERICDATA_TIMETAG_ID:

            return timeTag;

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
std::string CCSDSData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_GENERICDATA_COMMENTS_ID:

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
StringArray CCSDSData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_GENERICDATA_MEASUREMENT_ID:

	    return measurement;

	default:

	    return CCSDSObType::GetRealDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSData *myCCSDSData)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myData>    CCSDS data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSData *myCCSDSData)
{
    using namespace std;

    output << myCCSDSData->keywordID << " = " << myCCSDSData->timeTag
           << " " << myCCSDSData->measurement << endl;

    return output;
}