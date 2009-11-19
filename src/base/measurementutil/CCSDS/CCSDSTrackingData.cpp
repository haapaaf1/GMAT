#include "CCSDSTrackingData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSTrackingData::CCSDS_TRACKINGDATA_DESCRIPTIONS[EndCCSDSTrackingDataReps] =
{
    "Keyword",
    "Epoch",
    "Measurement",
    "Comment"
};

const std::string CCSDSTrackingData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMTypeReps] =
{
    "Angle1",
    "Angle2",
    "CarrierPower",
    "ClockBias",
    "ClockDrift",
    "Comment",
    "DopplerInstantaneous",
    "DopplerIntegrated",
    "DOR",
    "PCN0",
    "PRN0",
    "Pressure",
    "Range",
    "ReceiveFrequency",
    "ReceiveFrequency1",
    "ReceiveFrequency2",
    "ReceiveFrequency3",
    "ReceiveFrequency4",
    "ReceiveFrequency5",
    "RelativeHumidity",
    "STEC",
    "Temperature",
    "TransmitFrequency1",
    "TransmitFrequency2",
    "TransmitFrequency3",
    "TransmitFrequency4",
    "TransmitFrequency5",
    "TransmitFrequencyRate1",
    "TransmitFrequencyRate2",
    "TransmitFrequencyRate3",
    "TransmitFrequencyRate4",
    "TransmitFrequencyRate5",
    "TropoDry",
    "TropoWet",
    "VLBIDelay",
    "Comments"
};

const std::string CCSDSTrackingData::CCSDS_TRACKINGDATA_KEYWORDS[EndCCSDSTDMTypeReps] =
{
    "ANGLE_1",
    "ANGLE_2",
    "CARRIER_POWER",
    "CLOCK_BIAS",
    "CLOCK_DRIFT",
    "COMMENT",
    "DOPPLER_INSTANTANEOUS",
    "DOPPLER_INTEGRATED",
    "DOR",
    "PC_N0",
    "PR_N0",
    "PRESSURE",
    "RANGE",
    "RECEIVE_FREQ",
    "RECEIVE_FREQ_1",
    "RECEIVE_FREQ_2",
    "RECEIVE_FREQ_3",
    "RECEIVE_FREQ_4",
    "RECEIVE_FREQ_5",
    "RHUMIDITY",
    "STEC",
    "TEMPERATURE",
    "TRANSMIT_FREQ_1",
    "TRANSMIT_FREQ_2",
    "TRANSMIT_FREQ_3",
    "TRANSMIT_FREQ_4",
    "TRANSMIT_FREQ_5",
    "TRANSMIT_FREQ_RATE_1",
    "TRANSMIT_FREQ_RATE_2",
    "TRANSMIT_FREQ_RATE_3",
    "TRANSMIT_FREQ_RATE_4",
    "TRANSMIT_FREQ_RATE_5",
    "TROPO_DRY",
    "TROPO_WET",
    "VLBI_DELAY",
    "COMMENT"
};

const std::string CCSDSTrackingData::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps] =
{
    "deg",
    "deg",
    "dBW",
    "s",
    "s/s",
    "n/a",
    "km/s",
    "km/s",
    "s",
    "dBHz",
    "dBHz",
    "hPa",
    "km, s, or RU",
    "Hz",
    "Hz",
    "Hz",
    "Hz",
    "Hz",
    "Hz",
    "%",
    "TECU",
    "K",
    "Hz",
    "Hz",
    "Hz",
    "Hz",
    "Hz",
    "Hz/s",
    "Hz/s",
    "Hz/s",
    "Hz/s",
    "Hz/s",
    "m",
    "m",
    "s",
    ""
};

const bool CCSDSTrackingData::CCSDS_IS_REQUIRED[EndCCSDSTDMTypeReps] =
{
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false
};

const Gmat::ParameterType CCSDSTrackingData::CCSDS_PARAMETER_TYPE[EndCCSDSTDMTypeReps] =
{
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSTrackingData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSTrackingData class
 */
//------------------------------------------------------------------------------
CCSDSTrackingData::CCSDSTrackingData() : CCSDSData(),
    keywordID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    keyword(GmatBase::STRING_PARAMETER_UNDEFINED),
    timeTag(GmatBase::STRING_PARAMETER_UNDEFINED),
    measurement(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSTrackingData(const CCSDSTrackingData &data) :
//------------------------------------------------------------------------------
/**
 * Constructor for the generic CCSDS Data class
 */
//------------------------------------------------------------------------------
CCSDSTrackingData::CCSDSTrackingData(const CCSDSTrackingData &data) :
    CCSDSData(data),
    keywordID(data.keywordID),
    keyword(data.keyword),
    timeTag(data.timeTag),
    measurement(data.measurement),
    comments(data.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSTrackingData& operator=(const CCSDSTrackingData &data)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Data structures.
 *
 * @param <data> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSTrackingData& CCSDSTrackingData::operator=(const CCSDSTrackingData &data)

{
    if (&data == this)
        return *this;

    CCSDSData::operator=(data);

    keywordID = data.keywordID;
    keyword = data.keyword;
    timeTag = data.timeTag;
    measurement = data.measurement;
    comments = data.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSTrackingData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSTrackingData class
 */
//------------------------------------------------------------------------------
CCSDSTrackingData::~CCSDSTrackingData()
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
const std::string* CCSDSTrackingData::GetKeywords() const
{
   return CCSDS_TRACKINGDATA_KEYWORDS;
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
const Integer CCSDSTrackingData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMTypeReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_TRACKINGDATA_KEYWORDS[i]))
            return i;
    }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;

}

//------------------------------------------------------------------------------
//  std::string  GetUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSTrackingData::GetUnits(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMTypeReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
bool CCSDSTrackingData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSTDMTypeReps)
	return CCSDS_IS_REQUIRED[id];
    else
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
std::string CCSDSTrackingData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMTypeReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
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
Integer CCSDSTrackingData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMTypeReps; i++)
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
 * @see ObType
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSTrackingData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMTypeReps))
      return CCSDS_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSTrackingData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSTrackingData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TRACKINGDATA_KEYWORD_ID:

            return keyword;

	case CCSDS_TRACKINGDATA_TIMETAG_ID:

            return timeTag;

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
std::string CCSDSTrackingData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSTrackingData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_TRACKINGDATA_COMMENTS_ID:

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
StringArray CCSDSTrackingData::GetStringArrayDataParameter(const std::string &label) const
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
Real CCSDSTrackingData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TRACKINGDATA_MEASUREMENT_ID:

	    return measurement;

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
Real CCSDSTrackingData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}


//---------------------------------------------------------------------------
//  bool CCSDSCountRequiredNumberDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CCSDSCountRequiredNumberDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSTrackingData::EndCCSDSTrackingDataReps; id++)
        if (CCSDSTrackingData::CCSDS_IS_REQUIRED[id])
            num++;

    return num;
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
bool CCSDSTrackingData::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSTrackingDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
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
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSTrackingData *myCCSDSTrackingData)
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
                          const CCSDSTrackingData *myTrackingData)
{
    using namespace std;

    if (!myTrackingData->Validate()) return output;

    output << myTrackingData->keyword << " = " << myTrackingData->timeTag
           << " " << myTrackingData->measurement << endl;

    return output;
}
