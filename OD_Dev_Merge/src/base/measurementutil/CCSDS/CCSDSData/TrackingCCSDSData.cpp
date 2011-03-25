//$Header$
//------------------------------------------------------------------------------
//                             TrackingCCSDSData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the Tracking Data construct that is used by the
 * CCSDS Tracking Data message format.
 *
 */
//------------------------------------------------------------------------------

#include "TrackingCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string TrackingCCSDSData::CCSDS_TRACKINGDATA_DESCRIPTIONS[EndTrackingCCSDSDataReps] =
{
    "Keyword",
    "Epoch",
    "Measurement",
    "Comment"
};

const std::string TrackingCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMTypeReps] =
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

const std::string TrackingCCSDSData::CCSDS_TRACKINGDATA_KEYWORDS[EndCCSDSTDMTypeReps] =
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

const std::string TrackingCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps] =
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

const bool TrackingCCSDSData::CCSDS_IS_REQUIRED[EndCCSDSTDMTypeReps] =
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

const Gmat::ParameterType TrackingCCSDSData::CCSDS_PARAMETER_TYPE[EndCCSDSTDMTypeReps] =
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
//  TrackingCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the TrackingCCSDSData class
 */
//------------------------------------------------------------------------------
TrackingCCSDSData::TrackingCCSDSData() : CCSDSData(),
    keywordID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    keyword(GmatBase::STRING_PARAMETER_UNDEFINED),
    timeTag(GmatBase::STRING_PARAMETER_UNDEFINED),
    measurement(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  TrackingCCSDSData(const TrackingCCSDSData &data) :
//------------------------------------------------------------------------------
/**
 * Constructor for the generic CCSDS Data class
 */
//------------------------------------------------------------------------------
TrackingCCSDSData::TrackingCCSDSData(const TrackingCCSDSData &data) :
    CCSDSData(data),
    keywordID(data.keywordID),
    keyword(data.keyword),
    timeTag(data.timeTag),
    measurement(data.measurement),
    comments(data.comments)
{
}

//---------------------------------------------------------------------------
//  TrackingCCSDSData& operator=(const TrackingCCSDSData &data)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Data structures.
 *
 * @param <data> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const TrackingCCSDSData& TrackingCCSDSData::operator=(const TrackingCCSDSData &data)

{
    if (&data == this)
        return *this;

    TrackingCCSDSData::operator=(data);

    keywordID = data.keywordID;
    keyword = data.keyword;
    timeTag = data.timeTag;
    measurement = data.measurement;
    comments = data.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~TrackingCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the TrackingCCSDSData class
 */
//------------------------------------------------------------------------------
TrackingCCSDSData::~TrackingCCSDSData()
{
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS tracking data keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* TrackingCCSDSData::GetKeywords() const
{
   return CCSDS_TRACKINGDATA_KEYWORDS;
}

//------------------------------------------------------------------------------
// std::string GetKeyword(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Returns the string keyword associated with the tracking data keyword ID
 *
 * @param <id> ID of the desired string keyword
 * @return String keyword
 *
 */
//------------------------------------------------------------------------------
std::string TrackingCCSDSData::GetKeyword(const Integer id) const
{
   return CCSDS_TRACKINGDATA_KEYWORDS[id];
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
const Integer TrackingCCSDSData::GetKeywordID(const std::string str) const
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
std::string TrackingCCSDSData::GetUnits(const Integer &id) const
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
bool TrackingCCSDSData::IsParameterRequired(const Integer id) const
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
std::string TrackingCCSDSData::GetDataParameterText(const Integer id) const
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
Integer TrackingCCSDSData::GetDataParameterID(const std::string &str) const
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
Gmat::ParameterType TrackingCCSDSData::GetDataParameterType(const Integer id) const
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
std::string TrackingCCSDSData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string TrackingCCSDSData::GetStringDataParameter(const Integer id) const
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
// std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string TrackingCCSDSData::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer TrackingCCSDSData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TRACKINGDATA_KEYWORD_ID:

            return keywordID;

        default:

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer TrackingCCSDSData::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray TrackingCCSDSData::GetStringArrayDataParameter(const Integer id) const
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
StringArray TrackingCCSDSData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real TrackingCCSDSData::GetRealDataParameter(const Integer id) const
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
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real TrackingCCSDSData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Real &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool TrackingCCSDSData::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

	case CCSDS_TRACKINGDATA_MEASUREMENT_ID:

	    measurement = value;
            return true;

	default:

	    return false;

    }

}


//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const Real &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <label> String label identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool TrackingCCSDSData::SetDataParameter(const std::string &label, const Real &value)
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
bool TrackingCCSDSData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

	case CCSDS_TRACKINGDATA_KEYWORD_ID:

            keyword = value;
            keywordID = GetKeywordID(value);
            
            if (keywordID != GmatBase::INTEGER_PARAMETER_UNDEFINED)
                return true;
            else
            {
                keyword = GmatBase::STRING_PARAMETER_UNDEFINED;
                return false;
            }

	case CCSDS_TRACKINGDATA_TIMETAG_ID:

            timeTag = value;
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
bool TrackingCCSDSData::SetDataParameter(const std::string &label, const std::string &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Integer &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of an Integer parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool TrackingCCSDSData::SetDataParameter(const Integer id, const Integer &value)
{
    switch (id)
    {

	case CCSDS_TRACKINGDATA_KEYWORDID_ID:

            keywordID = value;
            keyword = GetKeyword(value);

            if (keyword != GmatBase::STRING_PARAMETER_UNDEFINED)
                return true;
            else
            {
                keywordID = GmatBase::INTEGER_PARAMETER_UNDEFINED;
                return false;
            }

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
bool TrackingCCSDSData::SetDataParameter(const std::string &label, const Integer &value)
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
bool TrackingCCSDSData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {
        case CCSDS_TRACKINGDATA_COMMENTS_ID:

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
bool TrackingCCSDSData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
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

    for (Integer id = 0; id < TrackingCCSDSData::EndTrackingCCSDSDataReps; id++)
        if (TrackingCCSDSData::CCSDS_IS_REQUIRED[id])
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
bool TrackingCCSDSData::Validate() const
{

    for (unsigned int i = 0; i < EndTrackingCCSDSDataReps; i++ )
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
//                           const TrackingCCSDSData *myTrackingCCSDSData)
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
                          const TrackingCCSDSData *myTrackingData)
{
    using namespace std;

    if (!myTrackingData->Validate()) return output;

    output << myTrackingData->keyword << " = " << myTrackingData->timeTag
           << " " << myTrackingData->measurement << endl;

    return output;
}
