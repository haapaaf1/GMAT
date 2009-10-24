#include "CCSDSTDMObType.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSTDMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMDataReps] =
{
    "Keyword",
    "Time Tag",
    "Measurement",
    "Comment"
};

const bool CCSDSTDMObType::CCSDS_IS_REQUIRED[EndCCSDSTDMDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSTDMObType::CCSDS_PARAMETER_TYPE[EndCCSDSTDMDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE
};

const std::string CCSDSTDMObType::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps] =
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
    "VLBIDelay"
};

const std::string CCSDSTDMObType::CCSDS_TDM_KEYWORDS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps] =
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
    "VLBI_DELAY"
};

const std::string CCSDSTDMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps] =
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
    "s"
};

const std::string CCSDSTDMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "GPS",
    "SCLK"
};

const std::string CCSDSTDMObType::MODE_DESCRIPTIONS[EndCCSDSTDMModeReps] =
{
    "SEQUENTIAL",
    "SINGLE_DIFF"
};

const std::string CCSDSTDMObType::TIMETAG_DESCRIPTIONS[EndCCSDSTDMTimetagReps] =
{
    "TRANSMIT",
    "RECEIVE"
};

const std::string CCSDSTDMObType::INTEGRATION_DESCRIPTIONS[EndCCSDSTDMIntegrationReps] =
{
    "START",
    "MIDDLE",
    "END"
};

const std::string CCSDSTDMObType::RANGEMODE_DESCRIPTIONS[EndCCSDSTDMRangeModeReps] =
{
    "COHERENT",
    "CONSTANT",
    "ONE_WAY"
};

const std::string CCSDSTDMObType::RANGEUNIT_DESCRIPTIONS[EndCCSDSTDMRangeUnitReps] =
{
    "km",
    "s",
    "RU"
};

const std::string CCSDSTDMObType::ANGLETYPE_DESCRIPTIONS[EndCCSDSTDMAngleTypeReps] =
{
    "AZEL",
    "RADEC",
    "XEYN",
    "XSYE"
};

const std::string CCSDSTDMObType::DATAQUALITY_DESCRIPTIONS[EndCCSDSTDMDataQualityReps] =
{
    "RAW",
    "VALIDATED",
    "DEGRADED"
};

//------------------------------------------------------------------------------
//  CCSDSTDMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::CCSDSTDMObType() : CCSDSObType("CCSDSTDMObType", ""),
	ccsdsTDMMetaData(NULL),
        ccsdsTDMData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSTDMObType(const CCSDSTDMObType &tdm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::CCSDSTDMObType(const CCSDSTDMObType &tdm) : CCSDSObType(tdm),
	ccsdsTDMMetaData(tdm.ccsdsTDMMetaData),
        ccsdsTDMData(tdm.ccsdsTDMData)
{
}

//---------------------------------------------------------------------------
//  CCSDSTDMObType& operator=(const CCSDSTDMObType &tdm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tdm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSTDMObType& CCSDSTDMObType::operator=(const CCSDSTDMObType &tdm)
{
   if (&tdm == this)
      return *this;

   CCSDSObType::operator=(tdm);

   ccsdsTDMMetaData = tdm.ccsdsTDMMetaData;
   ccsdsTDMData = tdm.ccsdsTDMData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSTDMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::~CCSDSTDMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSTDMDataFile.
 *
 * @return clone of the ProcessCCSDSTDMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSTDMObType::Clone() const
{
   GmatBase *clone = new CCSDSTDMObType(*this);
   return (clone);
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
const std::string* CCSDSTDMObType::GetKeywords() const
{
   return CCSDS_TDM_KEYWORDS;
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
const Integer CCSDSTDMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_TDM_KEYWORDS[i]))
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
std::string CCSDSTDMObType::GetUnits(const Integer &id) const
{
   return CCSDS_UNIT_DESCRIPTIONS[id];
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
bool CCSDSTDMObType::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSTDMDataReps)
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
bool CCSDSTDMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMDataReps; i++)
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
std::string CCSDSTDMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMDataReps))
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
std::string CCSDSTDMObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMDataReps))
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
Integer CCSDSTDMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";
    
    for (Integer i = 0; i < EndCCSDSTDMDataReps; i++)
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
Gmat::ParameterType CCSDSTDMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMDataReps))
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
std::string CCSDSTDMObType::GetDataParameterTypeString(const Integer id) const
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
Integer CCSDSTDMObType::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_TDM_KEYWORD_ID:

            return ccsdsTDMData->keywordID;

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
Integer CCSDSTDMObType::GetIntegerDataParameter(const std::string &label) const
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
std::string CCSDSTDMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_TIMETAG_ID:
	    
            return ccsdsTDMData->timeTag;
		
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
std::string CCSDSTDMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSTDMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_TDM_COMMENTS_ID:

	    return ccsdsTDMData->comments;

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
StringArray CCSDSTDMObType::GetStringArrayDataParameter(const std::string &label) const
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
Real CCSDSTDMObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {
	
	case CCSDS_TDM_MEASUREMENT_ID:
	    
	    return ccsdsTDMData->measurement;
	
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
Real CCSDSTDMObType::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string* GetDataTypes() const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the list of data types
 *
 * @return The data types
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSTDMObType::GetDataTypes() const
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
std::string CCSDSTDMObType::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMTypeReps))
   {
      return CCSDS_DATATYPE_DESCRIPTIONS[id];
   }

   return CCSDSObType::GetDataTypeText(id);
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
Integer CCSDSTDMObType::GetDataTypeID(const std::string &label)
{
    return CCSDSObType::GetDataTypeID(label);
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
const std::string* CCSDSTDMObType::GetTimeSystems() const
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
std::string CCSDSTDMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSTDMTimeReps))
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
Integer CCSDSTDMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSTDMTimeReps; i++)
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSTDMObType *myTDM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSTDMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS TDM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSTDMObType *myTDM)
{
    switch (myTDM->ccsdsHeader->dataType)
    {
        case CCSDSObType::GENERICDATA_ID:
            output << myTDM->ccsdsTDMData;
            break;
        default:
            break;
    }

    return output;
}
