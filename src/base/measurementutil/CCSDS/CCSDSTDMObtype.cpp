#include "CCSDSTDMObType.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSTDMObType::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTDMTypeReps] =
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

const std::string CCSDSTDMObType::CCSDS_TDM_KEYWORDS[EndCCSDSTDMTypeReps] =
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

const std::string CCSDSTDMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps] =
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
	ccsdsMetaData(NULL),
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
	ccsdsMetaData(tdm.ccsdsMetaData),
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

   ccsdsMetaData = tdm.ccsdsMetaData;
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

    for (Integer i = 0; i < EndCCSDSTDMTypeReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_TDM_KEYWORDS[i]))
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
std::string CCSDSTDMObType::GetUnits(const Integer &id) const
{
   return CCSDS_UNIT_DESCRIPTIONS[id];
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
Integer CCSDSTDMObType::GetDataTypeID(const std::string &label)
{
    std::string regex = "^" + label + "$";

    for (Integer i = 0; i < EndCCSDSTDMTypeReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_DATATYPE_DESCRIPTIONS[i]))
            return i;
    }
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
    if (myTDM->ccsdsTDMData != NULL)
        output << myTDM->ccsdsTDMData;
    return output;
}
