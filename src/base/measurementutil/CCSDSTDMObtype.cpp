#include "CCSDSTDMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSTDMObtype::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps] =
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

const std::string CCSDSTDMObtype::CCSDS_TDM_KEYWORDS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps] =
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

const std::string CCSDSTDMObtype::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps] =
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

const std::string CCSDSTDMObtype::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "GPS",
    "SCLK"
};

const std::string CCSDSTDMObtype::MODE_DESCRIPTIONS[EndCCSDSTDMModeReps] =
{
    "SEQUENTIAL",
    "SINGLE_DIFF"
};

const std::string CCSDSTDMObtype::TIMETAG_DESCRIPTIONS[EndCCSDSTDMTimetagReps] =
{
    "TRANSMIT",
    "RECEIVE"
};

const std::string CCSDSTDMObtype::INTEGRATION_DESCRIPTIONS[EndCCSDSTDMIntegrationReps] =
{
    "START",
    "MIDDLE",
    "END"
};

const std::string CCSDSTDMObtype::RANGEMODE_DESCRIPTIONS[EndCCSDSTDMRangeModeReps] =
{
    "COHERENT",
    "CONSTANT",
    "ONE_WAY"
};

const std::string CCSDSTDMObtype::RANGEUNIT_DESCRIPTIONS[EndCCSDSTDMRangeUnitReps] =
{
    "km",
    "s",
    "RU"
};

const std::string CCSDSTDMObtype::ANGLETYPE_DESCRIPTIONS[EndCCSDSTDMAngleTypeReps] =
{
    "AZEL",
    "RADEC",
    "XEYN",
    "XSYE"
};

const std::string CCSDSTDMObtype::DATAQUALITY_DESCRIPTIONS[EndCCSDSTDMDataQualityReps] =
{
    "RAW",
    "VALIDATED",
    "DEGRADED"
};

const std::string CCSDSTDMObtype::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMDataReps-EndCCSDSDataReps] =
{
    "MetadataComments",
    "TimeSystem",
    "StartTime",
    "StopTime",
    "Participant1",
    "Participant2",
    "Participant3",
    "Participant4",
    "Participant5",
    "Mode",
    "Path",
    "Path1",
    "Path2",
    "TransmitBand",
    "ReceiveBand",
    "TurnaroundNumerator",
    "TurnaroundDenominator",
    "TimeTagReference",
    "IntegrationInterval",
    "IntegrationReference",
    "FrequencyOffset",
    "RangeMode",
    "RangeModulus",
    "RangeUnits",
    "AngleType",
    "ReferenceFrame",
    "TransmitDelay1",
    "TransmitDelay2",
    "TransmitDelay3",
    "TransmitDelay4",
    "TransmitDelay5",
    "ReceiveDelay1",
    "ReceiveDelay2",
    "ReceiveDelay3",
    "ReceiveDelay4",
    "ReceiveDelay5",
    "DataQuality",
    "CorrectionAngle1",
    "CorrectionAngle2",
    "CorrectionDoppler",
    "CorrectionRange",
    "CorrectionReceive",
    "CorrectionTransmit",
    "CorrectionApplied",
    "TimeTag",
    "Measurement",
    "Units",
    "Keyword"
};

const bool CCSDSTDMObtype::CCSDS_IS_REQUIRED[EndCCSDSTDMDataReps-EndCCSDSDataReps] =
{
    false,
    true,
    false,
    false,
    true,
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
    false,
    false,
    false,
    false,
    true,
    true,
    false,
    true
};

const Gmat::ParameterType CCSDSTDMObtype::CCSDS_PARAMETER_TYPE[EndCCSDSTDMDataReps-EndCCSDSDataReps] =
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
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
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
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSTDMObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObtype::CCSDSTDMObtype() : CCSDSObtype(),
	ccsdsTDMMetaData(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSTDMObtype(const CCSDSTDMObtype &tdm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObtype::CCSDSTDMObtype(const CCSDSTDMObtype &tdm) : CCSDSObtype(tdm),
	ccsdsTDMMetaData(tdm.ccsdsTDMMetaData)
{
}

//---------------------------------------------------------------------------
//  CCSDSTDMObtype& operator=(const CCSDSTDMObtype &tdm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Obtype structures.
 *
 * @param <tdm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSTDMObtype& CCSDSTDMObtype::operator=(const CCSDSTDMObtype &tdm)
{
   if (&tdm == this)
      return *this;

    ccsdsTDMMetaData = tdm.ccsdsTDMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSTDMObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObtype::~CCSDSTDMObtype()
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
std::string CCSDSTDMObtype::GetDataParameterText(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return CCSDSObtype::GetDataParameterText(id);
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSTDMObtype::GetDataUnits(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return CCSDSObtype::GetDataUnits(id);
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer CCSDSTDMObtype::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";
    
    for (Integer i = EndCCSDSDataReps; i < EndCCSDSTDMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }
      
   return CCSDSObtype::GetDataParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSTDMObtype::GetDataParameterType(const Integer id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return CCSDSObtype::GetDataParameterType(id);
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see Obtype
 */
//---------------------------------------------------------------------------
std::string CCSDSTDMObtype::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObtype::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//---------------------------------------------------------------------------
Integer CCSDSTDMObtype::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

	    return ccsdsTDMMetaData->turnaroundNumerator;

	case CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

	    return ccsdsTDMMetaData->turnaroundDenominator;

	case CCSDS_TDM_KEYWORD_ID:

            return ccsdsData->keywordID;

        default:

            return CCSDSObtype::GetIntegerDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer CCSDSTDMObtype::GetIntegerDataParameter(const std::string &label) const
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
bool CCSDSTDMObtype::GetBoolDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_TDM_CORRECTIONAPPLIED_ID:
	    
	    return ccsdsTDMMetaData->correctionsApplied;
	    
	default:
	    
	    return CCSDSObtype::GetBoolDataParameter(id);
	    
    }
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
bool CCSDSTDMObtype::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSTDMObtype::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_TIMESYSTEM_ID:

            return ccsdsTDMMetaData->timeSystem;
	    
	case CCSDS_TDM_STARTTIME_ID:

            return ccsdsTDMMetaData->startTime;
	    
	case CCSDS_TDM_STOPTIME_ID:

            return ccsdsTDMMetaData->stopTime;
	    
	case CCSDS_TDM_PARTICIPANT1_ID:

            return ccsdsTDMMetaData->participants[0];

	case CCSDS_TDM_PARTICIPANT2_ID:

            return ccsdsTDMMetaData->participants[1];
	    
	case CCSDS_TDM_PARTICIPANT3_ID:

            return ccsdsTDMMetaData->participants[2];

	case CCSDS_TDM_PARTICIPANT4_ID:

            return ccsdsTDMMetaData->participants[3];

	case CCSDS_TDM_PARTICIPANT5_ID:

            return ccsdsTDMMetaData->participants[4];

	case CCSDS_TDM_MODE_ID:

            return ccsdsTDMMetaData->mode;

	case CCSDS_TDM_PATH_ID:

            return ccsdsTDMMetaData->path[0];

	case CCSDS_TDM_PATH1_ID:

            return ccsdsTDMMetaData->path[1];

	case CCSDS_TDM_PATH2_ID:

            return ccsdsTDMMetaData->path[2];

	case CCSDS_TDM_TRANSMITBAND_ID:

            return ccsdsTDMMetaData->transmitBand;

	case CCSDS_TDM_RECEIVEBAND_ID:

            return ccsdsTDMMetaData->receiveBand;

	case CCSDS_TDM_TIMETAGREF_ID:

            return ccsdsTDMMetaData->timeTagRef;

	case CCSDS_TDM_INTEGRATIONREF_ID:

            return ccsdsTDMMetaData->integrationRef;

	case CCSDS_TDM_RANGEMODE_ID:

            return ccsdsTDMMetaData->rangeMode;

	case CCSDS_TDM_RANGEUNITS_ID:

            return ccsdsTDMMetaData->rangeUnits;

	case CCSDS_TDM_ANGLETYPE_ID:	

            return ccsdsTDMMetaData->angleType;

	case CCSDS_TDM_REFERENCEFRAME_ID:

            return ccsdsTDMMetaData->referenceFrame;

	case CCSDS_TDM_DATAQUALITY_ID:	

            return ccsdsTDMMetaData->dataQuality;
	    
	case CCSDS_TDM_TIMETAG_ID:
	    
            return ccsdsData->timeTag;
		
        default:

            return CCSDSObtype::GetStringDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string CCSDSTDMObtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real CCSDSTDMObtype::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_INTEGRATIONINTERVAL_ID:

	    return ccsdsTDMMetaData->integrationInterval;

	case CCSDS_TDM_FREQUENCYOFFSET_ID:

	    return ccsdsTDMMetaData->frequencyOffset;

	case CCSDS_TDM_RANGEMODULUS_ID:

	    return ccsdsTDMMetaData->rangeModulus;
	
	case CCSDS_TDM_TRANSMITDELAY1_ID:
	    
	    return ccsdsTDMMetaData->transmitDelay[0];
	
	case CCSDS_TDM_TRANSMITDELAY2_ID:
	    
	    return ccsdsTDMMetaData->transmitDelay[1];
	
	case CCSDS_TDM_TRANSMITDELAY3_ID:
	    
	    return ccsdsTDMMetaData->transmitDelay[2];
	
	case CCSDS_TDM_TRANSMITDELAY4_ID:
	    
	    return ccsdsTDMMetaData->transmitDelay[3];
	
	case CCSDS_TDM_TRANSMITDELAY5_ID:
	    
	    return ccsdsTDMMetaData->transmitDelay[4];
	
	case CCSDS_TDM_RECEIVEDELAY1_ID:
	    
	    return ccsdsTDMMetaData->receiveDelay[0];
	
	case CCSDS_TDM_RECEIVEDELAY2_ID:
	    
	    return ccsdsTDMMetaData->receiveDelay[1];
	
	case CCSDS_TDM_RECEIVEDELAY3_ID:
	    
	    return ccsdsTDMMetaData->receiveDelay[2];
	
	case CCSDS_TDM_RECEIVEDELAY4_ID:
	    
	    return ccsdsTDMMetaData->receiveDelay[3];
	
	case CCSDS_TDM_RECEIVEDELAY5_ID:
	    
	    return ccsdsTDMMetaData->receiveDelay[4];
	
	case CCSDS_TDM_CORRECTIONANGLE1_ID:
	    
	    return ccsdsTDMMetaData->correctionAngle1;
	
	case CCSDS_TDM_CORRECTIONANGLE2_ID:
	    
	    return ccsdsTDMMetaData->correctionAngle2;
	
	case CCSDS_TDM_CORRECTIONDOPPLER_ID:
	    
	    return ccsdsTDMMetaData->correctionDoppler;
	
	case CCSDS_TDM_CORRECTIONRANGE_ID:
	    
	    return ccsdsTDMMetaData->correctionRange;
	
	case CCSDS_TDM_CORRECTIONRECEIVE_ID:
	    
	    return ccsdsTDMMetaData->correctionReceive;
	
	case CCSDS_TDM_CORRECTIONTRANSMIT_ID:	    
	    
	    return ccsdsTDMMetaData->correctionTransmit;
	
	case CCSDS_TDM_MEASUREMENT_ID:
	    
	    return ccsdsData->measurement;
	
	default:

	    return CCSDSObtype::GetRealDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real CCSDSTDMObtype::GetRealDataParameter(const std::string &label) const
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
const std::string* CCSDSTDMObtype::GetDataTypes() const
{
   return CCSDS_DATATYPE_DESCRIPTIONS;
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
const std::string* CCSDSTDMObtype::GetKeywords() const
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
const Integer CCSDSTDMObtype::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSTDMDataReps; i++)
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
std::string CCSDSTDMObtype::GetUnits(const Integer &id) const
{
   return CCSDS_UNIT_DESCRIPTIONS[id];
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
std::string CCSDSTDMObtype::GetDataTypeText(const Integer &id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMTypeReps))
   {
      return CCSDS_DATATYPE_DESCRIPTIONS[id];
   }

   return CCSDSObtype::GetDataTypeText(id);
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
Integer CCSDSTDMObtype::GetDataTypeID(const std::string &label)
{
    return CCSDSObtype::GetDataTypeID(label);
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
const std::string* CCSDSTDMObtype::GetTimeSystems() const
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
std::string CCSDSTDMObtype::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSTDMTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return CCSDSObtype::GetTimeSystemText(id);
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
Integer CCSDSTDMObtype::GetTimeSystemID(const std::string &label)
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
      
    return CCSDSObtype::GetTimeSystemID(label);
 
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
bool CCSDSTDMObtype::IsParameterRequired(const Integer id) const
{
if (id > EndCCSDSDataReps && id <= EndCCSDSTDMDataReps)
    return CCSDS_IS_REQUIRED[id];
else
    return CCSDSObtype::IsParameterRequired(id);
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
bool CCSDSTDMObtype::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSTDMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return CCSDSObtype::CheckDataAvailability(str);

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSTDMObtype *myTDM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSTDMObtype value and sends to output stream.
 *
 * @param  output  Output stream
 * @param  myTDM    CCSDS TDM observation to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSTDMObtype *myTDM)
{
   using namespace std;

   output.setf(std::ios::showpoint);
   output.setf(std::ios::scientific);

   output << (CCSDSObtype*)myTDM << std::endl;
   for (Integer i = 0; i < myTDM->ccsdsTDMMetaData->metadataComments.size(); i++ )
   {
       output << "Metadata Comments = " << myTDM->ccsdsTDMMetaData->metadataComments[i] << std::endl;
   }
   output << "Time System = " << myTDM->ccsdsTDMMetaData->timeSystem << std::endl;
   output << "Start Time = " << myTDM->ccsdsTDMMetaData->startTime << std::endl;
   output << "Stop Time = " << myTDM->ccsdsTDMMetaData->stopTime << std::endl;
   output << "Participant 1 = " << myTDM->ccsdsTDMMetaData->participants[0] << std::endl;
   output << "Participant 2 = " << myTDM->ccsdsTDMMetaData->participants[1] << std::endl;
   output << "Participant 3 = " << myTDM->ccsdsTDMMetaData->participants[2] << std::endl;
   output << "Participant 4 = " << myTDM->ccsdsTDMMetaData->participants[3] << std::endl;
   output << "Participant 5 = " << myTDM->ccsdsTDMMetaData->participants[4] << std::endl;
   output << "Mode = " << myTDM->ccsdsTDMMetaData->mode << std::endl;
   output << "Path = " << myTDM->ccsdsTDMMetaData->path[0] << std::endl;
   output << "Path 1 = " << myTDM->ccsdsTDMMetaData->path[1] << std::endl;
   output << "Path 2 = " << myTDM->ccsdsTDMMetaData->path[2] << std::endl;
   output << "Transmit Band = " << myTDM->ccsdsTDMMetaData->transmitBand << std::endl;
   output << "Receive Band = " << myTDM->ccsdsTDMMetaData->receiveBand << std::endl;
   output << "Turnaround Numerator = " << myTDM->ccsdsTDMMetaData->turnaroundNumerator << std::endl;
   output << "Turnaround Denominator = " << myTDM->ccsdsTDMMetaData->turnaroundDenominator << std::endl;
   output << "Time Tag Ref = " << myTDM->ccsdsTDMMetaData->timeTagRef << std::endl;
   output << "Integration Interval = " << myTDM->ccsdsTDMMetaData->integrationInterval << std::endl;
   output << "Integration Ref = " << myTDM->ccsdsTDMMetaData->integrationRef << std::endl;
   output << "Frequency Offset = " << myTDM->ccsdsTDMMetaData->frequencyOffset << std::endl;
   output << "Range Mode = " << myTDM->ccsdsTDMMetaData->rangeMode << std::endl;
   output << "Range Modulus = " << myTDM->ccsdsTDMMetaData->rangeModulus << std::endl;
   output << "Range Units = " << myTDM->ccsdsTDMMetaData->rangeUnits << std::endl;
   output << "Angle Type = " << myTDM->ccsdsTDMMetaData->angleType << std::endl;
   output << "Transmit Delay 1 = " << myTDM->ccsdsTDMMetaData->transmitDelay[0] << std::endl;
   output << "Transmit Delay 2 = " << myTDM->ccsdsTDMMetaData->transmitDelay[1] << std::endl;
   output << "Transmit Delay 3 = " << myTDM->ccsdsTDMMetaData->transmitDelay[2] << std::endl;
   output << "Transmit Delay 4 = " << myTDM->ccsdsTDMMetaData->transmitDelay[3] << std::endl;
   output << "Transmit Delay 5 = " << myTDM->ccsdsTDMMetaData->transmitDelay[4] << std::endl;
   output << "Receive Delay 1 = " << myTDM->ccsdsTDMMetaData->receiveDelay[0] << std::endl;
   output << "Receive Delay 2 = " << myTDM->ccsdsTDMMetaData->receiveDelay[1] << std::endl;
   output << "Receive Delay 3 = " << myTDM->ccsdsTDMMetaData->receiveDelay[2] << std::endl;
   output << "Receive Delay 4 = " << myTDM->ccsdsTDMMetaData->receiveDelay[3] << std::endl;
   output << "Receive Delay 5 = " << myTDM->ccsdsTDMMetaData->receiveDelay[4] << std::endl;
   output << "Data Quality = " << myTDM->ccsdsTDMMetaData->dataQuality << std::endl;
   output << "Correction Angle 1 = " << myTDM->ccsdsTDMMetaData->correctionAngle1 << std::endl;
   output << "Correction Angle 2 = " << myTDM->ccsdsTDMMetaData->correctionAngle2 << std::endl;
   output << "Correction Doppler = " << myTDM->ccsdsTDMMetaData->correctionDoppler << std::endl;
   output << "Correction Range = " << myTDM->ccsdsTDMMetaData->correctionRange << std::endl;
   output << "Correction Receive = " << myTDM->ccsdsTDMMetaData->correctionReceive << std::endl;
   output << "Correction Transmit = " << myTDM->ccsdsTDMMetaData->correctionTransmit << std::endl;
   output << "Corrections Applied = " << myTDM->ccsdsTDMMetaData->correctionsApplied << std::endl;
   output << "******************************************************" << std::endl;

   return output;
}
