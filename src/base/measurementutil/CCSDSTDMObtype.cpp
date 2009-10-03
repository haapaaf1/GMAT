#include "CCSDSTDMObType.hpp"

//---------------------------------
//  static data
//---------------------------------
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

const std::string CCSDSTDMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMDataReps-EndCCSDSDataReps] =
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

const bool CCSDSTDMObType::CCSDS_IS_REQUIRED[EndCCSDSTDMDataReps-EndCCSDSDataReps] =
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

const Gmat::ParameterType CCSDSTDMObType::CCSDS_PARAMETER_TYPE[EndCCSDSTDMDataReps-EndCCSDSDataReps] =
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
//  CCSDSTDMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMObType::CCSDSTDMObType() : CCSDSObType("CCSDSTDMObType", ""),
	ccsdsTDMMetaData(NULL)
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
	ccsdsTDMMetaData(tdm.ccsdsTDMMetaData)
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

    ccsdsTDMMetaData = tdm.ccsdsTDMMetaData;

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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMDataReps))
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMDataReps))
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
    
    for (Integer i = EndCCSDSDataReps; i < EndCCSDSTDMDataReps; i++)
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMDataReps))
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
	case CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

	    return ccsdsTDMMetaData->turnaroundNumerator;

	case CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

	    return ccsdsTDMMetaData->turnaroundDenominator;

	case CCSDS_TDM_KEYWORD_ID:

            return ccsdsData->keywordID;

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
bool CCSDSTDMObType::GetBoolDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_TDM_CORRECTIONAPPLIED_ID:
	    
	    return ccsdsTDMMetaData->correctionsApplied;
	    
	default:
	    
	    return CCSDSObType::GetBoolDataParameter(id);
	    
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
bool CCSDSTDMObType::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
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
// const std::string* GetDataTypes() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable data types.
 *
 * @return String array of all data types.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSTDMObType::GetDataTypes() const
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
std::string CCSDSTDMObType::GetUnits(const Integer &id) const
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
std::string CCSDSTDMObType::GetDataTypeText(const Integer &id) const
{
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSTDMTypeReps))
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
if (id > EndCCSDSDataReps && id <= EndCCSDSTDMDataReps)
    return CCSDS_IS_REQUIRED[id];
else
    return CCSDSObType::IsParameterRequired(id);
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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSTDMDataReps; i++)
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

   if (myTDM->ccsdsData != NULL)
   {
       output << myTDM->ccsdsData;
   }

   if (myTDM->ccsdsSpacecraftParameters != NULL)
   {
       output << myTDM->ccsdsSpacecraftParameters;
   }

   if (myTDM->ccsdsKeplerianElements != NULL)
   {
       output << myTDM->ccsdsKeplerianElements;
   }

   if (myTDM->ccsdsStateVector != NULL)
   {
       output << myTDM->ccsdsStateVector;
   }

   if (myTDM->ccsdsSpinStabilized != NULL)
   {
       output << myTDM->ccsdsSpinStabilized;
   }

   if (myTDM->ccsdsEulerAngle != NULL)
   {
       output << myTDM->ccsdsEulerAngle;
   }

   if (myTDM->ccsdsQuaternion != NULL)
   {
       output << myTDM->ccsdsQuaternion;
   }

   if (myTDM->ccsdsManeuver != NULL)
   {
       output << myTDM->ccsdsManeuver;
   }

   return output;

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSTDMMetaData *myMetadata)
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
std::ostream& operator<< (std::ostream &output,
                          const CCSDSTDMMetaData *myMetadata)
{

    using namespace std;

   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

    output << "META_START" << endl;
   for (Integer i = 0; i < myMetadata->metadataComments.size(); i++ )
   {
       output << "COMMENT " << myMetadata->metadataComments[i] << endl;
   }
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << endl;
   output << "START_TIME = " << myMetadata->startTime << endl;
   output << "STOP_TIME = " << myMetadata->stopTime << endl;
   output << "PARTICIPANT_1 = " << myMetadata->participants[0] << endl;
   output << "PARTICIPANT_2 = " << myMetadata->participants[1] << endl;
   output << "PARTICIPANT_3 = " << myMetadata->participants[2] << endl;
   output << "PARTICIPANT_4 = " << myMetadata->participants[3] << endl;
   output << "PARTICIPANT_5 = " << myMetadata->participants[4] << endl;
   output << "MODE = " << myMetadata->mode << endl;
   output << "PATH = " << myMetadata->path[0] << endl;
   output << "PATH_1 = " << myMetadata->path[1] << endl;
   output << "PATH_2 = " << myMetadata->path[2] << endl;
   output << "TRANSMIT_BAND = " << myMetadata->transmitBand << endl;
   output << "RECEIVE_BAND = " << myMetadata->receiveBand << endl;
   output << "TURNAROUND_NUMERATOR = " << myMetadata->turnaroundNumerator << endl;
   output << "TURNAROUND_DENOMINATOR = " << myMetadata->turnaroundDenominator << endl;
   output << "TIMETAG_REF = " << myMetadata->timeTagRef << endl;
   output << "INTEGRATION_INTERVAL = " << myMetadata->integrationInterval << endl;
   output << "INTEGRATION_REF = " << myMetadata->integrationRef << endl;
   output << "FREQ_OFFSET = " << myMetadata->frequencyOffset << endl;
   output << "RANGE_MODE = " << myMetadata->rangeMode << endl;
   output << "RANGE_MODULUS = " << myMetadata->rangeModulus << endl;
   output << "RANGE_UNITS = " << myMetadata->rangeUnits << endl;
   output << "ANGLE_TYPE = " << myMetadata->angleType << endl;
   output << "REFERENCE_FRAME = " << myMetadata->referenceFrame << endl;
   output << "TRANSMIT_DELAY_1 = " << myMetadata->transmitDelay[0] << endl;
   output << "TRANSMIT_DELAY_2 = " << myMetadata->transmitDelay[1] << endl;
   output << "TRANSMIT_DELAY_3 = " << myMetadata->transmitDelay[2] << endl;
   output << "TRANSMIT_DELAY_4 = " << myMetadata->transmitDelay[3] << endl;
   output << "TRANSMIT_DELAY_5 = " << myMetadata->transmitDelay[4] << endl;
   output << "RECEIVE_DELAY_1 = " << myMetadata->receiveDelay[0] << endl;
   output << "RECEIVE_DELAY_2 = " << myMetadata->receiveDelay[1] << endl;
   output << "RECEIVE_DELAY_3 = " << myMetadata->receiveDelay[2] << endl;
   output << "RECEIVE_DELAY_4 = " << myMetadata->receiveDelay[3] << endl;
   output << "RECEIVE_DELAY_5 = " << myMetadata->receiveDelay[4] << endl;
   output << "DATA_QUALITY = " << myMetadata->dataQuality << endl;
   output << "CORRECTION_ANGLE_1 = " << myMetadata->correctionAngle1 << endl;
   output << "CORRECTION_ANGLE_2 = " << myMetadata->correctionAngle2 << endl;
   output << "CORRECTION_DOPPLER = " << myMetadata->correctionDoppler << endl;
   output << "CORRECTION_RANGE = " << myMetadata->correctionRange << endl;
   output << "CORRECTION_RECEIVE = " << myMetadata->correctionReceive << endl;
   output << "CORRECTION_TRANSMIT = " << myMetadata->correctionTransmit << endl;
   output << "CORRECTIONS_APPLIED = " << myMetadata->correctionsApplied << endl;

   output << "META_STOP" << std::endl << endl;

   return output;
}
