#include "CCSDSTDMMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSTDMMetaData::CCSDS_TDM_METADATA_KEYWORDS[EndCCSDSTDMMetaDataReps] =
{
    "COMMENT",
    "TIME_SYSTEM",
    "START_TIME",
    "STOP_TIME",
    "PARTICIPANT_1",
    "PARTICIPANT_2",
    "PARTICIPANT_3",
    "PARTICIPANT_4",
    "PARTICIPANT_5",
    "MODE",
    "PATH",
    "PATH_1",
    "PATH_2",
    "TRANSMIT_BAND",
    "RECEIVE_BANE",
    "TURNAROUND_NUMERATOR",
    "TURNAROUND_DENOMINATOR",
    "TIMETAG_REF",
    "INTEGRATION_INTERVAL",
    "INTEGRATION_REF",
    "FREQ_OFFSET",
    "RANGE_MODE",
    "RANGE_MODULUS",
    "RANGE_UNITS",
    "ANGLE_TYPE",
    "REFERENCE_FRAME",
    "TRANSMIT_DELAY_1",
    "TRANSMIT_DELAY_2",
    "TRANSMIT_DELAY_3",
    "TRANSMIT_DELAY_4",
    "TRANSMIT_DELAY_5",
    "RECEIVE_DELAY_1",
    "RECEIVE_DELAY_2",
    "RECEIVE_DELAY_3",
    "RECEIVE_DELAY_4",
    "RECEIVE_DELAY_5",
    "DATA_QUALITY",
    "CORRECTION_ANGLE_1",
    "CORRECTION_ANGLE_2",
    "CORRECTION_DOPPLER",
    "CORRECTION_RANGE",
    "CORRECTION_RECEIVE",
    "CORRECTION_TRANSMIT",
    "CORRECTION_APPLIED"
};

const std::string CCSDSTDMMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMMetaDataReps] =
{
    "comments",
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
    "CorrectionApplied"
};

const std::string CCSDSTDMMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSTDMMetaDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "Hz",
    "Hz",
    "",
    "",
    "",
    "s",
    "",
    "Hz",
    "",
    "",
    "",
    "",
    "",
    "s",
    "s",
    "s",
    "s",
    "s",
    "s",
    "s",
    "s",
    "s",
    "s",
    "",
    "deg",
    "deg",
    "",
    "",
    "s",
    "s",
    ""
};


const bool CCSDSTDMMetaData::CCSDS_METADATA_IS_REQUIRED[EndCCSDSTDMMetaDataReps] =
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
    false
};

const Gmat::ParameterType CCSDSTDMMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSTDMMetaDataReps] =
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
    Gmat::BOOLEAN_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSTDMMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMMetaData::CCSDSTDMMetaData() : CCSDSMetaData(),
    comments(0),
    timeSystem(std::string("")),
    startTime(std::string("")),
    stopTime(std::string("")),
    mode(std::string("")),
    transmitBand(std::string("")),
    receiveBand(std::string("")),
    turnaroundNumerator(0),
    turnaroundDenominator(0),
    timeTagRef(std::string("")),
    integrationInterval(0),
    integrationRef(std::string("")),
    frequencyOffset(0),
    rangeMode(std::string("")),
    rangeModulus(0),
    rangeUnits(std::string("")),
    angleType(std::string("")),
    referenceFrame(std::string("")),
    dataQuality(std::string("")),
    correctionAngle1(0),
    correctionAngle2(0),
    correctionDoppler(0),
    correctionRange(0),
    correctionReceive(0),
    correctionTransmit(0),
    correctionsApplied(false)
{
    transmitDelay[0] = 0;
    transmitDelay[1] = 0;
    transmitDelay[2] = 0;
    transmitDelay[3] = 0;
    transmitDelay[4] = 0;
    receiveDelay[0] = 0;
    receiveDelay[1] = 0;
    receiveDelay[2] = 0;
    receiveDelay[3] = 0;
    receiveDelay[4] = 0;
    participants[0] = std::string("");
    participants[1] = std::string("");
    participants[2] = std::string("");
    participants[3] = std::string("");
    participants[4] = std::string("");
    path[0] = std::string("");
    path[1] = std::string("");
    path[2] = std::string("");
}

//------------------------------------------------------------------------------
//  CCSDSTDMMetaData(const CCSDSTDMMetaData &tdmd)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMMetaData::CCSDSTDMMetaData(const CCSDSTDMMetaData &tdmd) :
    CCSDSMetaData(tdmd),
    comments(tdmd.comments),
    timeSystem(tdmd.timeSystem),
    startTime(tdmd.startTime),
    stopTime(tdmd.stopTime),
    mode(tdmd.mode),
    transmitBand(tdmd.transmitBand),
    receiveBand(tdmd.receiveBand),
    turnaroundNumerator(tdmd.turnaroundNumerator),
    turnaroundDenominator(tdmd.turnaroundDenominator),
    timeTagRef(tdmd.timeTagRef),
    integrationInterval(tdmd.integrationInterval),
    integrationRef(tdmd.integrationRef),
    frequencyOffset(tdmd.frequencyOffset),
    rangeMode(tdmd.rangeMode),
    rangeModulus(tdmd.rangeModulus),
    rangeUnits(tdmd.rangeUnits),
    angleType(tdmd.angleType),
    referenceFrame(tdmd.referenceFrame),
    dataQuality(tdmd.dataQuality),
    correctionAngle1(tdmd.correctionAngle1),
    correctionAngle2(tdmd.correctionAngle2),
    correctionDoppler(tdmd.correctionDoppler),
    correctionRange(tdmd.correctionRange),
    correctionReceive(tdmd.correctionReceive),
    correctionTransmit(tdmd.correctionTransmit),
    correctionsApplied(tdmd.correctionsApplied)
{
    participants[0] = tdmd.participants[0];
    participants[1] = tdmd.participants[1];
    participants[2] = tdmd.participants[2];
    participants[3] = tdmd.participants[3];
    participants[4] = tdmd.participants[4];
    path[0] = tdmd.path[0];
    path[1] = tdmd.path[1];
    path[2] = tdmd.path[2];
    transmitDelay[0] = tdmd.transmitDelay[0];
    transmitDelay[1] = tdmd.transmitDelay[1];
    transmitDelay[2] = tdmd.transmitDelay[2];
    transmitDelay[3] = tdmd.transmitDelay[3];
    transmitDelay[4] = tdmd.transmitDelay[4];
    receiveDelay[0] = tdmd.receiveDelay[0];
    receiveDelay[1] = tdmd.receiveDelay[1];
    receiveDelay[2] = tdmd.receiveDelay[2];
    receiveDelay[3] = tdmd.receiveDelay[3];
    receiveDelay[4] = tdmd.receiveDelay[4];
}

//---------------------------------------------------------------------------
//  CCSDSTDMMetaData& operator=(const CCSDSTDMMetaData &tdm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tdmd> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSTDMMetaData& CCSDSTDMMetaData::operator=(const CCSDSTDMMetaData &tdmd)
{
    if (&tdmd == this)
        return *this;

    CCSDSMetaData::operator=(tdmd);

    comments = tdmd.comments;
    timeSystem = tdmd.timeSystem;
    startTime = tdmd.startTime;
    stopTime = tdmd.stopTime;
    participants[0] = tdmd.participants[0];
    participants[1] = tdmd.participants[1];
    participants[2] = tdmd.participants[2];
    participants[3] = tdmd.participants[3];
    participants[4] = tdmd.participants[4];
    mode = tdmd.mode;
    path[0] = tdmd.path[0];
    path[1] = tdmd.path[1];
    path[2] = tdmd.path[2];
    transmitBand = tdmd.transmitBand;
    receiveBand = tdmd.receiveBand;
    turnaroundNumerator = tdmd.turnaroundNumerator;
    turnaroundDenominator = tdmd.turnaroundDenominator;
    timeTagRef = tdmd.timeTagRef;
    integrationInterval = tdmd.integrationInterval;
    integrationRef = tdmd.integrationRef;
    frequencyOffset = tdmd.frequencyOffset;
    rangeMode = tdmd.rangeMode;
    rangeModulus = tdmd.rangeModulus;
    rangeUnits = tdmd.rangeUnits;
    angleType = tdmd.angleType;
    referenceFrame = tdmd.referenceFrame;
    transmitDelay[0] = tdmd.transmitDelay[0];
    transmitDelay[1] = tdmd.transmitDelay[1];
    transmitDelay[2] = tdmd.transmitDelay[2];
    transmitDelay[3] = tdmd.transmitDelay[3];
    transmitDelay[4] = tdmd.transmitDelay[4];
    receiveDelay[0] = tdmd.receiveDelay[0];
    receiveDelay[1] = tdmd.receiveDelay[1];
    receiveDelay[2] = tdmd.receiveDelay[2];
    receiveDelay[3] = tdmd.receiveDelay[3];
    receiveDelay[4] = tdmd.receiveDelay[4];
    dataQuality = tdmd.dataQuality;
    correctionAngle1 = tdmd.correctionAngle1;
    correctionAngle2 = tdmd.correctionAngle2;
    correctionDoppler = tdmd.correctionDoppler;
    correctionRange = tdmd.correctionRange;
    correctionReceive = tdmd.correctionReceive;
    correctionTransmit = tdmd.correctionTransmit;
    correctionsApplied = tdmd.correctionsApplied;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSTDMMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSTDMMetaData::~CCSDSTDMMetaData()
{
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
const std::string* CCSDSTDMMetaData::GetKeywords() const
{
   return CCSDS_TDM_METADATA_KEYWORDS;
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
const Integer CCSDSTDMMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_TDM_METADATA_KEYWORDS[i]))
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
std::string CCSDSTDMMetaData::GetUnits(const Integer &id) const
{
   return CCSDS_METADATA_UNIT_DESCRIPTIONS[id];
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
bool CCSDSTDMMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSTDMMetaDataReps)
        return CCSDS_METADATA_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool CountRequiredNumberTDMMetaDataParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberTDMMetaDataParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSTDMMetaData::EndCCSDSTDMMetaDataReps; id++)
        if (CCSDSTDMMetaData::CCSDS_METADATA_IS_REQUIRED[id])
            num++;

    return num;
}

//------------------------------------------------------------------------------
//  bool CheckMetaDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool CCSDSTDMMetaData::CheckMetaDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

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
std::string CCSDSTDMMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMMetaDataReps))
   {
      return CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[id];
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
Integer CCSDSTDMMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMMetaDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[i]))
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
Gmat::ParameterType CCSDSTDMMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMMetaDataReps))
      return CCSDS_METADATA_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSTDMMetaData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSTDMMetaData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

	    return turnaroundNumerator;

	case CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

	    return turnaroundDenominator;

        default:

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSTDMMetaData::GetIntegerDataParameter(const std::string &label) const
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
bool CCSDSTDMMetaData::GetBoolDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_TDM_CORRECTIONAPPLIED_ID:

	    return correctionsApplied;

	default:

	    return false;

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
bool CCSDSTDMMetaData::GetBoolDataParameter(const std::string &label) const
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
std::string CCSDSTDMMetaData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_TIMESYSTEM_ID:

            return timeSystem;

	case CCSDS_TDM_STARTTIME_ID:

            return startTime;

	case CCSDS_TDM_STOPTIME_ID:

            return stopTime;

	case CCSDS_TDM_PARTICIPANT1_ID:

            return participants[0];

	case CCSDS_TDM_PARTICIPANT2_ID:

            return participants[1];

	case CCSDS_TDM_PARTICIPANT3_ID:

            return participants[2];

	case CCSDS_TDM_PARTICIPANT4_ID:

            return participants[3];

	case CCSDS_TDM_PARTICIPANT5_ID:

            return participants[4];

	case CCSDS_TDM_MODE_ID:

            return mode;

	case CCSDS_TDM_PATH_ID:

            return path[0];

	case CCSDS_TDM_PATH1_ID:

            return path[1];

	case CCSDS_TDM_PATH2_ID:

            return path[2];

	case CCSDS_TDM_TRANSMITBAND_ID:

            return transmitBand;

	case CCSDS_TDM_RECEIVEBAND_ID:

            return receiveBand;

	case CCSDS_TDM_TIMETAGREF_ID:

            return timeTagRef;

	case CCSDS_TDM_INTEGRATIONREF_ID:

            return integrationRef;

	case CCSDS_TDM_RANGEMODE_ID:

            return rangeMode;

	case CCSDS_TDM_RANGEUNITS_ID:

            return rangeUnits;

	case CCSDS_TDM_ANGLETYPE_ID:

            return angleType;

	case CCSDS_TDM_REFERENCEFRAME_ID:

            return referenceFrame;

	case CCSDS_TDM_DATAQUALITY_ID:

            return dataQuality;

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
std::string CCSDSTDMMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSTDMMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_TDM_METADATACOMMENTS_ID:

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
StringArray CCSDSTDMMetaData::GetStringArrayDataParameter(const std::string &label) const
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
Real CCSDSTDMMetaData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_INTEGRATIONINTERVAL_ID:

	    return integrationInterval;

	case CCSDS_TDM_FREQUENCYOFFSET_ID:

	    return frequencyOffset;

	case CCSDS_TDM_RANGEMODULUS_ID:

	    return rangeModulus;

	case CCSDS_TDM_TRANSMITDELAY1_ID:

	    return transmitDelay[0];

	case CCSDS_TDM_TRANSMITDELAY2_ID:

	    return transmitDelay[1];

	case CCSDS_TDM_TRANSMITDELAY3_ID:

	    return transmitDelay[2];

	case CCSDS_TDM_TRANSMITDELAY4_ID:

	    return transmitDelay[3];

	case CCSDS_TDM_TRANSMITDELAY5_ID:

	    return transmitDelay[4];

	case CCSDS_TDM_RECEIVEDELAY1_ID:

	    return receiveDelay[0];

	case CCSDS_TDM_RECEIVEDELAY2_ID:

	    return receiveDelay[1];

	case CCSDS_TDM_RECEIVEDELAY3_ID:

	    return receiveDelay[2];

	case CCSDS_TDM_RECEIVEDELAY4_ID:

	    return receiveDelay[3];

	case CCSDS_TDM_RECEIVEDELAY5_ID:

	    return receiveDelay[4];

	case CCSDS_TDM_CORRECTIONANGLE1_ID:

	    return correctionAngle1;

	case CCSDS_TDM_CORRECTIONANGLE2_ID:

	    return correctionAngle2;

	case CCSDS_TDM_CORRECTIONDOPPLER_ID:

	    return correctionDoppler;

	case CCSDS_TDM_CORRECTIONRANGE_ID:

	    return correctionRange;

	case CCSDS_TDM_CORRECTIONRECEIVE_ID:

	    return correctionReceive;

	case CCSDS_TDM_CORRECTIONTRANSMIT_ID:

	    return correctionTransmit;

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
Real CCSDSTDMMetaData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
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
bool CCSDSTDMMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSTDMMetaDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::BOOLEAN_TYPE:
                    {
                    bool bvalue = GetBooleanDataParameter(i);
                    if (&bvalue == NULL)
                        return false;
                    }
                    break;
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSTDMMetaData *myMetadata)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSTDMMetaData value and sends to output stream.
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

   unsigned int i;
   for (i = 0; i < myMetadata->comments.size(); i++ )
   {
       output << "COMMENT " << myMetadata->comments[i] << endl;
   }
   if (i > 0) output << endl;

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

   output << "META_STOP" << endl;
   output << endl;

   return output;
}

