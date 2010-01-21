//$Header$
//------------------------------------------------------------------------------
//                             TDMCCSDSMetaData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/20
//
/**
 *
 * This class specifies the CCSDS Tracking Data MetaData class.
 *
 */
//------------------------------------------------------------------------------

#include "TDMCCSDSMetaData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string TDMCCSDSMetaData::MODE_DESCRIPTIONS[EndCCSDSTDMModeReps] =
{
    "SEQUENTIAL",
    "SINGLE_DIFF"
};

const std::string TDMCCSDSMetaData::TIMETAG_DESCRIPTIONS[EndCCSDSTDMTimetagReps] =
{
    "TRANSMIT",
    "RECEIVE"
};

const std::string TDMCCSDSMetaData::INTEGRATION_DESCRIPTIONS[EndCCSDSTDMIntegrationReps] =
{
    "START",
    "MIDDLE",
    "END"
};

const std::string TDMCCSDSMetaData::RANGEMODE_DESCRIPTIONS[EndCCSDSTDMRangeModeReps] =
{
    "COHERENT",
    "CONSTANT",
    "ONE_WAY"
};

const std::string TDMCCSDSMetaData::RANGEUNIT_DESCRIPTIONS[EndCCSDSTDMRangeUnitReps] =
{
    "km",
    "s",
    "RU"
};

const std::string TDMCCSDSMetaData::ANGLETYPE_DESCRIPTIONS[EndCCSDSTDMAngleTypeReps] =
{
    "AZEL",
    "RADEC",
    "XEYN",
    "XSYE"
};

const std::string TDMCCSDSMetaData::DATAQUALITY_DESCRIPTIONS[EndCCSDSTDMDataQualityReps] =
{
    "RAW",
    "VALIDATED",
    "DEGRADED"
};

const std::string TDMCCSDSMetaData::CCSDS_TDM_METADATA_KEYWORDS[EndTDMCCSDSMetaDataReps] =
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

const std::string TDMCCSDSMetaData::CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndTDMCCSDSMetaDataReps] =
{
    "Comments",
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

const std::string TDMCCSDSMetaData::CCSDS_METADATA_UNIT_DESCRIPTIONS[EndTDMCCSDSMetaDataReps] =
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


const bool TDMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[EndTDMCCSDSMetaDataReps] =
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

const Gmat::ParameterType TDMCCSDSMetaData::CCSDS_METADATA_PARAMETER_TYPE[EndTDMCCSDSMetaDataReps] =
{
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::BOOLEAN_TYPE
};

//------------------------------------------------------------------------------
//  TDMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
TDMCCSDSMetaData::TDMCCSDSMetaData() : CCSDSMetaData(),
    comments(),
    timeSystem(GmatBase::STRING_PARAMETER_UNDEFINED),
    startTime(GmatBase::STRING_PARAMETER_UNDEFINED),
    stopTime(GmatBase::STRING_PARAMETER_UNDEFINED),
    mode(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    transmitBand(GmatBase::STRING_PARAMETER_UNDEFINED),
    receiveBand(GmatBase::STRING_PARAMETER_UNDEFINED),
    turnaroundNumerator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    turnaroundDenominator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    timeTagRef(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    integrationInterval(GmatBase::REAL_PARAMETER_UNDEFINED),
    integrationRef(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    frequencyOffset(GmatBase::REAL_PARAMETER_UNDEFINED),
    rangeMode(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    rangeModulus(GmatBase::REAL_PARAMETER_UNDEFINED),
    rangeUnits(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    angleType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    referenceFrame(GmatBase::STRING_PARAMETER_UNDEFINED),
    dataQuality(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    correctionAngle1(GmatBase::REAL_PARAMETER_UNDEFINED),
    correctionAngle2(GmatBase::REAL_PARAMETER_UNDEFINED),
    correctionDoppler(GmatBase::REAL_PARAMETER_UNDEFINED),
    correctionRange(GmatBase::REAL_PARAMETER_UNDEFINED),
    correctionReceive(GmatBase::REAL_PARAMETER_UNDEFINED),
    correctionTransmit(GmatBase::REAL_PARAMETER_UNDEFINED),
    correctionsApplied(false)
{
    transmitDelay[0] = GmatBase::REAL_PARAMETER_UNDEFINED;
    transmitDelay[1] = GmatBase::REAL_PARAMETER_UNDEFINED;
    transmitDelay[2] = GmatBase::REAL_PARAMETER_UNDEFINED;
    transmitDelay[3] = GmatBase::REAL_PARAMETER_UNDEFINED;
    transmitDelay[4] = GmatBase::REAL_PARAMETER_UNDEFINED;
    receiveDelay[0] = GmatBase::REAL_PARAMETER_UNDEFINED;
    receiveDelay[1] = GmatBase::REAL_PARAMETER_UNDEFINED;
    receiveDelay[2] = GmatBase::REAL_PARAMETER_UNDEFINED;
    receiveDelay[3] = GmatBase::REAL_PARAMETER_UNDEFINED;
    receiveDelay[4] = GmatBase::REAL_PARAMETER_UNDEFINED;
    participants[0] = GmatBase::STRING_PARAMETER_UNDEFINED;
    participants[1] = GmatBase::STRING_PARAMETER_UNDEFINED;
    participants[2] = GmatBase::STRING_PARAMETER_UNDEFINED;
    participants[3] = GmatBase::STRING_PARAMETER_UNDEFINED;
    participants[4] = GmatBase::STRING_PARAMETER_UNDEFINED;
    path[0] = GmatBase::STRING_PARAMETER_UNDEFINED;
    path[1] = GmatBase::STRING_PARAMETER_UNDEFINED;
    path[2] = GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  TDMCCSDSMetaData(const TDMCCSDSMetaData &tdmd)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
TDMCCSDSMetaData::TDMCCSDSMetaData(const TDMCCSDSMetaData &tdmd) :
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
//  TDMCCSDSMetaData& operator=(const TDMCCSDSMetaData &tdm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tdmd> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const TDMCCSDSMetaData& TDMCCSDSMetaData::operator=(const TDMCCSDSMetaData &tdmd)
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
//  ~TDMCCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
TDMCCSDSMetaData::~TDMCCSDSMetaData()
{
}

//------------------------------------------------------------------------------
//  std::string GetModeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the tracking mode keyword for a specific ID
 *
 * @param <id> The tracking mode id
 * @return The tracking mode keyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetModeText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMModeReps))
   {
      return MODE_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetModeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with a tracking mode keyword
 *
 * @param <str> The tracking mode keyword
 * @return The tracking mode id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetModeID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMModeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(MODE_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetTimeTagText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the time tag keyword for a specific ID
 *
 * @param <id> The time tag id
 * @return The time tag keyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetTimeTagText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMTimetagReps))
   {
      return TIMETAG_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetTimeTagID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with a time tag keyword
 *
 * @param <str> The time tag keyword
 * @return The time tag id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetTimeTagID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMTimetagReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(TIMETAG_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetIntegrationText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the integration mode keyword for a specific ID
 *
 * @param <id> The integration mode id
 * @return The integration modekeyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetIntegrationText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMIntegrationReps))
   {
      return INTEGRATION_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetIntegrationID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an integration mode keyword
 *
 * @param <str> The integration mode keyword
 * @return The integration mode id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetIntegrationID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMIntegrationReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(INTEGRATION_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetRangeModeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the range mode keyword for a specific ID
 *
 * @param <id> The range mode id
 * @return The range mode keyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetRangeModeText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMRangeModeReps))
   {
      return RANGEMODE_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetRangeModeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with a range mode keyword
 *
 * @param <str> The range mode keyword
 * @return The range mode id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetRangeModeID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMRangeModeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(RANGEMODE_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetRangeUnitText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the range unit keyword for a specific ID
 *
 * @param <id> The range unit id
 * @return The range unit keyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetRangeUnitText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMRangeUnitReps))
   {
      return RANGEUNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetRangeUnitID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with a range unit keyword
 *
 * @param <str> The range unit keyword
 * @return The range unit id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetRangeUnitID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMRangeUnitReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(RANGEUNIT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetAngleTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the angle type keyword for a specific ID
 *
 * @param <id> The angle direction id
 * @return The angle direction keyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetAngleTypeText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMAngleTypeReps))
   {
      return ANGLETYPE_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetAngleTypeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an angle type keyword
 *
 * @param <str> The angle type keyword
 * @return The angle type id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetAngleTypeID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMAngleTypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(ANGLETYPE_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetDataQualityText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the data quality keyword for a specific ID
 *
 * @param <id> The data quality id
 * @return The data quality keyword
 *
 */
//------------------------------------------------------------------------------
std::string TDMCCSDSMetaData::GetDataQualityText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMDataQualityReps))
   {
      return DATAQUALITY_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetDataQualityID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with a data quality keyword
 *
 * @param <str> The data quality keyword
 * @return The data quality id
 *
 */
//------------------------------------------------------------------------------
Integer TDMCCSDSMetaData::GetDataQualityID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMDataQualityReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(DATAQUALITY_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
const std::string* TDMCCSDSMetaData::GetKeywords() const
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
const Integer TDMCCSDSMetaData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndTDMCCSDSMetaDataReps; i++)
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
std::string TDMCCSDSMetaData::GetUnits(const Integer &id) const
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
bool TDMCCSDSMetaData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndTDMCCSDSMetaDataReps)
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

    for (Integer id = 0; id < TDMCCSDSMetaData::EndTDMCCSDSMetaDataReps; id++)
        if (TDMCCSDSMetaData::CCSDS_METADATA_IS_REQUIRED[id])
            num++;

    return num;
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
std::string TDMCCSDSMetaData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndTDMCCSDSMetaDataReps))
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
Integer TDMCCSDSMetaData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndTDMCCSDSMetaDataReps; i++)
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
Gmat::ParameterType TDMCCSDSMetaData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndTDMCCSDSMetaDataReps))
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
std::string TDMCCSDSMetaData::GetDataParameterTypeString(const Integer id) const
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
Integer TDMCCSDSMetaData::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
	case TDMCCSDSMetaData::CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

	    return turnaroundNumerator;

	case TDMCCSDSMetaData::CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

	    return turnaroundDenominator;
            
	case TDMCCSDSMetaData::CCSDS_TDM_MODE_ID:

            return mode;

	case TDMCCSDSMetaData::CCSDS_TDM_TIMETAGREF_ID:

            return timeTagRef;

	case TDMCCSDSMetaData::CCSDS_TDM_INTEGRATIONREF_ID:

            return integrationRef;

	case TDMCCSDSMetaData::CCSDS_TDM_RANGEMODE_ID:

            return rangeMode;

	case TDMCCSDSMetaData::CCSDS_TDM_RANGEUNITS_ID:

            return rangeUnits;

	case TDMCCSDSMetaData::CCSDS_TDM_ANGLETYPE_ID:

            return angleType;

	case TDMCCSDSMetaData::CCSDS_TDM_DATAQUALITY_ID:

            return dataQuality;

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
Integer TDMCCSDSMetaData::GetIntegerDataParameter(const std::string &label) const
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
bool TDMCCSDSMetaData::GetBoolDataParameter(const Integer id) const
{
    switch (id)
    {
	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONAPPLIED_ID:

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
bool TDMCCSDSMetaData::GetBoolDataParameter(const std::string &label) const
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
std::string TDMCCSDSMetaData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case TDMCCSDSMetaData::CCSDS_TDM_TIMESYSTEM_ID:

            return timeSystem;

	case TDMCCSDSMetaData::CCSDS_TDM_STARTTIME_ID:

            return startTime;

	case TDMCCSDSMetaData::CCSDS_TDM_STOPTIME_ID:

            return stopTime;

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT1_ID:

            return participants[0];

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT2_ID:

            return participants[1];

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT3_ID:

            return participants[2];

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT4_ID:

            return participants[3];

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT5_ID:

            return participants[4];

	case TDMCCSDSMetaData::CCSDS_TDM_PATH_ID:

            return path[0];

	case TDMCCSDSMetaData::CCSDS_TDM_PATH1_ID:

            return path[1];

	case TDMCCSDSMetaData::CCSDS_TDM_PATH2_ID:

            return path[2];

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITBAND_ID:

            return transmitBand;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEBAND_ID:

            return receiveBand;

	case TDMCCSDSMetaData::CCSDS_TDM_REFERENCEFRAME_ID:

            return referenceFrame;

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
std::string TDMCCSDSMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray TDMCCSDSMetaData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case TDMCCSDSMetaData::CCSDS_TDM_METADATACOMMENTS_ID:

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
StringArray TDMCCSDSMetaData::GetStringArrayDataParameter(const std::string &label) const
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
Real TDMCCSDSMetaData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case TDMCCSDSMetaData::CCSDS_TDM_INTEGRATIONINTERVAL_ID:

	    return integrationInterval;

	case TDMCCSDSMetaData::CCSDS_TDM_FREQUENCYOFFSET_ID:

	    return frequencyOffset;

	case TDMCCSDSMetaData::CCSDS_TDM_RANGEMODULUS_ID:

	    return rangeModulus;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY1_ID:

	    return transmitDelay[0];

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY2_ID:

	    return transmitDelay[1];

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY3_ID:

	    return transmitDelay[2];

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY4_ID:

	    return transmitDelay[3];

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY5_ID:

	    return transmitDelay[4];

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY1_ID:

	    return receiveDelay[0];

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY2_ID:

	    return receiveDelay[1];

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY3_ID:

	    return receiveDelay[2];

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY4_ID:

	    return receiveDelay[3];

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY5_ID:

	    return receiveDelay[4];

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONANGLE1_ID:

	    return correctionAngle1;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONANGLE2_ID:

	    return correctionAngle2;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONDOPPLER_ID:

	    return correctionDoppler;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONRANGE_ID:

	    return correctionRange;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONRECEIVE_ID:

	    return correctionReceive;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONTRANSMIT_ID:

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
Real TDMCCSDSMetaData::GetRealDataParameter(const std::string &label) const
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
bool TDMCCSDSMetaData::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

	case TDMCCSDSMetaData::CCSDS_TDM_INTEGRATIONINTERVAL_ID:

	    integrationInterval = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_FREQUENCYOFFSET_ID:

	    frequencyOffset = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RANGEMODULUS_ID:

	    rangeModulus = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY1_ID:

	    transmitDelay[0] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY2_ID:

	    transmitDelay[1] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY3_ID:

	    transmitDelay[2] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY4_ID:

	    transmitDelay[3] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY5_ID:

	    transmitDelay[4] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY1_ID:

	    receiveDelay[0] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY2_ID:

	    receiveDelay[1] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY3_ID:

	    receiveDelay[2] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY4_ID:

	    receiveDelay[3] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY5_ID:

	    receiveDelay[4] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONANGLE1_ID:

	    correctionAngle1 = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONANGLE2_ID:

	    correctionAngle2 = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONDOPPLER_ID:

	    correctionDoppler = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONRANGE_ID:

	    correctionRange = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONRECEIVE_ID:

	    correctionReceive = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONTRANSMIT_ID:

	    correctionTransmit = value;
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
bool TDMCCSDSMetaData::SetDataParameter(const std::string &label, const Real &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Integer &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Integer parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool TDMCCSDSMetaData::SetDataParameter(const Integer id, const Integer &value)
{
    switch (id)
    {
	case TDMCCSDSMetaData::CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

	    turnaroundNumerator = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

	    turnaroundDenominator = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_MODE_ID:

            mode = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TIMETAGREF_ID:

            timeTagRef = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_INTEGRATIONREF_ID:

            integrationRef = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RANGEMODE_ID:

            rangeMode = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RANGEUNITS_ID:

            rangeUnits = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_ANGLETYPE_ID:

            angleType = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_DATAQUALITY_ID:

            dataQuality = value;
            return true;

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
bool TDMCCSDSMetaData::SetDataParameter(const std::string &label, const Integer &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const bool &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a boolean parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool TDMCCSDSMetaData::SetDataParameter(const Integer id, const bool &value)
{
    switch (id)
    {
	case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONAPPLIED_ID:

	    correctionsApplied = value;
            return true;

	default:

	    return false;
    }
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const bool &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a boolean parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool TDMCCSDSMetaData::SetDataParameter(const std::string &label, const bool &value)
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
bool TDMCCSDSMetaData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

	case TDMCCSDSMetaData::CCSDS_TDM_TIMESYSTEM_ID:

            timeSystem = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_STARTTIME_ID:

            startTime = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_STOPTIME_ID:

            stopTime = value;
            return true;

        case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT1_ID:

            participants[0] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT2_ID:

            participants[1] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT3_ID:

            participants[2] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT4_ID:

            participants[3] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT5_ID:

            participants[4] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PATH_ID:

            path[0] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PATH1_ID:

            path[1] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_PATH2_ID:

            path[2] = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITBAND_ID:

            transmitBand = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEBAND_ID:

            receiveBand = value;
            return true;

	case TDMCCSDSMetaData::CCSDS_TDM_REFERENCEFRAME_ID:

            referenceFrame = value;
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
bool TDMCCSDSMetaData::SetDataParameter(const std::string &label, const std::string &value)
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
bool TDMCCSDSMetaData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {
        case TDMCCSDSMetaData::CCSDS_TDM_METADATACOMMENTS_ID:

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
bool TDMCCSDSMetaData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
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
bool TDMCCSDSMetaData::Validate() const
{

    for (unsigned int i = 0; i < EndTDMCCSDSMetaDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::BOOLEAN_TYPE:
                    if (!IsParameterDefined(GetBooleanDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Boolean parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::INTEGER_TYPE:
                    if (!IsParameterDefined(GetIntegerDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Integer parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
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
//                           const TDMCCSDSMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Formats CTDMCCSDSMetaData value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetaData>    CCSDS TDM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const TDMCCSDSMetaData *myMetaData)
{

    if(!myMetaData->Validate()) return output;

    using namespace std;

   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

   output << "META_START" << endl;

   for (unsigned int i = 0; i < myMetaData->comments.size(); i++ )
       output << "COMMENT " << myMetaData->comments[i] << endl;

    for (unsigned int i = 0; i < TDMCCSDSMetaData::EndTDMCCSDSMetaDataReps; i++)
    {
        switch (i)
        {

            case TDMCCSDSMetaData::CCSDS_TDM_TIMESYSTEM_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->timeSystem);
                if (definedFlag)
                {
                    output << "TIME_SYSTEM = " << myMetaData->timeSystem;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_STARTTIME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->startTime);
                if (definedFlag)
                {
                    output << "START_TIME = " << myMetaData->startTime;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_STOPTIME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->stopTime);
                if (definedFlag)
                {
                    output << "STOP_TIME = " << myMetaData->stopTime;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT1_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->participants[0]);
                if (definedFlag)
                {
                    output << "PARTICIPANT_1 = " << myMetaData->participants[0];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT2_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->participants[1]);
                if (definedFlag)
                {
                    output << "PARTICIPANT_2 = " << myMetaData->participants[1];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT3_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->participants[2]);
                if (definedFlag)
                {
                    output << "PARTICIPANT_3 = " << myMetaData->participants[2];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT4_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->participants[3]);
                if (definedFlag)
                {
                    output << "PARTICIPANT_4 = " << myMetaData->participants[3];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PARTICIPANT5_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->participants[4]);
                if (definedFlag)
                {
                    output << "PARTICIPANT_5 = " << myMetaData->participants[4];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_MODE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->mode);
                if (definedFlag)
                {
                    output << "MODE = " << myMetaData->GetModeText(myMetaData->mode);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PATH_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->path[0]);
                if (definedFlag)
                {
                    output << "PATH = " << myMetaData->path[0];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PATH1_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->path[1]);
                if (definedFlag)
                {
                    output << "PATH_1 = " << myMetaData->path[1];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_PATH2_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->path[2]);
                if (definedFlag)
                {
                    output << "PATH_2 = " << myMetaData->path[2];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITBAND_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->transmitBand);
                if (definedFlag)
                {
                    output << "TRANSMIT_BAND = " << myMetaData->transmitBand;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEBAND_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->receiveBand);
                if (definedFlag)
                {
                    output << "RECEIVE_BAND = " << myMetaData->receiveBand;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TURNAROUNDNUMERATOR_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->turnaroundNumerator);
                if (definedFlag)
                {
                    output << "TURNAROUND_NUMERATOR = " << myMetaData->turnaroundNumerator;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->turnaroundDenominator);
                if (definedFlag)
                {
                    output << "TURNAROUND_DENOMINATOR = " << myMetaData->turnaroundDenominator;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TIMETAGREF_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->timeTagRef);
                if (definedFlag)
                {
                    output << "TIMETAG_REF = " << myMetaData->GetTimeTagText(myMetaData->timeTagRef);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_INTEGRATIONINTERVAL_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->integrationInterval);
                if (definedFlag)
                {
                    output << "INTEGRATION_INTERVAL = " << myMetaData->integrationInterval;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_INTEGRATIONREF_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->integrationRef);
                if (definedFlag)
                {
                    output << "INTEGRATION_REF = " << myMetaData->GetIntegrationText(myMetaData->integrationRef);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_FREQUENCYOFFSET_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->frequencyOffset);
                if (definedFlag)
                {
                    output << "FREQ_OFFSET = " << myMetaData->frequencyOffset;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RANGEMODE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->rangeMode);
                if (definedFlag)
                {
                    output << "RANGE_MODE = " << myMetaData->GetRangeModeText(myMetaData->rangeMode);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RANGEMODULUS_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->rangeModulus);
                if (definedFlag)
                {
                    output << "RANGE_MODULUS = " << myMetaData->rangeModulus;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RANGEUNITS_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->rangeUnits);
                if (definedFlag)
                {
                    output << "RANGE_UNITS = " << myMetaData->GetRangeUnitText(myMetaData->rangeUnits);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_ANGLETYPE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->angleType);
                if (definedFlag)
                {
                    output << "ANGLE_TYPE = " << myMetaData->GetAngleTypeText(myMetaData->angleType);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_REFERENCEFRAME_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->referenceFrame);
                if (definedFlag)
                {
                    output << "REFERENCE_FRAME = " << myMetaData->referenceFrame;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY1_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->transmitDelay[0]);
                if (definedFlag)
                {
                    output << "TRANSMIT_DELAY_1 = " << myMetaData->transmitDelay[0];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY2_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->transmitDelay[1]);
                if (definedFlag)
                {
                    output << "TRANSMIT_DELAY_2 = " << myMetaData->transmitDelay[1];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY3_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->transmitDelay[2]);
                if (definedFlag)
                {
                    output << "TRANSMIT_DELAY_3 = " << myMetaData->transmitDelay[2];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY4_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->transmitDelay[3]);
                if (definedFlag)
                {
                    output << "TRANSMIT_DELAY_4 = " << myMetaData->transmitDelay[3];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_TRANSMITDELAY5_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->transmitDelay[4]);
                if (definedFlag)
                {
                    output << "TRANSMIT_DELAY_5 = " << myMetaData->transmitDelay[4];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY1_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->receiveDelay[0]);
                if (definedFlag)
                {
                    output << "RECEIVE_DELAY_1 = " << myMetaData->receiveDelay[0];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY2_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->receiveDelay[1]);
                if (definedFlag)
                {
                    output << "RECEIVE_DELAY_2 = " << myMetaData->receiveDelay[1];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY3_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->receiveDelay[2]);
                if (definedFlag)
                {
                    output << "RECEIVE_DELAY_3 = " << myMetaData->receiveDelay[2];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY4_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->receiveDelay[3]);
                if (definedFlag)
                {
                    output << "RECEIVE_DELAY_4 = " << myMetaData->receiveDelay[3];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_RECEIVEDELAY5_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->receiveDelay[4]);
                if (definedFlag)
                {
                    output << "RECEIVE_DELAY_5 = " << myMetaData->receiveDelay[4];
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_DATAQUALITY_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->dataQuality);
                if (definedFlag)
                {
                    output << "DATA_QUALITY = " << myMetaData->GetDataQualityText(myMetaData->dataQuality);
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONANGLE1_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->correctionAngle1);
                if (definedFlag)
                {
                    output << "CORRECTION_ANGLE_1 = " << myMetaData->correctionAngle1;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONANGLE2_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->correctionAngle2);
                if (definedFlag)
                {
                    output << "CORRECTION_ANGLE_2 = " << myMetaData->correctionAngle2;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONDOPPLER_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->correctionDoppler);
                if (definedFlag)
                {
                    output << "CORRECTION_DOPPLER = " << myMetaData->correctionDoppler;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONRANGE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->correctionRange);
                if (definedFlag)
                {
                    output << "CORRECTION_RANGE = " << myMetaData->correctionRange;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONRECEIVE_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->correctionReceive);
                if (definedFlag)
                {
                    output << "CORRECTION_RECEIVE = " << myMetaData->correctionReceive;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONTRANSMIT_ID:
            {
                bool definedFlag = myMetaData->IsParameterDefined(myMetaData->correctionTransmit);
                if (definedFlag)
                {
                    output << "CORRECTION_TRANSMIT = " << myMetaData->correctionTransmit;
                    output << endl;
                }
            }

            break;

            case TDMCCSDSMetaData::CCSDS_TDM_CORRECTIONAPPLIED_ID:
            {
                bool definedFlag1 = myMetaData->IsParameterDefined(myMetaData->correctionAngle1);
                bool definedFlag2 = myMetaData->IsParameterDefined(myMetaData->correctionAngle2);
                bool definedFlag3 = myMetaData->IsParameterDefined(myMetaData->correctionDoppler);
                bool definedFlag4 = myMetaData->IsParameterDefined(myMetaData->correctionRange);
                bool definedFlag5 = myMetaData->IsParameterDefined(myMetaData->correctionReceive);
                bool definedFlag6 = myMetaData->IsParameterDefined(myMetaData->correctionTransmit);

                if (definedFlag1 || definedFlag2 || definedFlag3 ||
                    definedFlag4 || definedFlag5 || definedFlag6)
                {
                    if (myMetaData->correctionsApplied)
                        output << "CORRECTIONS_APPLIED = YES" << endl;
                    else
                        output << "CORRECTIONS_APPLIED = NO" << endl;
                }
            }
            break;

            default:
                break;
        }
    }

   output << "META_STOP";
   output << endl;

   return output;
}

