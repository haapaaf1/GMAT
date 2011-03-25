//$Header$
//------------------------------------------------------------------------------
//                             UTDFObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2010/03/01
//
/**
 *
 * This class specifies the UTDF base observation data type from which the
 * various data format observation types flow.
 *
 */
//------------------------------------------------------------------------------
#include "UTDFObType.hpp";

//---------------------------------
//  static data
//---------------------------------
const std::string UTDFObType::UTDF_FILEFORMAT_DESCRIPTIONS[EndUTDFObTypeReps] =
{
    "Router",
    "Year",
    "SIC",
    "VID",
    "SecsOfYear",
    "MicroSecsOfSec",
    "Angle1",
    "Angle2",
    "RoundTripLightTime",
    "Frequency",
    "DopplerCount",
    "GroundTransmitAntennaID",
    "GroundReceiveAntennaID",
    "ForwardLinkTDRSSSIC",
    "ReturnLinkTDRSSSIC",
    "MAReturnLinkID",
    "TDRSTrackingDataOnlyFlag",
    "TrackingServiceConfiguration",
    "AzElValidity",
    "DopplerValidity",
    "RangeValidity",
    "ServiceType",
    "FrequencyBand",
    "TrackerType",
    "LastFrameIndicator",
    "SampleRateFieldFlag",
    "SampleRate",
    "TDRSOrientationDataValidity",
    "BeamOrientationDataValidity",
    "ForwardLinkID",
    "ReturnLinkID",
    "UserBitRate",
    "TDRSTrackingDataTransponderID",
    "Yaw",
    "Roll",
    "Pitch",
    "BeamAz",
    "BeamEl",
    "DopplerCompensationFlag",
    "PNLockAtReceiverFlag",
    "CarrierLockAtReceiverFlag",
    "DopplerExtractorNumber",
    "RangeExtractorNumber",
    "ForwardGCEChain",
    "ReturnGCEChain"
};

const std::string UTDFObType::UTDF_UNIT_DESCRIPTIONS[EndUTDFObTypeReps] =
{
    "",
    "",
    "",
    "",
    "sec",
    "micro sec",
    "deg",
    "deg",
    "nano sec",
    "Hz",
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
    "",
    "",
    "",
    "sec",
    "",
    "",
    "",
    "",
    "",
    "",
    "deg",
    "deg",
    "deg",
    "deg",
    "deg",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
};

const Gmat::ParameterType UTDFObType::UTDF_PARAMETER_TYPE[EndUTDFObTypeReps] =
{
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
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
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::BOOLEAN_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE
};

const bool UTDFObType::UTDF_IS_REQUIRED[EndUTDFObTypeReps] =
{
    true,
    true,
    true,
    true,
    true,
    false,
    true,
    true,
    false,
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
    true,
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

//------------------------------------------------------------------------------
//  UTDFObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the UTDFObType class
 */
//------------------------------------------------------------------------------
UTDFObType::UTDFObType() : ObType("UTDFObType",""),
    router(GmatBase::STRING_PARAMETER_UNDEFINED),
    year(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    sic(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    vid(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    secsOfYear(GmatBase::REAL_PARAMETER_UNDEFINED),
    microSecsOfSec(GmatBase::REAL_PARAMETER_UNDEFINED),
    angle1(GmatBase::REAL_PARAMETER_UNDEFINED),
    angle2(GmatBase::REAL_PARAMETER_UNDEFINED),
    roundTripLightTime(GmatBase::REAL_PARAMETER_UNDEFINED),
    frequency(GmatBase::REAL_PARAMETER_UNDEFINED),
    dopplerCount(GmatBase::REAL_PARAMETER_UNDEFINED),
    groundTransmitAntennaID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    groundReceiveAntennaID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    forwardLinkTDRSSSIC(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    returnLinkTDRSSSIC(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    maReturnLinkID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    tdrsTrackingDataOnlyFlag(false),
    trackingServiceConfiguration(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    azElValidity(false),
    dopplerValidity(false),
    rangeValidity(false),
    serviceType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    frequencyBand(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    trackerType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    lastFrameIndicator(false),
    sampleRateFieldFlag(false),
    sampleRate(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    tdrsOrientationDataValidity(false),
    beamOrientationDataValidity(false),
    forwardLinkID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    returnLinkID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    userBitRate(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    tdrsTrackingDataTransponderID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    yaw(GmatBase::REAL_PARAMETER_UNDEFINED),
    roll(GmatBase::REAL_PARAMETER_UNDEFINED),
    pitch(GmatBase::REAL_PARAMETER_UNDEFINED),
    beamAz(GmatBase::REAL_PARAMETER_UNDEFINED),
    beamEl(GmatBase::REAL_PARAMETER_UNDEFINED),
    dopplerCompensationFlag(false),
    pnLockAtReceiverFlag(false),
    carrierLockAtReceiverFlag(false),
    dopplerExtractorNumber(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    rangeExtractorNumber(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    forwardGCEChain(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    returnGCEChain(GmatBase::INTEGER_PARAMETER_UNDEFINED)
{
}

//------------------------------------------------------------------------------
//  UTDFObType(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
UTDFObType::UTDFObType(const std::string &type, const std::string &name) :
   ObType(type, name),
    router(GmatBase::STRING_PARAMETER_UNDEFINED),
    year(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    sic(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    vid(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    secsOfYear(GmatBase::REAL_PARAMETER_UNDEFINED),
    microSecsOfSec(GmatBase::REAL_PARAMETER_UNDEFINED),
    angle1(GmatBase::REAL_PARAMETER_UNDEFINED),
    angle2(GmatBase::REAL_PARAMETER_UNDEFINED),
    roundTripLightTime(GmatBase::REAL_PARAMETER_UNDEFINED),
    frequency(GmatBase::REAL_PARAMETER_UNDEFINED),
    dopplerCount(GmatBase::REAL_PARAMETER_UNDEFINED),
    groundTransmitAntennaID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    groundReceiveAntennaID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    forwardLinkTDRSSSIC(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    returnLinkTDRSSSIC(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    maReturnLinkID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    tdrsTrackingDataOnlyFlag(false),
    trackingServiceConfiguration(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    azElValidity(false),
    dopplerValidity(false),
    rangeValidity(false),
    serviceType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    frequencyBand(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    trackerType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    lastFrameIndicator(false),
    sampleRateFieldFlag(false),
    sampleRate(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    tdrsOrientationDataValidity(false),
    beamOrientationDataValidity(false),
    forwardLinkID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    returnLinkID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    userBitRate(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    tdrsTrackingDataTransponderID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    yaw(GmatBase::REAL_PARAMETER_UNDEFINED),
    roll(GmatBase::REAL_PARAMETER_UNDEFINED),
    pitch(GmatBase::REAL_PARAMETER_UNDEFINED),
    beamAz(GmatBase::REAL_PARAMETER_UNDEFINED),
    beamEl(GmatBase::REAL_PARAMETER_UNDEFINED),
    dopplerCompensationFlag(false),
    pnLockAtReceiverFlag(false),
    carrierLockAtReceiverFlag(false),
    dopplerExtractorNumber(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    rangeExtractorNumber(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    forwardGCEChain(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    returnGCEChain(GmatBase::INTEGER_PARAMETER_UNDEFINED)
{
}

//------------------------------------------------------------------------------
//  UTDFObType(const UTDFObType &ob)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
UTDFObType::UTDFObType(const UTDFObType &ob) : ObType(ob),
    router(ob.router),
    year(ob.year),
    sic(ob.sic),
    vid(ob.vid),
    secsOfYear(ob.secsOfYear),
    microSecsOfSec(ob.microSecsOfSec),
    angle1(ob.angle1),
    angle2(ob.angle2),
    roundTripLightTime(ob.roundTripLightTime),
    frequency(ob.frequency),
    dopplerCount(ob.dopplerCount),
    groundTransmitAntennaID(ob.groundTransmitAntennaID),
    groundReceiveAntennaID(ob.groundReceiveAntennaID),
    forwardLinkTDRSSSIC(ob.forwardLinkTDRSSSIC),
    returnLinkTDRSSSIC(ob.returnLinkTDRSSSIC),
    maReturnLinkID(ob.maReturnLinkID),
    tdrsTrackingDataOnlyFlag(ob.tdrsTrackingDataOnlyFlag),
    trackingServiceConfiguration(ob.trackingServiceConfiguration),
    azElValidity(ob.azElValidity),
    dopplerValidity(ob.dopplerValidity),
    rangeValidity(ob.rangeValidity),
    serviceType(ob.serviceType),
    frequencyBand(ob.frequencyBand),
    trackerType(ob.trackerType),
    lastFrameIndicator(ob.lastFrameIndicator),
    sampleRateFieldFlag(ob.sampleRateFieldFlag),
    sampleRate(ob.sampleRate),
    tdrsOrientationDataValidity(ob.tdrsOrientationDataValidity),
    beamOrientationDataValidity(ob.beamOrientationDataValidity),
    forwardLinkID(ob.forwardLinkID),
    returnLinkID(ob.returnLinkID),
    userBitRate(ob.userBitRate),
    tdrsTrackingDataTransponderID(ob.tdrsTrackingDataTransponderID),
    yaw(ob.yaw),
    roll(ob.roll),
    pitch(ob.pitch),
    beamAz(ob.beamAz),
    beamEl(ob.beamEl),
    dopplerCompensationFlag(ob.dopplerCompensationFlag),
    pnLockAtReceiverFlag(ob.pnLockAtReceiverFlag),
    carrierLockAtReceiverFlag(ob.carrierLockAtReceiverFlag),
    dopplerExtractorNumber(ob.dopplerExtractorNumber),
    rangeExtractorNumber(ob.rangeExtractorNumber),
    forwardGCEChain(ob.forwardGCEChain),
    returnGCEChain(ob.returnGCEChain)
{
}

//---------------------------------------------------------------------------
//  UTDFObType& operator=(const UTDFObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const UTDFObType& UTDFObType::operator=(const UTDFObType &ob)
{
   if (&ob == this)
      return *this;

   UTDFObType::operator=(ob);

    router = ob.router;
    year = ob.year;
    sic = ob.sic;
    vid = ob.vid;
    secsOfYear = ob.secsOfYear;
    microSecsOfSec = ob.microSecsOfSec;
    angle1 = ob.angle1;
    angle2 = ob.angle2;
    roundTripLightTime = ob.roundTripLightTime;
    frequency = ob.frequency;
    dopplerCount = ob.dopplerCount;
    groundTransmitAntennaID = ob.groundTransmitAntennaID;
    groundReceiveAntennaID = ob.groundReceiveAntennaID;
    forwardLinkTDRSSSIC = ob.forwardLinkTDRSSSIC;
    returnLinkTDRSSSIC = ob.returnLinkTDRSSSIC;
    maReturnLinkID = ob.maReturnLinkID;
    tdrsTrackingDataOnlyFlag = ob.tdrsTrackingDataOnlyFlag;
    trackingServiceConfiguration = ob.trackingServiceConfiguration;
    azElValidity = ob.azElValidity;
    dopplerValidity = ob.dopplerValidity;
    rangeValidity = ob.rangeValidity;
    serviceType = ob.serviceType;
    frequencyBand = ob.frequencyBand;
    trackerType = ob.trackerType;
    lastFrameIndicator = ob.lastFrameIndicator;
    sampleRateFieldFlag = ob.sampleRateFieldFlag;
    sampleRate = ob.sampleRate;
    tdrsOrientationDataValidity = ob.tdrsOrientationDataValidity;
    beamOrientationDataValidity = ob.beamOrientationDataValidity;
    forwardLinkID = ob.forwardLinkID;
    returnLinkID = ob.returnLinkID;
    userBitRate = ob.userBitRate;
    tdrsTrackingDataTransponderID = ob.tdrsTrackingDataTransponderID;
    yaw = ob.yaw;
    roll = ob.roll;
    pitch = ob.pitch;
    beamAz = ob.beamAz;
    beamEl = ob.beamEl;
    dopplerCompensationFlag = ob.dopplerCompensationFlag;
    pnLockAtReceiverFlag = ob.pnLockAtReceiverFlag;
    carrierLockAtReceiverFlag = ob.carrierLockAtReceiverFlag;
    dopplerExtractorNumber = ob.dopplerExtractorNumber;
    rangeExtractorNumber = ob.rangeExtractorNumber;
    forwardGCEChain = ob.forwardGCEChain;
    returnGCEChain = ob.returnGCEChain;

   return *this;
}

//------------------------------------------------------------------------------
//  ~UTDFObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
UTDFObType::~UTDFObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the UTDFObType.
 *
 * @return clone of the UTDFObType.
 */
//------------------------------------------------------------------------------
GmatBase* UTDFObType::Clone() const
{
   GmatBase *clone = new UTDFObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string UTDFObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndUTDFObTypeReps))
   {
      return UTDF_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the unit text, given the input parameter ID.
 *
 * @param <id> Id for the requested unit text.
 *
 * @return unit text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string UTDFObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndUTDFObTypeReps))
   {
      return UTDF_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer UTDFObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndUTDFObTypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(UTDF_FILEFORMAT_DESCRIPTIONS[i]))
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
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType UTDFObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndUTDFObTypeReps))
      return UTDF_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the string associated with a parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return Text description for the type of the parameter, or the empty
 *         string ("").
 */
//---------------------------------------------------------------------------
std::string UTDFObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves Integer data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The Integer data parameter
 *
 */
//------------------------------------------------------------------------------
Integer UTDFObType::GetIntegerDataParameter(const Integer id) const
{
    switch(id)
    {
        case UTDF_YEAR_ID:
            return year;
            break;
        case UTDF_SIC_ID:
            return sic;
            break;
        case UTDF_VID_ID:
            return vid;
            break;
        case UTDF_GROUNDTRANSMITANTENNA_ID:
            return groundTransmitAntennaID;
            break;
        case UTDF_GROUNDRECEIVEANTENNA_ID:
            return groundReceiveAntennaID;
            break;
        case UTDF_FORWARDLINKTDRSSSIC_ID:
            return forwardLinkTDRSSSIC;
            break;
        case UTDF_RETURNLINKTDRSSSIC_ID:
            return returnLinkTDRSSSIC;
            break;
        case UTDF_MARETURNLINKSIC_ID:
            return maReturnLinkID;
            break;
        case UTDF_TRACKINGSERVICECONFIGURATION_ID:
            return trackingServiceConfiguration;
            break;
        case UTDF_SERVICETYPE_ID:
            return serviceType;
            break;
        case UTDF_FREQUENCYBAND_ID:
            return frequencyBand;
            break;
        case UTDF_TRACKERTYPE_ID:
            return trackerType;
            break;
        case UTDF_SAMPLERATE_ID:
            return sampleRate;
            break;
        case UTDF_FORWARDLINK_ID:
            return forwardLinkID;
            break;
        case UTDF_RETURNLINK_ID:
            return returnLinkID;
            break;
        case UTDF_USERBITRATE_ID:
            return userBitRate;
            break;
        case UTDF_TDRSTRACKINGDATATRANSPONDER_ID:
            return tdrsTrackingDataTransponderID;
            break;
        case UTDF_DOPPLEREXTRACTORNUMBER_ID:
            return dopplerExtractorNumber;
            break;
        case UTDF_RANGEEXTRACTORNUMBER_ID:
            return rangeExtractorNumber;
            break;
        case UTDF_FORWARDGCECHAIN_ID:
            return forwardGCEChain;
            break;
        case UTDF_RETURNGCECHAIN_ID:
            return returnGCEChain;
            break;
        default:
            return GmatBase::INTEGER_PARAMETER_UNDEFINED;
            break;
    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves Integer data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The Integer data parameter
 *
 */
//------------------------------------------------------------------------------
Integer UTDFObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves Real data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The Real data parameter
 *
 */
//------------------------------------------------------------------------------
Real UTDFObType::GetRealDataParameter(const Integer id) const
{

    switch(id)
    {
        case UTDF_SECSOFYEAR_ID:
            return secsOfYear;
            break;
        case UTDF_MICROSECSOFSEC_ID:
            return microSecsOfSec;
            break;
        case UTDF_ANGLE1_ID:
            return angle1;
            break;
        case UTDF_ANGLE2_ID:
            return angle2;
            break;
        case UTDF_RTLT_ID:
            return roundTripLightTime;
            break;
        case UTDF_FREQUENCY_ID:
            return frequency;
            break;
        case UTDF_DOPPLERCOUNT_ID:
            return dopplerCount;
            break;
        case UTDF_YAW_ID:
            return yaw;
            break;
        case UTDF_ROLL_ID:
            return roll;
            break;
        case UTDF_PITCH_ID:
            return pitch;
            break;
        case UTDF_BEAMAZ_ID:
            return beamAz;
            break;
        case UTDF_BEAMEL_ID:
            return beamEl;
            break;
        default:
            return GmatBase::REAL_PARAMETER_UNDEFINED;
            break;
    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves Real data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The Real data parameter
 *
 */
//------------------------------------------------------------------------------
Real UTDFObType::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
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
bool UTDFObType::GetBoolDataParameter(const Integer id) const
{
    switch(id)
    {
        case UTDF_TDRSTRACKINGDATAONLYFLAG_ID:
            return tdrsTrackingDataOnlyFlag;
            break;
        case UTDF_AZELVALIDITY_ID:
            return azElValidity;
            break;
        case UTDF_DOPPLERVALIDITY_ID:
            return dopplerValidity;
            break;
        case UTDF_RANGEVALIDITY_ID:
            return rangeValidity;
            break;
        case UTDF_LASTFRAMEINDICATOR_ID:
            return lastFrameIndicator;
            break;
        case UTDF_SAMPLERATEFIELDFLAG_ID:
            return sampleRateFieldFlag;
            break;
        case UTDF_TDRSORIENTATIONDATAVALIDITY_ID:
            return tdrsOrientationDataValidity;
            break;
        case UTDF_BEAMORIENTATIONDATAVALIDITY_ID:
            return beamOrientationDataValidity;
            break;
        case UTDF_DOPPLERCOMPENSATIONFLAG_ID:
            return dopplerCompensationFlag;
            break;
        case UTDF_PNLOCKATRECEIVERFLAG_ID:
            return pnLockAtReceiverFlag;
            break;
        case UTDF_CARRIERLOCKATRECEIVERFLAG_ID:
            return carrierLockAtReceiverFlag;
            break;
        default:
            return false;
            break;
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
bool UTDFObType::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves string data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The string data parameter
 *
 */
//------------------------------------------------------------------------------
std::string UTDFObType::GetStringDataParameter(const Integer id) const
{
    switch(id)
    {
        case UTDF_ROUTER_ID:
            return router;
            break;
        default:
            return GmatBase::STRING_PARAMETER_UNDEFINED;
            break;
    }
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves string data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The string data parameter
 *
 */
//------------------------------------------------------------------------------
std::string UTDFObType::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
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
bool UTDFObType::SetDataParameter(const Integer id, const Real &value)
{
    switch(id)
    {
        case UTDF_SECSOFYEAR_ID:
            secsOfYear = value;
            return true;
            break;
        case UTDF_MICROSECSOFSEC_ID:
            microSecsOfSec = value;
            return true;
            break;
        case UTDF_ANGLE1_ID:
            angle1 = value;
            return true;
            break;
        case UTDF_ANGLE2_ID:
            angle2 = value;
            return true;
            break;
        case UTDF_RTLT_ID:
            roundTripLightTime = value;
            return true;
            break;
        case UTDF_FREQUENCY_ID:
            frequency = value;
            return true;
            break;
        case UTDF_DOPPLERCOUNT_ID:
            dopplerCount = value;
            return true;
            break;
        case UTDF_YAW_ID:
            yaw = value;
            return true;
            break;
        case UTDF_ROLL_ID:
            roll = value;
            return true;
            break;
        case UTDF_PITCH_ID:
            pitch = value;
            return true;
            break;
        case UTDF_BEAMAZ_ID:
            beamAz = value;
            return true;
            break;
        case UTDF_BEAMEL_ID:
            beamEl = value;
            return true;
            break;
        default:
            return false;
            break;
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
bool UTDFObType::SetDataParameter(const std::string &label, const Real &value)
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
bool UTDFObType::SetDataParameter(const Integer id, const Integer &value)
{
    switch(id)
    {
        case UTDF_YEAR_ID:
            year = value;
            return true;
            break;
        case UTDF_SIC_ID:
            sic = value;
            return true;
            break;
        case UTDF_VID_ID:
            vid = value;
            return true;
            break;
        case UTDF_GROUNDTRANSMITANTENNA_ID:
            groundTransmitAntennaID = value;
            return true;
            break;
        case UTDF_GROUNDRECEIVEANTENNA_ID:
            groundReceiveAntennaID = value;
            return true;
            break;
        case UTDF_FORWARDLINKTDRSSSIC_ID:
            forwardLinkTDRSSSIC = value;
            return true;
            break;
        case UTDF_RETURNLINKTDRSSSIC_ID:
            returnLinkTDRSSSIC = value;
            return true;
            break;
        case UTDF_MARETURNLINKSIC_ID:
            maReturnLinkID = value;
            return true;
            break;
        case UTDF_TRACKINGSERVICECONFIGURATION_ID:
            trackingServiceConfiguration = value;
            return true;
            break;
        case UTDF_SERVICETYPE_ID:
            serviceType = value;
            return true;
            break;
        case UTDF_FREQUENCYBAND_ID:
            frequencyBand = value;
            return true;
            break;
        case UTDF_TRACKERTYPE_ID:
            trackerType = value;
            return true;
            break;
        case UTDF_SAMPLERATE_ID:
            sampleRate = value;
            return true;
            break;
        case UTDF_FORWARDLINK_ID:
            forwardLinkID = value;
            return true;
            break;
        case UTDF_RETURNLINK_ID:
            returnLinkID = value;
            return true;
            break;
        case UTDF_USERBITRATE_ID:
            userBitRate = value;
            return true;
            break;
        case UTDF_TDRSTRACKINGDATATRANSPONDER_ID:
            tdrsTrackingDataTransponderID = value;
            return true;
            break;
        case UTDF_DOPPLEREXTRACTORNUMBER_ID:
            dopplerExtractorNumber = value;
            return true;
            break;
        case UTDF_RANGEEXTRACTORNUMBER_ID:
            rangeExtractorNumber = value;
            return true;
            break;
        case UTDF_FORWARDGCECHAIN_ID:
            forwardGCEChain = value;
            return true;
            break;
        case UTDF_RETURNGCECHAIN_ID:
            returnGCEChain = value;
            return true;
            break;
        default:
            return false;
            break;
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
bool UTDFObType::SetDataParameter(const std::string &label, const Integer &value)
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
bool UTDFObType::SetDataParameter(const Integer id, const bool &value)
{
    switch(id)
    {
        case UTDF_TDRSTRACKINGDATAONLYFLAG_ID:
            tdrsTrackingDataOnlyFlag = value;
            return true;
            break;
        case UTDF_AZELVALIDITY_ID:
            azElValidity = value;
            return true;
            break;
        case UTDF_DOPPLERVALIDITY_ID:
            dopplerValidity = value;
            return true;
            break;
        case UTDF_RANGEVALIDITY_ID:
            rangeValidity = value;
            return true;
            break;
        case UTDF_LASTFRAMEINDICATOR_ID:
            lastFrameIndicator = value;
            return true;
            break;
        case UTDF_SAMPLERATEFIELDFLAG_ID:
            sampleRateFieldFlag = value;
            return true;
            break;
        case UTDF_TDRSORIENTATIONDATAVALIDITY_ID:
            tdrsOrientationDataValidity = value;
            return true;
            break;
        case UTDF_BEAMORIENTATIONDATAVALIDITY_ID:
            beamOrientationDataValidity = value;
            return true;
            break;
        case UTDF_DOPPLERCOMPENSATIONFLAG_ID:
            dopplerCompensationFlag = value;
            return true;
            break;
        case UTDF_PNLOCKATRECEIVERFLAG_ID:
            pnLockAtReceiverFlag = value;
            return true;
            break;
        case UTDF_CARRIERLOCKATRECEIVERFLAG_ID:
            carrierLockAtReceiverFlag = value;
            return true;
            break;
        default:
            return false;
            break;
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
bool UTDFObType::SetDataParameter(const std::string &label, const bool &value)
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
bool UTDFObType::SetDataParameter(const Integer id, const std::string &value)
{
    switch(id)
    {
        case UTDF_ROUTER_ID:
            router = value;
            return true;
            break;
        default:
            return false;
            break;
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
bool UTDFObType::SetDataParameter(const std::string &label, const std::string &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const UTDFObType &myUTDF)
//------------------------------------------------------------------------------
/**
 * Formats UTDFObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myUTDF>    UTDF observation to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const UTDFObType *myUTDF)
{
    using namespace std;

    output.setf(std::ios::showpoint);
    output.setf(std::ios::scientific);

    output.precision(16);

    output << ios::hex << 0x01;
    output << myUTDF->router[0] << myUTDF->router[1];
    output << myUTDF->year;
    output << myUTDF->sic;
    output << myUTDF->vid;
    output << myUTDF->secsOfYear;
    output << myUTDF->microSecsOfSec;
    output << myUTDF->angle1;
    output << myUTDF->angle2;
    // Convert light time from nsec to 1/256 nsec
    output << myUTDF->roundTripLightTime*0.00390625;
    output << myUTDF->dopplerCount;
    // Fixed not used for TDRSS
    output << ios::hex << 0x00;
    output << ios::hex << 0x00;
    // Convert frequency so that LSB = 10Hz
    output << (int)(myUTDF->frequency * 0.1);
    // Fixed to specify 18-m az/el transmit at WSGT for TDRSS
    output << ios::hex << 0x60;
    output << myUTDF->groundTransmitAntennaID;
    // Fixed to specify 18-m az/el receive at WSGT for TDRSS
    output << ios::hex << 0x60;
    output << myUTDF->groundReceiveAntennaID;
    output << myUTDF->forwardLinkTDRSSSIC;
    output << myUTDF->returnLinkTDRSSSIC;
    output << myUTDF->maReturnLinkID;
    output << myUTDF->tdrsTrackingDataOnlyFlag;
    output << myUTDF->trackingServiceConfiguration;
    output << myUTDF->azElValidity;
    output << myUTDF->dopplerValidity;
    output << myUTDF->rangeValidity;
    output << myUTDF->frequencyBand;
    output << myUTDF->serviceType;
    output << myUTDF->trackerType;
    output << myUTDF->lastFrameIndicator;
    output << myUTDF->sampleRateFieldFlag;
    output << myUTDF->sampleRate;
    output << myUTDF->tdrsOrientationDataValidity;
    output << myUTDF->beamOrientationDataValidity;
    output << myUTDF->forwardLinkID;
    output << myUTDF->returnLinkID;
    output << myUTDF->userBitRate;
    output << myUTDF->tdrsTrackingDataTransponderID;
    output << myUTDF->yaw;
    output << myUTDF->roll;
    output << myUTDF->pitch;
    output << myUTDF->beamAz;
    output << myUTDF->beamEl;
    output << myUTDF->dopplerCompensationFlag;
    output << myUTDF->pnLockAtReceiverFlag;
    output << myUTDF->carrierLockAtReceiverFlag;
    output << myUTDF->dopplerExtractorNumber;
    output << myUTDF->rangeExtractorNumber;
    output << myUTDF->forwardGCEChain;
    output << myUTDF->returnGCEChain;
    // Fixed
    output << ios::hex << 0x00;
    output << ios::hex << 0x00;
    output << ios::hex << 0x04;
    // Fixed control characters
    output << ios::hex << 0x0F;
    output << ios::hex << 0x0F;

    return output;

}