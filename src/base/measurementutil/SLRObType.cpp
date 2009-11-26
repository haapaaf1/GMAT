#include "SLRObType.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string SLRObType::SLR_DATATYPE_DESCRIPTIONS[EndSLRTypeReps] =
{
    "TwoWayTimeOfFlight"
};
    
const std::string SLRObType::SLR_TIMESYSTEM_DESCRIPTIONS[EndSLRTimeReps] =
{
    "UTC",
    "GPS",
    "BIPM",
    "BIH"
};

const std::string SLRObType::SLR_FILEFORMAT_DESCRIPTIONS[EndSLRDataReps] =
{
	"TimeOfLaserFiring",
	"TwoWayTimeOfFlight",
	"BinRMSRange",
	"SurfacePressure",
	"SurfaceTemp",
	"RelativeHumidity",
	"NumRawRanges",
	"DataReleaseFlag",
	"RawRangeFactor",
	"NormalPointWindowIndicator2",
	"SignalToNoiseRatio",
        "BurstCalSysDelay",
	"SignalStrength",
        "AngleOriginIndicator",
        "Azimuth",
        "Elevation"
};

const std::string SLRObType::SLR_UNIT_DESCRIPTIONS[EndSLRDataReps] =
{
	"sec",
	"sec",
	"picosec",
	"millibar",
	"Kelvin",
	"%",
	"",
	"",
	"",
	"",
	"",
        "",
	"",
        "",
        "deg",
        "deg"
};

const Gmat::ParameterType SLRObType::SLR_PARAMETER_TYPE[EndSLRDataReps] =
{
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE
};

const bool SLRObType::SLR_IS_REQUIRED[EndSLRDataReps] =
{
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
    false,
    false,
    false,
    false,
    false,
    false
};
    
    
//------------------------------------------------------------------------------
//  SLRObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the ObType class
 */
//------------------------------------------------------------------------------
SLRObType::SLRObType() : ObType("SLRObType",""),
        slrHeader(NULL),
	timeOfLaserFiring(0),
	twoWayTimeOfFlight(0),
	binRMSRange(0),
	surfacePressure(0),
	surfaceTemp(0),
	relativeHumidity(0),
	numRawRanges(0),
	dataReleaseFlag(0),
	rawRangeFactor(0),
	normalPointWindowIndicator2(0),
	signalToNoiseRatio(0),
        burstCalSysDelay(0),
	signalStrength(0),
        angleOriginIndicator(0),
        az(0),
        el(0)
{
    objectTypeNames.push_back("SLRObType");
}

//------------------------------------------------------------------------------
//  SLRObType(const SLRObType &slrOb)
//------------------------------------------------------------------------------
/**
 * Constructor for the ObType class
 */
//------------------------------------------------------------------------------
SLRObType::SLRObType(const SLRObType &slrOb) :
    ObType(slrOb),
    slrHeader(slrOb.slrHeader),
    timeOfLaserFiring(slrOb.timeOfLaserFiring),   
    twoWayTimeOfFlight(slrOb.twoWayTimeOfFlight),
    binRMSRange(slrOb.binRMSRange), 
    surfacePressure(slrOb.surfacePressure),
    surfaceTemp(slrOb.surfaceTemp),
    relativeHumidity(slrOb.relativeHumidity),
    numRawRanges(slrOb.numRawRanges),
    dataReleaseFlag(slrOb.dataReleaseFlag),
    rawRangeFactor(slrOb.rawRangeFactor),
    normalPointWindowIndicator2(slrOb.normalPointWindowIndicator2),
    signalToNoiseRatio(slrOb.signalToNoiseRatio),
    burstCalSysDelay(slrOb.burstCalSysDelay),
    signalStrength(slrOb.signalStrength),
    angleOriginIndicator(slrOb.angleOriginIndicator),
    az(slrOb.az),
    el(slrOb.el)
{
}

//---------------------------------------------------------------------------
//  SLRObType& operator=(const SLRObType &slrOb)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tleOb> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const SLRObType& SLRObType::operator=(const SLRObType &slrOb)
{
   if (&slrOb == this)
      return *this;

    slrHeader = slrOb.slrHeader;
    timeOfLaserFiring = slrOb.timeOfLaserFiring;   
    twoWayTimeOfFlight = slrOb.twoWayTimeOfFlight;
    binRMSRange = slrOb.binRMSRange; 
    surfacePressure = slrOb.surfacePressure;
    surfaceTemp = slrOb.surfaceTemp;
    relativeHumidity = slrOb.relativeHumidity;
    numRawRanges = slrOb.numRawRanges;
    dataReleaseFlag = slrOb.dataReleaseFlag;
    rawRangeFactor = slrOb.rawRangeFactor;
    normalPointWindowIndicator2 = slrOb.normalPointWindowIndicator2;
    signalToNoiseRatio = slrOb.signalToNoiseRatio;
    burstCalSysDelay = slrOb.burstCalSysDelay;
    signalStrength = slrOb.signalStrength;
    angleOriginIndicator = slrOb.angleOriginIndicator;
    az = slrOb.az;
    el = slrOb.el;
    
   return *this;
}

//------------------------------------------------------------------------------
//  ~SLRObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the ObType class
 */
//------------------------------------------------------------------------------
SLRObType::~SLRObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the SLRDataFile.
 *
 * @return clone of the SLRDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* SLRObType::Clone() const
{
   GmatBase *clone = new SLRObType(*this);
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
std::string SLRObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRDataReps))
   {
      return SLR_FILEFORMAT_DESCRIPTIONS[id];
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
std::string SLRObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRDataReps))
   {
      return SLR_UNIT_DESCRIPTIONS[id];
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
Integer SLRObType::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndSLRDataReps; i++)
   {
      if (str == SLR_FILEFORMAT_DESCRIPTIONS[i])
         return i;
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
Gmat::ParameterType SLRObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRDataReps))
      return SLR_PARAMETER_TYPE[id];

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
std::string SLRObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer SLRObType::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {
        case SLR_BINRMSRANGE_ID:

            return binRMSRange;

        case SLR_RELATIVEHUMIDITY_ID:

            return relativeHumidity;

        case SLR_NUMRAWRANGES_ID:

            return numRawRanges;

        case SLR_DATARELEASEFLAG_ID:

            return dataReleaseFlag;

        case SLR_RAWRANGEFACTOR_ID:

            return rawRangeFactor;

        case SLR_NORMALPOINTWINDOWINDICATOR2_ID:

            return normalPointWindowIndicator2;

        case SLR_BURSTCALSYSDELAY_ID:

            return burstCalSysDelay;

        case SLR_SIGNALSTRENGTH_ID:

            return signalStrength;

        case SLR_ANGLEORIGININDICATOR_ID:

            return angleOriginIndicator;

       default:

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer SLRObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real SLRObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case SLR_TIMEOFLASERFIRING_ID:

            return timeOfLaserFiring;

        case SLR_TWOWAYTIMEOFFLIGHT_ID:

            return twoWayTimeOfFlight;

        case SLR_SURFACEPRESSURE_ID:

            return surfacePressure;

        case SLR_SURFACETEMP_ID:

            return surfaceTemp;

        case SLR_SIGNALTONOISERATIO_ID:

            return signalToNoiseRatio;

        case SLR_AZIMUTH_ID:

            return az;

        case SLR_ELEVATION_ID:

            return el;
            
        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real SLRObType::GetRealDataParameter(const std::string &label) const
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
bool SLRObType::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

        case SLR_TIMEOFLASERFIRING_ID:

            timeOfLaserFiring = value;
            return true;

        case SLR_TWOWAYTIMEOFFLIGHT_ID:

            twoWayTimeOfFlight = value;
            return true;

        case SLR_SURFACEPRESSURE_ID:

            surfacePressure = value;
            return true;

        case SLR_SURFACETEMP_ID:

            surfaceTemp = value;
            return true;

        case SLR_SIGNALTONOISERATIO_ID:

            signalToNoiseRatio = value;
            return true;

        case SLR_AZIMUTH_ID:

            az = value;
            return true;

        case SLR_ELEVATION_ID:

            el = value;
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
bool SLRObType::SetDataParameter(const std::string &label, const Real &value)
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
bool SLRObType::SetDataParameter(const Integer id, const Integer &value)
{

    switch (id)
    {
        case SLR_BINRMSRANGE_ID:

            binRMSRange = value;
            return true;

        case SLR_RELATIVEHUMIDITY_ID:

            relativeHumidity = value;
            return true;

        case SLR_NUMRAWRANGES_ID:

            numRawRanges = value;
            return true;

        case SLR_DATARELEASEFLAG_ID:

            dataReleaseFlag = value;
            return true;

        case SLR_RAWRANGEFACTOR_ID:

            rawRangeFactor = value;
            return true;

        case SLR_NORMALPOINTWINDOWINDICATOR2_ID:

            normalPointWindowIndicator2 = value;
            return true;

        case SLR_BURSTCALSYSDELAY_ID:

            burstCalSysDelay = value;
            return true;

        case SLR_SIGNALSTRENGTH_ID:

            signalStrength = value;
            return true;

        case SLR_ANGLEORIGININDICATOR_ID:

            angleOriginIndicator = value;
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
bool SLRObType::SetDataParameter(const std::string &label, const Integer &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
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
const std::string* SLRObType::GetDataTypes() const
{
   return SLR_DATATYPE_DESCRIPTIONS;
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
std::string SLRObType::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndSLRTypeReps))
   {
      return SLR_DATATYPE_DESCRIPTIONS[id];
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
Integer SLRObType::GetDataTypeID(const std::string &label)
{
    return -1;
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
const std::string* SLRObType::GetTimeSystems() const
{
   return SLR_TIMESYSTEM_DESCRIPTIONS;
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
std::string SLRObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndSLRTimeReps))
   {
      return SLR_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer SLRObType::GetTimeSystemID(const std::string &label)
{
    return -1;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const SLRObType &mySLR)
//------------------------------------------------------------------------------
/**
 * Formats SLRObType value and sends to output stream.
 *
 * @param  output  Output stream
 * @param  mySLR   SLR observation to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const SLRObType *mySLR)
{

    ostringstream buffer;

    // Output either standard data record or engineering data record
    if ((mySLR->slrHeader)->GetSLRType() == 99999)
    {
        // The time of laser firing and the two way time of flight
        // is too large of an integer to store in an INT or even a LONG INT
        // So we break them up into real and fractional parts ( minus the period
        // of course ) for output purposes
        int tOLF = mySLR->timeOfLaserFiring;
        int tOLF2 = (mySLR->timeOfLaserFiring - tOLF) * 1e7 + 0.5;
        buffer << setw(5) << setfill('0') << right << tOLF;
        buffer << setw(7) << setfill('0') << right << tOLF2;
        int tWTOF = mySLR->twoWayTimeOfFlight * 1e7;
        int tWTOF2 = (mySLR->twoWayTimeOfFlight * 1e7 - tWTOF) * 1e5 + 0.5;
        buffer << setw(7) << setfill('0') << right << tWTOF;
        buffer << setw(5) << setfill('0') << right << tWTOF2;
        buffer << setw(7) << setfill('0') << right << mySLR->binRMSRange;
        int sP = mySLR->surfacePressure*10 + 0.5;
        buffer << setw(5) << setfill('0') << right << sP;
        int sT = mySLR->surfaceTemp*10 + 0.5;
        buffer << setw(4) << setfill('0') << right << sT;
        buffer << setw(3) << setfill('0') << right << mySLR->relativeHumidity;
        buffer << setw(4) << setfill('0') << right << mySLR->numRawRanges;
        buffer << setw(1) << mySLR->dataReleaseFlag;
	buffer << setw(1) << mySLR->rawRangeFactor;
	buffer << setw(1) << mySLR->normalPointWindowIndicator2;
	buffer << setw(2) << setfill('0') << right << mySLR->signalToNoiseRatio;

        output << buffer.str();
        output << setw(2) << SLRCheckSum(buffer.str()) << endl;
    }
    else if ((mySLR->slrHeader)->GetSLRType() == 88888)
    {

        // The time of laser firing and the two way time of flight
        // is too large of an integer to store in an INT or even a LONG INT
        // So we break them up into real and fractional parts ( minus the period
        // of course ) for output purposes
        int tOLF = mySLR->timeOfLaserFiring;
        int tOLF2 = (mySLR->timeOfLaserFiring - tOLF) * 1e7 + 0.5;
        buffer << setw(5) << setfill('0') << right << tOLF;
        buffer << setw(7) << setfill('0') << right << tOLF2;
        int tWTOF = mySLR->twoWayTimeOfFlight * 1e7;
        int tWTOF2 = (mySLR->twoWayTimeOfFlight * 1e7 - tWTOF) * 1e5 + 0.5;
        buffer << setw(7) << setfill('0') << right << tWTOF;
        buffer << setw(5) << setfill('0') << right << tWTOF2;
        int sP = mySLR->surfacePressure*10 + 0.5;
        buffer << setw(5) << setfill('0') << right << sP;
        int sT = mySLR->surfaceTemp*10 + 0.5;
        buffer << setw(4) << setfill('0') << right << sT;
        buffer << setw(3) << setfill('0') << right << mySLR->relativeHumidity;
        buffer << setw(8) << setfill('0') << right << mySLR->burstCalSysDelay;
	buffer << setw(4) << setfill('0') << right << mySLR->signalStrength;
        buffer << setw(1) << mySLR->angleOriginIndicator;
        int az = mySLR->az*1e4 + 0.5;
        buffer << setw(6) << setfill('0') << right << az;
        int el = mySLR->el*1e4 + 0.5;
        buffer << setw(5) << setfill('0') << right << el;

        // unused zero filled columns
        buffer << "00000";

        output << buffer.str();
        output << setw(2) << SLRCheckSum(buffer.str()) << endl;
    }

    return output;
}