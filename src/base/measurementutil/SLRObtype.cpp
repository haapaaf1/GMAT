#include "SLRObtype.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string SLRObtype::SLR_DATATYPE_DESCRIPTIONS[EndSLRTypeReps] =
{
    "TwoWayTimeOfFlight"
};
    
const std::string SLRObtype::SLR_TIMESYSTEM_DESCRIPTIONS[EndSLRTimeReps] =
{
    "UTC",
    "GPS",
    "BIPM",
    "BIH"
};

const std::string SLRObtype::SLR_FILEFORMAT_DESCRIPTIONS[EndSLRDataReps] =
{
	"SlrType",
	"IlrsSatnum",
	"Year",
	"DayOfYear",
	"CdpPadID",
	"CdpSysNum",
	"CdpOccupancySequenceNum",
	"Wavelength",
	"CalSysDelay",
	"CalDelayShift",
	"RmsSysDelay",
	"NormalPointWindowIndicator",
	"EpochTimeScaleIndicator",
	"SysCalMethodIndicator",
	"SchIndicator",
	"SciIndicator",
	"PassRMS",
	"DataQualAssessmentIndicator",
	"FormatRevisionNum",
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

const std::string SLRObtype::SLR_UNIT_DESCRIPTIONS[EndSLRDataReps] =
{
	"",
	"",
	"years",
	"DayOfYear",
	"",
	"",
	"",
	"nm",
	"picosec",
	"picosec",
	"picosec",
	"",
	"",
	"",
	"",
	"",
	"picosec",
	"",
	"",
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

const Gmat::ParameterType SLRObtype::SLR_PARAMETER_TYPE[EndSLRDataReps] =
{
	Gmat::INTEGER_TYPE,
	Gmat::STRING_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
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

const bool SLRObtype::SLR_IS_REQUIRED[EndSLRDataReps] =
{
    true,
    true,
    true,
    true,
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
    false,
    false,
    false,
    false,
    false,
    false
};
    
    
//------------------------------------------------------------------------------
//  SLRObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SLRObtype::SLRObtype() : Obtype(),
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
}

//------------------------------------------------------------------------------
//  SLRObtype(const SLRObtype &slrOb)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SLRObtype::SLRObtype(const SLRObtype &slrOb) :
    Obtype(slrOb),
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
//  SLRObtype& operator=(const SLRObtype &slrOb)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Obtype structures.
 *
 * @param <tleOb> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const SLRObtype& SLRObtype::operator=(const SLRObtype &slrOb)
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
//  ~SLRObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
SLRObtype::~SLRObtype()
{
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
std::string SLRObtype::GetDataParameterText(const Integer id) const
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
std::string SLRObtype::GetDataUnits(const Integer id) const
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
Integer SLRObtype::GetDataParameterID(const std::string &str) const
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
Gmat::ParameterType SLRObtype::GetDataParameterType(const Integer id) const
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
std::string SLRObtype::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer SLRObtype::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {
        case SLR_TYPE_ID:

            return slrHeader->slrType;

        case SLR_YEAR_ID:

            return slrHeader->year;

        case SLR_DAYOFYEAR_ID:

            return slrHeader->dayOfYear;

        case SLR_CDPPADID_ID:

            return slrHeader->cdpPadID;

        case SLR_CDPSYSNUM_ID:

            return slrHeader->cdpSysNum;

        case SLR_CDPOCCUPANCYSEQUENCENUM_ID:

            return slrHeader->cdpOccupancySequenceNum;

        case SLR_CALSYSDELAY_ID:

            return slrHeader->calSysDelay;

        case SLR_CALDELAYSHIFT_ID:

            return slrHeader->calDelayShift;

        case SLR_RMSSYSDELAY_ID:

            return slrHeader->rmsSysDelay;

        case SLR_NORMALPOINTWINDOWINDICATOR_ID:

            return slrHeader->normalPointWindowIndicator;

        case SLR_EPOCHTIMESCALEINDICATOR_ID:

            return slrHeader->epochTimeScaleIndicator;

        case SLR_SYSCALMETHODINDICATOR_ID:

            return slrHeader->sysCalMethodIndicator;

        case SLR_SCHINDICATOR_ID:

            return slrHeader->schIndicator;

        case SLR_SCIINDICATOR_ID:

            return slrHeader->sciIndicator;

        case SLR_PASSRMS_ID:

            return slrHeader->passRMS;

        case SLR_DATAQUALASSESSMENTINDICATOR_ID:

            return slrHeader->dataQualAssessmentIndicator;

        case SLR_FORMATREVISIONNUM_ID:

            return slrHeader->formatRevisionNum;

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
Integer SLRObtype::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string SLRObtype::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case SLR_ILRSSATNUM_ID:

            return slrHeader->ilrsSatnum;

        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string SLRObtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real SLRObtype::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case SLR_WAVELENGTH_ID:

            return slrHeader->wavelength;

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
Real SLRObtype::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
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
 bool SLRObtype::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSLRDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                                   .set_extended(true)
                       ).FullMatch(SLR_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

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
const std::string* SLRObtype::GetDataTypes() const
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
std::string SLRObtype::GetDataTypeText(const Integer &id) const
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
Integer SLRObtype::GetDataTypeID(const std::string &label)
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
const std::string* SLRObtype::GetTimeSystems() const
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
std::string SLRObtype::GetTimeSystemText(const Integer &id) const
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
Integer SLRObtype::GetTimeSystemID(const std::string &label)
{
    return -1;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const SLRObtype &myB3)
//------------------------------------------------------------------------------
/**
 * Formats SLRObtype value and sends to output stream.
 *
 * @param  output  Output stream
 * @param  mySLR   SLR observation to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const SLRObtype *mySLR) 
{
   using namespace std;
   
   output.setf(std::ios::showpoint);
   output.setf(std::ios::scientific);

   output << "Time of Firing = " << mySLR->timeOfLaserFiring << std::endl;
   output << "Two Way Time of Flight = " << mySLR->twoWayTimeOfFlight << std::endl;
   output << "RMS Range = " << mySLR->binRMSRange << std::endl;
   output << "Surface Pressure = " << mySLR->surfacePressure << std::endl;
   output << "Surface Temp = " << mySLR->surfaceTemp << std::endl;
   output << "Relative Humidity = " << mySLR->relativeHumidity << std::endl;
   output << "Num Raw Ranges = " << mySLR->numRawRanges << std::endl;
   output << "Data Release Flag = " << mySLR->dataReleaseFlag << std::endl;
   output << "Raw Range Factor = " << mySLR->rawRangeFactor << std::endl;
   output << "NPD Window Indicator 2 = " << mySLR->normalPointWindowIndicator2 << std::endl;
   output << "Signal to Noise Ratio = " << mySLR->signalToNoiseRatio << std::endl;
   output << "Burst Cal Sys Delay = " << mySLR->burstCalSysDelay << std::endl;
   output << "Signal Strength Indicator = " << mySLR->signalStrength << std::endl;
   output << "Angle Origin Indicator = " << mySLR->angleOriginIndicator << std::endl;
   output << "Azimuth = " << mySLR->az << std::endl;
   output << "Elevation = " << mySLR->el << std::endl;
   output << "-----------------------------" << std::endl;
   output << "SLR Type = " << (mySLR->slrHeader)->slrType << std::endl;
   output << "ILRS Satnum = " << (mySLR->slrHeader)->ilrsSatnum << std::endl;
   output << "Year = " << (mySLR->slrHeader)->year << std::endl;
   output << "DOY = " << (mySLR->slrHeader)->dayOfYear << std::endl;
   output << "CDP Pad ID = " << (mySLR->slrHeader)->cdpPadID << std::endl;
   output << "CDP Sys Num = " << (mySLR->slrHeader)->cdpSysNum << std::endl;
   output << "CDP Occupancy Num = " << (mySLR->slrHeader)->cdpOccupancySequenceNum << std::endl;
   output << "Wavelength = " << (mySLR->slrHeader)->wavelength << std::endl;
   output << "Cal Sys Delay = " << (mySLR->slrHeader)->calSysDelay << std::endl;
   output << "Cal Delay Shift = " << (mySLR->slrHeader)->calDelayShift << std::endl;
   output << "Sys Delay = " << (mySLR->slrHeader)->rmsSysDelay << std::endl;
   output << "NPD Window Indicator = " << (mySLR->slrHeader)->normalPointWindowIndicator << std::endl;
   output << "Epoch Scale Indicator = " << (mySLR->slrHeader)->epochTimeScaleIndicator << std::endl;
   output << "SysCal Indicator = " << (mySLR->slrHeader)->sysCalMethodIndicator << std::endl;
   output << "SCH Indicator = " << (mySLR->slrHeader)->schIndicator << std::endl;
   output << "SCI Indicator = " << (mySLR->slrHeader)->sciIndicator << std::endl;
   output << "Pass RMS = " << (mySLR->slrHeader)->passRMS << std::endl;
   output << "Data Quality Indicator = " << (mySLR->slrHeader)->dataQualAssessmentIndicator << std::endl;
   output << "Format Revision Num = " << (mySLR->slrHeader)->formatRevisionNum << std::endl;   
   output << "******************************************************" << std::endl;

   //output << setw(w) << setprecision(p) << prefix << a[i];
   
   return output;
}