#include "SLRHeader.hpp"
//---------------------------------
//  static data
//---------------------------------

const std::string SLRHeader::SLR_FILEFORMAT_DESCRIPTIONS[EndSLRHeaderDataReps] =
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
	"FormatRevisionNum"
};

const std::string SLRHeader::SLR_UNIT_DESCRIPTIONS[EndSLRHeaderDataReps] =
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
	""
};

const Gmat::ParameterType SLRHeader::SLR_PARAMETER_TYPE[EndSLRHeaderDataReps] =
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
	Gmat::INTEGER_TYPE
};

const bool SLRHeader::SLR_IS_REQUIRED[EndSLRHeaderDataReps] =
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
    true
};

//------------------------------------------------------------------------------
//  SLRHeader()
//------------------------------------------------------------------------------
/**
 * Constructor for the SLRHeader class
 */
//------------------------------------------------------------------------------
SLRHeader::SLRHeader() :
    slrType(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    ilrsSatnum(GmatBase::STRING_PARAMETER_UNDEFINED),
    year(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    dayOfYear(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    cdpPadID(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    cdpSysNum(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    cdpOccupancySequenceNum(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    wavelength(GmatBase::REAL_PARAMETER_UNDEFINED),
    calSysDelay(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    calDelayShift(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    rmsSysDelay(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    normalPointWindowIndicator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    epochTimeScaleIndicator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    sysCalMethodIndicator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    schIndicator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    sciIndicator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    passRMS(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    dataQualAssessmentIndicator(GmatBase::INTEGER_PARAMETER_UNDEFINED),
    formatRevisionNum(GmatBase::INTEGER_PARAMETER_UNDEFINED)
{
}

//------------------------------------------------------------------------------
//  SLRHeader(const SLRHeader &header)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
SLRHeader::SLRHeader(const SLRHeader &header) :
    slrType(header.slrType),
    ilrsSatnum(header.ilrsSatnum),
    year(header.year),
    dayOfYear(header.dayOfYear),
    cdpPadID(header.cdpPadID),
    cdpSysNum(header.cdpSysNum),
    cdpOccupancySequenceNum(header.cdpOccupancySequenceNum),
    wavelength(header.wavelength),
    calSysDelay(header.calSysDelay),
    calDelayShift(header.calDelayShift),
    rmsSysDelay(header.rmsSysDelay),
    normalPointWindowIndicator(header.normalPointWindowIndicator),
    epochTimeScaleIndicator(header.epochTimeScaleIndicator),
    sysCalMethodIndicator(header.sysCalMethodIndicator),
    schIndicator(header.schIndicator),
    sciIndicator(header.sciIndicator),
    passRMS(header.passRMS),
    dataQualAssessmentIndicator(header.dataQualAssessmentIndicator),
    formatRevisionNum(header.formatRevisionNum)
{
}

//---------------------------------------------------------------------------
//  SLRHeader& operator=(const SLRHeader &header)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <header> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const SLRHeader& SLRHeader::operator=(const SLRHeader &header)

{
    if (&header == this)
        return *this;

    slrType = header.slrType;
    ilrsSatnum = header.ilrsSatnum;
    year = header.year;
    dayOfYear = header.dayOfYear;
    cdpPadID = header.cdpPadID;
    cdpSysNum = header.cdpSysNum;
    cdpOccupancySequenceNum = header.cdpOccupancySequenceNum;
    wavelength = header.wavelength;
    calSysDelay = header.calSysDelay;
    calDelayShift = header.calDelayShift;
    rmsSysDelay = header.rmsSysDelay;
    normalPointWindowIndicator = header.normalPointWindowIndicator;
    epochTimeScaleIndicator = header.epochTimeScaleIndicator;
    sysCalMethodIndicator = header.sysCalMethodIndicator;
    schIndicator = header.schIndicator;
    sciIndicator = header.sciIndicator;
    passRMS = header.passRMS;
    dataQualAssessmentIndicator = header.dataQualAssessmentIndicator;
    formatRevisionNum = header.formatRevisionNum;

    return *this;
}

//------------------------------------------------------------------------------
//  ~SLRHeader()
//------------------------------------------------------------------------------
/**
 * Destructor for the SLRHeader class
 */
//------------------------------------------------------------------------------
SLRHeader::~SLRHeader()
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
std::string SLRHeader::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRHeaderDataReps))
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
std::string SLRHeader::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRHeaderDataReps))
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
Integer SLRHeader::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndSLRHeaderDataReps; i++)
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
Gmat::ParameterType SLRHeader::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRHeaderDataReps))
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
std::string SLRHeader::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer SLRHeader::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {
        case SLR_TYPE_ID:

            return slrType;

        case SLR_YEAR_ID:

            return year;

        case SLR_DAYOFYEAR_ID:

            return dayOfYear;

        case SLR_CDPPADID_ID:

            return cdpPadID;

        case SLR_CDPSYSNUM_ID:

            return cdpSysNum;

        case SLR_CDPOCCUPANCYSEQUENCENUM_ID:

            return cdpOccupancySequenceNum;

        case SLR_CALSYSDELAY_ID:

            return calSysDelay;

        case SLR_CALDELAYSHIFT_ID:

            return calDelayShift;

        case SLR_RMSSYSDELAY_ID:

            return rmsSysDelay;

        case SLR_NORMALPOINTWINDOWINDICATOR_ID:

            return normalPointWindowIndicator;

        case SLR_EPOCHTIMESCALEINDICATOR_ID:

            return epochTimeScaleIndicator;

        case SLR_SYSCALMETHODINDICATOR_ID:

            return sysCalMethodIndicator;

        case SLR_SCHINDICATOR_ID:

            return schIndicator;

        case SLR_SCIINDICATOR_ID:

            return sciIndicator;

        case SLR_PASSRMS_ID:

            return passRMS;

        case SLR_DATAQUALASSESSMENTINDICATOR_ID:

            return dataQualAssessmentIndicator;

        case SLR_FORMATREVISIONNUM_ID:

            return formatRevisionNum;

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
Integer SLRHeader::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string SLRHeader::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case SLR_ILRSSATNUM_ID:

            return ilrsSatnum;

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
std::string SLRHeader::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real SLRHeader::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case SLR_WAVELENGTH_ID:

            return wavelength;

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
Real SLRHeader::GetRealDataParameter(const std::string &label) const
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
bool SLRHeader::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

        case SLR_WAVELENGTH_ID:

            wavelength = value;
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
bool SLRHeader::SetDataParameter(const std::string &label, const Real &value)
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
bool SLRHeader::SetDataParameter(const Integer id, const Integer &value)
{

    switch (id)
    {
        case SLR_TYPE_ID:

            slrType = value;
            return true;

        case SLR_YEAR_ID:

            year = value;
            return true;

        case SLR_DAYOFYEAR_ID:

            dayOfYear = value;
            return true;

        case SLR_CDPPADID_ID:

            cdpPadID = value;
            return true;

        case SLR_CDPSYSNUM_ID:

            cdpSysNum = value;
            return true;

        case SLR_CDPOCCUPANCYSEQUENCENUM_ID:

            cdpOccupancySequenceNum = value;
            return true;

        case SLR_CALSYSDELAY_ID:

            calSysDelay = value;
            return true;

        case SLR_CALDELAYSHIFT_ID:

            calDelayShift = value;
            return true;

        case SLR_RMSSYSDELAY_ID:

            rmsSysDelay = value;
            return true;

        case SLR_NORMALPOINTWINDOWINDICATOR_ID:

            normalPointWindowIndicator = value;
            return true;

        case SLR_EPOCHTIMESCALEINDICATOR_ID:

            epochTimeScaleIndicator = value;
            return true;

        case SLR_SYSCALMETHODINDICATOR_ID:

            sysCalMethodIndicator = value;
            return true;

        case SLR_SCHINDICATOR_ID:

            schIndicator = value;
            return true;

        case SLR_SCIINDICATOR_ID:

            sciIndicator = value;
            return true;

        case SLR_PASSRMS_ID:

            passRMS = value;
            return true;

        case SLR_DATAQUALASSESSMENTINDICATOR_ID:

            dataQualAssessmentIndicator = value;
            return true;

        case SLR_FORMATREVISIONNUM_ID:

            formatRevisionNum = value;
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
bool SLRHeader::SetDataParameter(const std::string &label, const Integer &value)
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
bool SLRHeader::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {
        case SLR_ILRSSATNUM_ID:

            ilrsSatnum = value;
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
bool SLRHeader::SetDataParameter(const std::string &label, const std::string &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
//  Integer GetSLRType()
//------------------------------------------------------------------------------
/**
 * Retrieves the SLR type
 */
//------------------------------------------------------------------------------
Integer SLRHeader::GetSLRType()
{
    return slrType;
}

//------------------------------------------------------------------------------
//  Integer  SLRCheckSum(const std::string &str)
//------------------------------------------------------------------------------
/**
 * This method computes the checksum for a string of SLR data
 *
 * @param <str> String of data
 *
 * @return Integer checksum modulo 10
 */
//------------------------------------------------------------------------------
Integer SLRCheckSum(const std::string &str)
{

    Integer checksum = 0;
    int num = 0 ;

    for ( Integer pos = 0; pos < (Integer)str.length(); ++pos )
    {
        // If it's a number, add the number to the checksum
        // ignore everything else
        if (pcrecpp::RE("(\\d)").FullMatch(str.substr(pos,1),&num))
        {
            checksum += num;
            num = 0;
        }
    }

    return GmatMathUtil::Mod(checksum,100);

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, SLRHeader *mySLRheader)
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
std::ostream& operator<< (std::ostream &output, const SLRHeader *mySLRheader)
{
    // TLE's have a maximum length of 68 characters plus a 1 character checksum
    ostringstream buffer;

    // Write the record type
    // 99999 for regular data
    // 88888 for engineering data
    output << mySLRheader->slrType << endl;

    // Write the header record itself
    buffer << setw(7) << mySLRheader->ilrsSatnum;
    // Put year in two digit format
    if ( mySLRheader->year >= 2000 )
    {
        buffer << setw(2) << setfill('0') << right << mySLRheader->year-2000;
    }
    else
    {
        buffer << setw(2) << setfill('0') << right << mySLRheader->year-1900;
    }

    buffer << setw(3) << setfill('0') << right << mySLRheader->dayOfYear;
    buffer << setw(4) << mySLRheader->cdpPadID;
    buffer << setw(2) << mySLRheader->cdpSysNum;
    buffer << setw(2) << mySLRheader->cdpOccupancySequenceNum;
    if (mySLRheader->wavelength >= 300 && mySLRheader->wavelength < 1000)
    {
        int wv = mySLRheader->wavelength*10 + 0.5;
        buffer << setw(4) << setfill('0') << left << wv;
    }
    else if (mySLRheader->wavelength >= 1000 && mySLRheader->wavelength < 3000)
    {
        int wv = mySLRheader->wavelength + 0.5;
        buffer << setw(4) << setfill('0') << left << wv;
    }

    buffer << setw(8) << setfill('0') << right << mySLRheader->calSysDelay;
    buffer << setw(6) << setfill('0') << right << mySLRheader->calDelayShift;
    buffer << setw(4) << setfill('0') << right << mySLRheader->rmsSysDelay;
    buffer << setw(1) << mySLRheader->normalPointWindowIndicator;
    buffer << setw(1) << mySLRheader->epochTimeScaleIndicator;
    buffer << setw(1) << mySLRheader->sysCalMethodIndicator;
    buffer << setw(1) << mySLRheader->schIndicator;
    buffer << setw(1) << mySLRheader->sciIndicator;
    buffer << setw(4) << setfill('0') << right << mySLRheader->passRMS;
    buffer << setw(1) << mySLRheader->dataQualAssessmentIndicator;

    // Output buffer string to file along with checksum and format revision number
    output << buffer.str();
    output << setw(2) << SLRCheckSum(buffer.str());
    output << setw(1) << mySLRheader->formatRevisionNum << endl;

    return output;
}
