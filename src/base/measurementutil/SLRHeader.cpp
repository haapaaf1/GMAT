#include "SLRHeader.hpp"

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
