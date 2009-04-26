//$Header$
//------------------------------------------------------------------------------
//                             ProcessSP3cData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/22
//
/**
 *
 * Implements DataFile base class to read files written in the SP3c format.
 *
 */
//------------------------------------------------------------------------------

#include <ProcessSP3cData.hpp>
#include "gmatdefs.hpp"
#include "StringUtil.hpp"           // for ToString()
#include "pcrecpp.h"

//---------------------------------
//  static data
//---------------------------------

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the datafile object.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::Initialize()
{
    DataFile::Initialize();

    std::ifstream myFile;
    if(!OpenFile(myFile))
    {
	throw DataFileException("Unable to open data file: " + dataFileName);
	MessageInterface::ShowMessage("Unable to open data file: " + dataFileName);
    }

    // Make sure that the b3Data vector has space reserved for
    // a minimum number of observations. This ensures that the
    // compiler does not unnecessarily reallocate the vector storage too-often.
    // The function reserve() will ensure that we have room for at least 1000
    // elemnts. If the vector already has room for the required number of elements,
    // reserve() does nothing. In other words, reserve() will grow the allocated
    // storage of the vector, if necessary, but will never shrink it.
    SP3cData.reserve(1000);
    SP3cHeader.reserve(1000);

    // Initialize individual data struct
    // This needs new memory allocation because
    // we are storing pointers to this data
    SP3c_header *mySP3cheader = new SP3c_header;
    SP3c_obtype *mySP3c = new SP3c_obtype;

    // Locate the start of the first header record
    // The flag indicates whether the marker line containing
    // 88888 for engineering data or 99999 for real data
    // has been encountered. For the first read, the flag is set to 0
    // afterwards, flag is set to either 88888 or 99999 depending
    // upon which marker line was found while reading in data.
    Integer flag = 0;
    FindSP3cHeaderLine(myFile,mySP3cheader,flag);

    while (!IsEOF(myFile))
    {

        if (GetData(myFile,mySP3cheader,mySP3c))
        {
            // Associate this data point with the current header index
            mySP3c->headerVectorIndex = i_h;

            // Push this data point onto the stack.
            SP3cData.push_back(mySP3c);
        }

        // Allocate another struct in memory
        mySP3c = new SP3c_obtype;

    }

    // Set data iterator to beginning of vector container
    i = SP3cData.begin();

    // Reset the header iterator to the beginning of the vector container
    i_h = SP3cHeader.begin();

    /*
    FILE * outFile;
    outFile = fopen("SP3c.output","w");

    // Output to file to make sure all the data is properly stored
    for (std::vector<SP3c_obtype*>::const_iterator j=SP3cData.begin(); j!=SP3cData.end(); ++j)
    {

	    // Output resulting struct data to file

	    fprintf(outFile,"Time of Firing = %16.12g\n",(*j)->timeOfLaserFiring);
	    fprintf(outFile,"Two Way Time of Flight = %16.12g\n",(*j)->twoWayTimeOfFlight);
	    fprintf(outFile,"RMS Range = %d\n",(*j)->binRMSRange);
	    fprintf(outFile,"Surface Pressure = %16.8g\n",(*j)->surfacePressure);
	    fprintf(outFile,"Surface Temp = %16.8g\n",(*j)->surfaceTemp);
	    fprintf(outFile,"Relative Humidity = %d\n",(*j)->relativeHumidity);
	    fprintf(outFile,"Num Raw Ranges = %d\n",(*j)->numRawRanges);
	    fprintf(outFile,"Data Release Flag = %d\n",(*j)->dataReleaseFlag);
	    fprintf(outFile,"Raw Range Factor = %d\n",(*j)->rawRangeFactor);
	    fprintf(outFile,"NPD Window Indicator 2 = %d\n",(*j)->normalPointWindowIndicator2);
	    fprintf(outFile,"Signal to Noise Ratio = %16.8g\n",(*j)->signalToNoiseRatio);
	    fprintf(outFile,"Burst Cal Sys Delay = %d\n",(*j)->burstCalSysDelay);
	    fprintf(outFile,"Signal Strength Indicator = %d\n",(*j)->signalStrength);
	    fprintf(outFile,"Angle Origin Indicator = %d\n",(*j)->angleOriginIndicator);
	    fprintf(outFile,"Azimuth = %16.8g\n",(*j)->az);
	    fprintf(outFile,"Elevation = %16.8g\n",(*j)->el);
	    fprintf(outFile,"\n-----------------------------\n");
            fprintf(outFile,"SP3c Type = %d\n",(*(*j)->headerVectorIndex)->SP3cType);
	    fprintf(outFile,"ILRS Satnum = %s\n",(*(*j)->headerVectorIndex)->ilrsSatnum.c_str());
	    std::string intlDesignator = Ilrs2Cospar((*(*j)->headerVectorIndex)->ilrsSatnum);
	    fprintf(outFile,"COSPAR Satnum = %s\n",intlDesignator.c_str());
	    fprintf(outFile,"Year = %d\n",(*(*j)->headerVectorIndex)->year);
	    fprintf(outFile,"DOY = %d\n",(*(*j)->headerVectorIndex)->dayOfYear);
	    fprintf(outFile,"CDP Pad ID = %d\n",(*(*j)->headerVectorIndex)->cdpPadID);
	    fprintf(outFile,"CDP Sys Num = %d\n",(*(*j)->headerVectorIndex)->cdpSysNum);
	    fprintf(outFile,"CDP Occupancy Num = %d\n",(*(*j)->headerVectorIndex)->cdpOccupancySequenceNum);
	    fprintf(outFile,"Wavelength = %16.8g\n",(*(*j)->headerVectorIndex)->wavelength);
	    fprintf(outFile,"Cal Sys Delay = %d\n",(*(*j)->headerVectorIndex)->calSysDelay);
	    fprintf(outFile,"Cal Delay Shift = %d\n",(*(*j)->headerVectorIndex)->calDelayShift);
	    fprintf(outFile,"NPD Window Indicator = %d\n",(*(*j)->headerVectorIndex)->normalPointWindowIndicator);
	    fprintf(outFile,"Sys Delay = %d\n",(*(*j)->headerVectorIndex)->rmsSysDelay);
	    fprintf(outFile,"Epoch Scale Indicator = %d\n",(*(*j)->headerVectorIndex)->epochTimeScaleIndicator);
	    fprintf(outFile,"SysCal Indicator = %d\n",(*(*j)->headerVectorIndex)->sysCalMethodIndicator);
	    fprintf(outFile,"SCH Indicator = %d\n",(*(*j)->headerVectorIndex)->schIndicator);
	    fprintf(outFile,"SCI Indicator = %d\n",(*(*j)->headerVectorIndex)->sciIndicator);
	    fprintf(outFile,"Pass RMS = %d\n",(*(*j)->headerVectorIndex)->passRMS);
	    fprintf(outFile,"Data Quality Indicator = %d\n",(*(*j)->headerVectorIndex)->dataQualAssessmentIndicator);
	    fprintf(outFile,"Format Revision Num = %d\n",(*(*j)->headerVectorIndex)->formatRevisionNum);
	    fprintf(outFile,"\n******************************************************\n");


    }
    */

    if (!CloseFile(myFile))
        return false;

    return true;

}


//------------------------------------------------------------------------------
//  ProcessSP3cData()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessSP3cData structures
 */
ProcessSP3cData::ProcessSP3cData(const std::string &itsName) :
	DataFile ("SP3cDataFile", itsName)
{
   objectTypeNames.push_back("SP3cDataFile");
   fileFormatName = "SP3c";
   fileFormatID = 1;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessSP3cData::~ProcessSP3cData()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessSP3cData::~ProcessSP3cData()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessSP3cData.
 *
 * @return clone of the ProcessSP3cData.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessSP3cData::Clone() const
{
   GmatBase *clone = new ProcessSP3cData(*this);
   return (clone);
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool ProcessSP3cData::IsParameterReadOnly(const Integer id) const
{
   if (id == NUMLINES_ID)  return true;
   if (id == FILEFORMAT_ID)  return true;
   return GmatBase::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool ProcessSP3cData::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

////------------------------------------------------------------------------------
//// template <class T> bool ProcessSP3cData::from_string(T& t,
////		   const std::string& s,
////                 std::ios_base& (*f)(std::ios_base&))
////------------------------------------------------------------------------------
///**
// * Typesafe conversion from string to integer, float, etc
// */
////------------------------------------------------------------------------------
//
//template <class T> bool ProcessSP3cData::from_string(T& t, const std::string& s,
//                 std::ios_base& (*f)(std::ios_base&))
//{
//  std::istringstream iss(s);
//  return !(iss >> f >> t).fail();
//}

//------------------------------------------------------------------------------
// std::string Ilrs2Cospar(std::string ilrsSatnum)
//------------------------------------------------------------------------------
/**
 * Convert ILRS Satellite Number to COSPAR International Designator
 *
 * ILRS Satellite Identifier - 7 digit number based on COSPAR
 * Note: COSPAR ID to ILRS Satellite Identification Algorithm
 *
 * COSPAR ID Format: (YYYY-XXXA)
 *
 * YYYY is the four digit year when the launch vehicle was put in orbit
 * XXX is the sequential launch vehicle number for that year
 * A is the alpha numeric sequence number within a launch
 * Example: LAGEOS-1 COSPAR ID is 1976-039A
 * Explanation: LAGEOS-1 launch vehicle wasplaced in orbit in 1976;
 * was the 39th launch in that year; and LAGEOS-1 was the first object
 * injected into orbit from this launch.
 *
 * ILRS Satellite Identification Format: (YYXXXAA), based on the COSPAR ID
 * Where YY is the two digit year when the launch vehicle was put in orbit
 * Where XXX is the sequential launch vehicle number for that year
 * AA is the numeric sequence number within a launch
 * Example: LAGEOS-1 ILRS Satellite ID is 7603901
 */
//------------------------------------------------------------------------------
std::string ProcessSP3cData::Ilrs2Cospar(std::string ilrsSatnum)
{

    int year;

    from_string<int>(year,ilrsSatnum.substr(0,2),std::dec);

    if ( year < 50 )
    {
	year += 2000;
    } else {
	year += 1900;
    }

    std::string launchalpha;

    int index;
    from_string<int>(index,ilrsSatnum.substr(5,2),std::dec);

    static const char alpha[26] = {'A','B','C','D','E','F','G','H','I','J','K',
                  'L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};

    if (index <= 26)
    {

	// Account for zero indexed array so subtract 1
	launchalpha = alpha[index-1];

    }
    else
    {

	int index2 = -1;

	while (index > 26)
	{

	    index -= 26;
	    index2++;

	}

	launchalpha = alpha[index2] + alpha[index];

    }

    return GmatStringUtil::ToString(year,2) + ilrsSatnum.substr(2,3) + launchalpha;

}

//------------------------------------------------------------------------------
// bool AdvanceToNextOb()
//------------------------------------------------------------------------------
/**
 * Returns the next observation from the vector container.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::AdvanceToNextOb() {

    ++i;
    if (i==SP3cData.end()) return false;

    return true;

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
 bool ProcessSP3cData::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSP3cDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(SP3c_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// bool GetData(std::ifstream &theFile, SP3c_header *mySP3cheader, SP3c_obtype *mySP3cdata)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of SP3c data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetData(std::ifstream &theFile, SP3c_header *mySP3cheader, SP3c_obtype *mySP3cdata)
{

    // Read a line from file
    std::string line = ReadLineFromFile(theFile);
    line = Trim(line);

    // Check to see if we encountered a new header record.
    // This is supposed to be five digits but sometimes it is less
    if (line.size() <= 5 && pcrecpp::RE("^9+$").FullMatch(line))
    {
        Integer flag = 99999;

        // create a new header struct in memory
        SP3c_header *mySP3cheader = new SP3c_header;

        FindSP3cHeaderLine(theFile,mySP3cheader,flag);

    }
    else if (line.size() <= 5 && pcrecpp::RE("^8+$").FullMatch(line))
    {
        Integer flag = 88888;

        // create a new header struct in memory
        SP3c_header *mySP3cheader = new SP3c_header;

        FindSP3cHeaderLine(theFile,mySP3cheader,flag);

    }

    // Parse the data record
    // We pass the header record so that we know wether the data records
    // are real data or sampled engineering data. These two kinds of data
    // records have slightly different formats.
    return GetSP3cData(line,mySP3cheader,mySP3cdata);

}

//------------------------------------------------------------------------------
// bool FindSP3cHeaderLine(std::ifstream &theFile, SP3c_header *mySP3cheader,
//                          bool &flag)
//------------------------------------------------------------------------------
/**
 * The routine locates the start of an SP3c data block and then obtains the
 * header line of SP3c data.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::FindSP3cHeaderLine(std::ifstream &theFile,
                                       SP3c_header *mySP3cheader, Integer &flag )
{

    // Initialize headerType variable to 0
    // The header type discriminates between real SP3c data
    // and so-called "engineering" or simulated SP3c data records.
    Integer headerType = 0;

    // Now we must read in lines from the SP3c data file until we
    // encounter the data markers 99999 for real SP3c data or 88888
    // for sampled engineering data. The data header line will be the
    // line immediately following.

    if (flag == 0)
    {
        do
        {
            // Read in a line
            std::string line = ReadLineFromFile(theFile);
            line = Trim(line);

            // This is supposed to be five digits but sometimes it is less
            if (line.size() <= 5 && pcrecpp::RE("^9+$").FullMatch(line))
            {
                headerType = 99999;
            }
            else if (line.size() <= 5 && pcrecpp::RE("^8+$").FullMatch(line))
            {
                headerType = 88888;
            }

        } while ( headerType != 99999 && headerType != 88888 && !IsEOF(theFile) );

    }
    else
    {
        headerType = flag;
    }

    if (headerType == 99999 || headerType == 88888)
    {

	// set SP3c type variable so that we know how
	// to process the rest of the data.
	mySP3cheader->SP3cType = headerType;

        // Push a pointer to the header record onto the stack
        SP3cHeader.push_back(mySP3cheader);

        // Set the header vector container index
        if (flag == 0)
        {
            i_h = SP3cHeader.begin();
        }
        else
        {
            i_h++;
        }

	// read header line
	std::string headerline = ReadLineFromFile(theFile);

	return GetSP3cHeader(headerline,mySP3cheader);

    }
    else
    {
	return false;
    }

}

//------------------------------------------------------------------------------
// bool GetSP3cHeader(std::string lff, SP3c_header *mySP3cheader)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the compact SP3c Normal Point Data format.
 *
 * Each set of observations from a given station has one header record
 * associated with it. This header contains the ground station identifiers,
 * calibration information, and the date.
 */
//------------------------------------------------------------------------------

bool ProcessSP3cData::GetSP3cHeader(std::string &lff, SP3c_header *mySP3cheader)
{

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;

    // Remove any leading or trailing whitespace
    std::string lff2 = Trim(lff);

    // Check to make sure that the line length is at
    // least 54 characters long
    if (lff2.size() < 54)
    {
	return false;
    }

    // Replace extraneous "-" with the number 0
    pcrecpp::RE("-").GlobalReplace("0",&lff2);

    // If line contains anything but numbers skip
    if (!pcrecpp::RE("^\\d+$").FullMatch(lff2))
    {
	return false;
    }

    // Extract ILRS Satellite Number which is different
    // from the International Designator and the U.S.
    // Space Catalog number
    mySP3cheader->ilrsSatnum = Trim(lff2.substr(0,7));

    // Extract two digit year
    if (!from_string<int>(mySP3cheader->year,lff2.substr(7,2),std::dec)) return false;

    // Put year in four digit format
    if ( mySP3cheader->year < 50 )
    {
	mySP3cheader->year += 2000;
    }
    else
    {
	mySP3cheader->year += + 1900;
    }

    if (!from_string<int>(mySP3cheader->dayOfYear,lff2.substr(9,3),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->cdpPadID,lff2.substr(12,4),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->cdpSysNum,lff2.substr(16,2),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->cdpOccupancySequenceNum,lff2.substr(18,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff2.substr(20,4),std::dec)) return false;
    mySP3cheader->wavelength = itemp;
    // Convert wavelength to nanometers.
    // 3000 - 9999 is units of 0.1 nanometers
    // 1000 - 2999 is units of 1.0 nanometers so no conversion needed.
    if (mySP3cheader->wavelength >= 3000 && mySP3cheader->wavelength <= 9999)
    {
	mySP3cheader->wavelength *= 0.1;
    }

    if (!from_string<int>(mySP3cheader->calSysDelay,lff2.substr(24,8),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->calDelayShift,lff2.substr(32,6),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->rmsSysDelay,lff2.substr(38,4),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->normalPointWindowIndicator,lff2.substr(42,1),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->epochTimeScaleIndicator,lff2.substr(43,1),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->sysCalMethodIndicator,lff2.substr(44,1),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->schIndicator,lff2.substr(45,1),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->sciIndicator,lff2.substr(46,1),std::dec)) return false;
    if (!from_string<int>(mySP3cheader->passRMS,lff2.substr(47,4),std::dec)) return false;

    if (pcrecpp::RE("^\\s+$").FullMatch(lff2.substr(51,1)))
    {
	mySP3cheader->dataQualAssessmentIndicator = 0;
    }
    else
    {
	if (!from_string<int>(mySP3cheader->dataQualAssessmentIndicator,lff2.substr(51,1),std::dec)) return false;
    }

    if (pcrecpp::RE("^\\s+$").FullMatch(lff2.substr(54,1)))
    {
	mySP3cheader->formatRevisionNum = 0;
    }
    else
    {
        if (!from_string<int>(mySP3cheader->formatRevisionNum,lff2.substr(54,1),std::dec)) return false;
    }

    return true;

}

//------------------------------------------------------------------------------
// bool GetSP3cData(std::string lff, SP3c_header *mySP3cheader,
//                     SP3c_obtype *mySP3cdata)
//------------------------------------------------------------------------------
/**
 * Converts the compact SP3c Normal Point Data format into usable numbers.
 *
 * Note that the SP3cType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------

bool ProcessSP3cData::GetSP3cData(std::string &lff, SP3c_header *mySP3cheader,
				     SP3c_obtype *mySP3cdata)
{

    // Remove any leading or trailing whitespace
    std::string lff2 = Trim(lff);

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    double dtemp;

    // First keep track of which header record this data point
    // is associated with. This way we only have to store the header
    // record once and not have to keep two lists synced up.
    //mySP3cdata->SP3cHeader = mySP3cheader;

    // Initialize variables
    mySP3cdata->timeOfLaserFiring = 0.0;
    mySP3cdata->twoWayTimeOfFlight = 0.0;
    mySP3cdata->surfacePressure = 0.0;
    mySP3cdata->surfaceTemp = 0.0;
    mySP3cdata->relativeHumidity = 0;
    mySP3cdata->numRawRanges = 0;
    mySP3cdata->dataReleaseFlag = 0;
    mySP3cdata->rawRangeFactor = 0;
    mySP3cdata->normalPointWindowIndicator2 = 0;
    mySP3cdata->signalToNoiseRatio = 0;
    mySP3cdata->burstCalSysDelay = 0;
    mySP3cdata->signalStrength = 0;
    mySP3cdata->angleOriginIndicator = 0;
    mySP3cdata->az = 0.0;
    mySP3cdata->el = 0.0;

    switch(mySP3cheader->SP3cType)
    {

	case 99999:

	    // Check to make sure that the line length is at
	    // least 54 characters long
	    if (lff2.size() < 54)
	    {
		return false;
	    }

	    // Replace extraneous "-" with the number 0
	    pcrecpp::RE("-").GlobalReplace("0",&lff2);

	    // If line contains anything but numbers skip
	    if (!pcrecpp::RE("^\\d+$").FullMatch(lff2))
	    {
		return false;
	    }

	    if (!from_string<double>(dtemp,lff2.substr(0,12),std::dec)) return false;
	    // The data spec provides an integer in 0.1 microseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySP3cdata->timeOfLaserFiring = dtemp * 1.0e-7;

	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySP3cdata->twoWayTimeOfFlight = dtemp * 1.0e-12;

	    if (!from_string<int>(mySP3cdata->binRMSRange,lff2.substr(24,7),std::dec)) return false;

	    // Convert surface pressure to units of millibar
	    if (!from_string<int>(itemp,lff2.substr(31,5),std::dec)) return false;
	    mySP3cdata->surfacePressure = itemp * 0.1;

	    // Convert surface temp to units of degrees Kelvin
	    if (!from_string<int>(itemp,lff2.substr(36,4),std::dec)) return false;
	    mySP3cdata->surfaceTemp = itemp * 0.1;

	    if (!from_string<int>(mySP3cdata->relativeHumidity,lff2.substr(40,3),std::dec)) return false;
	    if (!from_string<int>(mySP3cdata->numRawRanges,lff2.substr(43,4),std::dec)) return false;
	    if (!from_string<int>(mySP3cdata->dataReleaseFlag,lff2.substr(47,1),std::dec)) return false;
	    if (!from_string<int>(mySP3cdata->rawRangeFactor,lff2.substr(48,1),std::dec)) return false;
	    // The Normal Point Window Indicator and the Signal to Noise Ratio
	    // are only used for LLR data
	    if (!from_string<int>(itemp,lff2.substr(49,1),std::dec)) return false;
	    mySP3cdata->normalPointWindowIndicator2 = itemp;
	    if (!from_string<int>(itemp,lff2.substr(50,2),std::dec)) return false;
	    mySP3cdata->signalToNoiseRatio =  itemp * 0.1;

	    break;

	case 88888:

	    // Check to make sure that the line length is at
	    // least 69 characters long
	    if (lff2.size() < 69)
	    {
	        return false;
	    }
	    // Replace extraneous "-" with the number 0
	    pcrecpp::RE("-").GlobalReplace("0",&lff2);

	    // If line contains anything but numbers skip
	    if (!pcrecpp::RE("^\\d+$").FullMatch(lff2))
	    {
		return false;
	    }

	    if (!from_string<double>(dtemp,lff2.substr(0,12),std::dec)) return false;
	    // The data spec provides an integer in 0.1 microseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in seconds.
	    mySP3cdata->timeOfLaserFiring = dtemp * 1.0e-7;

	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time
	    mySP3cdata->twoWayTimeOfFlight = dtemp * 1.0e-12;

	    // Convert surface pressure to units of millibar
            if (!from_string<int>(itemp,lff2.substr(24,5),std::dec)) return false;
	    mySP3cdata->surfacePressure = itemp * 0.1;

	    // Convert surface temp to units of degrees Kelvin
            if (!from_string<int>(itemp,lff2.substr(29,4),std::dec)) return false;
	    mySP3cdata->surfaceTemp = itemp * 0.1;

            if (!from_string<int>(mySP3cdata->relativeHumidity,lff2.substr(33,3),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->burstCalSysDelay,lff2.substr(36,8),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->signalStrength,lff2.substr(44,4),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->angleOriginIndicator,lff2.substr(48,1),std::dec)) return false;
            if (!from_string<int>(itemp,lff2.substr(49,7),std::dec)) return false;
	    // Convert az to degrees
	    mySP3cdata->az = itemp * 1e-4;
            if (!from_string<int>(itemp,lff2.substr(56,6),std::dec)) return false;
	    // Convert el to degrees
	    mySP3cdata->el = itemp * 1e-4;

	    break;

	default:

	    return false;

    }

    return true;

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
std::string ProcessSP3cData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
   {
      return SP3c_FILEFORMAT_DESCRIPTIONS[id];
   }
   return "";
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
std::string ProcessSP3cData::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
   {
      return SP3c_UNIT_DESCRIPTIONS[id];
   }
   return "";
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
Integer ProcessSP3cData::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cDataReps; i++)
   {
      if (str == SP3c_FILEFORMAT_DESCRIPTIONS[i])
         return i;
   }

   return -1;
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
Gmat::ParameterType ProcessSP3cData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
      return SP3c_PARAMETER_TYPE[id];

   return GmatBase::GetParameterType(id);
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
std::string ProcessSP3cData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {
        case SP3c_TYPE_ID:

            return (*(*i)->headerVectorIndex)->SP3cType;

        case SP3c_YEAR_ID:

            return (*(*i)->headerVectorIndex)->year;

        case SP3c_DAYOFYEAR_ID:

            return (*(*i)->headerVectorIndex)->dayOfYear;

        case SP3c_CDPPADID_ID:

            return (*(*i)->headerVectorIndex)->cdpPadID;

        case SP3c_CDPSYSNUM_ID:

            return (*(*i)->headerVectorIndex)->cdpSysNum;

        case SP3c_CDPOCCUPANCYSEQUENCENUM_ID:

            return (*(*i)->headerVectorIndex)->cdpOccupancySequenceNum;

        case SP3c_CALSYSDELAY_ID:

            return (*(*i)->headerVectorIndex)->calSysDelay;

        case SP3c_CALDELAYSHIFT_ID:

            return (*(*i)->headerVectorIndex)->calDelayShift;

        case SP3c_RMSSYSDELAY_ID:

            return (*(*i)->headerVectorIndex)->rmsSysDelay;

        case SP3c_NORMALPOINTWINDOWINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->normalPointWindowIndicator;

        case SP3c_EPOCHTIMESCALEINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->epochTimeScaleIndicator;

        case SP3c_SYSCALMETHODINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->sysCalMethodIndicator;

        case SP3c_SCHINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->schIndicator;

        case SP3c_SCIINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->sciIndicator;

        case SP3c_PASSRMS_ID:

            return (*(*i)->headerVectorIndex)->passRMS;

        case SP3c_DATAQUALASSESSMENTINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->dataQualAssessmentIndicator;

        case SP3c_FORMATREVISIONNUM_ID:

            return (*(*i)->headerVectorIndex)->formatRevisionNum;

        case SP3c_BINRMSRANGE_ID:

            return (*i)->binRMSRange;

        case SP3c_RELATIVEHUMIDITY_ID:

            return (*i)->relativeHumidity;

        case SP3c_NUMRAWRANGES_ID:

            return (*i)->numRawRanges;

        case SP3c_DATARELEASEFLAG_ID:

            return (*i)->dataReleaseFlag;

        case SP3c_RAWRANGEFACTOR_ID:

            return (*i)->rawRangeFactor;

        case SP3c_NORMALPOINTWINDOWINDICATOR2_ID:

            return (*i)->normalPointWindowIndicator2;

        case SP3c_BURSTCALSYSDELAY_ID:

            return (*i)->burstCalSysDelay;

        case SP3c_SIGNALSTRENGTH_ID:

            return (*i)->signalStrength;

        case SP3c_ANGLEORIGININDICATOR_ID:

            return (*i)->angleOriginIndicator;

       default:

            return -123456789;

    }

}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string ProcessSP3cData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case SP3c_ILRSSATNUM_ID:

            return (*(*i)->headerVectorIndex)->ilrsSatnum;

        default:

            return "";

    }
}


//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string ProcessSP3cData::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real ProcessSP3cData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case SP3c_WAVELENGTH_ID:

            return (*(*i)->headerVectorIndex)->wavelength;

        case SP3c_TIMEOFLASERFIRING_ID:

            return (*i)->timeOfLaserFiring;

        case SP3c_TWOWAYTIMEOFFLIGHT_ID:

            return (*i)->twoWayTimeOfFlight;

        case SP3c_SURFACEPRESSURE_ID:

            return (*i)->surfacePressure;

        case SP3c_SURFACETEMP_ID:

            return (*i)->surfaceTemp;

        case SP3c_SIGNALTONOISERATIO_ID:

            return (*i)->signalToNoiseRatio;

        case SP3c_AZIMUTH_ID:

            return (*i)->az;

        case SP3c_ELEVATION_ID:

            return (*i)->el;

        default:

            return -1234567.89;

    }

}


//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real ProcessSP3cData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

