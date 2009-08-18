//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRData
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
 * Implements DataFile base class to read files written in the SLR format.
 *
 */
//------------------------------------------------------------------------------

#include <ProcessSLRData.hpp>

//#define DEBUG_SLR_DATA

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
bool ProcessSLRData::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Make sure that the slrData vector has space reserved for
        // a minimum number of observations. This ensures that the
        // compiler does not unnecessarily reallocate the vector storage too-often.
        // The function reserve() will ensure that we have room for at least 1000
        // elements. If the vector already has room for the required number of elements,
        // reserve() does nothing. In other words, reserve() will grow the allocated
        // storage of the vector, if necessary, but will never shrink it.
        slrData.reserve(1000);
        slrHeader.reserve(1000);
    
        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        slr_header *mySLRheader = new slr_header;
        slr_obtype *mySLR = new slr_obtype;

        // Locate the start of the first header record
        // The flag indicates whether the marker line containing
        // 88888 for engineering data or 99999 for real data
        // has been encountered. For the first read, the flag is set to 0
        // afterwards, flag is set to either 88888 or 99999 depending
        // upon which marker line was found while reading in data.
        Integer flag = 0;
        FindSLRHeaderLine(mySLRheader,flag);
    
        while (!IsEOF())
        {

            // We now have the first header record stored in mySLRheader
            // Call the GetData function to populate mySLR
            // We pass the header record so that we know how to process
            // the data records as there are two different formats
            if (GetData(mySLRheader,mySLR))
            {
                // Associate this data point with the current header index
                mySLR->headerVectorIndex = i_h;

                // Push this data point onto the stack.
                slrData.push_back(mySLR);
            }
        
            // Allocate another struct in memory
            mySLR = new slr_obtype;

        }

        // Set data iterator to beginning of vector container
        i = slrData.begin();

        // Reset the header iterator to the beginning of the vector container
        i_h = slrHeader.begin();

        #ifdef DEBUG_SLR_DATA

            FILE * outFile;
            outFile = fopen("slr.output","w");

            // Output to file to make sure all the data is properly stored
            for (std::vector<slr_obtype*>::const_iterator j=slrData.begin(); j!=slrData.end(); ++j)
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
                fprintf(outFile,"SLR Type = %d\n",(*(*j)->headerVectorIndex)->slrType);
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
                fprintf(outFile,"Sys Delay = %d\n",(*(*j)->headerVectorIndex)->rmsSysDelay);
                fprintf(outFile,"NPD Window Indicator = %d\n",(*(*j)->headerVectorIndex)->normalPointWindowIndicator);
                fprintf(outFile,"Epoch Scale Indicator = %d\n",(*(*j)->headerVectorIndex)->epochTimeScaleIndicator);
                fprintf(outFile,"SysCal Indicator = %d\n",(*(*j)->headerVectorIndex)->sysCalMethodIndicator);
                fprintf(outFile,"SCH Indicator = %d\n",(*(*j)->headerVectorIndex)->schIndicator);
                fprintf(outFile,"SCI Indicator = %d\n",(*(*j)->headerVectorIndex)->sciIndicator);
                fprintf(outFile,"Pass RMS = %d\n",(*(*j)->headerVectorIndex)->passRMS);
                fprintf(outFile,"Data Quality Indicator = %d\n",(*(*j)->headerVectorIndex)->dataQualAssessmentIndicator);
                fprintf(outFile,"Format Revision Num = %d\n",(*(*j)->headerVectorIndex)->formatRevisionNum);
                fprintf(outFile,"\n******************************************************\n");

            }

            fclose(outFile);

        #endif

    }
    else if (pcrecpp::RE("^[Ww].*").FullMatch(readWriteMode))
    {
        // Currently do nothing if writing
        // wait to write stuff

    }
    else
    {
        throw DataFileException("Invalid Read/Write mode: " + readWriteMode);
        MessageInterface::ShowMessage("Invalid Read/Write mode: " + readWriteMode);
    }

    return true;

}


//------------------------------------------------------------------------------
//  ProcessSLRData() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessSLRData structures 
 */
ProcessSLRData::ProcessSLRData(const std::string &itsName) :
	DataFile ("SLRDataFile", itsName) 
{
   objectTypeNames.push_back("SLRDataFile");
   fileFormatName = "SLR";
   fileFormatID = 1;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessSLRData::~ProcessSLRData() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessSLRData::~ProcessSLRData() 
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessSLRData.
 *
 * @return clone of the ProcessSLRData.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessSLRData::Clone() const
{
   GmatBase *clone = new ProcessSLRData(*this);
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
bool ProcessSLRData::IsParameterReadOnly(const Integer id) const
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
bool ProcessSLRData::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

////------------------------------------------------------------------------------
//// template <class T> bool ProcessSLRData::from_string(T& t,
////		   const std::string& s,
////                 std::ios_base& (*f)(std::ios_base&))
////------------------------------------------------------------------------------
///**
// * Typesafe conversion from string to integer, float, etc
// */
////------------------------------------------------------------------------------
//
//template <class T> bool ProcessSLRData::from_string(T& t, const std::string& s,
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
std::string ProcessSLRData::Ilrs2Cospar(std::string ilrsSatnum)
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
bool ProcessSLRData::AdvanceToNextOb() {

    ++i;
    if (i==slrData.end()) return false;

    return true;

}

//------------------------------------------------------------------------------
// bool BackUpToPreviousOb()
//------------------------------------------------------------------------------
/**
 * Returns the previous observation from the vector container.
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::BackUpToPreviousOb() {

    --i;
    if (i==slrData.begin()) return false;

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
 bool ProcessSLRData::CheckDataAvailability(const std::string str) const
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
// bool GetData(slr_header *mySLRheader, slr_obtype *mySLRdata)
//------------------------------------------------------------------------------
/** 
 * Obtains the header line of SLR data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::GetData(slr_header *mySLRheader, slr_obtype *mySLRdata)
{

    // Read a line from file
    std::string line = ReadLineFromFile();
    line = Trim(line);

    // On the first pass through, we have already discovered the first
    // header record, but we must still check each line to see when/if
    // we encounter a new header record
    // This is supposed to be five digits but sometimes it is less
    if (line.size() <= 5 && pcrecpp::RE("^9+$").FullMatch(line))
    {
        Integer flag = 99999;

        // create a new header struct in memory
        slr_header *mySLRheader = new slr_header;

        FindSLRHeaderLine(mySLRheader,flag);

    }
    else if (line.size() <= 5 && pcrecpp::RE("^8+$").FullMatch(line))
    {
        Integer flag = 88888;

        // create a new header struct in memory
        slr_header *mySLRheader = new slr_header;

        FindSLRHeaderLine(mySLRheader,flag);

    }

    // Parse the data record
    // We pass the header record so that we know wether the data records
    // are real data or sampled engineering data. These two kinds of data
    // records have slightly different formats.
    return GetSLRData(line,mySLRheader,mySLRdata);
    
}

//------------------------------------------------------------------------------
// bool FindSLRHeaderLine(slr_header *mySLRheader, bool &flag)
//------------------------------------------------------------------------------
/** 
 * The routine locates the start of an SLR data block and then obtains the 
 * header line of SLR data.
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::FindSLRHeaderLine(slr_header *mySLRheader, Integer &flag )
{

    // Initialize headerType variable to 0
    // The header type discriminates between real SLR data
    // and so-called "engineering" or simulated SLR data records.
    Integer headerType = 0;
    
    // Now we must read in lines from the SLR data file until we
    // encounter the data markers 99999 for real SLR data or 88888
    // for sampled engineering data. The data header line will be the
    // line immediately following.

    // In some cases, we have already encountered the data record flag
    // 88888 or 99999. In this case, we can skip over checking for the flag.

    if (flag == 0)
    {
        do
        {
            // Read in a line
            std::string line = ReadLineFromFile();
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
        
        } while ( headerType != 99999 && headerType != 88888 && !IsEOF() );

    }
    else
    {
        headerType = flag;
    }

    // Now we can process the header record
    if (headerType == 99999 || headerType == 88888) 
    {
	    
	// set SLR type variable so that we know how
	// to process the rest of the data.
	mySLRheader->slrType = headerType;

        // Push a pointer to the header record onto the stack
        slrHeader.push_back(mySLRheader);

        // Set the header vector container index
        if (flag == 0)
        {
            i_h = slrHeader.begin();
        }
        else
        {
            i_h++;
        }
	
	// read header line
	std::string headerline = ReadLineFromFile();

	return GetSLRHeader(headerline,mySLRheader);

    } 
    else
    {
	return false;
    }

}

//------------------------------------------------------------------------------
// bool GetSLRHeader(std::string lff, slr_header *mySLRheader)
//------------------------------------------------------------------------------
/** 
 * Extracts header information from the compact SLR Normal Point Data format.
 *
 * Each set of observations from a given station has one header record
 * associated with it. This header contains the ground station identifiers,
 * calibration information, and the date.
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::GetSLRHeader(std::string &lff, slr_header *mySLRheader)
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
    mySLRheader->ilrsSatnum = Trim(lff2.substr(0,7));

    // Extract two digit year
    if (!from_string<int>(mySLRheader->year,lff2.substr(7,2),std::dec)) return false;
    
    // Put year in four digit format
    if ( mySLRheader->year < 50 )
    {
	mySLRheader->year += 2000;
    }
    else
    {
	mySLRheader->year += + 1900;
    }
    
    if (!from_string<int>(mySLRheader->dayOfYear,lff2.substr(9,3),std::dec)) return false;
    if (!from_string<int>(mySLRheader->cdpPadID,lff2.substr(12,4),std::dec)) return false;
    if (!from_string<int>(mySLRheader->cdpSysNum,lff2.substr(16,2),std::dec)) return false;
    if (!from_string<int>(mySLRheader->cdpOccupancySequenceNum,lff2.substr(18,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff2.substr(20,4),std::dec)) return false;
    mySLRheader->wavelength = itemp;
    // Convert wavelength to nanometers.
    // 3000 - 9999 is units of 0.1 nanometers
    // 1000 - 2999 is units of 1.0 nanometers so no conversion needed.
    if (mySLRheader->wavelength >= 3000 && mySLRheader->wavelength <= 9999)
    {
	mySLRheader->wavelength *= 0.1;
    }
    
    if (!from_string<int>(mySLRheader->calSysDelay,lff2.substr(24,8),std::dec)) return false;
    if (!from_string<int>(mySLRheader->calDelayShift,lff2.substr(32,6),std::dec)) return false;
    if (!from_string<int>(mySLRheader->rmsSysDelay,lff2.substr(38,4),std::dec)) return false;
    if (!from_string<int>(mySLRheader->normalPointWindowIndicator,lff2.substr(42,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->epochTimeScaleIndicator,lff2.substr(43,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->sysCalMethodIndicator,lff2.substr(44,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->schIndicator,lff2.substr(45,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->sciIndicator,lff2.substr(46,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->passRMS,lff2.substr(47,4),std::dec)) return false;

    if (pcrecpp::RE("^\\s+$").FullMatch(lff2.substr(51,1))) 
    {    
	mySLRheader->dataQualAssessmentIndicator = 0;
    }
    else
    {
	if (!from_string<int>(mySLRheader->dataQualAssessmentIndicator,lff2.substr(51,1),std::dec)) return false;
    }
    
    if (pcrecpp::RE("^\\s+$").FullMatch(lff2.substr(54,1))) 
    {
	mySLRheader->formatRevisionNum = 0;
    }
    else
    {
        if (!from_string<int>(mySLRheader->formatRevisionNum,lff2.substr(54,1),std::dec)) return false;
    }

    return true;    
    
}

//------------------------------------------------------------------------------
// bool GetSLRData(std::string lff, slr_header *mySLRheader,
//                     slr_obtype *mySLRdata)
//------------------------------------------------------------------------------
/** 
 * Converts the compact SLR Normal Point Data format into usable numbers.
 *
 * Note that the SLRType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------
bool ProcessSLRData::GetSLRData(std::string &lff, slr_header *mySLRheader,
				     slr_obtype *mySLRdata)
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
    //mySLRdata->slrHeader = mySLRheader;

    // Initialize variables
    mySLRdata->timeOfLaserFiring = 0.0;
    mySLRdata->twoWayTimeOfFlight = 0.0;
    mySLRdata->surfacePressure = 0.0;
    mySLRdata->surfaceTemp = 0.0;
    mySLRdata->relativeHumidity = 0;
    mySLRdata->numRawRanges = 0;
    mySLRdata->dataReleaseFlag = 0;
    mySLRdata->rawRangeFactor = 0;
    mySLRdata->normalPointWindowIndicator2 = 0;
    mySLRdata->signalToNoiseRatio = 0;
    mySLRdata->burstCalSysDelay = 0;
    mySLRdata->signalStrength = 0;
    mySLRdata->angleOriginIndicator = 0;
    mySLRdata->az = 0.0;
    mySLRdata->el = 0.0;
    
    switch(mySLRheader->slrType)
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
	    mySLRdata->timeOfLaserFiring = dtemp * 1.0e-7;
	    
	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySLRdata->twoWayTimeOfFlight = dtemp * 1.0e-12;

	    if (!from_string<int>(mySLRdata->binRMSRange,lff2.substr(24,7),std::dec)) return false;
	    
	    // Convert surface pressure to units of millibar
	    if (!from_string<int>(itemp,lff2.substr(31,5),std::dec)) return false;
	    mySLRdata->surfacePressure = itemp * 0.1;
	    
	    // Convert surface temp to units of degrees Kelvin
	    if (!from_string<int>(itemp,lff2.substr(36,4),std::dec)) return false;
	    mySLRdata->surfaceTemp = itemp * 0.1;
	    
	    if (!from_string<int>(mySLRdata->relativeHumidity,lff2.substr(40,3),std::dec)) return false;
	    if (!from_string<int>(mySLRdata->numRawRanges,lff2.substr(43,4),std::dec)) return false;
	    if (!from_string<int>(mySLRdata->dataReleaseFlag,lff2.substr(47,1),std::dec)) return false;
	    if (!from_string<int>(mySLRdata->rawRangeFactor,lff2.substr(48,1),std::dec)) return false;
	    // The Normal Point Window Indicator and the Signal to Noise Ratio
	    // are only used for LLR data
	    if (!from_string<int>(itemp,lff2.substr(49,1),std::dec)) return false;
	    mySLRdata->normalPointWindowIndicator2 = itemp;
	    if (!from_string<int>(itemp,lff2.substr(50,2),std::dec)) return false;
	    mySLRdata->signalToNoiseRatio =  itemp * 0.1;

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
	    mySLRdata->timeOfLaserFiring = dtemp * 1.0e-7;

	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time
	    mySLRdata->twoWayTimeOfFlight = dtemp * 1.0e-12;
	    
	    // Convert surface pressure to units of millibar
            if (!from_string<int>(itemp,lff2.substr(24,5),std::dec)) return false;
	    mySLRdata->surfacePressure = itemp * 0.1;
	    
	    // Convert surface temp to units of degrees Kelvin
            if (!from_string<int>(itemp,lff2.substr(29,4),std::dec)) return false;
	    mySLRdata->surfaceTemp = itemp * 0.1;

            if (!from_string<int>(mySLRdata->relativeHumidity,lff2.substr(33,3),std::dec)) return false;
            if (!from_string<int>(mySLRdata->burstCalSysDelay,lff2.substr(36,8),std::dec)) return false;
            if (!from_string<int>(mySLRdata->signalStrength,lff2.substr(44,4),std::dec)) return false;
            if (!from_string<int>(mySLRdata->angleOriginIndicator,lff2.substr(48,1),std::dec)) return false;
            if (!from_string<int>(itemp,lff2.substr(49,7),std::dec)) return false;
	    // Convert az to degrees
	    mySLRdata->az = itemp * 1e-4;
            if (!from_string<int>(itemp,lff2.substr(56,6),std::dec)) return false;
	    // Convert el to degrees
	    mySLRdata->el = itemp * 1e-4;
                                                                                                                                                                                                                                                                                    
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
std::string ProcessSLRData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRDataReps))
   {
      return SLR_FILEFORMAT_DESCRIPTIONS[id];
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
std::string ProcessSLRData::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRDataReps))
   {
      return SLR_UNIT_DESCRIPTIONS[id];
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
Integer ProcessSLRData::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndSLRDataReps; i++)
   {
      if (str == SLR_FILEFORMAT_DESCRIPTIONS[i])
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
Gmat::ParameterType ProcessSLRData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSLRDataReps))
      return SLR_PARAMETER_TYPE[id];

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
std::string ProcessSLRData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer ProcessSLRData::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {
        case SLR_TYPE_ID:

            return (*(*i)->headerVectorIndex)->slrType;

        case SLR_YEAR_ID:

            return (*(*i)->headerVectorIndex)->year;

        case SLR_DAYOFYEAR_ID:

            return (*(*i)->headerVectorIndex)->dayOfYear;

        case SLR_CDPPADID_ID:

            return (*(*i)->headerVectorIndex)->cdpPadID;

        case SLR_CDPSYSNUM_ID:

            return (*(*i)->headerVectorIndex)->cdpSysNum;

        case SLR_CDPOCCUPANCYSEQUENCENUM_ID:

            return (*(*i)->headerVectorIndex)->cdpOccupancySequenceNum;

        case SLR_CALSYSDELAY_ID:

            return (*(*i)->headerVectorIndex)->calSysDelay;

        case SLR_CALDELAYSHIFT_ID:

            return (*(*i)->headerVectorIndex)->calDelayShift;

        case SLR_RMSSYSDELAY_ID:

            return (*(*i)->headerVectorIndex)->rmsSysDelay;

        case SLR_NORMALPOINTWINDOWINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->normalPointWindowIndicator;

        case SLR_EPOCHTIMESCALEINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->epochTimeScaleIndicator;

        case SLR_SYSCALMETHODINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->sysCalMethodIndicator;

        case SLR_SCHINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->schIndicator;

        case SLR_SCIINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->sciIndicator;

        case SLR_PASSRMS_ID:

            return (*(*i)->headerVectorIndex)->passRMS;

        case SLR_DATAQUALASSESSMENTINDICATOR_ID:

            return (*(*i)->headerVectorIndex)->dataQualAssessmentIndicator;

        case SLR_FORMATREVISIONNUM_ID:

            return (*(*i)->headerVectorIndex)->formatRevisionNum;

        case SLR_BINRMSRANGE_ID:

            return (*i)->binRMSRange;

        case SLR_RELATIVEHUMIDITY_ID:

            return (*i)->relativeHumidity;

        case SLR_NUMRAWRANGES_ID:

            return (*i)->numRawRanges;

        case SLR_DATARELEASEFLAG_ID:

            return (*i)->dataReleaseFlag;

        case SLR_RAWRANGEFACTOR_ID:

            return (*i)->rawRangeFactor;

        case SLR_NORMALPOINTWINDOWINDICATOR2_ID:

            return (*i)->normalPointWindowIndicator2;

        case SLR_BURSTCALSYSDELAY_ID:

            return (*i)->burstCalSysDelay;

        case SLR_SIGNALSTRENGTH_ID:

            return (*i)->signalStrength;

        case SLR_ANGLEORIGININDICATOR_ID:

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
Integer ProcessSLRData::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string ProcessSLRData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case SLR_ILRSSATNUM_ID:

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
std::string ProcessSLRData::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real ProcessSLRData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case SLR_WAVELENGTH_ID:

            return (*(*i)->headerVectorIndex)->wavelength;

        case SLR_TIMEOFLASERFIRING_ID:

            return (*i)->timeOfLaserFiring;

        case SLR_TWOWAYTIMEOFFLIGHT_ID:

            return (*i)->twoWayTimeOfFlight;

        case SLR_SURFACEPRESSURE_ID:

            return (*i)->surfacePressure;

        case SLR_SURFACETEMP_ID:

            return (*i)->surfaceTemp;

        case SLR_SIGNALTONOISERATIO_ID:

            return (*i)->signalToNoiseRatio;

        case SLR_AZIMUTH_ID:

            return (*i)->az;

        case SLR_ELEVATION_ID:

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
Real ProcessSLRData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// bool WriteMeasurementHeader(slr_obtype *mySLRheader)
//------------------------------------------------------------------------------
/**
 * Writes a SLR normal point to file
 *
 * @param <myTLEdata> the SLR struct to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::WriteMeasurementHeader(slr_header *mySLRheader)
{

    // TLE's have a maximum length of 68 characters plus a 1 character checksum
    ostringstream buffer;

    // Write the record type
    // 99999 for regular data
    // 88888 for engineering data
    *theFile << mySLRheader->slrType << endl;

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
    *theFile << buffer.str();
    *theFile << setw(2) << SLRCheckSum(buffer.str());
    *theFile << setw(1) << mySLRheader->formatRevisionNum << endl;

    return true;

}

//------------------------------------------------------------------------------
// bool WriteMeasurement(slr_obtype *mySLRdata)
//------------------------------------------------------------------------------
/**
 * Writes a SLR normal point to file
 *
 * @param <myTLEdata> the SLR struct to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::WriteMeasurement(slr_obtype *mySLRdata)
{

    // TLE's have a maximum length of 68 characters plus a 1 character checksum
    ostringstream buffer;

    // Output either standard data record or engineering data record
    if ((*mySLRdata->headerVectorIndex)->slrType == 99999)
    {
        // The time of laser firing and the two way time of flight
        // is too large of an integer to store in an INT or even a LONG INT
        // So we break them up into real and fractional parts ( minus the period
        // of course ) for output purposes
        int tOLF = mySLRdata->timeOfLaserFiring;
        int tOLF2 = (mySLRdata->timeOfLaserFiring - tOLF) * 1e7 + 0.5;
        buffer << setw(5) << setfill('0') << right << tOLF;
        buffer << setw(7) << setfill('0') << right << tOLF2;
        int tWTOF = mySLRdata->twoWayTimeOfFlight * 1e7;
        int tWTOF2 = (mySLRdata->twoWayTimeOfFlight * 1e7 - tWTOF) * 1e5 + 0.5;
        buffer << setw(7) << setfill('0') << right << tWTOF;
        buffer << setw(5) << setfill('0') << right << tWTOF2;
        buffer << setw(7) << setfill('0') << right << mySLRdata->binRMSRange;
        int sP = mySLRdata->surfacePressure*10 + 0.5;
        buffer << setw(5) << setfill('0') << right << sP;
        int sT = mySLRdata->surfaceTemp*10 + 0.5;
        buffer << setw(4) << setfill('0') << right << sT;
        buffer << setw(3) << setfill('0') << right << mySLRdata->relativeHumidity;
        buffer << setw(4) << setfill('0') << right << mySLRdata->numRawRanges;
        buffer << setw(1) << mySLRdata->dataReleaseFlag;
	buffer << setw(1) << mySLRdata->rawRangeFactor;
	buffer << setw(1) << mySLRdata->normalPointWindowIndicator2;
	buffer << setw(2) << setfill('0') << right << mySLRdata->signalToNoiseRatio;

        *theFile << buffer.str();
        *theFile << setw(2) << SLRCheckSum(buffer.str()) << endl;
    }
    else if ((*mySLRdata->headerVectorIndex)->slrType == 88888)
    {

        // The time of laser firing and the two way time of flight
        // is too large of an integer to store in an INT or even a LONG INT
        // So we break them up into real and fractional parts ( minus the period
        // of course ) for output purposes
        int tOLF = mySLRdata->timeOfLaserFiring;
        int tOLF2 = (mySLRdata->timeOfLaserFiring - tOLF) * 1e7 + 0.5;
        buffer << setw(5) << setfill('0') << right << tOLF;
        buffer << setw(7) << setfill('0') << right << tOLF2;
        int tWTOF = mySLRdata->twoWayTimeOfFlight * 1e7;
        int tWTOF2 = (mySLRdata->twoWayTimeOfFlight * 1e7 - tWTOF) * 1e5 + 0.5;
        buffer << setw(7) << setfill('0') << right << tWTOF;
        buffer << setw(5) << setfill('0') << right << tWTOF2;
        int sP = mySLRdata->surfacePressure*10 + 0.5;
        buffer << setw(5) << setfill('0') << right << sP;
        int sT = mySLRdata->surfaceTemp*10 + 0.5;
        buffer << setw(4) << setfill('0') << right << sT;
        buffer << setw(3) << setfill('0') << right << mySLRdata->relativeHumidity;
        buffer << setw(8) << setfill('0') << right << mySLRdata->burstCalSysDelay;
	buffer << setw(4) << setfill('0') << right << mySLRdata->signalStrength;
        buffer << setw(1) << mySLRdata->angleOriginIndicator;
        int az = mySLRdata->az*1e4 + 0.5;
        buffer << setw(6) << setfill('0') << right << az;
        int el = mySLRdata->el*1e4 + 0.5;
        buffer << setw(5) << setfill('0') << right << el;

        // unused zero filled columns
        buffer << "00000";

        *theFile << buffer.str();
        *theFile << setw(2) << SLRCheckSum(buffer.str()) << endl;
    }
    else
    {
        return false;
    }

    return true;

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
Integer ProcessSLRData::SLRCheckSum(const std::string &str)
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