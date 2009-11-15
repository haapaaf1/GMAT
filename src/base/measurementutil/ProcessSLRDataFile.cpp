//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRDataFile
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
 
#include <ProcessSLRDataFile.hpp>

#define DEBUG_SLR_DATA

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  ProcessSLRDataFile() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessSLRDataFile structures 
 */
ProcessSLRDataFile::ProcessSLRDataFile(const std::string &itsName) :
	DataFile ("SLRDataFile", itsName),
        currentHeader(NULL),
        lastHeaderWritten(NULL),
        isHeaderWritten(false)
{
   objectTypeNames.push_back("SLRDataFile");
   fileFormatName = "SLR";
   fileFormatID = 1;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessSLRDataFile::ProcessSLRDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessSLRDataFile objects
 */
//------------------------------------------------------------------------------
ProcessSLRDataFile::ProcessSLRDataFile(const ProcessSLRDataFile &SLRdf) :
        DataFile      (SLRdf),
        currentHeader(SLRdf.currentHeader),
        lastHeaderWritten(SLRdf.lastHeaderWritten),
        isHeaderWritten(SLRdf.isHeaderWritten)
{
}


//------------------------------------------------------------------------------
//  ProcessSLRDataFile::ProcessSLRDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessSLRDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessSLRDataFile& ProcessSLRDataFile::operator=(const ProcessSLRDataFile &SLRdf)
{
    if (&SLRdf == this)
	return *this;

    DataFile::operator=(SLRdf);
    currentHeader = SLRdf.currentHeader;
    lastHeaderWritten = SLRdf.lastHeaderWritten;
    isHeaderWritten = SLRdf.isHeaderWritten;

    return *this;

}

//------------------------------------------------------------------------------
//  ProcessSLRDataFile::~ProcessSLRDataFile() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessSLRDataFile::~ProcessSLRDataFile() 
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessSLRDataFile.
 *
 * @return clone of the ProcessSLRDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessSLRDataFile::Clone() const
{
   GmatBase *clone = new ProcessSLRDataFile(*this);
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
bool ProcessSLRDataFile::IsParameterReadOnly(const Integer id) const
{
   if (id == SHOWPOINTMODE_ID)  return true;
   if (id == SCIENTIFICMODE_ID)  return true;
   if (id == PRECISION_ID)  return true;
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
bool ProcessSLRDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the datafile object.
 */
//------------------------------------------------------------------------------
bool ProcessSLRDataFile::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {
    
        SLRObType *mySLR = new SLRObType;
		
        while (!IsEOF())
        {

	    // Now check for headers and process data accordingly
            if (GetData(mySLR))
                // Push this data point onto the stack.
                theData.push_back(mySLR);
        
            // Allocate another struct in memory
            mySLR = new SLRObType;

        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();


        #ifdef DEBUG_SLR_DATA

            fstream *outFile = new fstream;
            outFile->open("slr.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile << (SLRObType*)(*j) << std::endl;
            }

            outFile->close();

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
// bool GetData(ObType *mySLRdata)
//------------------------------------------------------------------------------
/** 
 * Obtains the header line of SLR data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSLRDataFile::GetData(ObType *mySLRData)
{
    if (mySLRData->GetTypeName() != "SLRObType") return false;

    // Re-cast the generic ObType pointer as a B3Obtype pointer
    SLRObType *mySLR = (SLRObType*)mySLRData;

    // read a line from file
    std::string line = Trim(ReadLineFromFile());

    // We must first check each line to see when/if
    // we encounter a new header record.
    // This is supposed to be five digits but sometimes it is less
    if (line.size() <= 5 && pcrecpp::RE("^9+$").FullMatch(line))
    {
        // create a new header struct in memory
	SLRHeader *mySLRHeader = new SLRHeader;

	// set SLR type variable so that we know how
	// to process the rest of the data.
	mySLRHeader->slrType = 99999;

	// Now get the data from the header record itself
	// Read the header line from file
	line = Trim(ReadLineFromFile());
	
	if (GetSLRHeader(line,mySLRHeader))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentHeader = mySLRHeader;
	    
	    // Read the following data line from file
	    line = Trim(ReadLineFromFile());	    
	}
	else
	{
	    // failure to read header line, abort
	    currentHeader = NULL;
	    return false;
	}
    }
    else if (line.size() <= 5 && pcrecpp::RE("^8+$").FullMatch(line))
    {
        // create a new header struct in memory
	SLRHeader *mySLRHeader = new SLRHeader;

	// set SLR type variable so that we know how
	// to process the rest of the data.
	mySLRHeader->slrType = 88888;

	// Read the header line from file
	line = Trim(ReadLineFromFile());
	
	if (GetSLRHeader(line,mySLRHeader))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentHeader = mySLRHeader;
	    
	    // Read the following data line from file
	    line = Trim(ReadLineFromFile());
	}
	else
	{
	    // failure to read header line, abort
	    currentHeader = NULL;
	    return false;
	}
    }
    
    // Parse the data record making sure that we have identified
    // a header record previously
    if (currentHeader == NULL)
	return false;
    else
    {
	mySLR->slrHeader = currentHeader;
	return GetSLRData(line,mySLR);
    }    
}

//------------------------------------------------------------------------------
// bool GetSLRHeader(std::string lff, SLRHeader *mySLRheader)
//------------------------------------------------------------------------------
/** 
 * Extracts header information from the compact SLR Normal Point Data format.
 *
 * Each set of observations from a given station has one header record
 * associated with it. This header contains the ground station identifiers,
 * calibration information, and the date.
 */
//------------------------------------------------------------------------------
bool ProcessSLRDataFile::GetSLRHeader(std::string lff, 
				      SLRHeader *mySLRheader)
{   
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not 
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    
    // Check to make sure that the line length is at
    // least 54 characters long
    if (lff.size() < 54) 
    {
	return false;
    }

    // Replace extraneous "-" with the number 0
    pcrecpp::RE("-").GlobalReplace("0",&lff);

    // If line contains anything but numbers skip
    if (!pcrecpp::RE("^\\d+$").FullMatch(lff)) 
    {
	return false;
    }

    // Extract ILRS Satellite Number which is different
    // from the International Designator and the U.S.
    // Space Catalog number
    mySLRheader->ilrsSatnum = Trim(lff.substr(0,7));

    // Extract two digit year
    if (!from_string<int>(mySLRheader->year,lff.substr(7,2),std::dec)) return false;
    
    // Put year in four digit format
    if ( mySLRheader->year < 50 )
    {
	mySLRheader->year += 2000;
    }
    else
    {
	mySLRheader->year += + 1900;
    }
    
    if (!from_string<int>(mySLRheader->dayOfYear,lff.substr(9,3),std::dec)) return false;
    if (!from_string<int>(mySLRheader->cdpPadID,lff.substr(12,4),std::dec)) return false;
    if (!from_string<int>(mySLRheader->cdpSysNum,lff.substr(16,2),std::dec)) return false;
    if (!from_string<int>(mySLRheader->cdpOccupancySequenceNum,lff.substr(18,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff.substr(20,4),std::dec)) return false;
    mySLRheader->wavelength = itemp;
    // Convert wavelength to nanometers.
    // 3000 - 9999 is units of 0.1 nanometers
    // 1000 - 2999 is units of 1.0 nanometers so no conversion needed.
    if (mySLRheader->wavelength >= 3000 && mySLRheader->wavelength <= 9999)
    {
	mySLRheader->wavelength *= 0.1;
    }
    
    if (!from_string<int>(mySLRheader->calSysDelay,lff.substr(24,8),std::dec)) return false;
    if (!from_string<int>(mySLRheader->calDelayShift,lff.substr(32,6),std::dec)) return false;
    if (!from_string<int>(mySLRheader->rmsSysDelay,lff.substr(38,4),std::dec)) return false;
    if (!from_string<int>(mySLRheader->normalPointWindowIndicator,lff.substr(42,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->epochTimeScaleIndicator,lff.substr(43,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->sysCalMethodIndicator,lff.substr(44,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->schIndicator,lff.substr(45,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->sciIndicator,lff.substr(46,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader->passRMS,lff.substr(47,4),std::dec)) return false;

    if (pcrecpp::RE("^\\s+$").FullMatch(lff.substr(51,1))) 
    {    
	mySLRheader->dataQualAssessmentIndicator = 0;
    }
    else
    {
	if (!from_string<int>(mySLRheader->dataQualAssessmentIndicator,lff.substr(51,1),std::dec)) return false;
    }
    
    if (pcrecpp::RE("^\\s+$").FullMatch(lff.substr(54,1))) 
    {
	mySLRheader->formatRevisionNum = 0;
    }
    else
    {
        if (!from_string<int>(mySLRheader->formatRevisionNum,lff.substr(54,1),std::dec)) return false;
    }

    return true;    
    
}

//------------------------------------------------------------------------------
// bool GetSLRData(std::string lff, SLRObType *mySLRdata)
//------------------------------------------------------------------------------
/** 
 * Converts the compact SLR Normal Point Data format into usable numbers.
 *
 * Note that the SLRType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------
bool ProcessSLRDataFile::GetSLRData(std::string lff, SLRObType *mySLRdata)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not 
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    double dtemp;

    switch((mySLRdata->slrHeader)->slrType)
    {
	
	case 99999:
    
	    // Check to make sure that the line length is at
	    // least 54 characters long
	    if (lff.size() < 54) 
	    {
		return false;	
	    }
	    
	    // Replace extraneous "-" with the number 0
	    pcrecpp::RE("-").GlobalReplace("0",&lff);

	    // If line contains anything but numbers skip
	    if (!pcrecpp::RE("^\\d+$").FullMatch(lff)) 
	    {
		return false;
	    }

	    if (!from_string<double>(dtemp,lff.substr(0,12),std::dec)) return false;
	    // The data spec provides an integer in 0.1 microseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySLRdata->timeOfLaserFiring = dtemp * 1.0e-7;
	    
	    if (!from_string<double>(dtemp,lff.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySLRdata->twoWayTimeOfFlight = dtemp * 1.0e-12;

	    if (!from_string<int>(mySLRdata->binRMSRange,lff.substr(24,7),std::dec)) return false;
	    
	    // Convert surface pressure to units of millibar
	    if (!from_string<int>(itemp,lff.substr(31,5),std::dec)) return false;
	    mySLRdata->surfacePressure = itemp * 0.1;
	    
	    // Convert surface temp to units of degrees Kelvin
	    if (!from_string<int>(itemp,lff.substr(36,4),std::dec)) return false;
	    mySLRdata->surfaceTemp = itemp * 0.1;
	    
	    if (!from_string<int>(mySLRdata->relativeHumidity,lff.substr(40,3),std::dec)) return false;
	    if (!from_string<int>(mySLRdata->numRawRanges,lff.substr(43,4),std::dec)) return false;
	    if (!from_string<int>(mySLRdata->dataReleaseFlag,lff.substr(47,1),std::dec)) return false;
	    if (!from_string<int>(mySLRdata->rawRangeFactor,lff.substr(48,1),std::dec)) return false;
	    // The Normal Point Window Indicator and the Signal to Noise Ratio
	    // are only used for LLR data
	    if (!from_string<int>(itemp,lff.substr(49,1),std::dec)) return false;
	    mySLRdata->normalPointWindowIndicator2 = itemp;
	    if (!from_string<int>(itemp,lff.substr(50,2),std::dec)) return false;
	    mySLRdata->signalToNoiseRatio =  itemp * 0.1;

	    break;
	    
	case 88888:

	    // Check to make sure that the line length is at
	    // least 69 characters long
	    if (lff.size() < 69) 
	    {
	        return false;
	    }	
	    // Replace extraneous "-" with the number 0
	    pcrecpp::RE("-").GlobalReplace("0",&lff);

	    // If line contains anything but numbers skip
	    if (!pcrecpp::RE("^\\d+$").FullMatch(lff)) 
	    {
		return false;
	    }

	    if (!from_string<double>(dtemp,lff.substr(0,12),std::dec)) return false;
	    // The data spec provides an integer in 0.1 microseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in seconds.
	    mySLRdata->timeOfLaserFiring = dtemp * 1.0e-7;

	    if (!from_string<double>(dtemp,lff.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time
	    mySLRdata->twoWayTimeOfFlight = dtemp * 1.0e-12;
	    
	    // Convert surface pressure to units of millibar
            if (!from_string<int>(itemp,lff.substr(24,5),std::dec)) return false;
	    mySLRdata->surfacePressure = itemp * 0.1;
	    
	    // Convert surface temp to units of degrees Kelvin
            if (!from_string<int>(itemp,lff.substr(29,4),std::dec)) return false;
	    mySLRdata->surfaceTemp = itemp * 0.1;

            if (!from_string<int>(mySLRdata->relativeHumidity,lff.substr(33,3),std::dec)) return false;
            if (!from_string<int>(mySLRdata->burstCalSysDelay,lff.substr(36,8),std::dec)) return false;
            if (!from_string<int>(mySLRdata->signalStrength,lff.substr(44,4),std::dec)) return false;
            if (!from_string<int>(mySLRdata->angleOriginIndicator,lff.substr(48,1),std::dec)) return false;
            if (!from_string<int>(itemp,lff.substr(49,7),std::dec)) return false;
	    // Convert az to degrees
	    mySLRdata->az = itemp * 1e-4;
            if (!from_string<int>(itemp,lff.substr(56,6),std::dec)) return false;
	    // Convert el to degrees
	    mySLRdata->el = itemp * 1e-4;
                                                                                                                                                                                                                                                                                    
	    break;
	    
	default:
	    
	    return false;
	    	
    }

    return true;    
    
}

//------------------------------------------------------------------------------
// bool WriteDataHeader(const SLRHeader *mySLRheader)
//------------------------------------------------------------------------------
/**
 * Writes a SLR normal point header to file
 *
 * @param <mySLRheader> the SLR header struct to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessSLRDataFile::WriteDataHeader(const ObType *mySLRData)
{

    if (mySLRData->GetTypeName() != "SLRObType") return false;

    SLRHeader *mySLRheader = ((SLRObType*)mySLRData)->slrHeader;

    if (!isHeaderWritten)
    {
        *theFile << mySLRheader;
        lastHeaderWritten = mySLRheader;
        isHeaderWritten = true;
    }
    else if (isHeaderWritten && mySLRheader != lastHeaderWritten)
    {
        isHeaderWritten = false;
        lastHeaderWritten = NULL;

        *theFile << mySLRheader;

        lastHeaderWritten = mySLRheader;
        isHeaderWritten = true;
    }
    return true;
}

//------------------------------------------------------------------------------
// bool WriteData(const ObType *mySLRdata)
//------------------------------------------------------------------------------
/**
 * Writes a SLR normal point to file
 *
 * @param <mySLRdata> the SLR struct to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessSLRDataFile::WriteData(const ObType *mySLRData)
{
    if (mySLRData->GetTypeName() == "SLRObType")
    {
        WriteDataHeader(mySLRData);
        *theFile << (SLRObType*)mySLRData;
        return true;
    }
    else
        return false;
}