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
//  ProcessSLRData() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessSLRData structures 
 */
ProcessSLRData::ProcessSLRData(const std::string &itsName) :
	DataFile ("SLRDataFile", itsName)
{
   objectTypeNames.push_back("SLRDataFile");
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

//------------------------------------------------------------------------------
// bool GetData(std::ifstream &theFile, slr_header &mySLRheader, slr_obtype &mySLRdata)
//------------------------------------------------------------------------------
/** 
 * Obtains the header line of SLR data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::GetData(std::ifstream &theFile, slr_header &mySLRheader, slr_obtype &mySLRdata) 
{

    // Read a line from file
    std::string line = ReadLineFromFile(theFile);
    line = Trim(line);
    
    // Parse the data record
    // We pass the header record so that we know wether the data records
    // are real data or sampled engineering data. These two kinds of data
    // records have slightly different formats.
    return GetSLRData(line,mySLRheader,mySLRdata);
    
}

//------------------------------------------------------------------------------
// bool FindSLRHeaderLine(std::ifstream &theFile, slr_header &mySLRheader)
//------------------------------------------------------------------------------
/** 
 * The routine locates the start of an SLR data block and then obtains the 
 * header line of SLR data.
 */
//------------------------------------------------------------------------------
bool ProcessSLRData::FindSLRHeaderLine(std::ifstream &theFile, slr_header &mySLRheader ) 
{

    // Initialize headerType variable to 0
    // The header type discriminates between real SLR data
    // and so-called "engineering" or simulated SLR data records.
    Integer headerType = 0;
    
    // Now we must read in lines from the SLR data file until we
    // encounter the data markers 99999 for real SLR data or 88888
    // for sampled engineering data. The data header line will be the
    // line immediately following.
    
    do
    {
	// Read in a line
	std::string line = ReadLineFromFile(theFile);
	line = Trim(line);
    
	// This is supposed to be five digits but sometimes it is less
	if (line.size() < 5 && pcrecpp::RE("^9+$").FullMatch(line))
	{ 
	    headerType = 99999;
	}
	else if (line.size() < 5 && pcrecpp::RE("^8+$").FullMatch(line)) 
	{ 
	    headerType = 88888; 
	}
	
    } while ( headerType != 99999 && headerType != 88888 && !IsEOF(theFile) );
    
    if (headerType == 99999 || headerType == 88888) 
    {
	    
	// set SLR type variable so that we know how
	// to process the rest of the data.
	mySLRheader.slrType = headerType;
	
	// read header line
	std::string headerline = ReadLineFromFile(theFile);

	return GetSLRHeader(headerline,mySLRheader);

    } 
    else
    {
	return false;
    }

}

//------------------------------------------------------------------------------
// bool GetSLRHeader(std::string lff, slr_header &mySLRheader)
//------------------------------------------------------------------------------
/** 
 * Extracts header information from the compact SLR Normal Point Data format.
 *
 * Each set of observations from a given station has one header record
 * associated with it. This header contains the ground station identifiers,
 * calibration information, and the date.
 */
//------------------------------------------------------------------------------

bool ProcessSLRData::GetSLRHeader(std::string &lff, slr_header &mySLRheader) 
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
    mySLRheader.ilrsSatnum = Trim(lff2.substr(0,7));

    // Extract two digit year
    if (!from_string<int>(mySLRheader.year,lff2.substr(7,2),std::dec)) return false;
    
    // Put year in four digit format
    if ( mySLRheader.year < 50 ) 
    {
	mySLRheader.year += 2000;
    }
    else
    {
	mySLRheader.year += + 1900;	
    }
    
    if (!from_string<int>(mySLRheader.dayOfYear,lff2.substr(9,3),std::dec)) return false;
    if (!from_string<int>(mySLRheader.cdpPadID,lff2.substr(12,4),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.cdpSysNum,lff2.substr(16,2),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.cdpOccupancySequenceNum,lff2.substr(18,2),std::dec)) return false;    
    if (!from_string<int>(itemp,lff2.substr(20,4),std::dec)) return false;
    mySLRheader.wavelength = itemp;
    // Convert wavelength to nanometers.
    // 3000 - 9999 is units of 0.1 nanometers
    // 1000 - 2999 is units of 1.0 nanometers so no conversion needed.
    if (mySLRheader.wavelength >= 3000 && mySLRheader.wavelength <= 9999) 
    {
	mySLRheader.wavelength *= 0.1;
    }
    
    if (!from_string<int>(mySLRheader.calSysDelay,lff2.substr(24,8),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.calDelayShift,lff2.substr(32,6),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.rmsSysDelay,lff2.substr(38,4),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.normalPointWindowIndicator,lff2.substr(42,1),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.epochTimeScaleIndicator,lff2.substr(43,1),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.sysCalMethodIndicator,lff2.substr(44,1),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.schIndicator,lff2.substr(45,1),std::dec)) return false;    
    if (!from_string<int>(mySLRheader.sciIndicator,lff2.substr(46,1),std::dec)) return false;
    if (!from_string<int>(mySLRheader.passRMS,lff2.substr(47,4),std::dec)) return false;

    if (pcrecpp::RE("^\\s+$").FullMatch(lff2.substr(51,1))) 
    {    
	mySLRheader.dataQualAssessmentIndicator = 0;   
    }
    else
    {
	if (!from_string<int>(mySLRheader.dataQualAssessmentIndicator,lff2.substr(51,1),std::dec)) return false;
    }
    
    if (pcrecpp::RE("^\\s+$").FullMatch(lff2.substr(54,1))) 
    {
	mySLRheader.formatRevisionNum = 0;        
    }
    else
    {
        if (!from_string<int>(mySLRheader.formatRevisionNum,lff2.substr(54,1),std::dec)) return false;
    }
    
    return true;    
    
}

//------------------------------------------------------------------------------
// bool GetSLRData(std::string lff, slr_header &mySLRheader, 
//                     slr_obtype &mySLRdata)
//------------------------------------------------------------------------------
/** 
 * Converts the compact SLR Normal Point Data format into usable numbers.
 *
 * Note that the SLRType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------

bool ProcessSLRData::GetSLRData(std::string &lff, slr_header &mySLRheader,
				     slr_obtype &mySLRdata)
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
    
    switch(mySLRheader.slrType)
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
	    mySLRdata.timeOfLaserFiring = dtemp * 1.0e-7;
	    
	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySLRdata.twoWayTimeOfFlight = dtemp * 1.0e-12;

	    if (!from_string<int>(mySLRdata.binRMSRange,lff2.substr(24,7),std::dec)) return false;
	    
	    // Convert surface pressure to units of millibar
	    if (!from_string<int>(itemp,lff2.substr(31,5),std::dec)) return false;
	    mySLRdata.surfacePressure = itemp * 0.1;
	    
	    // Convert surface temp to units of degrees Kelvin
	    if (!from_string<int>(itemp,lff2.substr(36,4),std::dec)) return false;
	    mySLRdata.surfaceTemp = itemp * 0.1;
	    
	    if (!from_string<int>(mySLRdata.relativeHumidity,lff2.substr(40,3),std::dec)) return false;
	    if (!from_string<int>(mySLRdata.numRawRanges,lff2.substr(43,4),std::dec)) return false;
	    if (!from_string<int>(mySLRdata.dataReleaseFlag,lff2.substr(47,1),std::dec)) return false;
	    if (!from_string<int>(mySLRdata.rawRangeFactor,lff2.substr(48,1),std::dec)) return false;
	    // The Normal Point Window Indicator and the Signal to Noise Ratio
	    // are only used for LLR data
	    if (!from_string<int>(itemp,lff2.substr(49,1),std::dec)) return false;
	    mySLRdata.normalPointWindowIndicator2 = itemp;
	    if (!from_string<int>(itemp,lff2.substr(50,2),std::dec)) return false;
	    mySLRdata.signalToNoiseRatio =  itemp * 0.1;

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
	    mySLRdata.timeOfLaserFiring = dtemp * 1.0e-7;

	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time
	    mySLRdata.twoWayTimeOfFlight = dtemp * 1.0e-12;
	    
	    // Convert surface pressure to units of millibar
            if (!from_string<int>(itemp,lff2.substr(24,5),std::dec)) return false;
	    mySLRdata.surfacePressure = itemp * 0.1; 
	    
	    // Convert surface temp to units of degrees Kelvin
            if (!from_string<int>(itemp,lff2.substr(29,4),std::dec)) return false;
	    mySLRdata.surfaceTemp = itemp * 0.1;

            if (!from_string<int>(mySLRdata.relativeHumidity,lff2.substr(33,3),std::dec)) return false;
            if (!from_string<int>(mySLRdata.burstCalSysDelay,lff2.substr(36,8),std::dec)) return false;
            if (!from_string<int>(mySLRdata.signalStrength,lff2.substr(44,4),std::dec)) return false;
            if (!from_string<int>(mySLRdata.angleOriginIndicator,lff2.substr(48,1),std::dec)) return false;
            if (!from_string<int>(itemp,lff2.substr(49,7),std::dec)) return false;
	    // Convert az to degrees
	    mySLRdata.az = itemp * 1e-4;
            if (!from_string<int>(itemp,lff2.substr(56,6),std::dec)) return false;
	    // Convert el to degrees
	    mySLRdata.el = itemp * 1e-4;
                                                                                                                                                                                                                                                                                    
	    break;
	    
	default:
	    
	    return false;
	    	
    }

    return true;    
    
}
