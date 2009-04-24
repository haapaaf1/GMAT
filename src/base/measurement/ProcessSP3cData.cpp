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
#include "math.h"

//---------------------------------
//  static data
//---------------------------------

const std::string ProcessSP3cData::SP3c_TYPE_DESCRIPTIONS[EndSP3cTypeReps] =
{
    "GPS Only",
    "Mixed",
    "GLONASS Only",
    "LEO Only",
    "Galileo Only",
};

const std::string ProcessSP3cData::SP3c_TIME_DESCRIPTIONS[EndSP3cTimeReps] =
{
    "GPS Time",
    "GLONASS UTC Time",
    "Galileo System Time",
    "International Atomic Time",
    "Coordinated Universal Time",
};


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

    // Make sure that the SP3cData vector has space reserved for
    // a minimum number of observations. This ensures that the
    // compiler does not unnecessarily reallocate the vector storage too-often.
    // The function reserve() will ensure that we have room for at least 100
    // elemnts. If the vector already has room for the required number of elements,
    // reserve() does nothing. In other words, reserve() will grow the allocated
    // storage of the vector, if necessary, but will never shrink it.
    SP3cData.reserve(100);

    // Initialize individual data struct
    // This needs new memory allocation because
    // we are storing pointers to this data
    SP3c_obtype *mySP3c = new SP3c_obtype;

    while (!IsEOF(myFile))
    {

        if (GetData(myFile,mySP3c))
        {
            SP3cData.push_back(mySP3c);
        }

        // Allocate another struct in memory
        mySP3c = new SP3c_obtype;

    }

    // Set iterator to beginning of vector container
    i = SP3cData.begin();

    /*
    FILE * outFile;
    outFile = fopen("test.output","w");

    // Output to file to make sure all the data is properly stored
    for (std::vector<SP3c_obtype*>::const_iterator j=SP3cData.begin(); j!=SP3cData.end(); ++j)
    {

	    // Output resulting struct data to screen
	    fprintf(outFile,"Class = %s\n",(*j)->securityClassification.c_str());
	    fprintf(outFile,"Satnum = %d\n",(*j)->satelliteID);
	    fprintf(outFile,"Sensor ID = %d\n",(*j)->sensorID);
            if ((*j)->year < 57)
            {
                fprintf(outFile,"Year = %d ",(*j)->year+2000);
            }
            else
            {
                fprintf(outFile,"Year = %d ",(*j)->year+1900);
            }
	    fprintf(outFile,"Day of Year = %d ",(*j)->dayOfYear);
	    fprintf(outFile,"Hour = %d ",(*j)->hour);
	    fprintf(outFile,"Minutes = %d ",(*j)->minute);
	    fprintf(outFile,"Seconds = %16.8f\n",(*j)->seconds);
	    fprintf(outFile,"Elevation = %16.8g\n",(*j)->elevation);
	    fprintf(outFile,"Azimuth = %16.8g\n",(*j)->azimuth);
	    fprintf(outFile,"Declination = %16.8f\n",(*j)->declination);
	    fprintf(outFile,"Right Ascension = %16.8f\n",(*j)->rightAscension);
	    fprintf(outFile,"Range = %16.8f\n",(*j)->range);
	    fprintf(outFile,"Range Rate = %16.8f\n",(*j)->rangeRate);
	    fprintf(outFile,"ECF X = %16.8f\n",(*j)->ecf_X);
	    fprintf(outFile,"ECF Y = %16.8f\n",(*j)->ecf_Y);
	    fprintf(outFile,"ECF Z = %16.8f\n",(*j)->ecf_Z);
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
   fileFormatID = 0;
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

//------------------------------------------------------------------------------
// bool AdvanceToNextOb()
//------------------------------------------------------------------------------
/**
 * Returns the next observation from the vector container.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::AdvanceToNextOb()
{
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
// bool GetData(std::ifstream &theFile, SP3c_obtype *mySP3cData)
//------------------------------------------------------------------------------
/** 
 * Obtains the next line of SP3c data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetData(std::ifstream &theFile, SP3c_obtype *mySP3cData)
{
    
    std::string line = ReadLineFromFile(theFile);
    return ExtractSP3cData(line,mySP3cData);
}

//------------------------------------------------------------------------------
// bool ExtractSP3cData(std::string &lff, SP3c_obtype *mySP3cData)
//------------------------------------------------------------------------------
/** 
 * Converts the compact SP3c data format into usable numbers.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::ExtractSP3cData(std::string &lff, SP3c_obtype *mySP3cData)
{

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not 
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp, itemp2, itemp3;
    Real range;
    
    // Overpunch code for certain data fields
    std::string code;

    // Trim off any leading or trailing whitespace.
    std::string lff2 = Trim(lff);
    
    // Check to make sure that the line length is at
    // least 75 characters long

    if (lff2.length() < 75)
    {
        return false;
    }

    // Initialize all the struct variables
    mySP3cData->SP3cType = -1;
    mySP3cData->securityClassification = "";
    mySP3cData->satelliteID = -1;
    mySP3cData->sensorID = -1;
    mySP3cData->year = -1;
    mySP3cData->dayOfYear = -1;
    mySP3cData->hour = -1;
    mySP3cData->minute = -1;
    mySP3cData->seconds = -1.0;
    mySP3cData->range = 0.0;
    mySP3cData->rangeRate = 0.0;
    mySP3cData->elevation = 0.0;
    mySP3cData->azimuth = 0.0;
    mySP3cData->declination = 0.0;
    mySP3cData->rightAscension = 0.0;
    mySP3cData->ecf_X = 0.0;
    mySP3cData->ecf_Y = 0.0;
    mySP3cData->ecf_Z = 0.0;
        
    // Read the type of the SP3c observation
    // Possible obtype values and their meaning
    // 0 - Range rate only
    // 1 - Azimuth and elevation
    // 2 - Range, azimuth and elevation
    // 3 - Range, azimuth, elevation, and range rate
    // 4 - Range, azimuth, eelcation, and range rate 
    //    (extra measurements for azimuth rate, elevation rate, etc are ignored)
    // 5 - Right Ascension and Declination
    // 6 - Range only
    // 8 - Azimuth, elevation, sometimes range and ECF position of the sensor
    // 9 - Right ascension, declination, sometimes range and 
    //     ECF position of the sensor
    
    if (!from_string<int>(mySP3cData->SP3cType,lff2.substr(74,1),std::dec)) return false;
    
    mySP3cData->securityClassification = lff2.substr(0,1);
    if (!from_string<int>(mySP3cData->satelliteID,lff2.substr(1,5),std::dec)) return false;
    if (!from_string<int>(mySP3cData->sensorID,lff2.substr(6,3),std::dec)) return false;
    if (!from_string<int>(mySP3cData->year,lff2.substr(9,2),std::dec)) return false;
    if (!from_string<int>(mySP3cData->dayOfYear,lff2.substr(11,3),std::dec)) return false;
    if (!from_string<int>(mySP3cData->hour,lff2.substr(14,2),std::dec)) return false;
    if (!from_string<int>(mySP3cData->minute,lff2.substr(16,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff2.substr(18,5),std::dec)) return false;
    mySP3cData->seconds = itemp * 1e-3;

    switch (mySP3cData->SP3cType)
    {
	case RANGERATEONLY_ID:

	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		mySP3cData->rangeRate = itemp * 1e-5;
	    } 
	    else if (pcrecpp::RE("^\\.(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		mySP3cData->rangeRate = -itemp * 1e-5;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    break;
            
	case AZEL_ID:
	    
	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		mySP3cData->elevation = itemp * 1e-4;
	    } 
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code.c_str(),digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		mySP3cData->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    mySP3cData->azimuth = itemp * 1e-4;
	                
	    break;
            
	case RAZEL_ID:
            
	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		mySP3cData->elevation = itemp * 1e-4;
	    } 
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code.c_str(),digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		mySP3cData->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    mySP3cData->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

	    mySP3cData->range = (range * 1e-5) * pow(10,itemp);
	    
	    break;
            
	case RAZELRR_ID:

	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		mySP3cData->elevation = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		mySP3cData->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    mySP3cData->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

	    mySP3cData->range = (range * 1e-5) * pow(10,itemp);

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		mySP3cData->rangeRate = itemp * 1e-5;
	    } 
	    else if (pcrecpp::RE("^\\.(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		mySP3cData->rangeRate = -itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    break;
            
	case RAZELRR2_ID:

	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		mySP3cData->elevation = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec))
                {
                    return false;
                }

		mySP3cData->elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec))
            {
               return false;
            }

	    mySP3cData->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec))
            {
               return false;
            }

	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec))
            {
               return false;
            }

	    mySP3cData->range = (range * 1e-5) * pow(10,itemp);

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		mySP3cData->rangeRate = itemp * 1e-5;
	    }
	    else if (pcrecpp::RE("^\\.(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		mySP3cData->rangeRate = -itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }
            
	    break;
            
	case RADEC_ID:
	    
	    // Negative declination values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		mySP3cData->declination = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string decl = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,decl,std::dec))
                {
                    return false;
                }
		mySP3cData->declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
            if (pcrecpp::RE("^(\\d{2})(\\d{2})(\\d{3})$").FullMatch(lff2.substr(30,7),&itemp, &itemp2, &itemp3))
	    {
		mySP3cData->rightAscension = itemp + itemp2/60.0 + itemp3/3600.0;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }
	    
	    break;
            
	case RANGEONLY_ID:

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

	    mySP3cData->range = (range * 1e-5) * pow(10,itemp);
	    	    
	    break;
            
	case AZELSENSORPOS_ID:
            
	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {

		mySP3cData->elevation = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		mySP3cData->elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    mySP3cData->azimuth = itemp * 1e-4;

	    // Check to see if range value defined. If 0 or blank, skip over
	    if(!pcrecpp::RE("^[ 0]{6)$").FullMatch(lff2.substr(23,6))) 
	    {
		// Find range
		if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

		mySP3cData->range = (range * 1e-5) * pow(10,itemp);
	    }
	    else
	    {
		mySP3cData->range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff2.substr(46,9),std::dec)) return false;
	    mySP3cData->ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(55,9),std::dec)) return false;
	    mySP3cData->ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(64,9),std::dec)) return false;
	    mySP3cData->ecf_Z = itemp * 1e-3;
	    
	    break;
            
	case RADECSENSORPOS_ID:
            
	    // Negative declination values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {

		mySP3cData->declination = itemp * 1e-4;
		
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff2.substr(23,6),&itemp, &code)) 
	    {
		
		Integer digit, sign;
		
		// Figure out the overpunch code
		Overpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach overpunch digit to end of number string
		std:: string decl = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,decl,std::dec)) return false;
		mySP3cData->declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
	    if (pcrecpp::RE("^(\\d{2})(\\d{2})(\\d{3})$").FullMatch(lff2.substr(30,7),&itemp, &itemp2, &itemp3))
	    {
		mySP3cData->rightAscension = itemp + itemp2/60.0 + itemp3/3600.0;
	    }
	    else
	    {
		// Ill formed data
		return false;		
	    }

	    // Check to see if range value defined. If 0 or blank, skip over
	    if(!pcrecpp::RE("^[ 0]{6)$").FullMatch(lff2.substr(23,6))) 
	    {
		// Find range
		if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

		mySP3cData->range = (range * 1e-5) * pow(10,itemp);
	    }
	    else
	    {
		mySP3cData->range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff2.substr(46,9),std::dec)) return false;
	    mySP3cData->ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(55,9),std::dec)) return false;
	    mySP3cData->ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(64,9),std::dec)) return false;
	    mySP3cData->ecf_Z = itemp * 1e-3;

	    break;
            
	default:

	    // Ill formed data. All stop.           
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

            return (*i)->SP3cType;

        case SP3c_SATELLITE_ID:

            return (*i)->satelliteID;

        case SP3c_SENSORID_ID:

            return (*i)->sensorID;

        case SP3c_YEAR_ID:

            return (*i)->year;

        case SP3c_DAYOFYEAR_ID:

            return (*i)->dayOfYear;

        case SP3c_HOUR_ID:

            return (*i)->hour;

        case SP3c_MINUTE_ID:

            return (*i)->minute;

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
        case SP3c_SECURITYCLASSIFICATION_ID:

            return (*i)->securityClassification;

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

       case SP3c_SECONDS_ID:

            return (*i)->seconds;

        case SP3c_ELEVATION_ID:

            return (*i)->elevation;

        case SP3c_DECLINATION_ID:

            return (*i)->declination;

        case SP3c_RIGHTASCENSION_ID:

            return (*i)->rightAscension;

        case SP3c_AZIMUTH_ID:

            return (*i)->azimuth;

        case SP3c_RANGE_ID:

            return (*i)->range;

        case SP3c_RANGERATE_ID:

            return (*i)->rangeRate;

        case SP3c_ECFX_ID:

            return (*i)->ecf_X;

        case SP3c_ECFY_ID:

            return (*i)->ecf_Y;

        case SP3c_ECFZ_ID:

            return (*i)->ecf_Z;

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
