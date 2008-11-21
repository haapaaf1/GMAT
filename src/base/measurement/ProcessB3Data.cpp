//$Header$
//------------------------------------------------------------------------------
//                             ProcessB3Data
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
 * Implements DataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#include <ProcessB3Data.hpp>
#include "gmatdefs.hpp"
#include "StringUtil.hpp"           // for ToString()
#include "math.h"

//---------------------------------
//  static data
//---------------------------------

const std::string ProcessB3Data::B3_TYPE_DESCRIPTIONS[EndB3TypeReps] =
{
    "Range rate only",
    "Azimuth & elevation",
    "Range, azimuth, & elevation",
    "Range, azimuth, elevation, & range rate",
    "Range, azimuth, elevation, & range rate (extra measurements for azimuth rate, elevation rate, etc are ignored)",
    "Right Ascension & Declination",
    "Range only",
    "Azimuth, elevation, sometimes range and ECF position of the sensor",
    "Right ascension, declination, sometimes range and ECF position of the sensor",
};

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  ProcessB3Data() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessB3Data structures 
 */
ProcessB3Data::ProcessB3Data(const std::string &itsName) :
	DataFile ("B3DataFile", itsName)
{
   objectTypeNames.push_back("B3DataFile");
}

//------------------------------------------------------------------------------
//  ProcessB3Data::~ProcessB3Data() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessB3Data::~ProcessB3Data() 
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessB3Data.
 *
 * @return clone of the ProcessB3Data.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessB3Data::Clone() const
{
   GmatBase *clone = new ProcessB3Data(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// const std::string* GetB3TypeReps() const
//------------------------------------------------------------------------------
const std::string* ProcessB3Data::GetB3TypeDescriptions() const
{
   return B3_TYPE_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetB3TypeNameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the model name text corresponding to a model ID
 */
//------------------------------------------------------------------------------
std::string ProcessB3Data::GetB3TypeNameText(const Integer &id) const
{
   if ((id >= 0) && (id < EndB3TypeReps))
   {
      // Note that the B3_TYPE_REPS enumeration starts at 1 instead of 0
      // to conform to the data format definition
      return B3_TYPE_DESCRIPTIONS[id-1];
   }

   return "INVALID";
}

//------------------------------------------------------------------------------
// bool GetData(b3_obtype &myb3Data)
//------------------------------------------------------------------------------
/** 
 * Obtains the next line of b3 data from file.
 */
//------------------------------------------------------------------------------
bool ProcessB3Data::GetData(std::ifstream &theFile, b3_obtype &myB3Data) {
    
    std::string line = ReadLineFromFile(theFile);
    return GetB3Data(line,myB3Data);
    
}

//------------------------------------------------------------------------------
// std::string GetB3Data(std::string lff)
//------------------------------------------------------------------------------
/** 
 * Converts the compact b3 data format into usable numbers.
 */
//------------------------------------------------------------------------------
bool ProcessB3Data::GetB3Data(std::string &lff, b3_obtype &myb3Data) {

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
    
    if (lff2.size() < 75) 
    {
        return false;
    }
        
    // Read the type of the b3 observation
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

    if (!from_string<int>(myb3Data.b3Type,lff2.substr(74,1),std::dec)) return false;
    
    myb3Data.securityClassification = lff2.substr(0,1);	
    if (!from_string<int>(myb3Data.satelliteID,lff2.substr(1,5),std::dec)) return false;
    if (!from_string<int>(myb3Data.sensorID,lff2.substr(6,3),std::dec)) return false;
    if (!from_string<int>(myb3Data.year,lff2.substr(9,2),std::dec)) return false;
    if (!from_string<int>(myb3Data.dayOfYear,lff2.substr(11,3),std::dec)) return false;
    if (!from_string<int>(myb3Data.hour,lff2.substr(14,2),std::dec)) return false;
    if (!from_string<int>(myb3Data.minute,lff2.substr(16,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff2.substr(18,5),std::dec)) return false;
    myb3Data.seconds = itemp * 1e-3;
    
    switch (myb3Data.b3Type) 
    {
	case RANGERATEONLY_ID:

	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		myb3Data.rangeRate = itemp * 1e-5;
	    } 
	    else if (pcrecpp::RE("^\\.(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		myb3Data.rangeRate = -itemp * 1e-5;
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
		myb3Data.elevation = itemp * 1e-4;
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
		myb3Data.elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    myb3Data.azimuth = itemp * 1e-4;
	                
	    break;
            
	case RAZEL_ID:
            
	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		myb3Data.elevation = itemp * 1e-4;
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
		myb3Data.elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    myb3Data.azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

	    myb3Data.range = (range * 1e-5) * pow(10,itemp);	    
	    
	    break;
            
	case RAZELRR_ID:

	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {
		myb3Data.elevation = itemp * 1e-4;
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
		myb3Data.elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    myb3Data.azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

	    myb3Data.range = (range * 1e-5) * pow(10,itemp);	    

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		myb3Data.rangeRate = itemp * 1e-5;
	    } 
	    else if (pcrecpp::RE("^\\.(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		myb3Data.rangeRate = -itemp * 1e-5;
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
		myb3Data.elevation = itemp * 1e-4;
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
		myb3Data.elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    myb3Data.azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

	    myb3Data.range = (range * 1e-5) * pow(10,itemp);	    

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		myb3Data.rangeRate = itemp * 1e-5;
	    }
	    else if (pcrecpp::RE("^\\.(\\d+)$").FullMatch(lff2.substr(47,7),&itemp)) 
	    {
		myb3Data.rangeRate = -itemp * 1e-5;
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
		myb3Data.declination = itemp * 1e-4;
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
		myb3Data.declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
	    if (pcrecpp::RE("^(\\d{2})(\\d{2})(\\d{3})$").FullMatch(lff2.substr(23,6),&itemp, &itemp2, &itemp3)) 
	    {
		myb3Data.rightAscension = (itemp+itemp2/60+itemp3/3600);
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

	    myb3Data.range = (range * 1e-5) * pow(10,itemp);	    
	    	    
	    break;
            
	case AZELSENSORPOS_ID:
            
	    // Negative elevation values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {

		myb3Data.elevation = itemp * 1e-4;
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
		myb3Data.elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff2.substr(30,7),std::dec)) return false;
	    myb3Data.azimuth = itemp * 1e-4;

	    // Check to see if range value defined. If 0 or blank, skip over
	    if(!pcrecpp::RE("^[ 0]{6)$").FullMatch(lff2.substr(23,6))) 
	    {
		// Find range
		if (!from_string<int>(itemp,lff2.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff2.substr(45,1),std::dec)) return false;

		myb3Data.range = (range * 1e-5) * pow(10,itemp);	    
	    }
	    else
	    {
		myb3Data.range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff2.substr(46,9),std::dec)) return false;
	    myb3Data.ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(55,9),std::dec)) return false;
	    myb3Data.ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(64,9),std::dec)) return false;
	    myb3Data.ecf_Z = itemp * 1e-3;
	    
	    break;
            
	case RADECSENSORPOS_ID:
            
	    // Negative declination values are formatted as overpunched values
	    // Test to see if overpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff2.substr(23,6),&itemp)) 
	    {

		myb3Data.declination = itemp * 1e-4;
		
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
		myb3Data.declination = itemp * 1e-4;	
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
	    if (pcrecpp::RE("^(\\d{2})(\\d{2})(\\d{3})$").FullMatch(lff2.substr(23,6),&itemp, &itemp2, &itemp3)) 
	    {
		myb3Data.rightAscension = (itemp+itemp2/60+itemp3/3600);
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

		myb3Data.range = (range * 1e-5) * pow(10,itemp);
	    }
	    else
	    {
		myb3Data.range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff2.substr(46,9),std::dec)) return false;
	    myb3Data.ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(55,9),std::dec)) return false;
	    myb3Data.ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff2.substr(64,9),std::dec)) return false;
	    myb3Data.ecf_Z = itemp * 1e-3;

	    break;
            
	default:

	    // Ill formed data. All stop.           
	    return false;
    }
        
    return true;    
    
}
