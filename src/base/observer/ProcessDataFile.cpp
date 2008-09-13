//$Header$
//------------------------------------------------------------------------------
//                             ProcessDataFile
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/30
//
/**
 *
 * Implements LatLonHgt Base class to contain the associated elements
 * Latitude, Longitude, Height, and a flag to determine geocentric versus
 * geodetic angles.
 *
 */
//------------------------------------------------------------------------------

#include <ProcessDataFile.hpp>
#include "gmatdefs.hpp"
#include "math.h"
#include "iostream"
#include "fstream"
#include "sstream"
#include "UtilityException.hpp"
#include "StringUtil.hpp"           // for ToString()

//---------------------------------
//  static data
//---------------------------------
const std::string ProcessDataFile::DATA_TYPE_DESCRIPTIONS[NUM_DATA_TYPES] =
{
    "B3",
    "TLE",
    "SLR"
};

const std::string ProcessDataFile::B3_TYPE_DESCRIPTIONS[NUM_B3_TYPES] =
{
    "Range rate only ",
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
//  ProcessDataFile::ProcessDataFile() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessDataFile structures 
 */
ProcessDataFile::ProcessDataFile()
{
}

//------------------------------------------------------------------------------
//  ProcessDataFile::~ProcessDataFile() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessDataFile::~ProcessDataFile() 
{
    myFile.close();
}

//------------------------------------------------------------------------------
//  bool ProcessDataFile::OpenFile() 
//------------------------------------------------------------------------------
/**
 * Opens a physical file and creates a file handle pointing to it.
 *
 * @return Boolean success or failure
 */
bool ProcessDataFile::OpenFile() 
{
    myFile.open(dataFileName.c_str());
    
    if(myFile.is_open()) return true;
    else return false;
}

//------------------------------------------------------------------------------
//  bool ProcessDataFile::CloseFile() 
//------------------------------------------------------------------------------
/**
 * Closes an open file handle.
 *
 * @return Boolean success or failure
 */
bool ProcessDataFile::CloseFile() 
{
    myFile.close();

    if(myFile.is_open()) return false;
    else return true;
    
}

//------------------------------------------------------------------------------
//  bool ProcessDataFile::IsEOF() 
//------------------------------------------------------------------------------
/**
 * Check to see if the end of the file has been reached.
 *
 * @return True if end of file, False otherwise
 */
bool ProcessDataFile::IsEOF() 
{
    return myFile.eof();
}

//------------------------------------------------------------------------------
// std::string ProcessDataFile::ReadLineFromFile()
//------------------------------------------------------------------------------
/**
 * Read a single line from a file.
 *
 * @return Line from file 
 */
std::string ProcessDataFile::ReadLineFromFile() 
{
    std::string lff;
    getline(myFile,lff);
    return lff;
}

//----------------------------------------------------------------
//std::string ProcessDataFile::GetLine(Integer &lineNum)
//----------------------------------------------------------------
/**
 * Retrieve the lineFromFile parameter.
 *
 * @param <lineNum> Integer line number
 * @return Line from file
 */
std::string ProcessDataFile::GetLine(Integer &lineNum)
{
    return lineFromFile[lineNum];
}

//------------------------------------------------------------------------------
// void ProcessDataFile::SetLine(std::string &lff, Integer &lineNum)
//------------------------------------------------------------------------------
/**
 * Set the lineFromFile parameter.
 *
 * @param <lineNum> Integer line number
 * @param <lff> String of text
 */
void ProcessDataFile::SetLine(std::string &lff, Integer &lineNum)
{
    lineFromFile[lineNum] = lff;
}


//------------------------------------------------------------------------------
// std::string ProcessDataFile::GetFileName()
//------------------------------------------------------------------------------
/**
 * Retrieve the dataFileName parameter.
 *
  * @return dataFileName
 */
std::string ProcessDataFile::GetFileName() 
{
    return dataFileName;
}

//------------------------------------------------------------------------------
// void ProcessDataFile::SetFileName(std::string &myFileName)
//------------------------------------------------------------------------------
void ProcessDataFile::SetFileName(std::string &myFileName)
/**
 * Set the dataFileName parameter using strings.
 *
 * @param <myFileName> Desired file name
 */
{
    dataFileName = myFileName;
}

//------------------------------------------------------------------------------
// void ProcessDataFile::SetFileName(const char* myFileName)
//------------------------------------------------------------------------------
/**
 * Set the dataFileName parameter using const char.
 *
 * @param <myFileName> Desired file name
 */
void ProcessDataFile::SetFileName(const char* myFileName)
{
    dataFileName = myFileName;
}


//------------------------------------------------------------------------------
// std::string ProcessDataFile::Trim() const
//------------------------------------------------------------------------------
/** 
 * Removes leading and trailing blanks from a string
 */
//------------------------------------------------------------------------------
std::string ProcessDataFile::Trim(std::string str)
{
    pcrecpp::RE("^\\s+").GlobalReplace("", &str);
    pcrecpp::RE("\\s+$").GlobalReplace("", &str);
    
    return str;
}

//------------------------------------------------------------------------------
// template <class T> bool ProcessDataFile::from_string(T& t, 
//		   const std::string& s, 
//                 std::ios_base& (*f)(std::ios_base&))
//------------------------------------------------------------------------------
/** 
 * Typesafe conversion from string to integer, float, etc
 */
//------------------------------------------------------------------------------

template <class T> bool ProcessDataFile::from_string(T& t, const std::string& s, 
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}

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
std::string ProcessDataFile::Ilrs2Cospar(std::string ilrsSatnum)
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

    static const char alpha[26] = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};
    
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
// bool ProcessDataFile::Overpunch(const char &code, Integer &digit, Integer &sign )
//------------------------------------------------------------------------------
/** 
 * Converts overpunch code to numeric value and determines appropriate sign
 */
//------------------------------------------------------------------------------
bool ProcessDataFile::Overpunch(std::string code, Integer &digit, Integer &sign )
{
    if (!strcmp(code.c_str(), "}")) 
    {
	digit = 0; sign = -1;
    } 
    else if (!strcmp(code.c_str(), "J")) 
    {
	digit = 1; sign = -1;
    }
    else if (!strcmp(code.c_str(), "K")) 
    {
	digit = 2; sign = -1;
    }
    else if (!strcmp(code.c_str(), "L")) 
    {
	digit = 3; sign = -1;
    }
    else if (!strcmp(code.c_str(), "M")) 
    {
	digit = 4; sign = -1;
    }
    else if (!strcmp(code.c_str(), "N")) 
    {
	digit = 5; sign = -1;
    }
    else if (!strcmp(code.c_str(), "O")) 
    {
	digit = 6; sign = -1;
    }
    else if (!strcmp(code.c_str(), "P")) 
    {
	digit = 7; sign = -1;
    }
    else if (!strcmp(code.c_str(), "Q")) 
    {
	digit = 8; sign = -1;
    }
    else if (!strcmp(code.c_str(), "R")) 
    {
	digit = 9; sign = -1;
    }
    else if (!strcmp(code.c_str(), "{")) 
    {
	digit = 0; sign = +1;
    }
    else if (!strcmp(code.c_str(), "A")) 
    {
	digit = 1; sign = +1;
    }
    else if (!strcmp(code.c_str(), "B")) 
    {
	digit = 2; sign = +1;
    }
    else if (!strcmp(code.c_str(), "C")) 
    {
	digit = 3; sign = +1;
    }
    else if (!strcmp(code.c_str(), "D")) 
    {
	digit = 4; sign = +1;
    }
    else if (!strcmp(code.c_str(), "E")) 
    {
	digit = 5; sign = +1;
    }
    else if (!strcmp(code.c_str(), "F")) 
    {
	digit = 6; sign = +1;
    }
    else if (!strcmp(code.c_str(), "G")) 
    {
	digit = 7; sign = +1;
    }
    else if (!strcmp(code.c_str(), "H")) 
    {
	digit = 8; sign = +1;
    }
    else if (!strcmp(code.c_str(), "I")) 
    {
	digit = 9; sign = +1;
    }
    else 
    {
	return false;
    }
    
    
    return true;
}

//------------------------------------------------------------------------------
// std::string ProcessTLEData(std::string lff, std::string lff2)
//------------------------------------------------------------------------------
/** 
 * Converts the compact Two Line Element Set data into usable numbers
 */
//------------------------------------------------------------------------------
bool ProcessDataFile::ProcessTLEData(std::string &lff, 
   				     std::string &lff2,
				     tle_obtype &myTLEdata) 
{
        
    // initialize variables for later use
    std::string intldesig = "";
    std::string intldesigDefault = "NotAvailable";

    // check if the input strings are empty
    if (lff.c_str() == "" || lff2.c_str() == "") 
    { 
	return false; 
    }
           
    // note, indexing starts at 0 not 1
    int linenum;
    from_string<int>(linenum,lff.substr(0,1),std::dec);
    int linenum2;
    from_string<int>(linenum2,lff2.substr(0,1),std::dec);

    // this length is rather arbitrary
    if (linenum == 1 && lff.size() < 43) 
    { 
        // Ill formed data. All stop.
        return false;
    }
    
    // this length is more important for line 2
    if (linenum2 == 2 && lff2.size() < 43) 
    {
        // Ill formed data. All stop.
        return false;   
    }

    if (lff.size() >= 7) 
    {
	std::string satnum_1 = Trim(lff.substr(2,5).c_str());

	// make sure satnum only has numbers in it
        if (pcrecpp::RE("^\\d+$").FullMatch(satnum_1)) 
	{ 
            from_string<int>(myTLEdata.satnum,satnum_1,std::dec);   
        } 
	else 
	{
	    // Ill formed data. All stop.
	    return false;   
        }
    } 
    else 
    {
	// Ill formed data. All stop.
	return false;
    }

    if (lff.size() >= 8) 
    {
	std::string classification_1 = Trim(lff.substr(7,1));
	
        if (classification_1 != "") 
	{
            myTLEdata.securityClassification = classification_1;  
	}
	else
	{    
	    myTLEdata.securityClassification = ""; 	    
	}
    } 
    else 
    {
	// Ill formed data. All stop.
	return false;
    }
    
    if (lff.size() >= 17) 
    {
	std::string temp = Trim(lff.substr(9,8));
        // remove blanks
        pcrecpp::RE("\\s+").GlobalReplace("",&temp);
	
	if (intldesig == "" || intldesig == intldesigDefault || intldesig != temp) 
	{
	    myTLEdata.intlDesignator = temp;
	    intldesig = temp;
	}
    } 
    else
    {
	// Ill formed data. All stop.
	return false;
    }
    
    if (lff.size() >= 33) 
    {
	std::string temp = Trim(lff.substr(18,15));

	// sometimes zeros are blanks
	pcrecpp::RE("\\s+").GlobalReplace("0",&temp);

	int epYear;
	double epdayOfYear;

	if(pcrecpp::RE("(\\d{2})(\\d{3}\\.\\d{0,8})").FullMatch(temp,&epYear,&epdayOfYear))
	{
	    myTLEdata.epochYear = epYear;
	    myTLEdata.epochDayOfYear = epdayOfYear;
	} 
	else 
	{
	    // Ill formed data. All stop.
	    return false;
	}
    } 
    else
    {
	// Ill formed data. All stop.
	return false;
    }

    if (lff.size() >= 43)
    {
	std::string ndotby2_1 = Trim(lff.substr(33,10));
	
	if(ndotby2_1 == "" || pcrecpp::RE("^0.0+$").FullMatch(ndotby2_1)) 
	{
	    myTLEdata.ndotby2 = 0;   
	}
	else
	{
	    std::string p1, p2;
	    
	    if ( pcrecpp::RE("^([\\d+-]?)(.\\d{0,8})$").FullMatch(ndotby2_1,&p1,&p2) )
	    {
		double ndotby2;
		std::string str1 = p1 + p2;
		from_string<double>(ndotby2,str1,std::dec);
		myTLEdata.ndotby2 = ndotby2;
	    }
	    else
	    {
		// Ill formed data. All stop.	
		return false;	
	    }
	}
	
	if (lff.size() >= 52) 
	{
	    std::string nddotby6_1 = Trim(lff.substr(44,8));
        
	    if (nddotby6_1 == "" || pcrecpp::RE("^[+-]?0{5}[ +-]?0?$").FullMatch(nddotby6_1)) 
	    {
		myTLEdata.nddotby6 = 0;	
	    }
	    else
	    {
		std::string s1, s2;
		int i3;
	    
		if ( pcrecpp::RE("^([+-]?)(\\d{0,5})([ +-0]\\d?)$").FullMatch(nddotby6_1,&s1,&s2,&i3) ) 
		{
		    // add assumed decimal point
		    double nddotby6;
		    std::string str1 = s1+"."+s2;
		    from_string<double>(nddotby6,str1,std::dec);
		
		    myTLEdata.nddotby6 = nddotby6*pow(10,i3); 
		}
		else
		{
		    // Ill formed data. All stop.    
		    return false;
		}
	    }
	    
	    if (lff.size() >= 62) 
	    {
		// some data shifted by a space so read up to and including buffer space
		std::string bstar_1 = Trim(lff.substr(53,9));
        
		if(bstar_1 == "" || pcrecpp::RE("^[+-]?0{5}[ +-]?0?$").FullMatch(bstar_1)) 
		{
		    myTLEdata.bstar = 0;
		}
		else
		{
		    std::string s1, s2;
		    int i3;
	    
		    if ( pcrecpp::RE("^([+-]?)(\\d{0,5})([ +-0]\\d?)$").FullMatch(bstar_1,&s1,&s2,&i3) ) 
		    {
			// add assumed decimal point
			double bstar;
			
			std::string str1 = s1+"."+s2;
			from_string<double>(bstar,str1,std::dec);
			myTLEdata.bstar = bstar*pow(10,i3); 
		    } 
		    else
		    {
			// Ill formed data. All stop.	
			return false;
		    }
		}

		if (lff.size() >= 63) 
		{
		    std::string ephemtype_1 = lff.substr(62,1);
        
		    if (ephemtype_1 != "") 
		    {
			int ephemType;
			from_string<int>(ephemType,ephemtype_1,std::dec);
			myTLEdata.ephemerisType = ephemType;
		    } 
		    else
		    { 
			myTLEdata.ephemerisType = 0; 
		    }
		    
		    if (lff.size() >= 68) 
		    {
			std::string elnum_1 = Trim(lff.substr(64,4));
        
			if (elnum_1 != "") 
			{
			    int elnum;
			    from_string<int>(elnum,elnum_1 ,std::dec);
			    myTLEdata.elementNum = elnum;
			}  
		    } 
		    else
		    {
			// Line 1 contains no critical data after this point
			myTLEdata.elementNum = 0;
		    }
		} 
		else
		{
		    // Line 1 contains no critical data after this point
		    myTLEdata.ephemerisType = 0;
		    myTLEdata.elementNum = 0;        
		}
	    } 
	    else
	    {
		// Line 1 contains no critical data after this point
		myTLEdata.bstar = 0;
		myTLEdata.ephemerisType = 0;
		myTLEdata.elementNum = 0;
	    }
	} 
	else
	{
	    // Line 1 contains no critical data after this point
	    myTLEdata.nddotby6 = 0;
	    myTLEdata.bstar = 0;
	    myTLEdata.ephemerisType = 0;
	    myTLEdata.elementNum = 0;
	}
    } 
    else
    {
	// Line 1 contains no critical data after this point
	myTLEdata.ndotby2 = 0;
	myTLEdata.nddotby6 = 0;
	myTLEdata.bstar = 0;
	myTLEdata.ephemerisType = 0;
	myTLEdata.elementNum = 0;
    }    
    //
    // Start processing line 2 here
    //
    if (lff2.size() >= 7) 
    {
            
	int satnum2;
	std::string satnum2_1 = Trim(lff2.substr(2,5));
	from_string<int>(satnum2,satnum2_1,std::dec);	
	if (myTLEdata.satnum != satnum2) 
	{
	    // Ill formed data. All stop.
	    return false;	
	}
    } 
    else
    {
	// Ill formed data. All stop.
	return false;
    }

    if (lff2.size() >= 17) 
    {
	std::string incl = Trim(lff2.substr(8,9));

	// For some reason, some of the inclination data has a space in it.	
	pcrecpp::RE("\\s+").GlobalReplace("",&incl);

	// make sure that the inclination is a decimal number	
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(incl,&myTLEdata.inclination)) 
	{ 
	    // Ill formed data. All stop.
	    return false;
	}
    } 
    else
    {
	// Ill formed data. All stop.
	return false;
    }

    if (lff2.size() >= 25) 
    {
	std::string raan = Trim(lff2.substr(17,8));
        
	// sometimes zeros are blanks
	pcrecpp::RE("\\s+").GlobalReplace("0",&raan);
	
	// make sure that the raan is a decimal number
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(raan,&myTLEdata.raan)) 
	{ 
	    // Ill formed data. All stop.
	    return false;   
	}           
    } 
    else
    {
	// Ill formed data. All stop.
	return false;    	
    }
    
    if (lff2.size() >= 33) 
    {
	// assumed decimal point added
	std::string etemp = Trim(lff2.substr(26,7));
	std::string ecc = "."+etemp;

	// make sure that the eccentricity is a decimal number        
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(ecc,&myTLEdata.eccentricity)) 
	{       
	    // Ill formed data. All stop.
	    return false;   
	}           
    } 
    else
    {
	// Ill formed data. All stop.
	return false;    	
    }
        
    if (lff2.size() >= 42) 
    {
	std::string argper = Trim(lff2.substr(34,8));
        
	// sometimes zeros are blanks
	pcrecpp::RE("\\s+").GlobalReplace("0",&argper);

	// make sure that the argument of perigee is a decimal number
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(argper,&myTLEdata.argPerigee)) 
	{
	    // Ill formed data. All stop.
	    return false;   
	}           
    } 
    else
    {
	// Ill formed data. All stop.
	return false;    	
    }

    if (lff2.size() >= 51) 
    {
	std::string ma = Trim(lff2.substr(43,8));
        
	// sometimes zeros are blanks
	pcrecpp::RE("\\s+").GlobalReplace("0",&ma);

	// make sure that the mean anomaly is a decimal number
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(ma,&myTLEdata.meanAnomaly)) 
	{ 
	    // Ill formed data. All stop.
	    return false;   
	}           
    } 
    else
    {
	// Ill formed data. All stop.
	return false;    	
    }    
    
    if (lff2.size() >= 63) 
    {
	std::string n = Trim(lff2.substr(52,11));
        
	// sometimes zeros are blanks
	pcrecpp::RE("\\s+").GlobalReplace("0",&n);
	
	// make sure that the mean motion is a decimal number
	if (!pcrecpp::RE("^([+-]?\\d{0,3}\\.\\d{0,10})$").FullMatch(n,&myTLEdata.meanMotion)) 
	{
	    // Ill formed data. All stop.
	    return false;   
	}           
    } 
    else
    {
	// Ill formed data. All stop.
	return false;    	
    }    
    
    if (lff2.size() >= 63) 
    {
	std::string revnum = Trim(lff2.substr(63,5));
        
	// sometimes zeros are blanks
	pcrecpp::RE("\\s+").GlobalReplace("0",&revnum);
	
	if (revnum == "") 
	{
	    // if revnum is blank then assign -1 to indicate unavailable data
	    myTLEdata.revolutionNum = -1;
	
	// make sure that the rev number is a natural number
	} 
	else if (!pcrecpp::RE("^(\\d+)$").FullMatch(revnum,&myTLEdata.revolutionNum)) 
	{ 
            // Ill formed data. All stop.
	    return false;   
	}           
    } 
    else
    {
	// Ill formed data. All stop.
	return false;	    	
    }        
    
    return true;
    
}
	
//------------------------------------------------------------------------------
// std::string ProcessB3Data(std::string lff)
//------------------------------------------------------------------------------
/** 
 * Converts the compact b3 data format into usable numbers.
 */
//------------------------------------------------------------------------------

bool ProcessDataFile::ProcessB3Data(std::string &lff, b3_obtype &myb3Data) {

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

//------------------------------------------------------------------------------
// bool ProcessSLRHeader(std::string lff, slr_header &mySLRheader)
//------------------------------------------------------------------------------
/** 
 * Extracts header information from the compact SLR Normal Point Data format.
 *
 * Each set of observations from a given station has one header record
 * associated with it. This header contains the ground station identifiers,
 * calibration information, and the date.
 */
//------------------------------------------------------------------------------

bool ProcessDataFile::ProcessSLRHeader(std::string &lff, slr_header &mySLRheader) 
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
// bool ProcessSLRData(std::string lff, slr_header &mySLRheader, 
//                     slr_obtype &mySLRData)
//------------------------------------------------------------------------------
/** 
 * Converts the compact SLR Normal Point Data format into usable numbers.
 *
 * Note that the SLRType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------

bool ProcessDataFile::ProcessSLRData(std::string &lff, slr_header &mySLRheader,
				     slr_obtype &mySLRData)
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
	    mySLRData.timeOfLaserFiring = dtemp * 1.0e-7;
	    
	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time in units of seconds
	    mySLRData.twoWayTimeOfFlight = dtemp * 1.0e-12;

	    if (!from_string<int>(mySLRData.binRMSRange,lff2.substr(24,7),std::dec)) return false;
	    
	    // Convert surface pressure to units of millibar
	    if (!from_string<int>(itemp,lff2.substr(31,5),std::dec)) return false;
	    mySLRData.surfacePressure = itemp * 0.1;
	    
	    // Convert surface temp to units of degrees Kelvin
	    if (!from_string<int>(itemp,lff2.substr(36,4),std::dec)) return false;
	    mySLRData.surfaceTemp = itemp * 0.1;
	    
	    if (!from_string<int>(mySLRData.relativeHumidity,lff2.substr(40,3),std::dec)) return false;
	    if (!from_string<int>(mySLRData.numRawRanges,lff2.substr(43,4),std::dec)) return false;
	    if (!from_string<int>(mySLRData.dataReleaseFlag,lff2.substr(47,1),std::dec)) return false;
	    if (!from_string<int>(mySLRData.rawRangeFactor,lff2.substr(48,1),std::dec)) return false;
	    // The Normal Point Window Indicator and the Signal to Noise Ratio
	    // are only used for LLR data
	    if (!from_string<int>(itemp,lff2.substr(49,1),std::dec)) return false;
	    mySLRData.normalPointWindowIndicator2 = itemp;
	    if (!from_string<int>(itemp,lff2.substr(50,2),std::dec)) return false;
	    mySLRData.signalToNoiseRatio =  itemp * 0.1;

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
	    mySLRData.timeOfLaserFiring = dtemp * 1.0e-7;

	    if (!from_string<double>(dtemp,lff2.substr(12,12),std::dec)) return false;
	    // The data spec provides an integer in picoseconds that
	    // is too large to store efficiently. Here we convert to
	    // a real valued time
	    mySLRData.twoWayTimeOfFlight = dtemp * 1.0e-12;
	    
	    // Convert surface pressure to units of millibar
            if (!from_string<int>(itemp,lff2.substr(24,5),std::dec)) return false;
	    mySLRData.surfacePressure = itemp * 0.1; 
	    
	    // Convert surface temp to units of degrees Kelvin
            if (!from_string<int>(itemp,lff2.substr(29,4),std::dec)) return false;
	    mySLRData.surfaceTemp = itemp * 0.1;

            if (!from_string<int>(mySLRData.relativeHumidity,lff2.substr(33,3),std::dec)) return false;
            if (!from_string<int>(mySLRData.burstCalSysDelay,lff2.substr(36,8),std::dec)) return false;
            if (!from_string<int>(mySLRData.signalStrength,lff2.substr(44,4),std::dec)) return false;
            if (!from_string<int>(mySLRData.angleOriginIndicator,lff2.substr(48,1),std::dec)) return false;
            if (!from_string<int>(itemp,lff2.substr(49,7),std::dec)) return false;
	    // Convert az to degrees
	    mySLRData.az = itemp * 1e-4;
            if (!from_string<int>(itemp,lff2.substr(56,6),std::dec)) return false;
	    // Convert el to degrees
	    mySLRData.el = itemp * 1e-4;
                                                                                                                                                                                                                                                                                    
	    break;
	    
	default:
	    
	    return false;
	    	
    }

    return true;    
    
}
