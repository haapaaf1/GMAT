//$Header$
//------------------------------------------------------------------------------
//                             ProcessTLEData
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
 * Implements ProcessDataFile base class to read files written in the TLE format.
 *
 */
//------------------------------------------------------------------------------

#include "ProcessTLEData.hpp"
#include "gmatdefs.hpp"
#include "StringUtil.hpp"           // for ToString()
#include "ProcessDataFile.hpp"
#include "math.h"

//---------------------------------
//  static data
//---------------------------------

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  ProcessTLEData() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessTLEData structures 
 */
ProcessTLEData::ProcessTLEData(const std::string &itsType, 
			       const std::string &itsName) :
	ProcessDataFile (itsType, itsName)
{
   objectTypeNames.push_back("ProcessTLEData");
}


//------------------------------------------------------------------------------
//  ProcessTLEData::~ProcessTLEData() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessTLEData::~ProcessTLEData() 
{
    myFile.close();
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessTLEData.
 *
 * @return clone of the ProcessTLEData.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessTLEData::Clone() const
{
   GmatBase *clone = new ProcessTLEData(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// bool GetData(tle_obtype &myTLEdata)
//------------------------------------------------------------------------------
/** 
 * Obtains the next line of Two Line Element Set data from file.
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::GetData(tle_obtype &myTLEdata)
{
    
    // Read in two lines
    std::string line1 = ReadLineFromFile();
    std::string line2 = ReadLineFromFile();
    
    return GetTLEData(line1,line2,myTLEdata);
    
}

//------------------------------------------------------------------------------
// std::string GetTLEData(std::string lff, std::string lff2)
//------------------------------------------------------------------------------
/** 
 * Converts the compact Two Line Element Set data into usable numbers
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::GetTLEData(std::string &lff, 
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
	