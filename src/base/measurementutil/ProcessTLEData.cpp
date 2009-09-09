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
 * Implements DataFile base class to read files written in the TLE format.
 *
 */
//------------------------------------------------------------------------------

#include "ProcessTLEData.hpp"
#include "StringUtil.hpp"           // for ToString()
#include "math.h"

//#define DEBUG_TLE_DATA

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the datafile object.
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::Initialize()
{
    DataFile::Initialize();

    // check to make sure numLines is set
    if (numLines != 2 && numLines != 3)
    {
	MessageInterface::ShowMessage("\nThe number of lines in the TLE data format was not set!\n GMAT will assume that each TLE has no comment line.\n If this is not correct, set NumLines to the appropriate value and re-run the script.\n");
        numLines = 2;
    }

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Make sure that the b3Data vector has space reserved for
        // a minimum number of observations. This ensures that the
        // compiler does not unnecessarily reallocate the vector storage too-often.
        // The function reserve() will ensure that we have room for at least 50
        // elements. If the vector already has room for the required number of elements,
        // reserve() does nothing. In other words, reserve() will grow the allocated
        // storage of the vector, if necessary, but will never shrink it.
        tleData.reserve(50);

        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        TLEObtype *myTLE = new TLEObtype;

        while (!IsEOF())
        {
            if (GetData(myTLE))
            {
                tleData.push_back(myTLE);
            }
        
            // Allocate another struct in memory
            myTLE = new TLEObtype;

        }

        #ifdef DEBUG_TLE_DATA

            FILE * outFile;
            outFile = fopen("tle.output","w");

            // Output to file to make sure all the data is properly stored
            for (std::vector<TLEObtype*>::const_iterator j=tleData.begin(); j!=tleData.end(); ++j)
            {

                fprintf(outFile,"Satnum = %d\n",(*j)->satnum);
                fprintf(outFile,"Class = %s\n",(*j)->securityClassification.c_str());
                fprintf(outFile,"IntlDesignator = %s\n",(*j)->intlDesignator.c_str());
                fprintf(outFile,"Year = %d\n",(*j)->epochYear);
                fprintf(outFile,"Day Of Year = %16.8f\n",(*j)->epochDayOfYear);
                fprintf(outFile,"Ndotby2 = %16.8f\n",(*j)->ndotby2);
                fprintf(outFile,"Bstar = %16.8g\n",(*j)->bstar);
                fprintf(outFile,"Ndotby6 = %16.8g\n",(*j)->nddotby6);
                fprintf(outFile,"EphemType = %d\n",(*j)->ephemerisType);
                fprintf(outFile,"ElementNum = %d\n",(*j)->elementNum);
                fprintf(outFile,"Inclination = %16.8f\n",(*j)->inclination);
                fprintf(outFile,"Eccentricity = %16.8f\n",(*j)->eccentricity);
                fprintf(outFile,"RAAN = %16.8f\n",(*j)->raan);
                fprintf(outFile,"Argument of Perigee = %16.8f\n",(*j)->argPerigee);
                fprintf(outFile,"Mean Anomaly = %16.8f\n",(*j)->meanAnomaly);
                fprintf(outFile,"Mean Motion = %17.14f\n",(*j)->meanMotion);
                fprintf(outFile,"Rev Num = %d\n",(*j)->revolutionNum);
                fprintf(outFile,"\n******************************************************\n");

            }

            fclose(outFile);

        #endif
    
        // Set iterator to beginning of vector container
        i = tleData.begin();

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
//  ProcessTLEData() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessTLEData structures 
 */
ProcessTLEData::ProcessTLEData(const std::string &itsName) :
	DataFile ("TLEDataFile", itsName)
{
   objectTypeNames.push_back("TLEDataFile");
   fileFormatName = "TLE";
   fileFormatID = 2;
   // Initialize to 0 so that some value will be set and can be tested
   numLines = 0;
}


//------------------------------------------------------------------------------
//  ProcessTLEData::~ProcessTLEData() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessTLEData::~ProcessTLEData() 
{
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
bool ProcessTLEData::IsParameterReadOnly(const Integer id) const
{
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
bool ProcessTLEData::IsParameterReadOnly(const std::string &label) const
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
bool ProcessTLEData::AdvanceToNextOb() {

    ++i;

    if ( i == tleData.end() ) return false;
    
    return true;

}

//------------------------------------------------------------------------------
// bool BackUpToPreviousOb()
//------------------------------------------------------------------------------
/**
 * Returns the previous observation from the vector container.
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::BackUpToPreviousOb() {

    --i;

    if ( i == tleData.begin() ) return false;

    return true;

}

//------------------------------------------------------------------------------
// bool GetData(TLEObtype &myTLEdata)
//------------------------------------------------------------------------------
/** 
 * Obtains the next line of Two Line Element Set data from file.
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::GetData(TLEObtype *myTLEdata)
{

    if (numLines == 2)
    {
        // Read in two lines
        std::string line1 = ReadLineFromFile();
        std::string line2 = ReadLineFromFile();
        return GetTLEData(line1,line2,myTLEdata);

    }
    else if (numLines == 3)
    {

        // Read in three lines
        // TODO check to see whether 1st or 3rd line of 3 line element
        // set is the comment line
        // most likely need to use checksum feature to do this for certain
        // also adding the checksum would add a measure or data reliability
        std::string line1 = ReadLineFromFile();
        std::string line2 = ReadLineFromFile();
        std::string line3 = ReadLineFromFile();
        return GetTLEData(line1,line2,myTLEdata);

    }
    else
        return false;
    
    
}

//------------------------------------------------------------------------------
// bool GetTLEData(std::string lff, std::string lff2, TLEObtype *myTLEdata)
//------------------------------------------------------------------------------
/** 
 * Converts the compact Two Line Element Set data into usable numbers
 *
 * @param <lff> First of two lines required from file
 * @param <lff2> Second of two lines required from file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::GetTLEData(std::string &lff, std::string &lff2,
				TLEObtype *myTLEdata)
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
            from_string<int>(myTLEdata->satnum,satnum_1,std::dec);
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
            myTLEdata->securityClassification = classification_1;
	}
	else
	{    
	    myTLEdata->securityClassification = "";
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
	    myTLEdata->intlDesignator = temp;
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
	    myTLEdata->epochYear = epYear;
	    myTLEdata->epochDayOfYear = epdayOfYear;
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
	    myTLEdata->ndotby2 = 0;
	}
	else
	{
	    std::string p1, p2;
	    
	    if ( pcrecpp::RE("^([\\d+-]?)(.\\d{0,8})$").FullMatch(ndotby2_1,&p1,&p2) )
	    {
		double ndotby2;
		std::string str1 = p1 + p2;
		from_string<double>(ndotby2,str1,std::dec);
		myTLEdata->ndotby2 = ndotby2;
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
		myTLEdata->nddotby6 = 0;
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
		
		    myTLEdata->nddotby6 = nddotby6*pow(10,i3);
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
		    myTLEdata->bstar = 0;
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
			myTLEdata->bstar = bstar*pow(10,i3);
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
			myTLEdata->ephemerisType = ephemType;
		    } 
		    else
		    { 
			myTLEdata->ephemerisType = 0;
		    }
		    
		    if (lff.size() >= 68) 
		    {
			std::string elnum_1 = Trim(lff.substr(64,4));
        
			if (elnum_1 != "") 
			{
			    int elnum;
			    from_string<int>(elnum,elnum_1 ,std::dec);
			    myTLEdata->elementNum = elnum;
			}  
		    } 
		    else
		    {
			// Line 1 contains no critical data after this point
			myTLEdata->elementNum = 0;
		    }
		} 
		else
		{
		    // Line 1 contains no critical data after this point
		    myTLEdata->ephemerisType = 0;
		    myTLEdata->elementNum = 0;
		}
	    } 
	    else
	    {
		// Line 1 contains no critical data after this point
		myTLEdata->bstar = 0;
		myTLEdata->ephemerisType = 0;
		myTLEdata->elementNum = 0;
	    }
	} 
	else
	{
	    // Line 1 contains no critical data after this point
	    myTLEdata->nddotby6 = 0;
	    myTLEdata->bstar = 0;
	    myTLEdata->ephemerisType = 0;
	    myTLEdata->elementNum = 0;
	}
    } 
    else
    {
	// Line 1 contains no critical data after this point
	myTLEdata->ndotby2 = 0;
	myTLEdata->nddotby6 = 0;
	myTLEdata->bstar = 0;
	myTLEdata->ephemerisType = 0;
	myTLEdata->elementNum = 0;
    }    
    //
    // Start processing line 2 here
    //
    if (lff2.size() >= 7) 
    {
            
	int satnum2;
	std::string satnum2_1 = Trim(lff2.substr(2,5));
	from_string<int>(satnum2,satnum2_1,std::dec);	
	if (myTLEdata->satnum != satnum2)
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
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(incl,&myTLEdata->inclination))
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
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(raan,&myTLEdata->raan))
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
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(ecc,&myTLEdata->eccentricity))
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
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(argper,&myTLEdata->argPerigee))
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
	if (!pcrecpp::RE("^(-?(?:\\d+(?:\\.\\d*)?|\\.\\d+))$").FullMatch(ma,&myTLEdata->meanAnomaly))
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
	if (!pcrecpp::RE("^([+-]?\\d{0,3}\\.\\d{0,10})$").FullMatch(n,&myTLEdata->meanMotion))
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
	    myTLEdata->revolutionNum = -1;
	
	// make sure that the rev number is a natural number
	} 
	else if (!pcrecpp::RE("^(\\d+)$").FullMatch(revnum,&myTLEdata->revolutionNum))
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
// bool WriteData(TLEObtype *myTLEdata)
//------------------------------------------------------------------------------
/**
 * Writes a Two Line Element Set to file
 *
 * @param <myTLEdata> the TLE struct to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessTLEData::WriteData(TLEObtype *myTLEdata)
{

    // TLE's have a maximum length of 68 characters plus a 1 character checksum
    ostringstream buffer;

    // junk variable
    ostringstream temp;

    buffer << setw(2) << "1 ";
    buffer << setw(5) << setfill('0') << right << myTLEdata->satnum;
    buffer << setw(1) << myTLEdata->securityClassification;
    buffer << " " << setw(8) << left << setfill(' ') << myTLEdata->intlDesignator;
    buffer << " " << setw(2) << right << setfill('0') << myTLEdata->epochYear;
    buffer.precision(11);
    buffer << setw(12) << left << setfill(' ') << myTLEdata->epochDayOfYear;

    // Format first time derivative of the mean motion
    if (myTLEdata->ndotby2 == 0)
    {
        buffer << " " << setw(10) << "+.00000000";
    }
    else
    {
        char buffer2[15];
        sprintf(buffer2,"%.8f",myTLEdata->ndotby2);
        string str = buffer2;
        pcrecpp::RE("0\\.").GlobalReplace(".", &str);
        temp.str(std::string());
        temp << str;
        if (pcrecpp::RE("^-.*").FullMatch(temp.str()))
        {
            buffer << " " << setw(10) << setfill(' ') << right << temp.str();
        }
        else
        {
            buffer << " +" << setw(9) << left << temp.str();
        }
    }

    if (myTLEdata->nddotby6 == 0)
    {
        buffer << " " << setw(8) << "+00000-0";
    }
    else
    {
        char buffer2[15];
        // TODO: Find a better way to move decimal point
        // The multiplication by 10 is a lazy way
        sprintf(buffer2,"%+.4E",myTLEdata->nddotby6*10);
        string str = buffer2;
        pcrecpp::RE("\\.").GlobalReplace("", &str);
        pcrecpp::RE("E").GlobalReplace("", &str);
        pcrecpp::RE("-0").GlobalReplace("-", &str);
        pcrecpp::RE("+0").GlobalReplace("+", &str);
        temp.str(std::string());
        temp << str;
        buffer << " " << setw(8) << right << temp.str();
    }

    if (myTLEdata->bstar == 0)
    {
        buffer << " " << setw(8) << "+00000-0";
    }
    else
    {
        char buffer2[15];
        // TODO: Find a better way to move decimal point
        // The multiplication by 10 is a lazy way
        sprintf(buffer2,"%+.4E",myTLEdata->bstar*10);
        string str = buffer2;
        pcrecpp::RE("\\.").GlobalReplace("", &str);
        pcrecpp::RE("E").GlobalReplace("", &str);
        pcrecpp::RE("-0").GlobalReplace("-", &str);
        pcrecpp::RE("+0").GlobalReplace("+", &str);
        temp.str(std::string());
        temp << str;
        buffer << " " << setw(8) << right << temp.str();
    }

    buffer << " " << setw(1) << myTLEdata->ephemerisType;
    buffer << " " << setw(4) << right << myTLEdata->elementNum;

    *theFile << buffer.str();
    *theFile << setw(1) << TLECheckSum(buffer.str()) << endl;

    // Start output of line 2
    buffer.str(std::string());
    buffer << setw(2) << "2 ";
    buffer << setw(5) << setfill('0') << right << myTLEdata->satnum;
    char buffer3[15];
    sprintf(buffer3,"%08.4f",myTLEdata->inclination);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%08.4f",myTLEdata->raan);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%.7f",myTLEdata->eccentricity);
    string str = buffer3;
    pcrecpp::RE("0\\.").GlobalReplace("", &str);
    buffer << " " << setw(7) << right << str;
    sprintf(buffer3,"%08.4f",myTLEdata->argPerigee);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%08.4f",myTLEdata->meanAnomaly);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%011.8f",myTLEdata->meanMotion);
    buffer << " " << setw(11) << right << buffer3;
    buffer << setw(5) << setfill(' ') << right << myTLEdata->revolutionNum;

    *theFile << buffer.str();
    *theFile << setw(1) << TLECheckSum(buffer.str()) << endl;

    return true;
}

//------------------------------------------------------------------------------
//  Integer  TLECheckSum(const std::string &str)
//------------------------------------------------------------------------------
/**
 * This method computes the checksum for a string of TLE data
 *
 * @param <str> String of data
 *
 * @return Integer checksum modulo 10
 */
//------------------------------------------------------------------------------
Integer ProcessTLEData::TLECheckSum(const std::string &str)
{

    Integer checksum = 0;
    int num = 0 ;

    for ( Integer pos = 0; pos < (Integer)str.length(); ++pos )
    {
        // If it's a number, add the number to the checksum
        // if it's a minus sign, add +1
        // ignore everything else
        if (pcrecpp::RE("(\\d)").FullMatch(str.substr(pos,1),&num))
        {
            checksum += num;
            num = 0;
        }
        else if (pcrecpp::RE("-").FullMatch(str.substr(pos,1)))
        {
            checksum += 1;
        }
    }

    return GmatMathUtil::Mod(checksum,10);

}

