//$Header$
//------------------------------------------------------------------------------
//                             ProcessB3DataFile
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

#include <ProcessB3DataFile.hpp>

#define DEBUG_B3_DATA

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
bool ProcessB3DataFile::Initialize()
{
    DataFile::Initialize();
    
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        B3Obtype *myB3 = new B3Obtype;

	// read a line from file
	std::string lff = Trim(ReadLineFromFile());

        while (!IsEOF())
        {

            if (GetData(lff,myB3))
            {
	    	theData.push_back(myB3);
            }

            // Allocate another struct in memory
            myB3 = new B3Obtype;
	    
	    // read another line from file
	    lff = Trim(ReadLineFromFile());
	   
        }

        // Set iterator to beginning of vector container
        //i_theData = theData.begin();

        #ifdef DEBUG_B3_DATA

            fstream *outFile2 = new fstream;
            outFile2->open("test.output",ios::out);
	    
            // Output to file to make sure all the data is properly stored
            for (ObtypeVector::const_iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile2 << (B3Obtype*)(*j) << std::endl;
            }
        
            outFile2->close();

        #endif
/*	
	// This function will sort the data vector by epoch    
	SortByEpoch();
*/	
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
//  ProcessB3DataFile() 
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessB3DataFile structures 
 */
//------------------------------------------------------------------------------
ProcessB3DataFile::ProcessB3DataFile(const std::string &itsName) :
	DataFile ("B3DataFile", itsName)
{
   objectTypeNames.push_back("B3DataFile");
   fileFormatName = "B3";
   fileFormatID = 0;
   numLines = 1;
}


//------------------------------------------------------------------------------
//  ProcessB3DataFile::ProcessB3DataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessB3DataFile objects
 */
//------------------------------------------------------------------------------
ProcessB3DataFile::ProcessB3DataFile(const ProcessB3DataFile &B3df) :
    DataFile      (B3df)
{
}


//------------------------------------------------------------------------------
//  ProcessB3DataFile::ProcessB3DataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessB3DataFile objects
 */
//------------------------------------------------------------------------------
const ProcessB3DataFile& ProcessB3DataFile::operator=(const ProcessB3DataFile &B3df)
{
    if (&B3df == this)
	return *this;

    fileFormatName = B3df.fileFormatName;
    dataFileName = B3df.dataFileName;
    numLines = B3df.numLines;
    isOpen = B3df.isOpen;
    isSortedByEpoch = B3df.isSortedByEpoch;
    isSortedBySatID = B3df.isSortedBySatID;
    isSortedBySensorID = B3df.isSortedBySensorID;
    isSortedByInternationalDesignator = B3df.isSortedByInternationalDesignator;
    readWriteMode = B3df.readWriteMode;
    theFile = B3df.theFile;
    
    return *this;
	    
}

//------------------------------------------------------------------------------
//  ProcessB3DataFile::~ProcessB3DataFile() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessB3DataFile::~ProcessB3DataFile() 
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessB3DataFile.
 *
 * @return clone of the ProcessB3DataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessB3DataFile::Clone() const
{
   GmatBase *clone = new ProcessB3DataFile(*this);
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
bool ProcessB3DataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessB3DataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetData(B3Obtype *myB3Data)
//------------------------------------------------------------------------------
/** 
 * Obtains the next line of b3 data from file.
 *
 * @param <myB3Data> Output pointer to B3 data struct containing data from file
 * @return Boolean success or failure to store data in B3 data struct
 */
//------------------------------------------------------------------------------
bool ProcessB3DataFile::GetData(std::string lff, B3Obtype *myb3Data)
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
    
    // Check to make sure that the line length is at
    // least 75 characters long

    if (lff.length() < 75)
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
    
    if (!from_string<int>(myb3Data->b3Type,lff.substr(74,1),std::dec)) return false;
    
    myb3Data->securityClassification = lff.substr(0,1);
   
    if (!from_string<int>(myb3Data->satID,lff.substr(1,5),std::dec)) return false;
    // Set the NORAD satellite ID in the Obtype base class
    myb3Data->satelliteID = myb3Data->satID;
   
    if (!from_string<int>(myb3Data->sensID,lff.substr(6,3),std::dec)) return false;
    // Set the Sensor ID in the base class
    myb3Data->sensorID = myb3Data->sensID;

    if (!from_string<int>(myb3Data->year,lff.substr(9,2),std::dec)) return false;
    if (!from_string<int>(myb3Data->dayOfYear,lff.substr(11,3),std::dec)) return false;
    if (!from_string<int>(myb3Data->hour,lff.substr(14,2),std::dec)) return false;
    if (!from_string<int>(myb3Data->minute,lff.substr(16,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff.substr(18,5),std::dec)) return false;
    myb3Data->seconds = itemp * 1e-3;

    // Set the epoch in the obtype base class to Goddard A1 time
    myb3Data->epoch = A1Date(myb3Data->year,myb3Data->dayOfYear,myb3Data->hour,myb3Data->minute,myb3Data->seconds);

    switch (myb3Data->b3Type)
    {
	case B3Obtype::RANGERATEONLY_ID:

	    // Test to see if the "-" character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
            if (pcrecpp::RE("^([-]?\\d+)$").FullMatch(lff.substr(47,7),&itemp))
	    {
		myb3Data->rangeRate = itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }

	    break;
            
	case B3Obtype::AZEL_ID:
	    
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {
		myb3Data->elevation = itemp * 1e-4;
	    } 
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code.c_str(),digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		myb3Data->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myb3Data->azimuth = itemp * 1e-4;
	                
	    break;
            
	case B3Obtype::RAZEL_ID:
            
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {
		myb3Data->elevation = itemp * 1e-4;
	    } 
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code.c_str(),digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		myb3Data->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myb3Data->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

	    myb3Data->range = range * pow(10,itemp);
	    
	    break;
            
	case B3Obtype::RAZELRR_ID:

	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {
		myb3Data->elevation = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		myb3Data->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myb3Data->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

	    myb3Data->range = range * pow(10,itemp);

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
            if (pcrecpp::RE("^([-]?\\d+)$").FullMatch(lff.substr(47,7),&itemp))
	    {
		myb3Data->rangeRate = itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }

	    break;
            
	case B3Obtype::RAZELRR2_ID:

	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {
		myb3Data->elevation = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec))
                {
                    return false;
                }

		myb3Data->elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec))
            {
               return false;
            }

	    myb3Data->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec))
            {
               return false;
            }

	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec))
            {
               return false;
            }

	    myb3Data->range = range * pow(10,itemp);

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision 
            if (pcrecpp::RE("^([-]?\\d+)$").FullMatch(lff.substr(47,7),&itemp))
	    {
		myb3Data->rangeRate = itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }
            
	    break;
            
	case B3Obtype::RADEC_ID:
	    
	    // Negative declination values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {
		myb3Data->declination = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string decl = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,decl,std::dec))
                {
                    return false;
                }
		myb3Data->declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
            if (pcrecpp::RE("^(\\d{2})(\\d{2})(\\d{3})$").FullMatch(lff.substr(30,7),&itemp, &itemp2, &itemp3))
	    {
		myb3Data->rightAscension = itemp + itemp2/60.0 + itemp3/10.0/3600.0;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }
	    
	    break;
            
	case B3Obtype::RANGEONLY_ID:

	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

	    myb3Data->range = range * pow(10,itemp);
	    	    
	    break;
            
	case B3Obtype::AZELSENSORPOS_ID:
            
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {

		myb3Data->elevation = itemp * 1e-4;
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string elev = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,elev,std::dec)) return false;
		myb3Data->elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myb3Data->azimuth = itemp * 1e-4;

	    // Check to see if range value defined. If 0 or blank, skip over
	    if(!pcrecpp::RE("^[ 0]{6)$").FullMatch(lff.substr(23,6))) 
	    {
		// Find range
		if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

		myb3Data->range = range * pow(10,itemp);
	    }
	    else
	    {
		myb3Data->range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff.substr(46,9),std::dec)) return false;
	    myb3Data->ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(55,9),std::dec)) return false;
	    myb3Data->ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(64,9),std::dec)) return false;
	    myb3Data->ecf_Z = itemp * 1e-3;
	    
	    break;
            
	case B3Obtype::RADECSENSORPOS_ID:
            
	    // Negative declination values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
	    if (pcrecpp::RE("^(\\d+)$").FullMatch(lff.substr(23,6),&itemp)) 
	    {

		myb3Data->declination = itemp * 1e-4;
		
	    }
	    else if (pcrecpp::RE("^(\\d+)([a-zA-Z])$").FullMatch(lff.substr(23,6),&itemp, &code)) 
	    {
		
		Integer digit, sign;
		
		// Figure out the Overpunch code
		ReverseOverpunch(code,digit,sign);
		
		// Apply +/- sign
		itemp *= sign;
		
		// Attach ReverseOverpunch digit to end of number string
		std:: string decl = GmatStringUtil::ToString(itemp,6)+GmatStringUtil::ToString(digit,1);
		
		// Convert completed number string to final form
		if (!from_string<int>(itemp,decl,std::dec)) return false;
		myb3Data->declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
            if (pcrecpp::RE("^(\\d{2})(\\d{2})(\\d{3})$").FullMatch(lff.substr(30,7),&itemp, &itemp2, &itemp3))
	    {
		myb3Data->rightAscension = itemp + itemp2/60.0 + itemp3/10.0/3600.0;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Check to see if range value defined. If 0 or blank, skip over
	    if(!pcrecpp::RE("^[ 0]{6)$").FullMatch(lff.substr(23,6))) 
	    {
		// Find range
		if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

		myb3Data->range = range * pow(10,itemp);
	    }
	    else
	    {
		myb3Data->range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff.substr(46,9),std::dec)) return false;
	    myb3Data->ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(55,9),std::dec)) return false;
	    myb3Data->ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(64,9),std::dec)) return false;
	    myb3Data->ecf_Z = itemp * 1e-3;

	    break;
            
	default:

	    // Ill formed data. All stop.           
	    return false;
    }
    
    return true;    

}

//------------------------------------------------------------------------------
// bool WriteData(B3Obtype *myB3Data)
//------------------------------------------------------------------------------
/**
 * Writes B3 data to file
 * @param <myB3Data> Input pointer to B3 data struct containing data from file
 * @return Boolean success or failure to write data in B3 data struct to file
 */
//------------------------------------------------------------------------------
bool ProcessB3DataFile::WriteData(B3Obtype *myB3Data)
{

    // Verify the type of the b3 observation
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

    *theFile << setw(1) << myB3Data->securityClassification;
    *theFile << setw(5) << right << myB3Data->satelliteID;
    *theFile << setw(3) << right << myB3Data->sensorID;
    *theFile << setw(2) << setfill('0') << myB3Data->year;
    *theFile << setw(3) << setfill('0') << myB3Data->dayOfYear;
    *theFile << setw(2) << setfill('0') << myB3Data->hour;
    *theFile << setw(2) << setfill('0') << myB3Data->minute;
    int sec = myB3Data->seconds*1e3 + 0.5;
    *theFile << setw(5) << setfill('0') << sec;

    switch (myB3Data->b3Type)
    {
	case B3Obtype::RANGERATEONLY_ID:
        {

            *theFile << setw(23) << setfill(' ') << right << " ";

            if (myB3Data->rangeRate > 0)
            {
                int rangeRate = myB3Data->rangeRate*1e5 + 0.5;
                *theFile << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3Data->rangeRate*1e5;
                *theFile << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            *theFile << setw(21) << right << setfill(' ') << myB3Data->b3Type;

            break;
        }
	case B3Obtype::AZEL_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*theFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *theFile << " " << setw(7) << right << setfill('0') << azimuth;

            *theFile << setw(38) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RAZEL_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*theFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *theFile << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *theFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *theFile << " " << setw(7) << right << mantissa;
            *theFile << setw(1) << exponent;

            *theFile << setw(29) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RAZELRR_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*theFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *theFile << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *theFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *theFile << " " << setw(7) << right << mantissa;
            *theFile << setw(1) << exponent;

	    // For a negative range rate value, there is one less
	    // decimal place of precision
            // width is actually 7 but have to place at position 47

            if (myB3Data->rangeRate > 0)
            {
                int rangeRate = myB3Data->rangeRate*1e5 + 0.5;
                *theFile << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3Data->rangeRate*1e5;
                *theFile << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            *theFile << setw(21) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RAZELRR2_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*theFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *theFile << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *theFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *theFile << " " << setw(7) << right << mantissa;
            *theFile << setw(1) << exponent;

	    // For a negative range rate value, there is one less
	    // decimal place of precision
            // width is actually 7 but have to place at position 47
            if (myB3Data->rangeRate > 0)
            {
                int rangeRate = myB3Data->rangeRate*1e5 + 0.5;
                *theFile << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3Data->rangeRate*1e5;
                *theFile << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            *theFile << setw(21) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
        case B3Obtype::RADEC_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->declination > 0)
	    {
                int declination = myB3Data->declination*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << declination;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int declination = myB3Data->declination*1e4;
                *theFile << setw(6) << right << setfill('0') << Overpunch(declination);
            }

	    // Find right ascension in hours, minutes, and seconds
            int hr = myB3Data->rightAscension;
            double minutes = (myB3Data->rightAscension - hr)*60.0;
            int mn = minutes;
            double seconds = (minutes-mn)*60.0;
            // we want SSS but have SS.SSSSSS
            // so mutiply by 10 and truncate
            int sec = seconds*10 + 0.5;

            *theFile << " " << setw(2) << right << setfill('0') << hr;
            *theFile << setw(2) << right << setfill('0') << mn;
            *theFile << setw(3) << right << setfill('0') << sec;

            *theFile << setw(38) << right << setfill(' ') << myB3Data->b3Type;
            
	    break;
        }
	case B3Obtype::RANGEONLY_ID:
        {
            *theFile << setw(14) << right << setfill(' ') << " ";
            
	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *theFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *theFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *theFile << " " << setw(7) << right << mantissa;
            *theFile << setw(1) << exponent;

            *theFile << setw(29) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::AZELSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*theFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *theFile << " " << setw(7) << right << setfill('0') << azimuth;

            // If range is defined, then output range
            if (myB3Data->range > 0)
            {
                // Decompose range into RRRRRRR*10^E
                // ouptut integer range and range exponent separately
                ostringstream temp;
                char buffer[25];
                sprintf(buffer,"%010.7E",myB3Data->range);
                temp << buffer;
                int found = temp.str().find("E");
                std::string mantemp = temp.str().substr(0,found);
                std::string exptemp = temp.str().substr(found+1,temp.str().length());

                // Find range mantissa
                double mantissa1;
                if (!from_string<double>(mantissa1,mantemp,std::dec))
                {
                    *theFile << endl;
                    return false;
                }
                int mantissa = mantissa1*1e6+0.5;

                // Find range exponent
                int exponent;
                if (!from_string<int>(exponent,exptemp,std::dec))
                {
                    *theFile << endl;
                    return false;
                }
                if (exponent < 0 || exponent >= 5)
                {
                    *theFile << endl;
                    return false;
                }

                // Range is formated RR.RRRRRR*10^E
                // traditional scientific notation is R.RRRRRRR*10^E
                // So we must account for the movement of the decimal place
                // to display to correct exponent according to the B3 format
                exponent -= 1;

                *theFile << " " << setw(7) << right << mantissa;
                *theFile << setw(1) << exponent;

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Z;

            }
            else
            {

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *theFile << setw(18) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Z;

            }

            *theFile << " " << setw(1) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RADECSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->declination > 0)
	    {
                int declination = myB3Data->declination*1e4 + 0.5;
		*theFile << setw(6) << right << setfill('0') << declination;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int declination = myB3Data->declination*1e4;
                *theFile << setw(6) << right << setfill('0') << Overpunch(declination);
            }

	    // Find right ascension in hours, minutes, and seconds
            int hr = myB3Data->rightAscension;
            double minutes = (myB3Data->rightAscension - hr)*60.0;
            int mn = minutes;
            double seconds = (minutes-mn)*60.0;
            // we want SSS but have SS.SSSSSS
            // so mutiply by 10 and truncate
            int sec = seconds*10 + 0.5;

            *theFile << " " << setw(2) << right << setfill('0') << hr;
            *theFile << setw(2) << right << setfill('0') << mn;
            *theFile << setw(3) << right << setfill('0') << sec;

            // If range is defined, then output range
            // in addition to the sensor coordinates
            if (myB3Data->range > 0)
            {
                // Decompose range into RRRRRRR*10^E
                // ouptut integer range and range exponent separately
                ostringstream temp;
                char buffer[25];
                sprintf(buffer,"%010.7E",myB3Data->range);
                temp << buffer;
                int found = temp.str().find("E");
                std::string mantemp = temp.str().substr(0,found);
                std::string exptemp = temp.str().substr(found+1,temp.str().length());

                // Find range mantissa
                double mantissa1;
                if (!from_string<double>(mantissa1,mantemp,std::dec))
                {
                    *theFile << endl;
                    return false;
                }
                int mantissa = mantissa1*1e6+0.5;

                // Find range exponent
                int exponent;
                if (!from_string<int>(exponent,exptemp,std::dec))
                {
                    *theFile << endl;
                    return false;
                }
                if (exponent < 0 || exponent >= 5)
                {
                    *theFile << endl;
                    return false;
                }

                // Range is formated RR.RRRRRR*10^E
                // traditional scientific notation is R.RRRRRRR*10^E
                // So we must account for the movement of the decimal place
                // to display to correct exponent according to the B3 format
                exponent -= 1;

                *theFile << " " << setw(7) << right << mantissa;
                *theFile << setw(1) << exponent;

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Z;
            }
            else
            {

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *theFile << setw(18) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *theFile << setw(9) << right << ecf_Z;
            }

            *theFile << " " << setw(1) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	default:

	    // Not a recognized B3 data format. All stop.
	    return false;

    }

    *theFile << endl;

    return true;

}

//------------------------------------------------------------------------------
// bool WriteData(B3Obtype *myB3Data, fstream *myFile)
//------------------------------------------------------------------------------
/**
 * Writes B3 data to file
 * @param <myB3Data> Input pointer to B3 data struct containing data from file
 * @param <myFile> Fstream pointer to desired output file
 * @return Boolean success or failure to write data in B3 data struct to file
 */
//------------------------------------------------------------------------------
bool ProcessB3DataFile::WriteData(B3Obtype *myB3Data, fstream *myFile)
{

    // Verify the type of the b3 observation
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

    *myFile << setw(1) << myB3Data->securityClassification;
    *myFile << setw(5) << right << myB3Data->satelliteID;
    *myFile << setw(3) << right << myB3Data->sensorID;
    *myFile << setw(2) << setfill('0') << myB3Data->year;
    *myFile << setw(3) << setfill('0') << myB3Data->dayOfYear;
    *myFile << setw(2) << setfill('0') << myB3Data->hour;
    *myFile << setw(2) << setfill('0') << myB3Data->minute;
    int sec = myB3Data->seconds*1e3 + 0.5;
    *myFile << setw(5) << setfill('0') << sec;

    switch (myB3Data->b3Type)
    {
	case B3Obtype::RANGERATEONLY_ID:
        {

            *myFile << setw(23) << setfill(' ') << right << " ";

            if (myB3Data->rangeRate > 0)
            {
                int rangeRate = myB3Data->rangeRate*1e5 + 0.5;
                *myFile << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3Data->rangeRate*1e5;
                *myFile << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            *myFile << setw(21) << right << setfill(' ') << myB3Data->b3Type;

            break;
        }
	case B3Obtype::AZEL_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*myFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *myFile << " " << setw(7) << right << setfill('0') << azimuth;

            *myFile << setw(38) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RAZEL_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*myFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *myFile << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *myFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *myFile << " " << setw(7) << right << mantissa;
            *myFile << setw(1) << exponent;

            *myFile << setw(29) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RAZELRR_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*myFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *myFile << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *myFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *myFile << " " << setw(7) << right << mantissa;
            *myFile << setw(1) << exponent;

	    // For a negative range rate value, there is one less
	    // decimal place of precision
            // width is actually 7 but have to place at position 47

            if (myB3Data->rangeRate > 0)
            {
                int rangeRate = myB3Data->rangeRate*1e5 + 0.5;
                *myFile << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3Data->rangeRate*1e5;
                *myFile << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            *myFile << setw(21) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RAZELRR2_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*myFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *myFile << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *myFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *myFile << " " << setw(7) << right << mantissa;
            *myFile << setw(1) << exponent;

	    // For a negative range rate value, there is one less
	    // decimal place of precision
            // width is actually 7 but have to place at position 47
            if (myB3Data->rangeRate > 0)
            {
                int rangeRate = myB3Data->rangeRate*1e5 + 0.5;
                *myFile << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3Data->rangeRate*1e5;
                *myFile << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            *myFile << setw(21) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
        case B3Obtype::RADEC_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->declination > 0)
	    {
                int declination = myB3Data->declination*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << declination;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int declination = myB3Data->declination*1e4;
                *myFile << setw(6) << right << setfill('0') << Overpunch(declination);
            }

	    // Find right ascension in hours, minutes, and seconds
            int hr = myB3Data->rightAscension;
            double minutes = (myB3Data->rightAscension - hr)*60.0;
            int mn = minutes;
            double seconds = (minutes-mn)*60.0;
            // we want SSS but have SS.SSSSSS
            // so mutiply by 10 and truncate
            int sec = seconds*10 + 0.5;

            *myFile << " " << setw(2) << right << setfill('0') << hr;
            *myFile << setw(2) << right << setfill('0') << mn;
            *myFile << setw(3) << right << setfill('0') << sec;

            *myFile << setw(38) << right << setfill(' ') << myB3Data->b3Type;
            
	    break;
        }
	case B3Obtype::RANGEONLY_ID:
        {
            *myFile << setw(14) << right << setfill(' ') << " ";
            
	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3Data->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                *myFile << endl;
                return false;
            }
            if (exponent < 0 || exponent >= 5)
            {
                *myFile << endl;
                return false;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            *myFile << " " << setw(7) << right << mantissa;
            *myFile << setw(1) << exponent;

            *myFile << setw(29) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::AZELSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->elevation > 0)
	    {
                int elevation = myB3Data->elevation*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3Data->elevation*1e4;
		*myFile << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3Data->azimuth*1e4 + 0.5;
	    *myFile << " " << setw(7) << right << setfill('0') << azimuth;

            // If range is defined, then output range
            if (myB3Data->range > 0)
            {
                // Decompose range into RRRRRRR*10^E
                // ouptut integer range and range exponent separately
                ostringstream temp;
                char buffer[25];
                sprintf(buffer,"%010.7E",myB3Data->range);
                temp << buffer;
                int found = temp.str().find("E");
                std::string mantemp = temp.str().substr(0,found);
                std::string exptemp = temp.str().substr(found+1,temp.str().length());

                // Find range mantissa
                double mantissa1;
                if (!from_string<double>(mantissa1,mantemp,std::dec))
                {
                    *myFile << endl;
                    return false;
                }
                int mantissa = mantissa1*1e6+0.5;

                // Find range exponent
                int exponent;
                if (!from_string<int>(exponent,exptemp,std::dec))
                {
                    *myFile << endl;
                    return false;
                }
                if (exponent < 0 || exponent >= 5)
                {
                    *myFile << endl;
                    return false;
                }

                // Range is formated RR.RRRRRR*10^E
                // traditional scientific notation is R.RRRRRRR*10^E
                // So we must account for the movement of the decimal place
                // to display to correct exponent according to the B3 format
                exponent -= 1;

                *myFile << " " << setw(7) << right << mantissa;
                *myFile << setw(1) << exponent;

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Z;

            }
            else
            {

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *myFile << setw(18) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Z;

            }

            *myFile << " " << setw(1) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	case B3Obtype::RADECSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3Data->declination > 0)
	    {
                int declination = myB3Data->declination*1e4 + 0.5;
		*myFile << setw(6) << right << setfill('0') << declination;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int declination = myB3Data->declination*1e4;
                *myFile << setw(6) << right << setfill('0') << Overpunch(declination);
            }

	    // Find right ascension in hours, minutes, and seconds
            int hr = myB3Data->rightAscension;
            double minutes = (myB3Data->rightAscension - hr)*60.0;
            int mn = minutes;
            double seconds = (minutes-mn)*60.0;
            // we want SSS but have SS.SSSSSS
            // so mutiply by 10 and truncate
            int sec = seconds*10 + 0.5;

            *myFile << " " << setw(2) << right << setfill('0') << hr;
            *myFile << setw(2) << right << setfill('0') << mn;
            *myFile << setw(3) << right << setfill('0') << sec;

            // If range is defined, then output range
            // in addition to the sensor coordinates
            if (myB3Data->range > 0)
            {
                // Decompose range into RRRRRRR*10^E
                // ouptut integer range and range exponent separately
                ostringstream temp;
                char buffer[25];
                sprintf(buffer,"%010.7E",myB3Data->range);
                temp << buffer;
                int found = temp.str().find("E");
                std::string mantemp = temp.str().substr(0,found);
                std::string exptemp = temp.str().substr(found+1,temp.str().length());

                // Find range mantissa
                double mantissa1;
                if (!from_string<double>(mantissa1,mantemp,std::dec))
                {
                    *myFile << endl;
                    return false;
                }
                int mantissa = mantissa1*1e6+0.5;

                // Find range exponent
                int exponent;
                if (!from_string<int>(exponent,exptemp,std::dec))
                {
                    *myFile << endl;
                    return false;
                }
                if (exponent < 0 || exponent >= 5)
                {
                    *myFile << endl;
                    return false;
                }

                // Range is formated RR.RRRRRR*10^E
                // traditional scientific notation is R.RRRRRRR*10^E
                // So we must account for the movement of the decimal place
                // to display to correct exponent according to the B3 format
                exponent -= 1;

                *myFile << " " << setw(7) << right << mantissa;
                *myFile << setw(1) << exponent;

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Z;
            }
            else
            {

                // Find sensor position in meters
                int ecf_X = myB3Data->ecf_X * 1e3 + 0.5;
                *myFile << setw(18) << right << ecf_X;
                int ecf_Y = myB3Data->ecf_Y * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Y;
                int ecf_Z = myB3Data->ecf_Z * 1e3 + 0.5;
                *myFile << setw(9) << right << ecf_Z;
            }

            *myFile << " " << setw(1) << right << setfill(' ') << myB3Data->b3Type;

	    break;
        }
	default:

	    // Not a recognized B3 data format. All stop.
	    return false;

    }

    *myFile << endl;

    return true;

}

//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is required, false (the default)
 */
//---------------------------------------------------------------------------
bool ProcessB3DataFile::IsParameterRequired(const Integer id) const
{
   if (id == NUMLINES_ID)  return true;
   if (id == FILEFORMAT_ID)  return true;
   return GmatBase::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterRequired(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is required, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool ProcessB3DataFile::IsParameterRequired(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}
