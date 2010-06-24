//$Header$
//------------------------------------------------------------------------------
//                             B3DataFile
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

#include <B3DataFile.hpp>

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
bool B3DataFile::Initialize()
{
    DataFile::Initialize();

    pcrecpp::RE re1("^[Rr].*");
    pcrecpp::RE re2("^[Ww].*");
    if (re1.FullMatch(readWriteMode))
    {

        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        B3ObType *myB3 = new B3ObType;

        while (!IsEOF())
        {
            if (GetData(myB3))
                theData.push_back(myB3);

            // Allocate another struct in memory
            myB3 = new B3ObType;
        }

        // Set iterator to beginning of vector container
        //i_theData = theData.begin();

        #ifdef DEBUG_B3_DATA

            B3DataFile myOutFile("theFile");
            myOutFile.SetReadWriteMode("w");
            myOutFile.SetFileName("B3.output");
            myOutFile.Initialize();
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
                myOutFile.WriteData((*j));
            myOutFile.CloseFile();

        #endif
	
	// This function will sort the data vector by epoch    
	SortByEpoch();
	
    }
    else if (re2.FullMatch(readWriteMode))
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
//  B3DataFile() 
//------------------------------------------------------------------------------
/**
 * Constructs base B3DataFile structures 
 */
//------------------------------------------------------------------------------
B3DataFile::B3DataFile(const std::string &itsName) :
	DataFile ("B3DataFile", itsName)
{
   objectTypeNames.push_back("B3DataFile");
   fileFormatName = "B3";
   fileFormatID = DataFile::B3_ID;
   numLines = 1;
}


//------------------------------------------------------------------------------
//  B3DataFile::B3DataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for B3DataFile objects
 */
//------------------------------------------------------------------------------
B3DataFile::B3DataFile(const B3DataFile &B3df) :
    DataFile      (B3df)
{
}


//------------------------------------------------------------------------------
//  B3DataFile::B3DataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for B3DataFile objects
 */
//------------------------------------------------------------------------------
const B3DataFile& B3DataFile::operator=(const B3DataFile &B3df)
{
    if (&B3df == this)
	return *this;

    DataFile::operator=(B3df);
    
    return *this;
	    
}

//------------------------------------------------------------------------------
//  B3DataFile::~B3DataFile() 
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
B3DataFile::~B3DataFile() 
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the B3DataFile.
 *
 * @return clone of the B3DataFile.
 */
//------------------------------------------------------------------------------
GmatBase* B3DataFile::Clone() const
{
   GmatBase *clone = new B3DataFile(*this);
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
bool B3DataFile::IsParameterReadOnly(const Integer id) const
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
bool B3DataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetData(ObType *myB3Data)
//------------------------------------------------------------------------------
/** 
 * Obtains the next line of b3 data from file.
 *
 * @param <myB3Data> Output pointer to B3 data struct containing data from file
 * @return Boolean success or failure to store data in B3 data struct
 */
//------------------------------------------------------------------------------
bool B3DataFile::GetData(ObType *myB3Data)
{

    if (myB3Data->GetTypeName() != "B3ObType") return false;

    // Re-cast the generic ObType pointer as a B3Obtype pointer
    B3ObType *myB3 = (B3ObType*)myB3Data;

    // read a line from file
    std::string lff = Trim(ReadLineFromFile());
        
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
    
    if (!from_string<int>(myB3->b3Type,lff.substr(74,1),std::dec)) return false;
    
    myB3->securityClassification = lff.substr(0,1);
   
    if (!from_string<int>(myB3->satID,lff.substr(1,5),std::dec)) return false;
    // Set the NORAD satellite ID in the ObType base class
    myB3->satelliteID = myB3->satID;
   
    if (!from_string<int>(myB3->sensID,lff.substr(6,3),std::dec)) return false;
    // Set the Sensor ID in the base class
    myB3->sensorID = myB3->sensID;

    if (!from_string<int>(myB3->year,lff.substr(9,2),std::dec)) return false;
    if (!from_string<int>(myB3->dayOfYear,lff.substr(11,3),std::dec)) return false;
    if (!from_string<int>(myB3->hour,lff.substr(14,2),std::dec)) return false;
    if (!from_string<int>(myB3->minute,lff.substr(16,2),std::dec)) return false;
    if (!from_string<int>(itemp,lff.substr(18,5),std::dec)) return false;
    myB3->seconds = itemp * 1e-3;

    // Set the epoch in the obtype base class to Goddard A1 time
    myB3->epoch = A1Date(myB3->year,myB3->dayOfYear,myB3->hour,myB3->minute,myB3->seconds);

    switch (myB3->b3Type)
    {
	case B3ObType::RANGERATEONLY_ID:
        {

	    // Test to see if the "-" character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision
            pcrecpp::RE re("^([-]?\\d+)$");
            if (re.FullMatch(lff.substr(47,7),&itemp))
	    {
		myB3->rangeRate = itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }
        }

	break;
            
	case B3ObType::AZEL_ID:
        {
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {
		myB3->elevation = itemp * 1e-4;
	    } 
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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
		myB3->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myB3->azimuth = itemp * 1e-4;
        }

        break;
            
	case B3ObType::RAZEL_ID:
        {
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {
		myB3->elevation = itemp * 1e-4;
	    } 
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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
		myB3->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myB3->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

	    myB3->range = range * pow(10,itemp);
        }

	break;
            
	case B3ObType::RAZELRR_ID:
        {

	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {
		myB3->elevation = itemp * 1e-4;
	    }
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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
		myB3->elevation = itemp * 1e-4;
	    } 
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myB3->azimuth = itemp * 1e-4;

	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

	    myB3->range = range * pow(10,itemp);

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision
            pcrecpp::RE re("^([-]?\\d+)$");
            if (re.FullMatch(lff.substr(47,7),&itemp))
	    {
		myB3->rangeRate = itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }
        }

	break;
            
	case B3ObType::RAZELRR2_ID:
        {
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {
		myB3->elevation = itemp * 1e-4;
	    }
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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

		myB3->elevation = itemp * 1e-4;
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

	    myB3->azimuth = itemp * 1e-4;

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

	    myB3->range = range * pow(10,itemp);

	    // Find range rate
    	    // Test to see if the "." character is present
	    // This represents a negative range rate value and there is
	    // one less decimal place of precision
            pcrecpp::RE re("^([-]?\\d+)$");
            if (re.FullMatch(lff.substr(47,7),&itemp))
	    {
		myB3->rangeRate = itemp * 1e-5;
	    }
	    else
	    {
		// Ill formed data

                return false;
	    }
        }

        break;
            
	case B3ObType::RADEC_ID:
        {
	    // Negative declination values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {
		myB3->declination = itemp * 1e-4;
	    }
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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
		myB3->declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
            pcrecpp::RE re("^(\\d{2})(\\d{2})(\\d{3})$");
            if (re.FullMatch(lff.substr(30,7),&itemp, &itemp2, &itemp3))
	    {
		myB3->rightAscension = itemp + itemp2/60.0 + itemp3/10.0/3600.0;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }
        }

        break;
            
	case B3ObType::RANGEONLY_ID:
        {
	    // Find range
	    if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
	    range = itemp * 1e-5;

	    // Find range exponent
	    if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

	    myB3->range = range * pow(10,itemp);
        }
        
        break;
            
	case B3ObType::AZELSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {

		myB3->elevation = itemp * 1e-4;
	    }
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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
		myB3->elevation = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find azimuth
	    if (!from_string<int>(itemp,lff.substr(30,7),std::dec)) return false;
	    myB3->azimuth = itemp * 1e-4;

	    // Check to see if range value defined. If 0 or blank, skip over
            pcrecpp::RE re("^[ 0]{6)$");
	    if(!re.FullMatch(lff.substr(23,6)))
	    {
		// Find range
		if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

		myB3->range = range * pow(10,itemp);
	    }
	    else
	    {
		myB3->range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff.substr(46,9),std::dec)) return false;
	    myB3->ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(55,9),std::dec)) return false;
	    myB3->ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(64,9),std::dec)) return false;
	    myB3->ecf_Z = itemp * 1e-3;
        }

        break;
            
	case B3ObType::RADECSENSORPOS_ID:
        {
	    // Negative declination values are formatted as ReverseOverpunched values
	    // Test to see if ReverseOverpunched and handle appropriately
            pcrecpp::RE re1("^(\\d+)$");
            pcrecpp::RE re2("^(\\d+)([a-zA-Z])$");
	    if (re1.FullMatch(lff.substr(23,6),&itemp))
	    {

		myB3->declination = itemp * 1e-4;
		
	    }
            else if (re2.FullMatch(lff.substr(23,6),&itemp, &code))
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
		myB3->declination = itemp * 1e-4;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Find right ascension in hours
            pcrecpp::RE re3("^(\\d{2})(\\d{2})(\\d{3})$");
            if (re3.FullMatch(lff.substr(30,7),&itemp, &itemp2, &itemp3))
	    {
		myB3->rightAscension = itemp + itemp2/60.0 + itemp3/10.0/3600.0;
	    }
	    else
	    {
		// Ill formed data
		return false;
	    }

	    // Check to see if range value defined. If 0 or blank, skip over
            pcrecpp::RE re4("^[ 0]{6)$");
	    if(!re4.FullMatch(lff.substr(23,6)))
	    {
		// Find range
		if (!from_string<int>(itemp,lff.substr(38,7),std::dec)) return false;
		range = itemp * 1e-5;

		// Find range exponent
		if (!from_string<int>(itemp,lff.substr(45,1),std::dec)) return false;

		myB3->range = range * pow(10,itemp);
	    }
	    else
	    {
		myB3->range = 0;
	    }
	    
	    // Find sensor position in kilometers
	    if (!from_string<int>(itemp,lff.substr(46,9),std::dec)) return false;
	    myB3->ecf_X = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(55,9),std::dec)) return false;
	    myB3->ecf_Y = itemp * 1e-3;
	    if (!from_string<int>(itemp,lff.substr(64,9),std::dec)) return false;
	    myB3->ecf_Z = itemp * 1e-3;
        }
	
        break;
            
	default:

	    // Ill formed data. All stop.           
	    return false;
    }
    
    return true;    

}

//------------------------------------------------------------------------------
// bool WriteData(const ObType *myB3Data)
//------------------------------------------------------------------------------
/**
 * Writes B3 data to file
 * @param <myB3Data> Input pointer to B3 data struct containing data from file
 * @return Boolean success or failure to write data in B3 data struct to file
 */
//------------------------------------------------------------------------------
bool B3DataFile::WriteData(const ObType *myB3Data)
{
    if (myB3Data->GetTypeName() == "B3ObType")
    {
        *theFile << (B3ObType*)myB3Data;
        return true;
    }
    else
        return false;
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
bool B3DataFile::IsParameterRequired(const Integer id) const
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
bool B3DataFile::IsParameterRequired(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}
