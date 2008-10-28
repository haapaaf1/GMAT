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
#include "iostream"
#include "fstream"
#include <pcrecpp.h>
#include "StringUtil.hpp"           // for ToString()

//---------------------------------
//  static data
//---------------------------------

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
    CloseFile();

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

    if(myFile.is_open())
    {
	SetIsOpen(true);
	return true;
    }
    else
    {
	SetIsOpen(false);
	return false;
    }
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

    if(myFile.is_open())
    {
	SetIsOpen(false);
	return false;
    }
    else
    {
	SetIsOpen(true);
	return true;
    }

}


//------------------------------------------------------------------------------
//  bool GetData(slr_header &mySLRheader)
//------------------------------------------------------------------------------
/**
 * Retrieves slr data.
 *
 * @return Boolean success or failure
 */
bool ProcessDataFile::GetData(slr_header &mySLRheader)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool GetData(slr_header &mySLRheader, slr_obtype &mySLRdata)
//------------------------------------------------------------------------------
/**
 * Retrieves slr data.
 *
 * @return Boolean success or failure
 */
bool ProcessDataFile::GetData(slr_header &mySLRheader, slr_obtype &mySLRdata)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool GetData(tle_obtype &myTLEdata)
//------------------------------------------------------------------------------
/**
 * Retrieves tle data.
 *
 * @return Boolean success or failure
 */
bool ProcessDataFile::GetData(tle_obtype &myTLEdata)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool GetData(tle_obtype &myB3data)
//------------------------------------------------------------------------------
/**
 * Retrieves B3 data.
 *
 * @return Boolean success or failure
 */
bool ProcessDataFile::GetData(b3_obtype &myB3data)
{
   return false;
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
// bool GetIsOpen() const
//------------------------------------------------------------------------------
bool ProcessDataFile::GetIsOpen() const
{
        return isOpen;
}

//------------------------------------------------------------------------------
// void SetIsOpen(const bool flag)
//------------------------------------------------------------------------------
void ProcessDataFile::SetIsOpen(const bool &flag)
{
        isOpen = flag;
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

////------------------------------------------------------------------------------
//// template <class T> bool ProcessDataFile::from_string(T& t,
////		   const std::string& s,
////                 std::ios_base& (*f)(std::ios_base&))
////------------------------------------------------------------------------------
///**
// * Typesafe conversion from string to integer, float, etc
// */
////------------------------------------------------------------------------------
//
//template <class T> bool ProcessDataFile::from_string(T& t, const std::string& s,
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

