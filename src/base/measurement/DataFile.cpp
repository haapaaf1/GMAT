//$Header$
//------------------------------------------------------------------------------
//                             DataFile
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

#include <DataFile.hpp>
#include "gmatdefs.hpp"
#include "iostream"
#include "fstream"
#include <pcrecpp.h>
#include "StringUtil.hpp"           // for ToString()

//---------------------------------
//  static data
//---------------------------------
const std::string DataFile::FILEFORMAT_DESCRIPTIONS[NUM_FILEFORMATS] =
{
    "B3 Data",
    "SLR Data",
    "TLE Data"
};

const std::string
DataFile::PARAMETER_TEXT[DataFileParamCount - GmatBaseParamCount] =
{
   "FileName",
   "FileFormat",
   "NumLines"
};


const Gmat::ParameterType
DataFile::PARAMETER_TYPE[DataFileParamCount - GmatBaseParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::INTEGER_TYPE
};

//---------------------------------
// methods overridden from GMAT base
//---------------------------------

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the DataFile.
 *
 * @return clone of the DataFile.
 */
//------------------------------------------------------------------------------
GmatBase* DataFile::Clone() const
{
   GmatBase *clone = new DataFile(*this);
   return (clone);
}

//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void DataFile::Copy(const GmatBase* orig)
{
   operator=(*((DataFile *)(orig)));
}

//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string DataFile::GetParameterText(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < DataFileParamCount))
   {
      //MessageInterface::ShowMessage("'%s':\n",
      //   PARAMETER_TEXT[id - GmatBaseParamCount].c_str());
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   }
   return GmatBase::GetParameterText(id);
}


//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer DataFile::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < DataFileParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType DataFile::GetParameterType(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < DataFileParamCount))
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the value for a std::string parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
std::string DataFile::GetStringParameter(const Integer id) const
{
   if (id == FILENAME_ID)
      return dataFileName;

   if (id == FILEFORMAT_ID)
      return fileFormatName;
          
   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool DataFile::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == FILENAME_ID)
   {
      dataFileName = value;
      return true;
   }

   if (id == FILEFORMAT_ID)
   {
      fileFormatName = value;
      fileFormatID = GetFileFormatID(value);
      return true;
   }
 
   return GmatBase::SetStringParameter(id, value);
   
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer DataFile::GetIntegerParameter(const Integer id) const
{
    if (id == NUMLINES_ID)
      return numLines;

    if (id == FILEFORMAT_ID)
      return fileFormatID;

    
    return GmatBase::GetIntegerParameter(id);
}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer DataFile::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 *
 */
//------------------------------------------------------------------------------
Integer DataFile::SetIntegerParameter(const Integer id, const Integer value)
{

   if (id == NUMLINES_ID)
   {
         numLines = value;
         return value;
   }
   
   return GmatBase::SetIntegerParameter(id, value);

}


//------------------------------------------------------------------------------
// virtual Integer SetIntegerParameter(std::string &label, const Integer value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer DataFile::SetIntegerParameter(const std::string &label, const Integer value)
{
   return SetIntegerParameter(GetParameterID(label), value);
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
bool DataFile::IsParameterReadOnly(const Integer id) const
{
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
bool DataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


//---------------------------------
//  public methods
//---------------------------------

// Initialize

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the datafile object.
 */
//------------------------------------------------------------------------------
bool DataFile::Initialize()
{
    std::ifstream myFile;
    if(!OpenFile(myFile))
    {
	throw DataFileException("Unable to open data file: " + dataFileName);
	MessageInterface::ShowMessage("Unable to open data file: " + dataFileName);
    }
}

//------------------------------------------------------------------------------
//  DataFile(Gmat::ObjectType ofType, const std::string &itsType,
//             const std::string &itsName)
//------------------------------------------------------------------------------
/**
 * Constructs base DataFile structures used in derived classes
 * (default constructor).
 *
 * @param <ofType>  Gmat::ObjectTypes enumeration for the object.
 * @param <itsType> GMAT script string associated with this type of object.
 * @param <itsName> Optional name for the object.  Defaults to "".
 *
 * @note There is no parameter free constructor for DataFile.  Derived
 *       classes must pass in the typeId and typeStr parameters.
 */
//------------------------------------------------------------------------------
DataFile::DataFile(const std::string &itsType, 
				 const std::string &itsName) :
    GmatBase(Gmat::DATA_FILE,itsType,itsName),
    fileFormatName (""),
//    numLines (0),
//    lineFromFile (""),
//    dataFileName (""),
    isOpen (false)
{
   objectTypes.push_back(Gmat::DATA_FILE);
   objectTypeNames.push_back("DataFile");
}

//------------------------------------------------------------------------------
//  DataFile::DataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for DataFile objects
 */
//------------------------------------------------------------------------------
DataFile::DataFile(const DataFile &pdf) :
    GmatBase       (pdf),
    fileFormatName (pdf.fileFormatName),
    numLines (pdf.numLines),
    lineFromFile (pdf.lineFromFile),
    dataFileName (pdf.dataFileName),
    isOpen (pdf.isOpen)
{
}

//------------------------------------------------------------------------------
//  DataFile::~DataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
DataFile::~DataFile()
{
}

//------------------------------------------------------------------------------
//  bool DataFile::OpenFile(std::ifstream &theFile)
//------------------------------------------------------------------------------
/**
 * Opens a physical file and creates a file handle pointing to it.
 *
 * @return Boolean success or failure
 */
bool DataFile::OpenFile(const std::ifstream &theFile)
{
    theFile.open(dataFileName.c_str());

    if(theFile.is_open())
    {
	isOpen = true;
	return true;
    }
    else
    {
	isOpen = false;
	return false;
    }
}

//------------------------------------------------------------------------------
//  bool DataFile::CloseFile()
//------------------------------------------------------------------------------
/**
 * Closes an open file handle.
 *
 * @return Boolean success or failure
 */
bool DataFile::CloseFile(const std::ifstream &theFile)
{
    theFile.close();

    if(theFile.is_open())
    {
	isOpen = false;
	return false;
    }
    else
    {
	isOpen = true;
	return true;
    }

}

//------------------------------------------------------------------------------
// const std::string* GetFileFormatDescriptions() const
//------------------------------------------------------------------------------
const std::string* DataFile::GetFileFormatDescriptions() const
{
   return FILEFORMAT_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetFileFormatDescriptionText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data format name text corresponding to a ID
 */
//------------------------------------------------------------------------------
std::string DataFile::GetFileFormatDescriptionText(const Integer &id) const
{
   if ((id >= 0) && (id < EndFileFormatReps))
   {
      return FILEFORMAT_DESCRIPTIONS[id];
   }

   return "INVALID";
}

//------------------------------------------------------------------------------
// Integer GetFileFormatID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data format ID
 */
//------------------------------------------------------------------------------
Integer DataFile::GetFileFormatID(const std::string &label)
{
    Integer retval = -1;
    
    if (label == "B3")
    {
       return B3_ID;
    } 
    else if (label == "SLR")
    {
       return SLR_ID;
    }
    else if (label == "TLE")
    {
       return TLE_ID;
    }
    else
     return retval;

}

//------------------------------------------------------------------------------
// Integer SetFileFormatID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Sets the name of the process data file object.
 */
//------------------------------------------------------------------------------
void DataFile::SetFileFormatID(const std::string &label)
{
   
    if (label == "B3")
    {
       fileFormatID = B3_ID;
    } 
    else if (label == "SLR")
    {
       fileFormatID = SLR_ID;
    }
    else if (label == "TLE")
    {
       fileFormatID = TLE_ID;
    }

}

//------------------------------------------------------------------------------
// Integer SetFileFormatID(Integer &pdfId)
//------------------------------------------------------------------------------
/**
 * Sets the format ID# of the data file object.
 *
 * @param <pdfId> The desired data file format ID to be set.
 */
//------------------------------------------------------------------------------
void DataFile::SetFileFormatID(const Integer &pdfId)
{
   fileFormatID = pdfId;
}

//------------------------------------------------------------------------------
// Integer GetFileFormatID() const
//------------------------------------------------------------------------------
/**
 * Finds the data format ID# of the data file object.
 *
 * @return The data format ID#.
 */
//------------------------------------------------------------------------------
Integer DataFile::GetFileFormatID() const
{
   return fileFormatID;
}


//------------------------------------------------------------------------------
// Integer SetNumLines(const Integer &nl)
//------------------------------------------------------------------------------
/**
 * Sets the number of lines of the data file object.
 *
 * @param <pdfId> The desired number of lines to be set.
 */
//------------------------------------------------------------------------------
void DataFile::SetNumLines(const Integer &nl)
{
   numLines = nl;
}

//------------------------------------------------------------------------------
// Integer GetNumLines() const
//------------------------------------------------------------------------------
/**
 * Finds the number of lines of the data file object.
 *
 * @return The number of lines to read in at a time.
 */
//------------------------------------------------------------------------------
Integer DataFile::GetNumLines() const
{
   return numLines;
}

//------------------------------------------------------------------------------
// void SetFileFormatName(std::string &fName)
//------------------------------------------------------------------------------
/**
 * Sets the name of the data file format.
 *
 * @param <fName> The desired data file format name to be set.
 */
//------------------------------------------------------------------------------
void DataFile::SetFileFormatName(const std::string &fName)
{
   fileFormatName = fName;
}

//------------------------------------------------------------------------------
// Integer GetFileFormatName() const
//------------------------------------------------------------------------------
/**
 * Finds the data format name of the data file object.
 *
 * @return The data format name.
 */
//------------------------------------------------------------------------------
std::string DataFile::GetFileFormatName() const
{
   return fileFormatName;
}

//------------------------------------------------------------------------------
//  bool GetData(std::ifstream &theFile, slr_header &mySLRheader)
//------------------------------------------------------------------------------
/**
 * Retrieves slr data.
 *
 * @return Boolean success or failure
 */
bool DataFile::GetData(std::ifstream &theFile, slr_header &mySLRheader)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool GetData(std::ifstream &theFile, slr_header &mySLRheader, slr_obtype &mySLRdata)
//------------------------------------------------------------------------------
/**
 * Retrieves slr data.
 *
 * @return Boolean success or failure
 */
bool DataFile::GetData(std::ifstream &theFile, slr_header &mySLRheader, slr_obtype &mySLRdata)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool GetData(std::ifstream &theFile, tle_obtype &myTLEdata)
//------------------------------------------------------------------------------
/**
 * Retrieves tle data.
 *
 * @return Boolean success or failure
 */
bool DataFile::GetData(std::ifstream &theFile, tle_obtype &myTLEdata)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool GetData(std::ifstream &theFile, tle_obtype &myB3data)
//------------------------------------------------------------------------------
/**
 * Retrieves B3 data.
 *
 * @return Boolean success or failure
 */
bool DataFile::GetData( std::ifstream &theFile, b3_obtype &myB3data)
{
   return false;
}


//------------------------------------------------------------------------------
//  bool DataFile::IsEOF(std::ifstream &theFile)
//------------------------------------------------------------------------------
/**
 * Check to see if the end of the file has been reached.
 *
 * @return True if end of file, False otherwise
 */
bool DataFile::IsEOF(std::ifstream &theFile)
{
    return theFile.eof();
}

//------------------------------------------------------------------------------
// std::string DataFile::ReadLineFromFile(std::ifstream &theFile)
//------------------------------------------------------------------------------
/**
 * Read a single line from a file.
 *
 * @return Line from file
 */
std::string DataFile::ReadLineFromFile(std::ifstream &theFile)
{
    std::string lff;
    getline(theFile,lff);
    return lff;
}

//----------------------------------------------------------------
//std::string DataFile::GetLine(Integer &lineNum)
//----------------------------------------------------------------
/**
 * Retrieve the lineFromFile parameter.
 *
 * @param <lineNum> Integer line number
 * @return Line from file
 */
std::string DataFile::GetLine(Integer &lineNum)
{
    return lineFromFile[lineNum];
}

//------------------------------------------------------------------------------
// void DataFile::SetLine(std::string &lff, Integer &lineNum)
//------------------------------------------------------------------------------
/**
 * Set the lineFromFile parameter.
 *
 * @param <lineNum> Integer line number
 * @param <lff> String of text
 */
void DataFile::SetLine(std::string &lff, Integer &lineNum)
{
    lineFromFile[lineNum] = lff;
}


//------------------------------------------------------------------------------
// std::string DataFile::GetFileName()
//------------------------------------------------------------------------------
/**
 * Retrieve the dataFileName parameter.
 *
  * @return dataFileName
 */
std::string DataFile::GetFileName()
{
    return dataFileName;
}

//------------------------------------------------------------------------------
// void DataFile::SetFileName(std::string &myFileName)
//------------------------------------------------------------------------------
void DataFile::SetFileName(std::string &myFileName)
/**
 * Set the dataFileName parameter using strings.
 *
 * @param <myFileName> Desired file name
 */
{
    dataFileName = myFileName;
}

//------------------------------------------------------------------------------
// void DataFile::SetFileName(const char* myFileName)
//------------------------------------------------------------------------------
/**
 * Set the dataFileName parameter using const char.
 *
 * @param <myFileName> Desired file name
 */
void DataFile::SetFileName(const char* myFileName)
{
    dataFileName = myFileName;
}


//------------------------------------------------------------------------------
// bool GetIsOpen() const
//------------------------------------------------------------------------------
bool DataFile::GetIsOpen() const
{
        return isOpen;
}

//------------------------------------------------------------------------------
// void SetIsOpen(const bool flag)
//------------------------------------------------------------------------------
void DataFile::SetIsOpen(const bool &flag)
{
        isOpen = flag;
}

//------------------------------------------------------------------------------
// std::string DataFile::Trim() const
//------------------------------------------------------------------------------
/**
 * Removes leading and trailing blanks from a string
 */
//------------------------------------------------------------------------------
std::string DataFile::Trim(std::string str)
{
    pcrecpp::RE("^\\s+").GlobalReplace("", &str);
    pcrecpp::RE("\\s+$").GlobalReplace("", &str);

    return str;
}

//------------------------------------------------------------------------------
// bool DataFile::Overpunch(const char &code, Integer &digit, Integer &sign )
//------------------------------------------------------------------------------
/**
 * Converts overpunch code to numeric value and determines appropriate sign
 */
//------------------------------------------------------------------------------
bool DataFile::Overpunch(std::string code, Integer &digit, Integer &sign )
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

