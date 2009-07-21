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

//---------------------------------
//  static data
//---------------------------------
const std::string DataFile::FILEFORMAT_DESCRIPTIONS[EndFileFormatReps] =
{
    "B3 Data",
    "SLR Data",
    "TLE Data",
    "SP3c Data",
    "RINEX Data",
    "UTDF Data"
};

const std::string
DataFile::PARAMETER_TEXT[DataFileParamCount - GmatBaseParamCount] =
{
   "FileName",
   "FileFormat",
   "NumLines",
   "ReadWriteMode"
};


const Gmat::ParameterType
DataFile::PARAMETER_TYPE[DataFileParamCount - GmatBaseParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::STRING_TYPE
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



//---------------------------------------------------------------------------
//  std::string GetParameterTypeString(const Integer id) const
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
std::string DataFile::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}


//------------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string DataFile::GetStringParameter(const Integer id) const
{
   if (id == FILENAME_ID)
      return dataFileName;

   if (id == READWRITEMODE_ID)
      return readWriteMode;

   if (id == FILEFORMAT_ID)
      return fileFormatName;
          
   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool DataFile::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == FILENAME_ID)
   {
      dataFileName = value;
      return true;
   }

   if (id == READWRITEMODE_ID)
   {
      readWriteMode = value;
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
/**
 * @see GmatBase
 */
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
// virtual IntegerArray GetIntegerArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
IntegerArray DataFile::GetIntegerArrayDataParameter(const Integer id) const
{

    return GmatBase::GetIntegerArrayParameter(id);
}


//------------------------------------------------------------------------------
// virtual IntegerArray GetIntegerArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
IntegerArray DataFile::GetIntegerArrayDataParameter(const std::string &label) const
{
   return GetIntegerArrayParameter(GetParameterID(label));
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
    return OpenFile();
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
        numMeasurements (0),
//    lineFromFile (""),
//    dataFileName (""),
    isOpen (false),
    isSorted (false),
    readWriteMode ("read")
{
   objectTypes.push_back(Gmat::DATA_FILE);
   objectTypeNames.push_back("DataFile");
   theFile = new fstream;
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
    dataFileName (pdf.dataFileName),
    numLines (pdf.numLines),
    numMeasurements (pdf.numMeasurements),
//    lineFromFile (pdf.lineFromFile),
    isOpen (pdf.isOpen),
    isSorted (pdf.isSorted),
    readWriteMode(pdf.readWriteMode),
    theFile (pdf.theFile)
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
    CloseFile();
}

//------------------------------------------------------------------------------
//  bool DataFile::OpenFile()
//------------------------------------------------------------------------------
/**
 * Opens a physical file and creates a file handle pointing to it.
 *
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool DataFile::OpenFile()
{
    
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {
        theFile->open(dataFileName.c_str(),ios::in);
    }
    else if (pcrecpp::RE("^[Ww].*").FullMatch(readWriteMode))
    {
        theFile->open(dataFileName.c_str(),ios::out);
    }
    else
    {
        throw DataFileException("Invalid Read/Write mode: " + readWriteMode);
        MessageInterface::ShowMessage("Invalid Read/Write mode: " + readWriteMode);
    }

    if(theFile->is_open())
    {
        isOpen = true;
        return true;
    }
    else
    {
        isOpen = false;
	throw DataFileException("Unable to open data file: " + dataFileName);
	MessageInterface::ShowMessage("Unable to open data file: " + dataFileName);
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
//------------------------------------------------------------------------------
bool DataFile::CloseFile()
{
    theFile->close();

    if(theFile->is_open())
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
/**
 * Returns the string array of allowable file formats.
 *
 * @return String array of all file format descriptions
 *
 */
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
 *
 * @param <id> Integer ID associated with the file format
 * @return The string description of the file format
 *
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
 *
 * @param <label> The string label associated with the file format
 * @return The integer file format ID
 *
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
    else if (label == "SP3C")
    {
       return SP3C_ID;
    }
    else if (label == "RINEX")
    {
       return RINEX_ID;
    }
    else if (label == "UTDF")
    {
       return UTDF_ID;
    }
    else

    return retval;

}

//------------------------------------------------------------------------------
// void SetFileFormatID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Sets the name of the process data file object.
 *
 * @param <label> The string label associated with the file format
 * 
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
    else if (label == "SP3C")
    {
       fileFormatID = SP3C_ID;
    }
    else if (label == "RINEX")
    {
       fileFormatID = RINEX_ID;
    }
    else if (label == "UTDF")
    {
       fileFormatID = UTDF_ID;
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
// Integer SetNumMeasurements(const Integer &nm)
//------------------------------------------------------------------------------
/**
 * Sets the number of measurements of the data file object.
 *
 * @param <pdfId> The desired number of measurements to be set.
 */
//------------------------------------------------------------------------------
void DataFile::SetNumMeasurements(const Integer &nm)
{
   numMeasurements = nm;
}

//------------------------------------------------------------------------------
// Integer GetNumMeasurements() const
//------------------------------------------------------------------------------
/**
 * Finds the number of measurements of the data file object.
 *
 * @return The number of measurements
 */
//------------------------------------------------------------------------------
Integer DataFile::GetNumMeasurements() const
{
   return numMeasurements;
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
//  bool AdvanceToNextOb()
//------------------------------------------------------------------------------
/**
 * Increments the vector container index
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
 bool DataFile::AdvanceToNextOb()
{
   return false;
}

//------------------------------------------------------------------------------
//  bool BackUpToPreviousOb()
//------------------------------------------------------------------------------
/**
 * Decrements the vector container index
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
 bool DataFile::BackUpToPreviousOb()
{
   return false;
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
 bool DataFile::CheckDataAvailability(const std::string str) const
{
   return false;
}

//------------------------------------------------------------------------------
//  bool DataFile::IsEOF()
//------------------------------------------------------------------------------
/**
 * Check to see if the end of the file has been reached.
 *
 * @return True if end of file, False otherwise
 */
//------------------------------------------------------------------------------
bool DataFile::IsEOF()
{
    return theFile->eof();
}

//------------------------------------------------------------------------------
// std::string DataFile::ReadLineFromFile()
//------------------------------------------------------------------------------
/**
 * Read a single line from a file.
 *
 * @return Line from file
 */
//------------------------------------------------------------------------------
std::string DataFile::ReadLineFromFile()
{
    std::string lff;
    getline((*theFile),lff);
    return lff;
}

//------------------------------------------------------------------------------
// std::string DataFile::GetFileName()
//------------------------------------------------------------------------------
/**
 * Retrieve the dataFileName parameter.
 *
  * @return dataFileName
 */
//------------------------------------------------------------------------------
std::string DataFile::GetFileName()
{
    return dataFileName;
}

//------------------------------------------------------------------------------
// void DataFile::SetFileName(std::string &myFileName)
//------------------------------------------------------------------------------
/**
 * Set the dataFileName parameter using strings.
 *
 * @param <myFileName> Desired file name
 */
//------------------------------------------------------------------------------
void DataFile::SetFileName(std::string &myFileName)
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
//------------------------------------------------------------------------------
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
// bool GetIsSorted() const
//------------------------------------------------------------------------------
bool DataFile::GetIsSorted() const
{
        return isSorted;
}

//------------------------------------------------------------------------------
// void SetIsSorted(const bool flag)
//------------------------------------------------------------------------------
void DataFile::SetIsSorted(const bool &flag)
{
        isSorted = flag;
}

//------------------------------------------------------------------------------
// std::string DataFile::Trim(std::string str) const
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
// std::string Overpunch(const Real &number )
//------------------------------------------------------------------------------
/**
 * Converts numeric value to overpunch code version for output in text file.
 */
//------------------------------------------------------------------------------
std::string DataFile::Overpunch(const Real &number )
{
    ostringstream numstr;
    int digit;

    if (number < 0)
    {

        // convert number to stringstream
        // making sure to strip the negative sign
        numstr << -number << flush;

        // extract last digit to determine which overpunch value to use
        if (!from_string<int>(digit,numstr.str().substr(numstr.str().length()-1,1),std::dec))
        {
            return "";
        };

        // Remove last digit to be replaced with overpunched character
        numstr.str().erase(numstr.str().length() - 1, 1);

        if (digit == 0)
        {
            numstr << "}";
        }
        else if (digit == 1)
        {
            numstr << "J";
        }
        else if (digit == 2)
        {
            numstr << "K";
        }
        else if (digit == 3)
        {
            numstr << "L";
        }
        else if (digit == 4)
        {
            numstr << "M";
        }
        else if (digit == 5)
        {
            numstr << "N";
        }
        else if (digit == 6)
        {
            numstr << "O";
        }
        else if (digit == 7)
        {
            numstr << "P";
        }
        else if (digit == 8)
        {
            numstr << "Q";
        }
        else if (digit == 9)
        {
            numstr << "R";
        }
        else
        {
            return "";
        }
    }
    else
    {

        // conver number to stringstream
        numstr << number << flush;

        // extract last digit to determine which overpunch value to use
        if (!from_string<int>(digit,numstr.str().substr(numstr.str().length()-1,1),std::dec))
        {
            return "";
        };

        // Remove last digit to be replaced with overpunched character
        numstr.str().erase(numstr.str().length() - 1, 1);

        if (digit == 0)
        {
            numstr << "{";
        }
        else if (digit == 1)
        {
            numstr << "A";
        }
        else if (digit == 2)
        {
            numstr << "B";
        }
        else if (digit == 3)
        {
            numstr << "C";
        }
        else if (digit == 4)
        {
            numstr << "D";
        }
        else if (digit == 5)
        {
            numstr << "E";
        }
        else if (digit == 6)
        {
            numstr << "F";
        }
        else if (digit == 7)
        {
            numstr << "G";
        }
        else if (digit == 8)
        {
            numstr << "H";
        }
        else if (digit == 9)
        {
            numstr << "I";
        }
        else
        {
            return "";
        }
    }

    return numstr.str();
}

//------------------------------------------------------------------------------
// bool ReverseOverpunch(const char &code, Integer &digit, Integer &sign )
//------------------------------------------------------------------------------
/**
 * Converts overpunch code to numeric value and determines appropriate sign
 *
 * @param <code> Input overpunch code
 * @param <digit> Output Integer corresponding to overpunch code
 * @param <sign> Output sign (+/-1) corresponding to overpunch code
 *
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool DataFile::ReverseOverpunch(std::string code, Integer &digit, Integer &sign )
{
    if (!pcrecpp::RE("}").FullMatch(code))
    {
	digit = 0;
        sign = -1;
    }
    else if (!pcrecpp::RE("J").FullMatch(code))
    {
	digit = 1;
        sign = -1;
    }
    else if (!pcrecpp::RE("K").FullMatch(code))
    {
	digit = 2;
        sign = -1;
    }
    else if (!pcrecpp::RE("L").FullMatch(code))
    {
	digit = 3;
        sign = -1;
    }
    else if (!pcrecpp::RE("M").FullMatch(code))
    {
	digit = 4;
        sign = -1;
    }
    else if (!pcrecpp::RE("N").FullMatch(code))
    {
	digit = 5;
        sign = -1;
    }
    else if (!pcrecpp::RE("O").FullMatch(code))
    {
	digit = 6;
        sign = -1;
    }
    else if (!pcrecpp::RE("P").FullMatch(code))
    {
	digit = 7;
        sign = -1;
    }
    else if (!pcrecpp::RE("Q").FullMatch(code))
    {
	digit = 8;
        sign = -1;
    }
    else if (!pcrecpp::RE("R").FullMatch(code))
    {
	digit = 9;
        sign = -1;
    }
    else if (!pcrecpp::RE("{").FullMatch(code))
    {
	digit = 0;
        sign = +1;
    }
    else if (!pcrecpp::RE("A").FullMatch(code))
    {
	digit = 1;
        sign = +1;
    }
    else if (!pcrecpp::RE("B").FullMatch(code))
    {
	digit = 2;
        sign = +1;
    }
    else if (!pcrecpp::RE("C").FullMatch(code))
    {
	digit = 3;
        sign = +1;
    }
    else if (!pcrecpp::RE("D").FullMatch(code))
    {
	digit = 4;
        sign = +1;
    }
    else if (!pcrecpp::RE("E").FullMatch(code))
    {
	digit = 5;
        sign = +1;
    }
    else if (!pcrecpp::RE("F").FullMatch(code))
    {
	digit = 6;
        sign = +1;
    }
    else if (!pcrecpp::RE("G").FullMatch(code))
    {
	digit = 7;
        sign = +1;
    }
    else if (!pcrecpp::RE("H").FullMatch(code))
    {
	digit = 8;
        sign = +1;
    }
    else if (!pcrecpp::RE("I").FullMatch(code))
    {
	digit = 9;
        sign = +1;
    }
    else
    {
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
std::string DataFile::GetDataParameterText(const Integer id) const
{
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
std::string DataFile::GetDataUnits(const Integer id) const
{
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
Integer DataFile::GetDataParameterID(const std::string &str) const
{
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
Gmat::ParameterType DataFile::GetDataParameterType(const Integer id) const
{
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
std::string DataFile::GetDataParameterTypeString(const Integer id) const
{
   return "";
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves Integer data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The Integer data parameter
 *
 */
//------------------------------------------------------------------------------
Integer DataFile::GetIntegerDataParameter(const Integer id) const
{
    return -123456789;
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves Integer data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The Integer data parameter
 *
 */
//------------------------------------------------------------------------------
Integer DataFile::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves Real data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The Real data parameter
 *
 */
//------------------------------------------------------------------------------
Real DataFile::GetRealDataParameter(const Integer id) const
{
    return -1234567.89;
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves Real data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The Real data parameter
 *
 */
//------------------------------------------------------------------------------
Real DataFile::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool DataFile::GetBoolDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool DataFile::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves string data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The string data parameter
 *
 */
//------------------------------------------------------------------------------
std::string DataFile::GetStringDataParameter(const Integer id) const
{
    return "";
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves string data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The string data parameter
 *
 */
//------------------------------------------------------------------------------
std::string DataFile::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual StringArray GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves string array data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The string array data parameter
 *
 */
//------------------------------------------------------------------------------
StringArray DataFile::GetStringArrayDataParameter(const Integer id) const
{
    StringArray str;
    return str;
}

//------------------------------------------------------------------------------
// virtual StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves string array data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The string array data parameter
 *
 */
//------------------------------------------------------------------------------
StringArray DataFile::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual bool GetData()
//------------------------------------------------------------------------------
/**
 * Obtains data from file
 */
//------------------------------------------------------------------------------
bool DataFile::GetData()
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool WriteMeasurement()
//------------------------------------------------------------------------------
/**
 * Writes data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteMeasurement()
{
    return false;
}