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
 * Implements the DataFile base class that allows a local file to be processed
 * according to a specified ASCII and/or binary data file format.
 *
 */
//------------------------------------------------------------------------------

#include <DataFile.hpp>

//---------------------------------
//  static data
//---------------------------------


const std::string DataFile::OPTIONAL_SIGN = "[-+]?";
const std::string DataFile::MANDATORY_DIGITS = "\\d+";
const std::string DataFile::DECIMAL_POINT = "\\.";
const std::string DataFile::OPTIONAL_DIGITS = "\\d*";
const std::string DataFile::OPTIONAL_EXPONENT = "[Ee]?" + OPTIONAL_SIGN +
                                                OPTIONAL_DIGITS;

const std::string DataFile::REGEX_NUMBER = "(?:" + OPTIONAL_SIGN +
                    MANDATORY_DIGITS + DECIMAL_POINT + OPTIONAL_DIGITS + ")";
const std::string DataFile::REGEX_LETTER = "[a-zA-Z]";
const std::string DataFile::REGEX_DATE = "\\d{2}.\\d{2}.\\d{4}";
const std::string DataFile::REGEX_SCINUMBER = "(?:" + OPTIONAL_SIGN +
                    MANDATORY_DIGITS + DECIMAL_POINT + OPTIONAL_DIGITS +
                    OPTIONAL_EXPONENT + ")";
const std::string DataFile::REGEX_CCSDS_DATE = "[\\d{4}-\\d{2,3}[-\\d\\d]?T\\d{2}:\\d{2}:\\d{2}[.\\d+]?[Z]?";
const std::string DataFile::REGEX_CCSDS_SAVETHEDATE1 = "[(\\d{4})-(\\d{2})-(\\d\\d)T(\\d{2}):(\\d{2}):(\\d{2}[.\\d+]?)([Z]?)";
const std::string DataFile::REGEX_CCSDS_SAVETHEDATE2 = "[(\\d{4})-(\\d{3})T(\\d{2}):(\\d{2}):(\\d{2}[.\\d+]?)([Z]?)";
const std::string DataFile::REGEX_CCSDS_KEYWORD = "(\\w*[_]?\\w+)";

const std::string DataFile::FILEFORMAT_DESCRIPTIONS[EndFileFormatReps] =
{
    "B3 Data",
    "SLR Data",
    "TLE Data",
    "SP3c Data",
    "RINEX Data",
    "UTDF Data",
    "TRK-2-34 Data",
    "CCSDS Tracking Data Message"
    "CCSDS Orbit Parameter Message"
    "CCSDS Orbit Ephemeris Message"
    "CCSDS Attitude Parameter Message"
    "CCSDS Attitude Ephemeris Message"
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

    // Make sure that the data vector has space reserved for a minimum number 
    // of observations. This ensures that the compiler does not unnecessarily  
    // reallocate the vector storage too-often. In this case, the function 
    // reserve() will ensure that we have room for at least 100 elements. 
    // If the vector already has room for the required number of elements,
    // reserve() does nothing. In other words, reserve() will grow the allocated
    // storage of the vector, if necessary, but will never shrink it.
    theData.reserve(100);    
    
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
//    lineFromFile (""),
//    dataFileName (""),
    isOpen (false),
    sortedBy (0),
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
    isOpen (pdf.isOpen),
    sortedBy (pdf.sortedBy),
    readWriteMode(pdf.readWriteMode),
    theFile (pdf.theFile)
{
}


//------------------------------------------------------------------------------
//  DataFile::DataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for DataFile objects
 */
//------------------------------------------------------------------------------
const DataFile& DataFile::operator=(const DataFile &pdf)
{
    if (&pdf == this)
	return *this;

    fileFormatName = pdf.fileFormatName;
    dataFileName = pdf.dataFileName;
    numLines = pdf.numLines;
    isOpen = pdf.isOpen;
    sortedBy = pdf.sortedBy;
    readWriteMode = pdf.readWriteMode;
    theFile = pdf.theFile;
    
    return *this;
	    
}

//------------------------------------------------------------------------------
//  DataFile::~DataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
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
    Integer retval = GmatBase::INTEGER_PARAMETER_UNDEFINED;
    
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
    else if (label == "TRK234")
    {
       return TRK234_ID;
    }
    else if (label == "CCSDSTDM")
    {
       return CCSDS_TDM_ID;
    }
    else if (label == "CCSDSOPM")
    {
       return CCSDS_OPM_ID;
    }
    else if (label == "CCSDSOEM")
    {
       return CCSDS_OEM_ID;
    }
    else if (label == "CCSDSAPM")
    {
       return CCSDS_APM_ID;
    }
    else if (label == "CCSDSAEM")
    {
       return CCSDS_AEM_ID;
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
    else if (label == "TRK234")
    {
       fileFormatID = TRK234_ID;
    }
    else if (label == "CCSDSTDM")
    {
       fileFormatID = CCSDS_TDM_ID;
    }
    else if (label == "CCSDSOPM")
    {
       fileFormatID = CCSDS_OPM_ID;
    }
    else if (label == "CCSDSOEM")
    {
       fileFormatID = CCSDS_OEM_ID;
    }
    else if (label == "CCSDSAPM")
    {
       fileFormatID = CCSDS_APM_ID;
    }
    else if (label == "CCSDSAEM")
    {
       fileFormatID = CCSDS_AEM_ID;
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
   return theData.size();
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
// void SetFstream(fstream *myFstream)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the fstream
 *
 * @param <myFstream> The desired pointer to the fstream.
 */
//------------------------------------------------------------------------------
void DataFile::SetFstream(fstream *myFstream)
{
   theFile = myFstream;
}

//------------------------------------------------------------------------------
// fstream* GetFstream() const
//------------------------------------------------------------------------------
/**
 * Retrieves the pointer to the fstream
 *
 * @return The fstream pointer.
 */
//------------------------------------------------------------------------------
fstream* DataFile::GetFstream() const
{
   return theFile;
}

//------------------------------------------------------------------------------
// void SetSortedBy(const Integer sortID)
//------------------------------------------------------------------------------
/**
 * Sets the sorted by enumeration ID
 *
 * @param <sortID> Desired integer ID corresponding to sorted by enumeration.
 */
//------------------------------------------------------------------------------
void DataFile::SetSortedBy(const Integer sortID)
{
   sortedBy = sortID;
}

//------------------------------------------------------------------------------
// Integer GetSortedBy() const
//------------------------------------------------------------------------------
/**
 * Retrieves the sorted by enumeration ID
 *
 * @return Integer ID of sorted by enumeration.
 */
//------------------------------------------------------------------------------
Integer DataFile::GetSortedBy() const
{
   return sortedBy;
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
   i_theData++;
   
   if (i_theData == theData.end()) 
       return false;
   else
       return true;
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
   i_theData++;
   
   if (i_theData == theData.begin()) 
       return false;
   else
       return true;
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
// bool IsSortedByEpoch() const
//------------------------------------------------------------------------------
bool DataFile::IsSortedByEpoch() const
{
    if (sortedBy == SORTED_BY_EPOCH)
        return true;
    else
        return false;
}

//------------------------------------------------------------------------------
// bool IsSortedBySatID() const
//------------------------------------------------------------------------------
bool DataFile::IsSortedBySatID() const
{
    if (sortedBy == SORTED_BY_SATID)
        return true;
    else
        return false;

}

//------------------------------------------------------------------------------
// bool IsSortedBySensorID() const
//------------------------------------------------------------------------------
bool DataFile::IsSortedBySensorID() const
{
    if (sortedBy == SORTED_BY_SENSORID)
        return true;
    else
        return false;

}

//------------------------------------------------------------------------------
// bool IsSortedByInternationalDesignator() const
//------------------------------------------------------------------------------
bool DataFile::IsSortedByInternationalDesignator() const
{
    if (sortedBy == SORTED_BY_INTERNATIONALDESIGNATOR)
        return true;
    else
        return false;

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
// virtual bool WriteData(Obtype *myObtype)
//------------------------------------------------------------------------------
/**
 * Writes data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteData(Obtype *myObtype)
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool WriteDataHeader()
//------------------------------------------------------------------------------
/**
 * Writes header data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteDataHeader()
{
    return false;
}
//------------------------------------------------------------------------------
// virtual bool WriteDataSubHeader()
//------------------------------------------------------------------------------
/**
 * Writes header data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteDataSubHeader()
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool WriteMetadata()
//------------------------------------------------------------------------------
/**
 * Writes header data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteMetadata()
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool WriteData(fstream *myFile, Obtype *myObtype)
//------------------------------------------------------------------------------
/**
 * Writes data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteData(fstream *myFile, Obtype *myObtype)
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool WriteDataHeader(fstream *myFile)
//------------------------------------------------------------------------------
/**
 * Writes header data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteDataHeader(fstream *myFile)
{
    return false;
}
//------------------------------------------------------------------------------
// virtual bool WriteDataSubHeader(fstream *myFile)
//------------------------------------------------------------------------------
/**
 * Writes header data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteDataSubHeader(fstream *myFile)
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool WriteMetadata(fstream *myFile)
//------------------------------------------------------------------------------
/**
 * Writes header data to file
 */
//------------------------------------------------------------------------------
bool DataFile::WriteMetadata(fstream *myFile)
{
    return false;
}

//------------------------------------------------------------------------------
// void SortByEpoch(bool sortOrder)
//------------------------------------------------------------------------------
/**
 * Sorts the vector of data by epoch
 *
 * @param <sortOrder> Desired sort order (DESCENDING=true, ASCENDING=false)
 */
//------------------------------------------------------------------------------
void DataFile::SortByEpoch(bool sortOrder)
{
    if (sortOrder)
	std::sort(theData.begin(), theData.end(), DescendingEpochSort());
    else
	std::sort(theData.begin(), theData.end(), AscendingEpochSort());
    
    sortedBy = SORTED_BY_EPOCH;
}

//------------------------------------------------------------------------------
// void SortBySatID(bool sortOrder)
//------------------------------------------------------------------------------
/**
 * Sorts the vector of data by satellite ID
 *
 * @param <sortOrder> Desired sort order (DESCENDING=true, ASCENDING=false)
 */
//------------------------------------------------------------------------------
void DataFile::SortBySatID(bool sortOrder)
{
    if (sortOrder)
	std::sort(theData.begin(), theData.end(), DescendingSatIDSort());
    else
	std::sort(theData.begin(), theData.end(), AscendingSatIDSort());

    sortedBy = SORTED_BY_SATID;
}

//------------------------------------------------------------------------------
// void SortBySensorID(bool sortOrder)
//------------------------------------------------------------------------------
/**
 * Sorts the vector of data by sensor ID
 *
 * @param <sortOrder> Desired sort order (DESCENDING=true, ASCENDING=false)
 */
//------------------------------------------------------------------------------
void DataFile::SortBySensorID(bool sortOrder)
{
    if (sortOrder)
	std::sort(theData.begin(), theData.end(), DescendingSensorIDSort());
    else
	std::sort(theData.begin(), theData.end(), AscendingSensorIDSort());

    sortedBy = SORTED_BY_SENSORID;
}

//------------------------------------------------------------------------------
// void SortByInternationalDesignator(bool sortOrder)
//------------------------------------------------------------------------------
/**
 * Sorts the vector of data by international designator
 *
 * @param <sortOrder> Desired sort order (DESCENDING=true, ASCENDING=false)
 */
//------------------------------------------------------------------------------
void DataFile::SortByInternationalDesignator(bool sortOrder)
{
    if (sortOrder)
	std::sort(theData.begin(), theData.end(), 
		  DescendingInternationalDesignatorSort());
    else
	std::sort(theData.begin(), theData.end(), 
		  AscendingInternationalDesignatorSort());

    sortedBy = SORTED_BY_INTERNATIONALDESIGNATOR;
}