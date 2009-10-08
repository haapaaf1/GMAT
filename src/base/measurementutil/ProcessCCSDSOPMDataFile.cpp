//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOPMDataFile
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/08/30
//
/**
 *
 * Implements DataFile base class to read files written in CCSDS tracking
 * data message format.
 *
 */
//------------------------------------------------------------------------------

#include <ProcessCCSDSOPMDataFile.hpp>

//#define DEBUG_CCSDSOPM_DATA

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
bool ProcessCCSDSOPMDataFile::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSOPMObType *myOPM = new CCSDSOPMObType;

        // Read the first line from file
	std::string line = Trim(ReadLineFromFile());

        while (!IsEOF())
        {
            if (line != "")
            {
                // Now check for headers and process data accordingly
                if (GetData(myOPM))
                {
                    // Push this data point onto the stack.
                    theData.push_back(myOPM);
                }
                else
                {
                    delete myOPM;
                }

                // Allocate another struct in memory
                myOPM = new CCSDSOPMObType;
            }

	    // Read a line from file
            // After grabbing the header and metadata information
            // This call to read a line from file should be grabbing
            // rows of data between DATA_START and DATA_STOP
	    line = Trim(ReadLineFromFile());
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSOPM_DATA

            fstream *outFile = new fstream;
            outFile->open("OPM.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile << (CCSDSOPMObType*)(*j) << std::endl;
            }

            outFile->close();

        #endif

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
//  ProcessCCSDSOPMDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSOPMDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSOPMDataFile::ProcessCCSDSOPMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSOPMDataFile", itsName),
	currentCCSDSMetaData(NULL)
{
   objectTypeNames.push_back("CCSDSOPMDataFile");
   fileFormatName = "CCSDSOPM";
   fileFormatID = 8;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSOPMDataFile::ProcessCCSDSOPMDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSOPMDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSOPMDataFile::ProcessCCSDSOPMDataFile(const ProcessCCSDSOPMDataFile &CCSDSOPMdf) :
    ProcessCCSDSDataFile(CCSDSOPMdf),
    currentCCSDSMetaData(CCSDSOPMdf.currentCCSDSMetaData)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSOPMDataFile::ProcessCCSDSOPMDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSOPMDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSOPMDataFile& ProcessCCSDSOPMDataFile::operator=(const ProcessCCSDSOPMDataFile &CCSDSOPMdf)
{
    if (&CCSDSOPMdf == this)
	return *this;

    ProcessCCSDSDataFile::operator=(CCSDSOPMdf);
    currentCCSDSMetaData = CCSDSOPMdf.currentCCSDSMetaData;
    return *this;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSOPMDataFile::~ProcessCCSDSOPMDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSOPMDataFile::~ProcessCCSDSOPMDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOPMDataFile.
 *
 * @return clone of the ProcessCCSDSOPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSOPMDataFile::Clone() const
{
   GmatBase *clone = new ProcessCCSDSOPMDataFile(*this);
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
bool ProcessCCSDSOPMDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSOPMDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetCCSDSData(ObType *myOPM)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of OPM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetData(ObType *myOPMData)
{

    if (myOPMData->GetTypeName() != "CCSDSOPMObType") return false;

    // Re-cast the generic ObType pointer as a CCSDSOPMObtype pointer
    CCSDSOPMObType *myOPM = (CCSDSOPMObType*)myOPMData;

    // Read the first line from file
    std::string line = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_OPM_VERS.*").FullMatch(line))
    {
        // Initialize the header data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        CCSDSHeader *myCCSDSHeader = new CCSDSHeader;

	if (GetCCSDSHeader(line,myCCSDSHeader))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSHeader = myCCSDSHeader;
	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSHeader = NULL;
	    return false;
	}
    }

    // Test for the prescence of meta data
    // If the header data was just processed, line should now contain
    // the "META_START" line
    while (!IsEOF() && pcrecpp::RE("^META_START.*").FullMatch(line))
    {
        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        CCSDSOPMMetaData *myMetaData = new CCSDSOPMMetaData;

	// Read the next metadata line from file
	line = Trim(ReadLineFromFile());

	if (GetCCSDSMetaData(line,myMetaData))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetaData = myMetaData;

	    // Read the following data line from file
	    line = Trim(ReadLineFromFile());
	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSMetaData = NULL;
	    return false;
	}
    }

    // Test for the presence of the start data marker
    // If the meta data was just processed, line should contain
    // the "DATA_START" marker which we can skip over to start processing
    // the actual data.
    if (pcrecpp::RE("^DATA_START.*").FullMatch(line))
    {
        line = Trim(ReadLineFromFile());
    }

    // Parse the data record making sure that we have identified
    // a header record and a metadata record previously
    if (currentCCSDSHeader == NULL || currentCCSDSMetaData == NULL)
	return false;

    if (!pcrecpp::RE("^DATA_STOP.*").FullMatch(line) && !pcrecpp::RE("").FullMatch(line))
    {
        CCSDSData *myOPMData = new CCSDSData;
	myOPM->ccsdsHeader = currentCCSDSHeader;
        myOPM->ccsdsOPMMetaData = currentCCSDSMetaData;
	return GetCCSDSData(line,myOPMData,myOPM);
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff,
//                       CCSDSOPMMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSMetaData(std::string &lff,
                                CCSDSOPMMetaData *myMetaData)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    std::string stemp;

    // Read lines until we have encountered the first meta data start

    while (!pcrecpp::RE("^DATA_START.*").FullMatch(lff))
    {
        if (pcrecpp::RE("^COMMENT\\s*(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->comments.push_back(stemp);
        }
        else if (pcrecpp::RE("^OBJECT_NAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->objectName = stemp;
        }
        else if (pcrecpp::RE("^OBJECT_ID\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->internationalDesignator = stemp;
        }
        else if (pcrecpp::RE("^CENTER_NAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->refFrameOrigin = stemp;
        }
        else if (pcrecpp::RE("^REF_FRAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->refFrame = stemp;
        }
        else if (pcrecpp::RE("^TIME_SYSTEM\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->timeSystem = stemp;
        }
        else
        {
            // Ill formed data - these are the only keywords
            // allowed in the header
            return false;
        }

        // Read in another line
        lff = Trim(ReadLineFromFile());
    }

    return true;
}

//------------------------------------------------------------------------------
// bool WriteData(ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes a CCSDS orbit ephemeris message to file
 *
 * @param <myOPM> the OPM data to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::WriteData(ObType *myOb)
{
    if (myOb->GetTypeName() != "CCSDSOPMObType") return false;

    CCSDSOPMObType *theOPM = (CCSDSOPMObType*)myOb;
    WriteDataHeader(theOPM);
    WriteMetaData(theOPM);
    *theFile << theOPM;
    return true;
}