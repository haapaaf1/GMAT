//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOEMDataFile
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

#include <ProcessCCSDSOEMDataFile.hpp>

//#define DEBUG_CCSDSOEM_DATA

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
bool ProcessCCSDSOEMDataFile::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSOEMObtype *myOEM = new CCSDSOEMObtype;

        // Read the first line from file
	std::string line = Trim(ReadLineFromFile());

        while (!IsEOF())
        {
            if (line != "")
            {
                // Now check for headers and process data accordingly
                if (GetData(line,myOEM))
                {
                    // Push this data point onto the stack.
                    theData.push_back(myOEM);
                }
                else
                {
                    delete myOEM;
                }

                // Allocate another struct in memory
                myOEM = new CCSDSOEMObtype;
            }

	    // Read a line from file
            // After grabbing the header and metadata information
            // This call to read a line from file should be grabbing
            // rows of data between DATA_START and DATA_STOP
	    line = Trim(ReadLineFromFile());
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSOEM_DATA

            fstream *outFile = new fstream;
            outFile->open("oem.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObtypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile << (CCSDSOEMObtype*)(*j) << std::endl;
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
//  ProcessCCSDSOEMDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSOEMDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSOEMDataFile::ProcessCCSDSOEMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSOEMDataFile", itsName),
	currentCCSDSMetadata(NULL)
{
   objectTypeNames.push_back("CCSDSOEMDataFile");
   fileFormatName = "CCSDSOEM";
   fileFormatID = 9;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSOEMDataFile::ProcessCCSDSOEMDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSOEMDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSOEMDataFile::ProcessCCSDSOEMDataFile(const ProcessCCSDSOEMDataFile &CCSDSOEMdf) :
    ProcessCCSDSDataFile(CCSDSOEMdf),
    currentCCSDSMetadata(CCSDSOEMdf.currentCCSDSMetadata)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSOEMDataFile::ProcessCCSDSOEMDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSOEMDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSOEMDataFile& ProcessCCSDSOEMDataFile::operator=(const ProcessCCSDSOEMDataFile &CCSDSOEMdf)
{
    if (&CCSDSOEMdf == this)
	return *this;

    ProcessCCSDSDataFile::operator=(CCSDSOEMdf);
    currentCCSDSMetadata = CCSDSOEMdf.currentCCSDSMetadata;
    return *this;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSOEMDataFile::~ProcessCCSDSOEMDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSOEMDataFile::~ProcessCCSDSOEMDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOEMDataFile.
 *
 * @return clone of the ProcessCCSDSOEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSOEMDataFile::Clone() const
{
   GmatBase *clone = new ProcessCCSDSOEMDataFile(*this);
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
bool ProcessCCSDSOEMDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSOEMDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetData(std::string line, CCSDSOEMObtype *myOEM)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of OEM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetData(std::string line, CCSDSOEMObtype *myOEM)
{

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_OEM_VERS.*").FullMatch(line))
    {
        // Initialize the header data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        CCSDSOEMObtype::ccsds_header *myCCSDSHeader =
                                            new CCSDSOEMObtype::ccsds_header;

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
        CCSDSOEMObtype::ccsds_oem_metadata *myMetaData =
                                        new CCSDSOEMObtype::ccsds_oem_metadata;

	// Read the next metadata line from file
	line = Trim(ReadLineFromFile());

	if (GetCCSDSMetadata(line,myMetaData))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetadata = myMetaData;

	    // Read the following data line from file
	    line = Trim(ReadLineFromFile());
	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSMetadata = NULL;
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
    if (currentCCSDSHeader == NULL || currentCCSDSMetadata == NULL)
	return false;

    if (!pcrecpp::RE("^DATA_STOP.*").FullMatch(line))
    {
        CCSDSOEMObtype::ccsds_data *myOEMData = new CCSDSOEMObtype::ccsds_data;
	myOEM->ccsdsHeader = currentCCSDSHeader;
        myOEM->ccsdsOEMMetaData = currentCCSDSMetadata;
	return GetCCSDSData(line,myOEMData,myOEM);
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSMetadata(std::string &lff,
//                       CCSDSOEMObtype::ccsds_oem_metadata *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetCCSDSMetadata(std::string &lff,
                                CCSDSOEMObtype::ccsds_oem_metadata *myMetaData)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    std::string stemp;

    // Read lines until we have encountered the first meta data start

    while (!pcrecpp::RE("^DATA_START.*").FullMatch(lff))
    {
        if (pcrecpp::RE("^COMMENT\\s*(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->metadataComments.push_back(stemp);
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
        else if (pcrecpp::RE("^START_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->startTime = stemp;
        }
        else if (pcrecpp::RE("^STOP_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->stopTime = stemp;
        }
        else if (pcrecpp::RE("^USEABLE_START_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->useableStartTime = stemp;
        }
        else if (pcrecpp::RE("^USEABLE_STOP_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->useableStopTime = stemp;
        }
        else if (pcrecpp::RE("^INTERPOLATION\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->interpolationMethod = stemp;
        }
        else if (pcrecpp::RE("^INTERPOLATION_DEGREE\\s*=(.*)").FullMatch(lff,&itemp))
        {
	    myMetaData->interpolationDegree = itemp;
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