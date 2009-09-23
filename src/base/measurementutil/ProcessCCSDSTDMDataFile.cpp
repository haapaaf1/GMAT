//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSTDMDataFile
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

#include <ProcessCCSDSTDMDataFile.hpp>

#define DEBUG_CCSDSTDM_DATA

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
bool ProcessCCSDSTDMDataFile::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSTDMObtype *myTDM = new CCSDSTDMObtype;

        // Read the first line from file
	std::string line = Trim(ReadLineFromFile());

        while (!IsEOF())
        {
            if (line != "")
            {
                // Now check for headers and process data accordingly
                if (GetData(line,myTDM))
                {
                    // Push this data point onto the stack.
                    theData.push_back(myTDM);
                }
                else
                {
                    delete myTDM;
                }

                // Allocate another struct in memory
                myTDM = new CCSDSTDMObtype;
            }

	    // Read a line from file
            // After grabbing the header and metadata information
            // This call to read a line from file should be grabbing
            // rows of data between DATA_START and DATA_STOP
	    line = Trim(ReadLineFromFile());
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSTDM_DATA

            fstream *outFile = new fstream;
            outFile->open("tdm.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObtypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile << (CCSDSTDMObtype*)(*j) << std::endl;
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
//  ProcessCCSDSTDMDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSTDMDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSTDMDataFile", itsName),
	currentCCSDSMetadata(NULL)
{
   objectTypeNames.push_back("CCSDSTDMDataFile");
   fileFormatName = "CCSDSTDM";
   fileFormatID = 7;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSTDMDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile(const ProcessCCSDSTDMDataFile &CCSDSTDMdf) :
    ProcessCCSDSDataFile(CCSDSTDMdf),
    currentCCSDSMetadata(CCSDSTDMdf.currentCCSDSMetadata)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSTDMDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSTDMDataFile& ProcessCCSDSTDMDataFile::operator=(const ProcessCCSDSTDMDataFile &CCSDSTDMdf)
{
    if (&CCSDSTDMdf == this)
	return *this;

    ProcessCCSDSDataFile::operator=(CCSDSTDMdf);
    currentCCSDSMetadata = CCSDSTDMdf.currentCCSDSMetadata;
    return *this;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSTDMDataFile::~ProcessCCSDSTDMDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSTDMDataFile::~ProcessCCSDSTDMDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSTDMDataFile.
 *
 * @return clone of the ProcessCCSDSTDMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSTDMDataFile::Clone() const
{
   GmatBase *clone = new ProcessCCSDSTDMDataFile(*this);
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
bool ProcessCCSDSTDMDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSTDMDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetData(std::string line, CCSDSTDMObtype *myTDM)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of TDM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSTDMDataFile::GetData(std::string line, CCSDSTDMObtype *myTDM)
{

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_TDM_VERS.*").FullMatch(line))
    {
        // Initialize the header data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        CCSDSTDMObtype::ccsds_header *myCCSDSHeader =
                                            new CCSDSTDMObtype::ccsds_header;
	
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
        CCSDSTDMObtype::ccsds_tdm_metadata *myMetaData =
                                        new CCSDSTDMObtype::ccsds_tdm_metadata;
        
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

    if (!pcrecpp::RE("^DATA_STOP.*").FullMatch(line) && !pcrecpp::RE("").FullMatch(line))
    {
        CCSDSTDMObtype::ccsds_data *myTDMData = new CCSDSTDMObtype::ccsds_data;
	myTDM->ccsdsHeader = currentCCSDSHeader;
        myTDM->ccsdsTDMMetaData = currentCCSDSMetadata;
	return GetCCSDSData(line,myTDMData,myTDM);
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSMetadata(std::string &lff,
//                       CCSDSTDMObtype::ccsds_tdm_metadata *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSTDMDataFile::GetCCSDSMetadata(std::string &lff,
                                CCSDSTDMObtype::ccsds_tdm_metadata *myMetaData)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    double dtemp;
    std::string stemp;

    // Read lines until we have encountered the first meta data start

    while (!pcrecpp::RE("^DATA_START.*").FullMatch(lff))
    {
        if (pcrecpp::RE("^COMMENT\\s*(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->metadataComments.push_back(stemp);
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
        else if (pcrecpp::RE("^PARTICIPANT_1\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->participants[0] = stemp;
        }
        else if (pcrecpp::RE("^PARTICIPANT_2\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->participants[1] = stemp;
        }
        else if (pcrecpp::RE("^PARTICIPANT_3\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->participants[2] = stemp;
        }
        else if (pcrecpp::RE("^PARTICIPANT_4\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->participants[3] = stemp;
        }
        else if (pcrecpp::RE("^PARTICIPANT_5\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->participants[4] = stemp;
        }
        else if (pcrecpp::RE("^MODE\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->mode = stemp;
        }
        else if (pcrecpp::RE("^PATH\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->path[0] = stemp;
        }
        else if (pcrecpp::RE("^PATH_1\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->path[1] = stemp;
        }
        else if (pcrecpp::RE("^PATH_2\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->path[2] = stemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_BAND\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->transmitBand = stemp;
        }
        else if (pcrecpp::RE("^RECEIVE_BAND\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->receiveBand = stemp;
        }
        else if (pcrecpp::RE("^TURNAROUND_NUMERATOR\\s*=(.*)").FullMatch(lff,&itemp))
        {
            myMetaData->turnaroundNumerator = itemp;
        }
        else if (pcrecpp::RE("^TURNAROUND_DENOMINATOR\\s*=(.*)").FullMatch(lff,&itemp))
        {
            myMetaData->turnaroundDenominator = itemp;
        }
        else if (pcrecpp::RE("^TIMETAG_REF\\s*=(.*)").FullMatch(lff,&itemp))
        {
            myMetaData->timeTagRef = itemp;
        }
        else if (pcrecpp::RE("^INTEGRATION_INTERVAL\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->integrationInterval = dtemp;
        }
        else if (pcrecpp::RE("^INTEGRATION_REF\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->integrationRef = stemp;
        }
        else if (pcrecpp::RE("^FREQ_OFFSET\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->frequencyOffset = dtemp;
        }
        else if (pcrecpp::RE("^RANGE_MODE\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->rangeMode = stemp;
        }
        else if (pcrecpp::RE("^RANGE_MODULUS\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->rangeModulus = dtemp;
        }
        else if (pcrecpp::RE("^RANGE_UNITS\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->rangeUnits = stemp;
        }
        else if (pcrecpp::RE("^ANGLE_TYPE\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->angleType = stemp;
        }
        else if (pcrecpp::RE("^REFERENCE_FRAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->referenceFrame = stemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_1\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->transmitDelay[0] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_2\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->transmitDelay[1] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_3\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->transmitDelay[2] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_4\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->transmitDelay[3] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_5\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->transmitDelay[4] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_1\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->receiveDelay[0] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_2\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->receiveDelay[1] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_3\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->receiveDelay[2] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_4\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->receiveDelay[3] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_5\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->receiveDelay[4] = dtemp;
        }
        else if (pcrecpp::RE("^DATA_QUALITY\\s*=(.*)").FullMatch(lff,&stemp))
        {
            myMetaData->dataQuality = stemp;
        }
        else if (pcrecpp::RE("^CORRECTION_ANGLE_1\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->correctionAngle1 = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_ANGLE_2\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->correctionAngle2 = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_DOPPLER\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->correctionDoppler = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_RECEIVE\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->correctionReceive = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_TRANSMIT\\s*=(.*)").FullMatch(lff,&dtemp))
        {
            myMetaData->correctionTransmit = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTIONS_APPLIED\\s*=(.*)").FullMatch(lff,&stemp))
        {
            if (pcrecpp::RE("^YES$").FullMatch(Trim(stemp)))
                myMetaData->correctionsApplied = true;
            else
                myMetaData->correctionsApplied = false;
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