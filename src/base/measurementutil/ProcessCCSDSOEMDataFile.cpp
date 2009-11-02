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

#define DEBUG_CCSDSOEM_DATA

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
    if (!ProcessCCSDSDataFile::Initialize()) return false;

    requiredNumberMetaDataParameters = CountRequiredNumberOEMMetaDataParameters();

    // Test to see if we are reading or writing
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Construct the first of many orbit ephemeris message obtypes
        CCSDSOEMObType *myOEM = new CCSDSOEMObType;

        if (!IsEOF())
        {
            // The GetData function will attempt to populate the
            // OEM obtype variables
            if (!GetData(myOEM))
            {
                delete myOEM;
                return false;
            }
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSOEM_DATA

            fstream *outFile = new fstream;
            outFile->open("oem.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
		*outFile << (CCSDSOEMObType*)(*j) << std::endl;

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
	currentCCSDSMetaData(NULL),
	lastMetaDataWritten(NULL),
        isMetaDataWritten(false),
        requiredNumberMetaDataParameters(0)
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
    currentCCSDSMetaData(CCSDSOEMdf.currentCCSDSMetaData),
    lastMetaDataWritten(CCSDSOEMdf.lastMetaDataWritten),
    isMetaDataWritten(CCSDSOEMdf.isMetaDataWritten),
    requiredNumberMetaDataParameters(CCSDSOEMdf.requiredNumberMetaDataParameters)
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
    currentCCSDSMetaData = CCSDSOEMdf.currentCCSDSMetaData;
    lastMetaDataWritten = CCSDSOEMdf.lastMetaDataWritten;
    isMetaDataWritten = CCSDSOEMdf.isMetaDataWritten;
    requiredNumberMetaDataParameters = CCSDSOEMdf.requiredNumberMetaDataParameters;
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
// bool GetCCSDSData(ObType *myOEM)
//------------------------------------------------------------------------------
/**
 * Obtains the header lff of OEM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetData(ObType *myOEMData)
{

    if (!pcrecpp::RE("^CCSDSOEMObType").FullMatch(myOEMData->GetTypeName())) return false;

    // Re-cast the generic ObType pointer as a CCSDSOEMObtype pointer
    CCSDSOEMObType *myOEM = (CCSDSOEMObType*)myOEMData;

    // Read the first line from file (lff)
    std::string lff = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    // The first lff of any CCSDS data file should be the version
    if (!IsEOF() && currentCCSDSHeader == NULL && pcrecpp::RE("^CCSDS_OEM_VERS.*").FullMatch(lff))
    {
	if (GetCCSDSHeader(lff,myOEM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSHeader = myOEM->ccsdsHeader;
            MessageInterface::ShowMessage("Processed complete OEM Header\n");
	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSHeader = NULL;
	    return false;
	}
    }

    while (!IsEOF())
    {

        // Read in metadata
	if (GetCCSDSMetaData(lff,myOEM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetaData = myOEM->ccsdsOEMMetaData;
            MessageInterface::ShowMessage("Processed complete OEM MetaData\n");

	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSMetaData = NULL;
	    return false;
	}

        while (!pcrecpp::RE("^META_START.*").FullMatch(lff))
        {
            myOEM->ccsdsHeader = currentCCSDSHeader;
            myOEM->ccsdsOEMMetaData = currentCCSDSMetaData;

            if (GetCCSDSOEMData(lff,myOEM))
            {
                // Push this data point onto the obtype data stack
                MessageInterface::ShowMessage("Found OEM Data\n");
                theData.push_back(myOEM);
            }
            else
            {
                delete myOEM;
                return false;
            }

            // Allocate another struct in memory
            myOEM = new CCSDSOEMObType;

            // Read another line from file
            lff = ReadLineFromFile();

        }
    }

    return true;
}
//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff, CCSDSOEMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetCCSDSMetaData(std::string &lff,
                                               CCSDSOEMObType *myOb)
{

    CCSDSOEMMetaData *myMetaData = new CCSDSOEMMetaData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;
        MessageInterface::ShowMessage("Keyword = " + keyword + "\n");

        Integer keyID = myMetaData->GetKeywordID(keyword);

        if(myMetaData->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSOEMMetaData::CCSDS_OEM_METADATACOMMENTS_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                }
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_OBJECTNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->objectName))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_OBJECTID_ID:

                if (!GetCCSDSValue(lff,myMetaData->internationalDesignator))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_CENTERNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrameOrigin))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrame))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_STARTEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->startEpoch))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_STOPEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->stopEpoch))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_USEABLE_STARTEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->useableStartEpoch))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_USEABLE_STOPEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->useableStopEpoch))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_INTERPOLATION_ID:

                if (!GetCCSDSValue(lff,myMetaData->interpolationMethod))
                    return false;
                break;

            case CCSDSOEMMetaData::CCSDS_OEM_INTERPOLATIONDEGREE_ID:

                if (!GetCCSDSValue(lff,myMetaData->interpolationDegree))
                    return false;
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while(requiredCount < requiredNumberMetaDataParameters &&
          !pcrecpp::RE("^META_STOP.*$").FullMatch(lff));

    myOb->ccsdsOEMMetaData = myMetaData;

    return true;
}

//------------------------------------------------------------------------------
// bool GetCCSDSOEMData(std::string &lff, CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the orbit ephemeris message.
 *
 * @param <lff> Line from file
 * @param <myOb> Pointer to OEM data
 * @return Boolean success or failure
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetCCSDSOEMData(std::string &lff,
                                              CCSDSOEMObType *myOb)
{
    CCSDSOEMStateVector *myOEMData = new CCSDSOEMStateVector;

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp1, dtemp2, dtemp3, dtemp4, dtemp5, dtemp6;
    std::string stemp;

    std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" + REGEX_SCINUMBER +
            ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER +
            ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER +
            ")\\s*(" + REGEX_SCINUMBER + ")$";

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,&dtemp3,
                                     &dtemp4,&dtemp5,&dtemp6))
    {
        myOEMData->timeTag = stemp;
        if (!CCSDSTimeTag2A1Date(myOEMData->timeTag,myOb->epoch)) return false;
        myOEMData->x = dtemp1;
        myOEMData->y = dtemp2;
        myOEMData->z = dtemp3;
        myOEMData->xDot = dtemp4;
        myOEMData->yDot = dtemp5;
        myOEMData->zDot = dtemp6;

        myOb->ccsdsOEMStateVector = myOEMData;
        myOb->ccsdsHeader->dataType = CCSDSHeader::STATEVECTOR_ID;
        
        return true;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool WriteData(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes a CCSDS orbit ephemeris message to file
 *
 * @param <myOEM> the OEM data to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::WriteData(const ObType *myOb)
{
    if (myOb->GetTypeName() != "CCSDSOEMObType") return false;

    CCSDSOEMObType *theOEM = (CCSDSOEMObType*)myOb;
    WriteDataHeader(theOEM);
    WriteMetaData(theOEM);
    *theFile << theOEM;
    return true;
}