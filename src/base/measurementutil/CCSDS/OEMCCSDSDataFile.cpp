//$Header$
//------------------------------------------------------------------------------
//                             OEMCCSDSDataFile
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

#include <OEMCCSDSDataFile.hpp>

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
bool OEMCCSDSDataFile::Initialize()
{

    if (!CCSDSDataFile::Initialize()) return false;

    requiredNumberMetaDataParameters = CountRequiredNumberOEMMetaDataParameters();

    // Test to see if we are reading or writing
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Construct the first of many orbit ephemeris message obtypes
        OEMCCSDSObType *myOEM = new OEMCCSDSObType;

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

            OEMCCSDSDataFile myOutFile("theFile");
            myOutFile.SetReadWriteMode("w");
            myOutFile.SetFileName("OEM.output");
            myOutFile.Initialize();
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
                myOutFile.WriteData((*j));
            myOutFile.CloseFile();

        #endif

        MessageInterface::ShowMessage("Completed reading OEM data\n");
            
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
//  OEMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base OEMCCSDSDataFile structures
 */
//------------------------------------------------------------------------------
OEMCCSDSDataFile::OEMCCSDSDataFile(const std::string &itsName) :
	CCSDSDataFile ("OEMCCSDSDataFile", itsName)
{
   objectTypeNames.push_back("OEMCCSDSDataFile");
   fileFormatName = "OEM";
   fileFormatID = 9;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  OEMCCSDSDataFile::OEMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for OEMCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
OEMCCSDSDataFile::OEMCCSDSDataFile
                                   (const OEMCCSDSDataFile &CCSDSOEMdf) :
    CCSDSDataFile(CCSDSOEMdf)
{
}


//------------------------------------------------------------------------------
//  OEMCCSDSDataFile::OEMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for OEMCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
const OEMCCSDSDataFile& OEMCCSDSDataFile::operator=
                                     (const OEMCCSDSDataFile &CCSDSOEMdf)
{
    if (&CCSDSOEMdf == this)
	return *this;

    CCSDSDataFile::operator=(CCSDSOEMdf);

    return *this;
}

//------------------------------------------------------------------------------
//  OEMCCSDSDataFile::~OEMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
OEMCCSDSDataFile::~OEMCCSDSDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the OEMCCSDSDataFile.
 *
 * @return clone of the OEMCCSDSDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* OEMCCSDSDataFile::Clone() const
{
   GmatBase *clone = new OEMCCSDSDataFile(*this);
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
bool OEMCCSDSDataFile::IsParameterReadOnly(const Integer id) const
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
bool OEMCCSDSDataFile::IsParameterReadOnly(const std::string &label) const
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
bool OEMCCSDSDataFile::GetData(ObType *myOEMData)
{

    if (!pcrecpp::RE("^OEMCCSDSObType").FullMatch(myOEMData->GetTypeName()))
        return false;

    // Re-cast the generic ObType pointer as a CCSDSOEMObtype pointer
    OEMCCSDSObType *myOEM = (OEMCCSDSObType*)myOEMData;

    // Read the first line from file (lff)
    std::string lff = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    // The first lff of any CCSDS data file should be the version
    if (!IsEOF() && currentCCSDSHeader == NULL
                 && pcrecpp::RE("^CCSDS_OEM_VERS.*").FullMatch(lff))
    {
	if (GetCCSDSHeader(lff,myOEM))
	{
            // success so set currentHeader pointer to the
	    // one just ed
	    currentCCSDSHeader = myOEM->ccsdsHeader;
	}
	else
	{
	    // failure to read header line, abort
            MessageInterface::ShowMessage("Failed to read OEM Header! Abort!\n");
	    currentCCSDSHeader = NULL;
	    return false;
	}
    }

    do
    {

        // Read in metadata
	if (GetCCSDSMetaData(lff,myOEM))
	{
	    // success so set currentHeader pointer to the
	    // one just ed
	    currentCCSDSMetaData = myOEM->ccsdsMetaData;
	}
	else
	{
	    // failure to read header line, abort
            MessageInterface::ShowMessage("Failed to read OEM MetaData! Abort!\n");
	    currentCCSDSMetaData = NULL;
	    return false;
	}

        do
        {

            myOEM->ccsdsHeader = currentCCSDSHeader;
            myOEM->ccsdsMetaData = (OEMCCSDSMetaData*)currentCCSDSMetaData;

            // Container for any comments found before the first state vector
            StringArray comments;

            bool commentsFound = GetCCSDSComments(lff,comments);

            if (GetCCSDSOEMData(lff,myOEM))
            {
                if (commentsFound)
                    myOEM->ccsdsOEMStateVector->comments = comments;

                // Push this data point onto the obtype data stack
                theData.push_back(myOEM);
            }
            else
            {
                delete myOEM;
                return false;
            }

            // Allocate another struct in memory
            myOEM = new OEMCCSDSObType;

            // Read another line from file
            lff = ReadLineFromFile();

        }
        while (!IsEOF() && !pcrecpp::RE("^META_START.*").FullMatch(lff));
    }
    while (!IsEOF());
        
    return true;
}
//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff, OEMCCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool OEMCCSDSDataFile::GetCCSDSMetaData(std::string &lff,
                                               OEMCCSDSObType *myOb)
{
    OEMCCSDSMetaData *myMetaData = new OEMCCSDSMetaData;

    Integer requiredCount = 0;
    std::string keyword;

    if (pcrecpp::RE("^META_START.*$").FullMatch(lff))
        lff = ReadLineFromFile();

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;
        Integer keyID = myMetaData->GetKeywordID(keyword);
        if(myMetaData->IsParameterRequired(keyID)) requiredCount++;
        switch (keyID)
        {

            case OEMCCSDSMetaData::CCSDS_OEM_METADATACOMMENTS_ID:
                {
                std::string stemp;
                if (!GetCCSDSComment(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                }
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_OBJECTNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->objectName))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_OBJECTID_ID:

                if (!GetCCSDSValue(lff,myMetaData->internationalDesignator))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_CENTERNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrameOrigin))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrame))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_STARTEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->startEpoch))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_STOPEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->stopEpoch))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_USEABLE_STARTEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->useableStartEpoch))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_USEABLE_STOPEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->useableStopEpoch))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_INTERPOLATION_ID:

                if (!GetCCSDSValue(lff,myMetaData->interpolationMethod))
                    return false;
                break;

            case OEMCCSDSMetaData::CCSDS_OEM_INTERPOLATIONDEGREE_ID:

                if (!GetCCSDSValue(lff,myMetaData->interpolationDegree))
                    return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword +
                                     " : This data not allowed in MetaData!\n");
                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while(requiredCount <= requiredNumberMetaDataParameters &&
          !pcrecpp::RE("^META_STOP.*$").FullMatch(lff) && !IsEOF());


    if (requiredCount < requiredNumberMetaDataParameters)
    {
        MessageInterface::ShowMessage("Error: MetaData does not contain all required elements! Abort!\n");
        return false;
    }
    else
    {
        myOb->ccsdsMetaData = myMetaData;
        // Read past the META_STOP line
        lff = ReadLineFromFile();
        return true;
    }
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
bool OEMCCSDSDataFile::GetCCSDSOEMData(std::string &lff,
                                              OEMCCSDSObType *myOb)
{
    OEMStateVectorCCSDSData *myOEMData = new OEMStateVectorCCSDSData;

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp1, dtemp2, dtemp3, dtemp4, dtemp5, dtemp6;
    std::string stemp;

    std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" + REGEX_SCINUMBER +
            ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER +
            ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER +
            ")\\s*(" + REGEX_SCINUMBER + ")$";

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,&dtemp3,
                                     &dtemp4,&dtemp5,&dtemp6))
    {
        myOEMData->timeTag = stemp;
        if (!CCSDSTimeTag2A1Date(myOEMData->timeTag,myOb->epoch))
        {
            MessageInterface::ShowMessage("Failed to convert CCSDS Time Tag to A1Date! Abort!\n");            
            return false;
        }
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
    else
    {
        MessageInterface::ShowMessage("Failed to  OEM data line! Abort!\n");
        return false;
    }
}