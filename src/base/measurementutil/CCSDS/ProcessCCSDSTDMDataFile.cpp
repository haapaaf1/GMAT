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
    if (!ProcessCCSDSDataFile::Initialize()) return false;

    requiredNumberMetaDataParameters = CountRequiredNumberTDMMetaDataParameters();

    // Test to see if we are reading or writing
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Construct an orbit parameter message obtype
        CCSDSTDMObType *myTDM = new CCSDSTDMObType;

        while (!IsEOF())
        {
            // The GetData function will attempt to populate the
            // TDM obtype variables
            if (GetData(myTDM))
                // Push this data point onto the obtype data stack
                theData.push_back(myTDM);
            else
                delete myTDM;

            // Allocate another struct in memory
            myTDM = new CCSDSTDMObType;
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSTDM_DATA

            ProcessCCSDSTDMDataFile myOutFile("theFile");
            myOutFile.SetReadWriteMode("w");
            myOutFile.SetFileName("TDM.output");
            myOutFile.Initialize();
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
                myOutFile.WriteData((*j));
            }
            myOutFile.CloseFile();

        #endif

        MessageInterface::ShowMessage("Completed reading TDM data\n");

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
//  ProcessCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSTDMDataFile", itsName)
{
   objectTypeNames.push_back("CCSDSTDMDataFile");
   fileFormatName = "TDM";
   fileFormatID = 7;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile(const ProcessCCSDSTDMDataFile &CCSDSTDMdf) :
    ProcessCCSDSDataFile(CCSDSTDMdf)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSTDMDataFile::ProcessCCSDSTDMDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSTDMDataFile& ProcessCCSDSTDMDataFile::operator=(const ProcessCCSDSTDMDataFile &CCSDSTDMdf)
{
    if (&CCSDSTDMdf == this)
	return *this;

    ProcessCCSDSDataFile::operator=(CCSDSTDMdf);

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
 * This method returns a clone of the ProcessCCSDSDataFile.
 *
 * @return clone of the ProcessCCSDSDataFile.
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
// bool GetData(ObType *myTDMData)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of TDM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSTDMDataFile::GetData(ObType *myTDMData)
{
    
    if (!pcrecpp::RE("^CCSDSTDMObType").FullMatch(myTDMData->GetTypeName()))
        return false;

    // Re-cast the generic ObType pointer as a CCSDSTDMObtype pointer
    CCSDSTDMObType *myTDM = (CCSDSTDMObType*)myTDMData;

    // Read the first line from file
    std::string lff = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    if (!IsEOF() && currentCCSDSHeader == NULL
                 &&  pcrecpp::RE("^CCSDS_TDM_VERS.*").FullMatch(lff))
    {
	if (GetCCSDSHeader(lff,myTDM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
            MessageInterface::ShowMessage("Completed reading TDM Header...\n");
	    currentCCSDSHeader = myTDM->ccsdsHeader;
	}
	else
	{
	    // failure to read header line, abort
            MessageInterface::ShowMessage("Failed to read TDM Header! Abort!\n");
	    currentCCSDSHeader = NULL;
	    return false;
	}
    }

    do
    {

        // Test for DATA_STOP and advance to next line
        // This really only applies after going through this do-while
        // loop more than one time.
        if (pcrecpp::RE("^DATA_STOP.*$").FullMatch(lff))
        {
            lff = ReadLineFromFile();
            if (IsEOF()) break;
        }

        // Read in metadata
	if (GetCCSDSMetaData(lff,myTDM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
            MessageInterface::ShowMessage("Completed reading TDM MetaData...\n");
	    currentCCSDSMetaData = myTDM->ccsdsMetaData;
	}
	else
	{
	    // failure to read header line, abort
            MessageInterface::ShowMessage("Failed to read TDM MetaData! Abort!\n");
	    currentCCSDSMetaData = NULL;
	    return false;
	}

        do
        {

            myTDM->ccsdsHeader = currentCCSDSHeader;
            myTDM->ccsdsMetaData = (CCSDSTDMMetaData*)currentCCSDSMetaData;

            if (GetCCSDSTDMData(lff,myTDM))
            {
                // Push this data point onto the obtype data stack
                theData.push_back(myTDM);
            }
            else
            {
                delete myTDM;
                return false;
            }

            // Allocate another struct in memory
            myTDM = new CCSDSTDMObType;

            lff = ReadLineFromFile();

        }
        while (!IsEOF() && !pcrecpp::RE("^DATA_STOP.*").FullMatch(lff));

    }
    while (!IsEOF());

    return true;
}

//------------------------------------------------------------------------------
// bool GetCCSDSTDMData(std::string &lff, CCSDSTrackingData *myData,
//                      CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSTDMDataFile::GetCCSDSTDMData(std::string &lff,
                                              CCSDSTDMObType *myOb)
{

    StringArray comments;
    bool commentsFound = false;

    if (pcrecpp::RE("^DATA_START.*$").FullMatch(lff))
    {
        // Read  past DATA_START line
        lff = ReadLineFromFile();
        // Attempt to extract comments, if any
        commentsFound = GetCCSDSComments(lff,comments);
    }

    std::string keyword, ccsdsEpoch;
    Real value;

    MessageInterface::ShowMessage(lff+"\n");

    GetCCSDSKeyEpochValueData(lff,keyword,ccsdsEpoch,value);

    CCSDSTrackingData *myTDMData = new CCSDSTrackingData;

    myTDMData->keywordID = myTDMData->GetKeywordID(keyword);
    if(myTDMData->keywordID >= 0)
    {
        myTDMData->keyword = keyword;
        myTDMData->timeTag = ccsdsEpoch;
        if (!CCSDSTimeTag2A1Date(myTDMData->timeTag,myOb->epoch))
            return false;
        myTDMData->measurement = value;

        if (commentsFound)
            myTDMData->comments = comments;

        myOb->ccsdsTrackingData =  myTDMData;
        myOb->ccsdsHeader->dataType = CCSDSHeader::TRACKINGDATA_ID;
        return true;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff,
//                       ccsdsMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSTDMDataFile::GetCCSDSMetaData(std::string &lff,
                                CCSDSTDMObType *myOb)
{

    // Initialize individual data struct
    // This needs new memory allocation because
    // we are storing pointers to this data
    CCSDSTDMMetaData *myMetaData = new CCSDSTDMMetaData;

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

            case CCSDSTDMMetaData::CCSDS_TDM_METADATACOMMENTS_ID:
            {
                std::string stemp;
                if (!GetCCSDSComment(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                {
                    MessageInterface::ShowMessage("Failed to get timeSystem\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_STARTTIME_ID:

                if (!GetCCSDSValue(lff,myMetaData->startTime))
                {
                    MessageInterface::ShowMessage("Failed to get startTime\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_STOPTIME_ID:

                if (!GetCCSDSValue(lff,myMetaData->stopTime))
                {
                    MessageInterface::ShowMessage("Failed to get stopTime\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT1_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[0]))
                {
                    MessageInterface::ShowMessage("Failed to get participants[0]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT2_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[1]))
                {
                    MessageInterface::ShowMessage("Failed to get participants[1]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT3_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[2]))
                {
                    MessageInterface::ShowMessage("Failed to get participants[2]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT4_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[3]))
                {
                    MessageInterface::ShowMessage("Failed to get participants[3]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT5_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[4]))
                {
                    MessageInterface::ShowMessage("Failed to get participants[4]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_MODE_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get mode\n");
                    return false;
                }
                myMetaData->mode = myMetaData->GetModeID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PATH_ID:

                if (!GetCCSDSValue(lff,myMetaData->path[0]))
                {
                    MessageInterface::ShowMessage("Failed to get path\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PATH1_ID:

                if (!GetCCSDSValue(lff,myMetaData->path[1]))
                {
                    MessageInterface::ShowMessage("Failed to get path[1]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_PATH2_ID:

                if (!GetCCSDSValue(lff,myMetaData->path[2]))
                {
                    MessageInterface::ShowMessage("Failed to get path[2]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITBAND_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitBand))
                {
                    MessageInterface::ShowMessage("Failed to get transmitBand\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEBAND_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveBand))
                {
                    MessageInterface::ShowMessage("Failed to get receiveBand\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

                if (!GetCCSDSValue(lff,myMetaData->turnaroundNumerator))
                {
                    MessageInterface::ShowMessage("Failed to get turnaroundNumerator\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

                if (!GetCCSDSValue(lff,myMetaData->turnaroundDenominator))
                {
                    MessageInterface::ShowMessage("Failed to get turnaroundDenominator\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TIMETAGREF_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get timeTagRef\n");
                    return false;
                }
                myMetaData->timeTagRef = myMetaData->GetTimeTagID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_INTEGRATIONINTERVAL_ID:

                if (!GetCCSDSValue(lff,myMetaData->integrationInterval))
                {
                    MessageInterface::ShowMessage("Failed to get integrationInterval\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_INTEGRATIONREF_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get integrationRef\n");
                    return false;
                }
                myMetaData->integrationRef = myMetaData->GetIntegrationID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_FREQUENCYOFFSET_ID:

                if (!GetCCSDSValue(lff,myMetaData->frequencyOffset))
                {
                    MessageInterface::ShowMessage("Failed to get frequencyOffset\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RANGEMODE_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get rangeMode\n");
                    return false;
                }
                myMetaData->rangeMode = myMetaData->GetRangeModeID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RANGEMODULUS_ID:

                if (!GetCCSDSValue(lff,myMetaData->rangeModulus))
                {
                    MessageInterface::ShowMessage("Failed to get rangeModulus\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RANGEUNITS_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get rangeUnits\n");
                    return false;
                }
                myMetaData->rangeUnits = myMetaData->GetRangeUnitID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_ANGLETYPE_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get angleType\n");
                    return false;
                }
                myMetaData->angleType = myMetaData->GetAngleTypeID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_REFERENCEFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->referenceFrame))
                {
                    MessageInterface::ShowMessage("Failed to get referenceFrame\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY1_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[0]))
                {
                    MessageInterface::ShowMessage("Failed to get transmitDelay[0]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY2_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[1]))
                {
                    MessageInterface::ShowMessage("Failed to get transmitDelay[1]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY3_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[2]))
                {
                    MessageInterface::ShowMessage("Failed to get transmitDelay[2]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY4_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[3]))
                {
                    MessageInterface::ShowMessage("Failed to get transmitDelay[3]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY5_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[4]))
                {
                    MessageInterface::ShowMessage("Failed to get transmitDelay[4]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY1_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[0]))
                {
                    MessageInterface::ShowMessage("Failed to get receiveDelay[0]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY2_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[1]))
                {
                    MessageInterface::ShowMessage("Failed to get receiveDelay[1]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY3_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[2]))
                {
                    MessageInterface::ShowMessage("Failed to get receiveDelay[2]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY4_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[3]))
                {
                    MessageInterface::ShowMessage("Failed to get receiveDelay[3]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY5_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[4]))
                {
                    MessageInterface::ShowMessage("Failed to get receiveDelay[4]\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_DATAQUALITY_ID:
            {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get dataQuality\n");
                    return false;
                }
                myMetaData->dataQuality = myMetaData->GetDataQualityID(stemp);
            }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONANGLE1_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionAngle1))
                {
                    MessageInterface::ShowMessage("Failed to get correctionAngle1\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONANGLE2_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionAngle2))
                {
                    MessageInterface::ShowMessage("Failed to get correctionAngle2\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONDOPPLER_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionDoppler))
                {
                    MessageInterface::ShowMessage("Failed to get correctionDoppler\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONRANGE_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionRange))
                {
                    MessageInterface::ShowMessage("Failed to get correctionRange\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONRECEIVE_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionReceive))
                {
                    MessageInterface::ShowMessage("Failed to get correctionReceive\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONTRANSMIT_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionTransmit))
                {
                    MessageInterface::ShowMessage("Failed to get correctionTransmit\n");
                    return false;
                }

            break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONAPPLIED_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                {
                    MessageInterface::ShowMessage("Failed to get correctionsApplied\n");
                    return false;
                }
                if (pcrecpp::RE("^YES$").FullMatch(Trim(stemp)))
                    myMetaData->correctionsApplied = true;
                else
                    myMetaData->correctionsApplied = false;

                }

            break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in TDM MetaData\n");
                return false;

            break;

        }

        lff = ReadLineFromFile();
    }
    while(requiredCount <= requiredNumberMetaDataParameters &&
          !pcrecpp::RE("^META_STOP$").FullMatch(lff));

    if (requiredCount < requiredNumberMetaDataParameters)
    {
        MessageInterface::ShowMessage("Error: MetaData does not contain all required elements! Abort!\n");
        return false;
    }

    myOb->ccsdsMetaData = myMetaData;

    // Read past the META_STOP line
    lff = ReadLineFromFile();
    
    return true;

}

