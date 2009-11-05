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

            fstream *outFile = new fstream;
            outFile->open("tdm.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
		*outFile << (CCSDSTDMObType*)(*j) << std::endl;

            outFile->close();

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
    
    if (myTDMData->GetTypeName() != "CCSDSTDMObType") return false;

    // Re-cast the generic ObType pointer as a CCSDSTDMObtype pointer
    CCSDSTDMObType *myTDM = (CCSDSTDMObType*)myTDMData;

    // Read the first line from file
    std::string lff = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_TDM_VERS.*").FullMatch(lff))
    {
    {
	if (GetCCSDSHeader(lff,myTDM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSHeader = myTDM->ccsdsHeader;
	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSHeader = NULL;
	    return false;
	}
    }
    }

    // Test for the prescence of meta data
    // If the header data was just processed, line should now contain
    // the "META_START" line
    while (!IsEOF() && pcrecpp::RE("^META_START.*").FullMatch(lff))
    {       
	if (GetCCSDSMetaData(lff,myTDM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetaData = myTDM->ccsdsMetaData;
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
    if (pcrecpp::RE("^DATA_START.*").FullMatch(lff))
    {
        lff = ReadLineFromFile();
    }

    // Parse the data record making sure that we have identified
    // a header record and a metadata record previously
    if (currentCCSDSHeader == NULL || currentCCSDSMetaData == NULL)
	return false;

    if (!pcrecpp::RE("^DATA_STOP.*").FullMatch(lff) && !pcrecpp::RE("").FullMatch(lff))
    {
        myTDM->ccsdsHeader = currentCCSDSHeader;
        myTDM->ccsdsMetaData = (CCSDSTDMMetaData*)currentCCSDSMetaData;
	return GetCCSDSTDMData(lff,myTDM);
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSTDMData(std::string &lff, CCSDSData *myData,
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
    std::string keyword, ccsdsEpoch;
    Real value;

    CCSDSData *myTDMData = new CCSDSData;

    GetCCSDSKeyEpochValueData(lff,keyword,ccsdsEpoch,value);

    myTDMData->keywordID = myOb->GetKeywordID(keyword);
    if(myTDMData->keywordID >= 0)
    {
        myTDMData->keyword = keyword;
        myTDMData->timeTag = ccsdsEpoch;
        if (!CCSDSTimeTag2A1Date(myTDMData->timeTag,myOb->epoch)) return false;
        myTDMData->measurement = value;
        myOb->ccsdsTDMData =  myTDMData;
        myOb->ccsdsHeader->dataType = CCSDSHeader::GENERICDATA_ID;
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
                if (!GetCCSDSValue(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                }
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_STARTTIME_ID:

                if (!GetCCSDSValue(lff,myMetaData->startTime))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_STOPTIME_ID:

                if (!GetCCSDSValue(lff,myMetaData->stopTime))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT1_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[0]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT2_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[1]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT3_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[2]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT4_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[3]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PARTICIPANT5_ID:

                if (!GetCCSDSValue(lff,myMetaData->participants[4]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_MODE_ID:

                if (!GetCCSDSValue(lff,myMetaData->mode))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PATH_ID:

                if (!GetCCSDSValue(lff,myMetaData->path[0]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PATH1_ID:

                if (!GetCCSDSValue(lff,myMetaData->path[1]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_PATH2_ID:

                if (!GetCCSDSValue(lff,myMetaData->path[2]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITBAND_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitBand))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEBAND_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveBand))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TURNAROUNDNUMERATOR_ID:

                if (!GetCCSDSValue(lff,myMetaData->turnaroundNumerator))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:

                if (!GetCCSDSValue(lff,myMetaData->turnaroundDenominator))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TIMETAGREF_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeTagRef))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_INTEGRATIONINTERVAL_ID:

                if (!GetCCSDSValue(lff,myMetaData->integrationInterval))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_INTEGRATIONREF_ID:

                if (!GetCCSDSValue(lff,myMetaData->integrationRef))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_FREQUENCYOFFSET_ID:

                if (!GetCCSDSValue(lff,myMetaData->frequencyOffset))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RANGEMODE_ID:

                if (!GetCCSDSValue(lff,myMetaData->rangeMode))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RANGEMODULUS_ID:

                if (!GetCCSDSValue(lff,myMetaData->rangeModulus))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RANGEUNITS_ID:

                if (!GetCCSDSValue(lff,myMetaData->rangeUnits))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_ANGLETYPE_ID:

                if (!GetCCSDSValue(lff,myMetaData->angleType))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_REFERENCEFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->referenceFrame))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY1_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[0]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY2_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[1]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY3_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[2]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY4_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[3]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_TRANSMITDELAY5_ID:

                if (!GetCCSDSValue(lff,myMetaData->transmitDelay[4]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY1_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[0]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY2_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[1]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY3_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[2]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY4_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[3]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_RECEIVEDELAY5_ID:

                if (!GetCCSDSValue(lff,myMetaData->receiveDelay[4]))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_DATAQUALITY_ID:

                if (!GetCCSDSValue(lff,myMetaData->dataQuality))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONANGLE1_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionAngle1))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONANGLE2_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionAngle2))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONDOPPLER_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionDoppler))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONRANGE_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionRange))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONRECEIVE_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionReceive))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONTRANSMIT_ID:

                if (!GetCCSDSValue(lff,myMetaData->correctionTransmit))
                    return false;
                break;

            case CCSDSTDMMetaData::CCSDS_TDM_CORRECTIONAPPLIED_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp)) return false;
                if (pcrecpp::RE("^YES$").FullMatch(Trim(stemp)))
                    myMetaData->correctionsApplied = true;
                else
                    myMetaData->correctionsApplied = false;

                }
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while(requiredCount < requiredNumberMetaDataParameters &&
          !pcrecpp::RE("^DATA_START$").FullMatch(lff));

    myOb->ccsdsMetaData = myMetaData;

    return true;

}

