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
    if (!ProcessCCSDSDataFile::Initialize()) return false;

    requiredNumberMetaDataParameters = CCSDSOPMCountRequiredNumberMetaDataParameters();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSOPMObType *myOPM = new CCSDSOPMObType;

        // Read the first line from file
	std::string line = ReadLineFromFile();

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
	    line = ReadLineFromFile();
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
	currentCCSDSMetaData(NULL),
	lastMetaDataWritten(NULL),
        isMetaDataWritten(false),
        requiredNumberMetaDataParameters(0)
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
    currentCCSDSMetaData(CCSDSOPMdf.currentCCSDSMetaData),
    lastMetaDataWritten(CCSDSOPMdf.lastMetaDataWritten),
    isMetaDataWritten(CCSDSOPMdf.isMetaDataWritten),
    requiredNumberMetaDataParameters(CCSDSOPMdf.requiredNumberMetaDataParameters)
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
    lastMetaDataWritten = CCSDSOPMdf.lastMetaDataWritten;
    isMetaDataWritten = CCSDSOPMdf.isMetaDataWritten;
    requiredNumberMetaDataParameters = CCSDSOPMdf.requiredNumberMetaDataParameters;
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
    std::string lff = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_OPM_VERS.*").FullMatch(lff))
    {

	if (GetCCSDSHeader(lff,myOPM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSHeader = myOPM->ccsdsHeader;
	}
	else
	{
	    // failure to read header data, abort
	    currentCCSDSHeader = NULL;
	    return false;
	}
    }

    if (GetCCSDSMetaData(lff,myOPM))
    {
        // success so set currentHeader pointer to the
        // one just processed
        currentCCSDSMetaData = myOPM->ccsdsOPMMetaData;
    }
    else
    {
        // failure to read metadata, abort
        currentCCSDSMetaData = NULL;
        return false;
    }

    // Container for any comments found before the first state vector
    StringArray comments;

    bool commentsFound = GetCCSDSComments(lff,comments);

    // Parse the data record making sure that we have identified
    // a header record and a metadata record previously
    if (currentCCSDSHeader == NULL || currentCCSDSMetaData == NULL)
	return false;

    if (pcrecpp::RE("^EPOCH.*").FullMatch(lff))
    {
	myOPM->ccsdsHeader = currentCCSDSHeader;
        myOPM->ccsdsOPMMetaData = currentCCSDSMetaData;
	if (GetCCSDSOPMData(lff,myOPM))
        {
            if (commentsFound)
                myOPM->ccsdsOPMStateVector->comments = comments;
            return true;
        }
        else
            return false;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSOPMData(std::string &lff, CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSOPMData(std::string &lff,
                                              CCSDSOPMObType *myOb)
{
    // Container for any comments found
    StringArray comments;

    bool commentsFound = GetCCSDSComments(lff,comments);

    std::string regex = "^EPOCH\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if (GetCCSDSOPMStateVector(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsOPMStateVector->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^SEMI_MAJOR_AXIS\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if (GetCCSDSOPMKeplerianElements(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsOPMKeplerianElements->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^MASS\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if (GetCCSDSOPMSpacecraftParameters(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsOPMSpacecraftParameters->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    myOb->i_ccsdsOPMManeuvers = myOb->ccsdsOPMManeuvers.begin();

    regex = "^MAN_EPOCH_IGNITION\\s*=.*";

    while (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {
        if (GetCCSDSOPMManeuver(lff,myOb))
        {
            myOb->i_ccsdsOPMManeuvers++;
            if (commentsFound)
            {
                (*myOb->i_ccsdsOPMManeuvers)->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
    }

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSOPMStateVector(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the state vector information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSOPMStateVector(std::string &lff,
                                                     CCSDSOPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSOPMStateVector *myOPMStateVector = new CCSDSOPMStateVector;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_EPOCH_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->epoch)) return false;
                if (!CCSDSTimeTag2A1Date(myOPMStateVector->epoch,
                                         myOb->epoch)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_X_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->x)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_Y_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->y)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_Z_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->z)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_XDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->xDot)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_YDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->yDot)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_ZDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->zDot)) return false;
                count++;
                break;

            default:

                return false;
                break;
            }

        lff = ReadLineFromFile();

    }
    while ( count < 7 );

    myOb->ccsdsOPMStateVector = myOPMStateVector;

    return true;

}


//------------------------------------------------------------------------------
// bool GetCCSDSOPMKeplerianElements(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the Keplerian element information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSOPMKeplerianElements(std::string &lff,
                                                           CCSDSOPMObType *myOb)
{
    Integer count = 0;
    std::string keyword;

    CCSDSOPMKeplerianElements *myOPMKeplerianElements =
                                                  new CCSDSOPMKeplerianElements;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->semiMajorAxis)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_ECCENTRICITY_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->eccentricity)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_INCLINATION_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->inclination)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_RAAN_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->raan)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->argumentOfPericenter)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_TRUEANOMALY_ID:
                {
                Real value;
                if (!GetCCSDSValue(lff,value)) return false;
                myOPMKeplerianElements->theAnomaly.Set(
                                          myOPMKeplerianElements->semiMajorAxis,
                                          myOPMKeplerianElements->eccentricity,
                                          value, Anomaly::TA);
                count++;
                }
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_MEANANOMALY_ID:
                {
                Real value;
                if (!GetCCSDSValue(lff,value)) return false;
                myOPMKeplerianElements->theAnomaly.Set(
                                          myOPMKeplerianElements->semiMajorAxis,
                                          myOPMKeplerianElements->eccentricity,
                                          value, Anomaly::MA);
                count++;
                }
                break;

            case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->gravitationalCoefficient)) return false;
                count++;
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();

    }
    while ( count < 7 );

    myOb->ccsdsOPMKeplerianElements = myOPMKeplerianElements;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSOPMSpacecraftParameters(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the spacecraft parameter information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSOPMSpacecraftParameters(std::string &lff,
                                                     CCSDSOPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSOPMSpacecraftParameters *myOPMSpacecraftParameters =
                                               new CCSDSOPMSpacecraftParameters;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_MASS_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->mass)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->solarRadiationArea)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->solarRadiationCoefficient)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGAREA_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->dragArea)) return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->dragCoefficient)) return false;
                count++;
                break;

            default:

                return false;
                break;
            }

        lff = ReadLineFromFile();

    }
    while ( count < 5 );

    myOb->ccsdsOPMSpacecraftParameters = myOPMSpacecraftParameters;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSOPMManeuver(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the maneuver information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSOPMManeuver(std::string &lff,
                                                     CCSDSOPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSOPMManeuver *myOPMManeuver = new CCSDSOPMManeuver;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {
            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_IGNITIONEPOCH_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->ignitionEpoch))
                    return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DURATION_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->duration))
                    return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAMASS_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaMass))
                    return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->refFrame))
                    return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAV1_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV1))
                    return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAV2_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV2))
                    return false;
                count++;
                break;

            case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAV3_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV3))
                    return false;
                count++;
                break;

            default:
                return false;
                break;

        }

        lff = ReadLineFromFile();

    }
    while ( count < 7 );

    myOb->ccsdsOPMManeuvers.push_back(myOPMManeuver);

    return true;

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
                                CCSDSOPMObType *myOb)
{
    
   // Initialize individual data struct
    // This needs new memory allocation because
    // we are storing pointers to this data
    CCSDSOPMMetaData *myMetaData = new CCSDSOPMMetaData;

    Integer count = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myMetaData->GetKeywordID(keyword))
        {

            case CCSDSOPMMetaData::CCSDS_OPM_METADATACOMMENTS_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                if (myMetaData->IsParameterRequired(CCSDSOPMMetaData::CCSDS_OPM_METADATACOMMENTS_ID))
                    count++;
                }
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_OBJECTNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->objectName))
                    return false;
                if (myMetaData->IsParameterRequired(CCSDSOPMMetaData::CCSDS_OPM_OBJECTNAME_ID))
                    count++;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_OBJECTID_ID:

                if (!GetCCSDSValue(lff,myMetaData->internationalDesignator))
                    return false;
                if (myMetaData->IsParameterRequired(CCSDSOPMMetaData::CCSDS_OPM_OBJECTID_ID))
                    count++;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_CENTERNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrameOrigin))
                    return false;
                if (myMetaData->IsParameterRequired(CCSDSOPMMetaData::CCSDS_OPM_CENTERNAME_ID))
                    count++;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrame))
                    return false;
                if (myMetaData->IsParameterRequired(CCSDSOPMMetaData::CCSDS_OPM_REFFRAME_ID))
                    count++;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                if (myMetaData->IsParameterRequired(CCSDSOPMMetaData::CCSDS_OPM_TIMESYSTEM_ID))
                    count++;
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while(count < requiredNumberMetaDataParameters ||
          pcrecpp::RE("^COMMENT\\s*.*$").FullMatch(lff));

    myOb->ccsdsOPMMetaData = myMetaData;

    return true;
}

//------------------------------------------------------------------------------
// bool WriteData(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes a CCSDS orbit ephemeris message to file
 *
 * @param <myOPM> the OPM data to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::WriteData(const ObType *myOb)
{
    if (myOb->GetTypeName() != "CCSDSOPMObType") return false;

    CCSDSOPMObType *theOPM = (CCSDSOPMObType*)myOb;
    WriteDataHeader(theOPM);
    WriteMetaData(theOPM);
    *theFile << theOPM;
    return true;
}