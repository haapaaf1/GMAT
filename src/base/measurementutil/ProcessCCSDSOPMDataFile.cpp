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

#define DEBUG_CCSDSOPM_DATA

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

    requiredNumberMetaDataParameters = CountRequiredNumberOPMMetaDataParameters();
    requiredNumberStateVectorParameters = CountRequiredNumberStateVectorParameters();
    requiredNumberKeplerianElementsParameters = CountRequiredNumberKeplerianElementsParameters();
    requiredNumberSpacecraftParameters = CountRequiredNumberSpacecraftParameters();
    requiredNumberManeuverParameters = CountRequiredNumberManeuverParameters();

    // Test to see if we are reading or writing
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Construct an orbit parameter message obtype
        CCSDSOPMObType *myOPM = new CCSDSOPMObType;

        while (!IsEOF())
        {
            // The GetData function will attempt to populate the
            // OPM obtype variables
            if (GetData(myOPM))
                // Push this data point onto the obtype data stack
                theData.push_back(myOPM);
            else
                delete myOPM;

            // Allocate another struct in memory
            myOPM = new CCSDSOPMObType;
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSOPM_DATA

            fstream *outFile = new fstream;
            outFile->open("opm.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
		*outFile << (CCSDSOPMObType*)(*j) << std::endl;

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
        requiredNumberMetaDataParameters(0),
        requiredNumberStateVectorParameters(0),
        requiredNumberKeplerianElementsParameters(0),
        requiredNumberSpacecraftParameters(0),
        requiredNumberManeuverParameters(0)
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
    requiredNumberMetaDataParameters(CCSDSOPMdf.requiredNumberMetaDataParameters),
    requiredNumberStateVectorParameters(CCSDSOPMdf.requiredNumberStateVectorParameters),
    requiredNumberKeplerianElementsParameters(CCSDSOPMdf.requiredNumberKeplerianElementsParameters),
    requiredNumberSpacecraftParameters(CCSDSOPMdf.requiredNumberSpacecraftParameters),
    requiredNumberManeuverParameters(CCSDSOPMdf.requiredNumberManeuverParameters)
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
    requiredNumberStateVectorParameters = CCSDSOPMdf.requiredNumberStateVectorParameters;
    requiredNumberKeplerianElementsParameters = CCSDSOPMdf.requiredNumberKeplerianElementsParameters;
    requiredNumberSpacecraftParameters = CCSDSOPMdf.requiredNumberSpacecraftParameters;
    requiredNumberManeuverParameters = CCSDSOPMdf.requiredNumberManeuverParameters;
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
        if (GetCCSDSKeplerianElements(lff,myOb))
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
        if (GetCCSDSSpacecraftParameters(lff,myOb))
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
        if (GetCCSDSManeuver(lff,myOb))
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

    CCSDSOPMStateVector *myOPMStateVector = new CCSDSOPMStateVector;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myOPMStateVector->GetKeywordID(keyword);

        if(myOPMStateVector->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSStateVector::CCSDS_STATEVECTOR_TIMETAG_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->timeTag)) return false;
                if (!CCSDSTimeTag2A1Date(myOPMStateVector->timeTag,
                                         myOb->epoch)) return false;
                break;

            case CCSDSStateVector::CCSDS_STATEVECTOR_X_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->x)) return false;
                break;

            case CCSDSStateVector::CCSDS_STATEVECTOR_Y_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->y)) return false;
                break;

            case CCSDSStateVector::CCSDS_STATEVECTOR_Z_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->z)) return false;
                break;

            case CCSDSStateVector::CCSDS_STATEVECTOR_XDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->xDot)) return false;
                break;

            case CCSDSStateVector::CCSDS_STATEVECTOR_YDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->yDot)) return false;
                break;

            case CCSDSStateVector::CCSDS_STATEVECTOR_ZDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->zDot)) return false;
                break;

            default:

                return false;
                break;
            }

        lff = ReadLineFromFile();

    }
    while ( requiredCount < requiredNumberStateVectorParameters );

    myOb->ccsdsOPMStateVector = myOPMStateVector;

    return true;

}


//------------------------------------------------------------------------------
// bool GetCCSDSKeplerianElements(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the Keplerian element information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSKeplerianElements(std::string &lff,
                                                           CCSDSOPMObType *myOb)
{
    CCSDSKeplerianElements *myOPMKeplerianElements = new CCSDSKeplerianElements;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myOPMKeplerianElements->GetKeywordID(keyword);

        if(myOPMKeplerianElements->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->semiMajorAxis)) return false;
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->eccentricity)) return false;
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_INCLINATION_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->inclination)) return false;
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_RAAN_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->raan)) return false;
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->argumentOfPericenter)) return false;
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID:
                {
                Real value;
                if (!GetCCSDSValue(lff,value)) return false;
                myOPMKeplerianElements->theAnomaly.Set(
                                          myOPMKeplerianElements->semiMajorAxis,
                                          myOPMKeplerianElements->eccentricity,
                                          value, Anomaly::TA);
                }
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID:
                {
                Real value;
                if (!GetCCSDSValue(lff,value)) return false;
                myOPMKeplerianElements->theAnomaly.Set(
                                          myOPMKeplerianElements->semiMajorAxis,
                                          myOPMKeplerianElements->eccentricity,
                                          value, Anomaly::MA);
                }
                break;

            case CCSDSKeplerianElements::CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMKeplerianElements->gravitationalCoefficient)) return false;
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();

    }
    while ( requiredCount < requiredNumberKeplerianElementsParameters );

    myOb->ccsdsOPMKeplerianElements = myOPMKeplerianElements;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSSpacecraftParameters(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the spacecraft parameter information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSSpacecraftParameters(std::string &lff,
                                                     CCSDSOPMObType *myOb)
{

    CCSDSSpacecraftParameters *myOPMSpacecraftParameters =
                                               new CCSDSSpacecraftParameters;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myOPMSpacecraftParameters->GetKeywordID(keyword);

        if(myOPMSpacecraftParameters->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSSpacecraftParameters::CCSDS_SPACECRAFTPARAMETERS_MASS_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->mass)) return false;
                break;

            case CCSDSSpacecraftParameters::CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->solarRadiationArea)) return false;
                break;

            case CCSDSSpacecraftParameters::CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->solarRadiationCoefficient)) return false;
                break;

            case CCSDSSpacecraftParameters::CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->dragArea)) return false;
                break;

            case CCSDSSpacecraftParameters::CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->dragCoefficient)) return false;
                break;

            default:

                return false;
                break;
            }

        lff = ReadLineFromFile();

    }
    while ( requiredCount < requiredNumberSpacecraftParameters );

    myOb->ccsdsOPMSpacecraftParameters = myOPMSpacecraftParameters;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSManeuver(std::string &lff, CCSDSOPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the maneuver information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOPMDataFile::GetCCSDSManeuver(std::string &lff,
                                                     CCSDSOPMObType *myOb)
{

    CCSDSManeuver *myOPMManeuver = new CCSDSManeuver;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myOPMManeuver->GetKeywordID(keyword);

        if(myOPMManeuver->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSManeuver::CCSDS_MANUEVER_IGNITIONEPOCH_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->ignitionEpoch))
                    return false;
                break;

            case CCSDSManeuver::CCSDS_MANUEVER_DURATION_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->duration))
                    return false;
                break;

            case CCSDSManeuver::CCSDS_MANUEVER_DELTAMASS_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaMass))
                    return false;
                break;

            case CCSDSManeuver::CCSDS_MANUEVER_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->refFrame))
                    return false;
                break;

            case CCSDSManeuver::CCSDS_MANUEVER_DELTAV1_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV1))
                    return false;
                break;

            case CCSDSManeuver::CCSDS_MANUEVER_DELTAV2_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV2))
                    return false;
                break;

            case CCSDSManeuver::CCSDS_MANUEVER_DELTAV3_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV3))
                    return false;
                break;

            default:
                return false;
                break;

        }

        lff = ReadLineFromFile();

    }
    while ( requiredCount < requiredNumberManeuverParameters );

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

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myMetaData->GetKeywordID(keyword);

        if(myMetaData->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSOPMMetaData::CCSDS_OPM_METADATACOMMENTS_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                }
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_OBJECTNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->objectName))
                    return false;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_OBJECTID_ID:

                if (!GetCCSDSValue(lff,myMetaData->internationalDesignator))
                    return false;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_CENTERNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrameOrigin))
                    return false;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrame))
                    return false;
                break;

            case CCSDSOPMMetaData::CCSDS_OPM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while(requiredCount < requiredNumberMetaDataParameters ||
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