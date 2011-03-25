//$Header$
//------------------------------------------------------------------------------
//                             OPMCCSDSDataFile
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

#include <OPMCCSDSDataFile.hpp>

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
bool OPMCCSDSDataFile::Initialize()
{
    if (!CCSDSDataFile::Initialize()) return false;

    requiredNumberMetaDataParameters = CountRequiredNumberOPMMetaDataParameters();
    requiredNumberStateVectorParameters = CountRequiredNumberStateVectorParameters();
    requiredNumberKeplerianElementsParameters = CountRequiredNumberKeplerianElementsParameters();
    requiredNumberSpacecraftParameters = CountRequiredNumberSpacecraftParameters();
    requiredNumberManeuverParameters = CountRequiredNumberManeuverParameters();

    // Test to see if we are reading or writing
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Construct an orbit parameter message obtype
        OPMCCSDSObType *myOPM = new OPMCCSDSObType;

        if (!IsEOF())
        {
            // The GetData function will attempt to populate the
            // OPM obtype variables
            if (GetData(myOPM))
                // Push this data point onto the obtype data stack
                theData.push_back(myOPM);
            else
                delete myOPM;
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSOPM_DATA

            OPMCCSDSDataFile myOutFile("theFile");
            myOutFile.SetReadWriteMode("w");
            myOutFile.SetFileName("OPM.output");
            myOutFile.Initialize();
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
                myOutFile.WriteData((*j));
            myOutFile.CloseFile();

        #endif

        MessageInterface::ShowMessage("Completed reading OPM data\n");

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
//  OPMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base OPMCCSDSDataFile structures
 */
//------------------------------------------------------------------------------
OPMCCSDSDataFile::OPMCCSDSDataFile(const std::string &itsName) :
	CCSDSDataFile ("OPMCCSDSDataFile", itsName),
        requiredNumberStateVectorParameters(0),
        requiredNumberKeplerianElementsParameters(0),
        requiredNumberSpacecraftParameters(0),
        requiredNumberManeuverParameters(0)
{
   objectTypeNames.push_back("OPMCCSDSDataFile");
   fileFormatName = "OPM";
   fileFormatID = 8;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  OPMCCSDSDataFile::OPMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for OPMCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
OPMCCSDSDataFile::OPMCCSDSDataFile(const OPMCCSDSDataFile &CCSDSOPMdf) :
    CCSDSDataFile(CCSDSOPMdf),
    requiredNumberStateVectorParameters(CCSDSOPMdf.requiredNumberStateVectorParameters),
    requiredNumberKeplerianElementsParameters(CCSDSOPMdf.requiredNumberKeplerianElementsParameters),
    requiredNumberSpacecraftParameters(CCSDSOPMdf.requiredNumberSpacecraftParameters),
    requiredNumberManeuverParameters(CCSDSOPMdf.requiredNumberManeuverParameters)
{
}


//------------------------------------------------------------------------------
//  OPMCCSDSDataFile::OPMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for OPMCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
const OPMCCSDSDataFile& OPMCCSDSDataFile::operator=(const OPMCCSDSDataFile &CCSDSOPMdf)
{
    if (&CCSDSOPMdf == this)
	return *this;

    CCSDSDataFile::operator=(CCSDSOPMdf);
    requiredNumberStateVectorParameters = CCSDSOPMdf.requiredNumberStateVectorParameters;
    requiredNumberKeplerianElementsParameters = CCSDSOPMdf.requiredNumberKeplerianElementsParameters;
    requiredNumberSpacecraftParameters = CCSDSOPMdf.requiredNumberSpacecraftParameters;
    requiredNumberManeuverParameters = CCSDSOPMdf.requiredNumberManeuverParameters;
    return *this;
}

//------------------------------------------------------------------------------
//  OPMCCSDSDataFile::~OPMCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
OPMCCSDSDataFile::~OPMCCSDSDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the OPMCCSDSDataFile.
 *
 * @return clone of the OPMCCSDSDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* OPMCCSDSDataFile::Clone() const
{
   GmatBase *clone = new OPMCCSDSDataFile(*this);
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
bool OPMCCSDSDataFile::IsParameterReadOnly(const Integer id) const
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
bool OPMCCSDSDataFile::IsParameterReadOnly(const std::string &label) const
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
bool OPMCCSDSDataFile::GetData(ObType *myOPMData)
{

    if (!pcrecpp::RE("^OPMCCSDSObType").FullMatch(myOPMData->GetTypeName())) return false;

    // Re-cast the generic ObType pointer as a CCSDSOPMObtype pointer
    OPMCCSDSObType *myOPM = (OPMCCSDSObType*)myOPMData;

    // Read the first line from file
    std::string lff = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    if (!IsEOF() && currentCCSDSHeader == NULL
                 && pcrecpp::RE("^CCSDS_OPM_VERS.*").FullMatch(lff))
    {

	if (GetCCSDSHeader(lff,myOPM))
	{
            // success so set currentHeader pointer to the
	    // one just ed
	    currentCCSDSHeader = myOPM->ccsdsHeader;
        }
	else
	{
	    // failure to read header data, abort
	    currentCCSDSHeader = NULL;
            MessageInterface::ShowMessage("Failed to read OPM Header! Abort!\n");
	    return false;
	}
    }

    if (!IsEOF() && GetCCSDSMetaData(lff,myOPM))
    {
        // success so set currentHeader pointer to the
        // one just ed
        currentCCSDSMetaData = myOPM->ccsdsMetaData;
    }
    else
    {
        // failure to read metadata, abort
        currentCCSDSMetaData = NULL;
        MessageInterface::ShowMessage("Failed to read OPM Meta Data! Abort!\n");
        return false;
    }

    // Container for any comments found before the first state vector
    StringArray comments;

    bool commentsFound = GetCCSDSComments(lff,comments);

    if (pcrecpp::RE("^EPOCH.*").FullMatch(lff))
    {
	myOPM->ccsdsHeader = currentCCSDSHeader;
        myOPM->ccsdsMetaData = (OPMCCSDSMetaData*)currentCCSDSMetaData;
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
bool OPMCCSDSDataFile::GetCCSDSOPMData(std::string &lff,
                                              OPMCCSDSObType *myOb)
{
    // Container for any comments found
    StringArray comments;

    // This will capture comments after the meta data but before the
    // first line of expected data
    bool commentsFound = GetCCSDSComments(lff,comments);

    std::string regex = "^EPOCH\\s*=.*";
    if (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {

        //MessageInterface::ShowMessage("Found State Vector\n");

        if (GetOPMStateVectorCCSDSData(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsOPMStateVector->comments = comments;
                commentsFound = false;
            }

            // This will capture comments after the state vector
            // but only if the state vector is successully found
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^SEMI_MAJOR_AXIS\\s*=.*";
    if (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {
        //MessageInterface::ShowMessage("Found Keplerian Elements\n");

        if (GetKeplerianElementsCCSDSData(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsOPMKeplerianElements->comments = comments;
                commentsFound = false;
            }
            // This will capture comments after the Keplerian elements
            // but only if the Keplerian elements are successully found
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^MASS\\s*=.*";
    if (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {
        //MessageInterface::ShowMessage("Found Spacecraft Parameters\n");

        if (GetSpacecraftParametersCCSDSData(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsOPMSpacecraftParameters->comments = comments;
                commentsFound = false;
            }
            // This will capture comments after the spacecraft parameters
            // but only if the spacecraft parameters are successully found
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    myOb->i_ccsdsOPMManeuvers = myOb->ccsdsOPMManeuvers.begin();

    regex = "^MAN_EPOCH_IGNITION\\s*=.*";

    while (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {

        if (GetManeuverCCSDSData(lff,myOb))
        {
            if (commentsFound)
                (myOb->ccsdsOPMManeuvers.back())->comments = comments;

            // This will capture comments after the maneuver
            // but only if the maneuver is successully found
            commentsFound = GetCCSDSComments(lff,comments);
        }
    }

    myOb->i_ccsdsOPMManeuvers = myOb->ccsdsOPMManeuvers.begin();

    return true;

}

//------------------------------------------------------------------------------
// bool GetOPMStateVectorCCSDSData(std::string &lff, OPMCCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the state vector information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool OPMCCSDSDataFile::GetOPMStateVectorCCSDSData(std::string &lff,
                                                     OPMCCSDSObType *myOb)
{

    OPMStateVectorCCSDSData *myOPMStateVector = new OPMStateVectorCCSDSData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;
        Integer keyID = myOPMStateVector->GetKeywordID(keyword);
        if(myOPMStateVector->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_TIMETAG_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->timeTag)) return false;
                if (!CCSDSTimeTag2A1Date(myOPMStateVector->timeTag,myOb->epoch))
                    return false;
                break;

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_X_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->x)) return false;
                break;

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_Y_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->y)) return false;
                break;

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_Z_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->z)) return false;
                break;

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_XDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->xDot)) return false;
                break;

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_YDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->yDot)) return false;
                break;

            case StateVectorCCSDSData::CCSDS_STATEVECTOR_ZDOT_ID:

                if (!GetCCSDSValue(lff,myOPMStateVector->zDot)) return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in State Vector\n");
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
// bool GetKeplerianElementsCCSDSData(std::string &lff, OPMCCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the Keplerian element information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool OPMCCSDSDataFile::GetKeplerianElementsCCSDSData(std::string &lff,
                                                           OPMCCSDSObType *myOb)
{
    KeplerianElementsCCSDSData *myOPMKeplerianElements = new KeplerianElementsCCSDSData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;
        Integer keyID = myOPMKeplerianElements->GetKeywordID(keyword);
        if(myOPMKeplerianElements->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:
                if (!GetCCSDSValue(lff,myOPMKeplerianElements->semiMajorAxis)) return false;
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID:
                if (!GetCCSDSValue(lff,myOPMKeplerianElements->eccentricity)) return false;
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_INCLINATION_ID:
                if (!GetCCSDSValue(lff,myOPMKeplerianElements->inclination)) return false;
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_RAAN_ID:
                if (!GetCCSDSValue(lff,myOPMKeplerianElements->raan)) return false;
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:
                if (!GetCCSDSValue(lff,myOPMKeplerianElements->argumentOfPericenter)) return false;
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID:
                {
                Real value;
                if (!GetCCSDSValue(lff,value)) return false;
                myOPMKeplerianElements->theAnomaly.Set(
                                          myOPMKeplerianElements->semiMajorAxis,
                                          myOPMKeplerianElements->eccentricity,
                                          value, Anomaly::TA);
                }
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID:
                {
                Real value;
                if (!GetCCSDSValue(lff,value)) return false;
                myOPMKeplerianElements->theAnomaly.Set(
                                          myOPMKeplerianElements->semiMajorAxis,
                                          myOPMKeplerianElements->eccentricity,
                                          value, Anomaly::MA);
                }
                break;

            case KeplerianElementsCCSDSData::CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:
                if (!GetCCSDSValue(lff,myOPMKeplerianElements->gravitationalCoefficient)) return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in Keplerian Elements\n");
                return false;
                break;

        }

        lff = ReadLineFromFile();

    }
    while ( requiredCount < requiredNumberKeplerianElementsParameters ||
          pcrecpp::RE("^TRUE_ANOMALY\\s*.*$").FullMatch(lff) ||
          pcrecpp::RE("^MEAN_ANOMALY\\s*.*$").FullMatch(lff));

    myOb->ccsdsOPMKeplerianElements = myOPMKeplerianElements;

    return true;

}

//------------------------------------------------------------------------------
// bool GetSpacecraftParametersCCSDSData(std::string &lff, OPMCCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the spacecraft parameter information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool OPMCCSDSDataFile::GetSpacecraftParametersCCSDSData(std::string &lff,
                                                     OPMCCSDSObType *myOb)
{

    SpacecraftParametersCCSDSData *myOPMSpacecraftParameters =
                                               new SpacecraftParametersCCSDSData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {
        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myOPMSpacecraftParameters->GetKeywordID(keyword);

        if(myOPMSpacecraftParameters->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case SpacecraftParametersCCSDSData::CCSDS_SPACECRAFTPARAMETERS_MASS_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->mass)) return false;
                break;

            case SpacecraftParametersCCSDSData::CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->solarRadiationArea)) return false;
                break;

            case SpacecraftParametersCCSDSData::CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->solarRadiationCoefficient)) return false;
                break;

            case SpacecraftParametersCCSDSData::CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->dragArea)) return false;
                break;

            case SpacecraftParametersCCSDSData::CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:

                if (!GetCCSDSValue(lff,myOPMSpacecraftParameters->dragCoefficient)) return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in Spacecraft Parameters\n");
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
// bool GetManeuverCCSDSData(std::string &lff, OPMCCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the maneuver information from the orbit parameter message.
 */
//
//------------------------------------------------------------------------------
bool OPMCCSDSDataFile::GetManeuverCCSDSData(std::string &lff,
                                                     OPMCCSDSObType *myOb)
{

    ManeuverCCSDSData *myOPMManeuver = new ManeuverCCSDSData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myOPMManeuver->GetKeywordID(keyword);

        if(myOPMManeuver->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case ManeuverCCSDSData::CCSDS_MANUEVER_IGNITIONEPOCH_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->ignitionEpoch))
                    return false;
                break;

            case ManeuverCCSDSData::CCSDS_MANUEVER_DURATION_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->duration))
                    return false;
                break;

            case ManeuverCCSDSData::CCSDS_MANUEVER_DELTAMASS_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaMass))
                    return false;
                break;

            case ManeuverCCSDSData::CCSDS_MANUEVER_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->refFrame))
                    return false;
                break;

            case ManeuverCCSDSData::CCSDS_MANUEVER_DELTAV1_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV1))
                    return false;
                break;

            case ManeuverCCSDSData::CCSDS_MANUEVER_DELTAV2_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV2))
                    return false;
                break;

            case ManeuverCCSDSData::CCSDS_MANUEVER_DELTAV3_ID:

                if (!GetCCSDSValue(lff,myOPMManeuver->deltaV3))
                    return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in Maneuvers\n");
                return false;
                break;

        }

        lff = ReadLineFromFile();

    }
    while ( requiredCount < requiredNumberManeuverParameters );

    (myOb->ccsdsOPMManeuvers).push_back(myOPMManeuver);
    myOb->i_ccsdsOPMManeuvers++;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff,
//                       OPMCCSDSMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool OPMCCSDSDataFile::GetCCSDSMetaData(std::string &lff,
                                OPMCCSDSObType *myOb)
{

    // Initialize individual data struct
    // This needs new memory allocation because
    // we are storing pointers to this data
    OPMCCSDSMetaData *myMetaData = new OPMCCSDSMetaData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myMetaData->GetKeywordID(keyword);

        if(myMetaData->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case OPMCCSDSMetaData::CCSDS_OPM_METADATACOMMENTS_ID:
                {
                std::string stemp;
                if (!GetCCSDSComment(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                }
                break;

            case OPMCCSDSMetaData::CCSDS_OPM_OBJECTNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->objectName))
                    return false;
                break;

            case OPMCCSDSMetaData::CCSDS_OPM_OBJECTID_ID:

                if (!GetCCSDSValue(lff,myMetaData->internationalDesignator))
                    return false;
                break;

            case OPMCCSDSMetaData::CCSDS_OPM_CENTERNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrameOrigin))
                    return false;
                break;

            case OPMCCSDSMetaData::CCSDS_OPM_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrame))
                    return false;
                break;

            case OPMCCSDSMetaData::CCSDS_OPM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in MetaData\n");
                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while( requiredCount < requiredNumberMetaDataParameters && !IsEOF());

    if (requiredCount < requiredNumberMetaDataParameters)
    {
        MessageInterface::ShowMessage("Error: MetaData does not contain all required elements! Abort!\n");
        return false;
    }
    else
    {
        myOb->ccsdsMetaData = myMetaData;
        return true;
    }
}
