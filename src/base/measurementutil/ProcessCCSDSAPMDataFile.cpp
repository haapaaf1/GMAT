//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSAPMDataFile
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

#include <ProcessCCSDSAPMDataFile.hpp>

//#define DEBUG_CCSDSAPM_DATA

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
bool ProcessCCSDSAPMDataFile::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSAPMObType *myAPM = new CCSDSAPMObType;

        // Read the first line from file
	std::string line = Trim(ReadLineFromFile());

        while (!IsEOF())
        {
            if (line != "")
            {
                // Now check for headers and process data accordingly
                if (GetData(myAPM))
                {
                    // Push this data point onto the stack.
                    theData.push_back(myAPM);
                }
                else
                {
                    delete myAPM;
                }

                // Allocate another struct in memory
                myAPM = new CCSDSAPMObType;
            }

	    // Read a line from file
            // After grabbing the header and metadata information
            // This call to read a line from file should be grabbing
            // rows of data between DATA_START and DATA_STOP
	    line = Trim(ReadLineFromFile());
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSAPM_DATA

            fstream *outFile = new fstream;
            outFile->open("APM.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile << (CCSDSAPMObType*)(*j) << std::endl;
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
//  ProcessCCSDSAPMDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSAPMDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSAPMDataFile::ProcessCCSDSAPMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSAPMDataFile", itsName),
	currentCCSDSMetaData(NULL)
{
   objectTypeNames.push_back("CCSDSAPMDataFile");
   fileFormatName = "CCSDSAPM";
   fileFormatID = 10;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSAPMDataFile::ProcessCCSDSAPMDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSAPMDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSAPMDataFile::ProcessCCSDSAPMDataFile(const ProcessCCSDSAPMDataFile &CCSDSAPMdf) :
    ProcessCCSDSDataFile(CCSDSAPMdf),
    currentCCSDSMetaData(CCSDSAPMdf.currentCCSDSMetaData)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSAPMDataFile::ProcessCCSDSAPMDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSAPMDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSAPMDataFile& ProcessCCSDSAPMDataFile::operator=(const ProcessCCSDSAPMDataFile &CCSDSAPMdf)
{
    if (&CCSDSAPMdf == this)
	return *this;

    ProcessCCSDSDataFile::operator=(CCSDSAPMdf);
    currentCCSDSMetaData = CCSDSAPMdf.currentCCSDSMetaData;
    return *this;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSAPMDataFile::~ProcessCCSDSAPMDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSAPMDataFile::~ProcessCCSDSAPMDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAPMDataFile.
 *
 * @return clone of the ProcessCCSDSAPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSAPMDataFile::Clone() const
{
   GmatBase *clone = new ProcessCCSDSAPMDataFile(*this);
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
bool ProcessCCSDSAPMDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSAPMDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetCCSDSData(ObType *myAPM)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of APM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetData(ObType *myAPMData)
{

    if (myAPMData->GetTypeName() != "CCSDSAPMObType") return false;

    // Re-cast the generic ObType pointer as a CCSDSAPMObtype pointer
    CCSDSAPMObType *myAPM = (CCSDSAPMObType*)myAPMData;

    // Read the first line from file
    std::string lff = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_APM_VERS.*").FullMatch(lff))
    {
	if (GetCCSDSHeader(lff,myAPM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSHeader = myAPM->ccsdsHeader;
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
    while (!IsEOF() && pcrecpp::RE("^META_START.*").FullMatch(lff))
    {
        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        CCSDSAPMMetaData *myMetaData = new CCSDSAPMMetaData;

	// Read the next metadata line from file
	lff = Trim(ReadLineFromFile());

	if (GetCCSDSMetaData(lff,myMetaData))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetaData = myMetaData;

	    // Read the following data line from file
	    lff = Trim(ReadLineFromFile());
	}
	else
	{
	    // failure to read header line, abort
	    currentCCSDSMetaData = NULL;
	    return false;
	}
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
	myAPM->ccsdsHeader = currentCCSDSHeader;
        myAPM->ccsdsAPMMetaData = currentCCSDSMetaData;
	if (GetCCSDSAPMData(lff,myAPM))
        {
            if (commentsFound)
                myAPM->ccsdsAPMQuaternion->comments = comments;
            return true;
        }
        else
            return false;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSAPMData(std::string &lff, CCSDSAPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the attitude parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSAPMData(std::string &lff,
                                              CCSDSAPMObType *myOb)
{
    
    // Container for any comments found
    StringArray comments;

    bool commentsFound = GetCCSDSComments(lff,comments);

    std::string regex = "^EPOCH\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if (GetCCSDSAPMQuaternion(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsAPMQuaternion->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^EULER_FRAME_A\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if(GetCCSDSAPMEulerAngle(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsAPMEulerAngle->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^SPIN_FRAME_A\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if(GetCCSDSAPMSpinStabilized(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsAPMSpinStabilized->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^INERTIA_REF_FRAME\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        if(GetCCSDSAPMSpacecraftInertia(lff,myOb))
        {
            if (commentsFound)
            {
                myOb->ccsdsAPMSpacecraftInertia->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
        else return false;
    }

    regex = "^MAN_EPOCH_START\\s*=.*";

    myOb->i_ccsdsAPMAttitudeManeuvers = myOb->ccsdsAPMAttitudeManeuvers.begin();
    
    while (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {
        if (GetCCSDSAPMAttitudeManeuver(lff,myOb))
        {
            myOb->i_ccsdsAPMAttitudeManeuvers++;
            if (commentsFound)
            {
                (*myOb->i_ccsdsAPMAttitudeManeuvers)->comments = comments;
                comments.clear();
            }
            commentsFound = GetCCSDSComments(lff,comments);
        }
    }

    // Reset iterator to begining again
    myOb->i_ccsdsAPMAttitudeManeuvers = myOb->ccsdsAPMAttitudeManeuvers.begin();

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSAPMQuaternion(std::string &lff, CCSDSAPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the quaternion information from the attitude parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSAPMQuaternion(std::string &lff,
                                                    CCSDSAPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSAPMQuaternion *myAPMQuaternion = new CCSDSAPMQuaternion;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_EPOCH_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->epoch)) return false;
                if (!CCSDSTimeTag2A1Date(myAPMQuaternion->epoch,
                                         myOb->epoch)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_FRAMEA_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMQuaternion->frameA = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_FRAMEB_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMQuaternion->frameB = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_DIRECTION_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMQuaternion->direction = GetAttitudeDirID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_Q1_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->q1)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_Q2_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->q2)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_Q3_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->q3)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_QC_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->qC)) return false;
                count++;
                break;

            default:

                return false;
                break;

        }

        lff = Trim(ReadLineFromFile());

    }
    while ( count < 8 );

    myAPMQuaternion->quaternionType = CCSDSObType::CCSDS_QUATERNION_ID;

    std::string regex = "^Q1_DOT\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;

        do
        {

            if (!GetCCSDSKeyword(lff,keyword)) return false;

            switch (myOb->GetKeywordID(keyword))
            {

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_Q1DOT_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->q1Dot)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_Q2DOT_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->q2Dot)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_Q3DOT_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->q3Dot)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_QUATERNION_QCDOT_ID:

                if (!GetCCSDSValue(lff,myAPMQuaternion->qCDot)) return false;
                count++;
                break;

            default:

                return false;
                break;
            }

            lff = Trim(ReadLineFromFile());
        
        }
        while ( count < 4 );

        myAPMQuaternion->quaternionType = CCSDSObType::CCSDS_QUATERNION_DERIVATIVE_ID;

    }

    /*
    regex = "^X_RATE\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;

        do
        {

            if (!GetCCSDSKeyword(lff,keyword)) return false;

            switch (myOb->GetKeywordID(keyword))
            {
                case CCSDSAPMObType::CCSDS_APM_QUATERNION_XRATE_ID:

                    if (!GetCCSDSValue(lff,myAPMQuaternion->xRate)) return false;
                    count++;
                    break;

                case CCSDSAPMObType::CCSDS_APM_QUATERNION_YRATE_ID:

                    if (!GetCCSDSValue(lff,myAPMQuaternion->yRate)) return false;
                    count++;
                    break;

                case CCSDSAPMObType::CCSDS_APM_QUATERNION_ZRATE_ID:

                    if (!GetCCSDSValue(lff,myAPMQuaternion->zRate)) return false;
                    count++;
                    break;

                default:

                    return false;
                    break;
            }

            lff = Trim(ReadLineFromFile());

        }
        while ( count < 3 );

        myAPMQuaternion->quaternionType = CCSDSObType::CCSDS_QUATERNION_RATE_ID;

    }
    */

    myOb->ccsdsAPMQuaternion = myAPMQuaternion;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSAPMEulerAngle(std::string &lff, CCSDSAPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the Euler angle information from the attitude parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSAPMEulerAngle(std::string &lff,
                                                    CCSDSAPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSAPMEulerAngle *myAPMEulerAngle = new CCSDSAPMEulerAngle;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_FRAMEA_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMEulerAngle->frameA = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_FRAMEB_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMEulerAngle->frameB = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_DIRECTION_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMEulerAngle->direction = GetAttitudeDirID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_ROTATIONSEQUENCE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->rotationSequence)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_RATEFRAME_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMEulerAngle->rateFrame = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_XANGLE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->xAngle)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_YANGLE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->yAngle)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_ZANGLE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->zAngle)) return false;
                count++;
                break;

            default:

                return false;
                break;

        }

        lff = Trim(ReadLineFromFile());

    }
    while ( count < 4 );

    myAPMEulerAngle->eulerAngleType = CCSDSObType::CCSDS_EULER_ANGLE_ID;

    std::string regex = "^X_RATE\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;

        do
        {

            if (!GetCCSDSKeyword(lff,keyword)) return false;

            switch (myOb->GetKeywordID(keyword))
            {

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_XRATE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->xRate)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_YRATE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->yRate)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_EULERANGLE_ZRATE_ID:

                if (!GetCCSDSValue(lff,myAPMEulerAngle->zRate)) return false;
                count++;
                break;

            default:

                return false;
                break;
            }

        lff = Trim(ReadLineFromFile());

        }
        while ( count < 3 );

        myAPMEulerAngle->eulerAngleType = CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID;


    }

    myOb->ccsdsAPMEulerAngle = myAPMEulerAngle;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSAPMSpinStabilized(std::string &lff, CCSDSAPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the spin stabilized information from the attitude parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSAPMSpinStabilized(std::string &lff,
                                                        CCSDSAPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSAPMSpinStabilized *myAPMSpinStabilized = new CCSDSAPMSpinStabilized;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_FRAMEA_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMSpinStabilized->frameA = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_FRAMEB_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMSpinStabilized->frameB = GetRateFrameID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_DIRECTION_ID:
                {
                std::string svalue;
                if (!GetCCSDSValue(lff,svalue)) return false;
                myAPMSpinStabilized->direction = GetAttitudeDirID(svalue);
                count++;
                }
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_SPINALPHA_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->spinAlpha)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_SPINDELTA_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->spinDelta)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_SPINANGLE_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->spinAngle)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_SPINANGLEVEOCITY_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->spinAngleVelocity)) return false;
                count++;
                break;

            default:

                return false;
                break;

        }

        lff = Trim(ReadLineFromFile());

    }
    while ( count < 7 );

    myAPMSpinStabilized->attitudeType = CCSDSObType::CCSDS_SPIN_ID;

    std::string regex = "^NUTATION\\s*=.*";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;

        do
        {

            if (!GetCCSDSKeyword(lff,keyword)) return false;

            switch (myOb->GetKeywordID(keyword))
            {

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_NUTATION_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->nutation)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_NUTATIONPERIOD_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->nutationPeriod)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPINSTABILIZED_NUTATIONPHASE_ID:

                if (!GetCCSDSValue(lff,myAPMSpinStabilized->nutationPhase)) return false;
                count++;
                break;

            default:

                return false;
                break;
            }

            lff = Trim(ReadLineFromFile());

        }
        while ( count < 3 );

        myAPMSpinStabilized->attitudeType = CCSDSObType::CCSDS_SPIN_NUTATION_ID;

    }

    myOb->ccsdsAPMSpinStabilized = myAPMSpinStabilized;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSAPMSpacecraftInertia(std::string &lff, CCSDSAPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the spacecraft inertia information from the attitude parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSAPMSpacecraftInertia(std::string &lff,
                                                           CCSDSAPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSAPMSpacecraftInertia *myAPMSpacecraftInertia = new CCSDSAPMSpacecraftInertia;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_INERTIAREFFRAME_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->inertiaRefFrame)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_I11_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->i11)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_I22_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->i22)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_I33_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->i33)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_I12_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->i12)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_I13_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->i13)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_SPACECRAFTINERTIA_I23_ID:

                if (!GetCCSDSValue(lff,myAPMSpacecraftInertia->i23)) return false;
                count++;
                break;

            default:

                return false;
                break;

        }

        lff = Trim(ReadLineFromFile());

    }
    while ( count < 7 );

    myOb->ccsdsAPMSpacecraftInertia = myAPMSpacecraftInertia;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSAPMAttitudeManeuver(std::string &lff, CCSDSAPMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the maneuver information from the attitude parameter message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSAPMAttitudeManeuver(std::string &lff,
                                                          CCSDSAPMObType *myOb)
{

    Integer count = 0;
    std::string keyword;

    CCSDSAPMAttitudeManeuver *myAPMManeuver = new CCSDSAPMAttitudeManeuver;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        switch (myOb->GetKeywordID(keyword))
        {
            case CCSDSAPMObType::CCSDS_APM_ATTITUDEMANUEVER_EPOCHSTART_ID:

                if (!GetCCSDSValue(lff,myAPMManeuver->epochStart)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_ATTITUDEMANUEVER_DURATION_ID:

                if (!GetCCSDSValue(lff,myAPMManeuver->duration)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_ATTITUDEMANUEVER_REFFRAME_ID:

                if (!GetCCSDSValue(lff,myAPMManeuver->refFrame)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_ATTITUDEMANUEVER_TOR1_ID:

                if (!GetCCSDSValue(lff,myAPMManeuver->tor1)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_ATTITUDEMANUEVER_TOR2_ID:

                if (!GetCCSDSValue(lff,myAPMManeuver->tor2)) return false;
                count++;
                break;

            case CCSDSAPMObType::CCSDS_APM_ATTITUDEMANUEVER_TOR3_ID:

                if (!GetCCSDSValue(lff,myAPMManeuver->tor3)) return false;
                count++;
                break;

            default:
                return false;
                break;

        }

        lff = Trim(ReadLineFromFile());

    }
    while ( count < 6 );

    myOb->ccsdsAPMAttitudeManeuvers.push_back(myAPMManeuver);

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff,
//                       CCSDSAPMMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::GetCCSDSMetaData(std::string &lff,
                                CCSDSAPMMetaData *myMetaData)
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
// bool WriteData(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes a CCSDS orbit ephemeris message to file
 *
 * @param <myAPM> the APM data to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSAPMDataFile::WriteData(const ObType *myOb)
{
    if (GetTypeName() != "CCSDSAPMObType") return false;

    CCSDSAPMObType *theAPM = (CCSDSAPMObType*)myOb;
    WriteDataHeader(theAPM);
    WriteMetaData(theAPM);
    *theFile << theAPM;
    return true;
}