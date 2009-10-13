//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSAEMDataFile
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

#include <ProcessCCSDSAEMDataFile.hpp>

//#define DEBUG_CCSDSAEM_DATA
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
bool ProcessCCSDSAEMDataFile::Initialize()
{
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSAEMObType *myAEM = new CCSDSAEMObType;

        // Read the first line from file
	std::string line = Trim(ReadLineFromFile());

        while (!IsEOF())
        {
            if (line != "")
            {
                // Now check for headers and process data accordingly
                if (GetData(myAEM))
                {
                    // Push this data point onto the stack.
                    theData.push_back(myAEM);
                }
                else
                {
                    delete myAEM;
                }

                // Allocate another struct in memory
                myAEM = new CCSDSAEMObType;
            }

	    // Read a line from file
            // After grabbing the header and metadata information
            // This call to read a line from file should be grabbing
            // rows of data between DATA_START and DATA_STOP
	    line = Trim(ReadLineFromFile());
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSAEM_DATA

            fstream *outFile = new fstream;
            outFile->open("AEM.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
            {
		*outFile << (CCSDSAEMObType*)(*j) << std::endl;
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
//  ProcessCCSDSAEMDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSAEMDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSAEMDataFile::ProcessCCSDSAEMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSAEMDataFile", itsName),
	currentCCSDSMetaData(NULL)
{
   objectTypeNames.push_back("CCSDSAEMDataFile");
   fileFormatName = "CCSDSAEM";
   fileFormatID = 11;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSAEMDataFile::ProcessCCSDSAEMDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSAEMDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSAEMDataFile::ProcessCCSDSAEMDataFile(const ProcessCCSDSAEMDataFile &CCSDSAEMdf) :
    ProcessCCSDSDataFile(CCSDSAEMdf),
    currentCCSDSMetaData(CCSDSAEMdf.currentCCSDSMetaData)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSAEMDataFile::ProcessCCSDSAEMDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSAEMDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSAEMDataFile& ProcessCCSDSAEMDataFile::operator=(const ProcessCCSDSAEMDataFile &CCSDSAEMdf)
{
    if (&CCSDSAEMdf == this)
	return *this;

    ProcessCCSDSDataFile::operator=(CCSDSAEMdf);
    currentCCSDSMetaData = CCSDSAEMdf.currentCCSDSMetaData;
    return *this;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSAEMDataFile::~ProcessCCSDSAEMDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSAEMDataFile::~ProcessCCSDSAEMDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAEMDataFile.
 *
 * @return clone of the ProcessCCSDSAEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSAEMDataFile::Clone() const
{
   GmatBase *clone = new ProcessCCSDSAEMDataFile(*this);
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
bool ProcessCCSDSAEMDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSAEMDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetData(ObType *myAEM)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of AEM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSAEMDataFile::GetData(ObType *myAEMData)
{

    if (myAEMData->GetTypeName() != "CCSDSAEMObType") return false;

    // Re-cast the generic ObType pointer as a CCSDSAEMObtype pointer
    CCSDSAEMObType *myAEM = (CCSDSAEMObType*)myAEMData;

    // Read the first line from file
    std::string line = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_AEM_VERS.*").FullMatch(line))
    {
        // Initialize the header data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        CCSDSHeader *myCCSDSHeader = new CCSDSHeader;

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
        CCSDSAEMMetaData *myMetaData = new CCSDSAEMMetaData;

	// Read the next metadata line from file
	line = Trim(ReadLineFromFile());

	if (GetCCSDSMetaData(line,myMetaData))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetaData = myMetaData;

	    // Read the following data line from file
	    line = Trim(ReadLineFromFile());
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
    if (pcrecpp::RE("^DATA_START.*").FullMatch(line))
    {
        line = Trim(ReadLineFromFile());
    }

    // Parse the data record making sure that we have identified
    // a header record and a metadata record previously
    if (currentCCSDSHeader == NULL || currentCCSDSMetaData == NULL)
	return false;

    if (!pcrecpp::RE("^DATA_STOP.*").FullMatch(line) && !pcrecpp::RE("").FullMatch(line))
    {
	myAEM->ccsdsHeader = currentCCSDSHeader;
        myAEM->ccsdsAEMMetaData = currentCCSDSMetaData;
	return GetCCSDSAEMData(line,myAEM);
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSOEMData(std::string &lff, CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the orbit ephemeris message.
 *
 * @param <lff> Line from file
 * @param <myOb> Pointer to AEM data
 * @return Boolean success or failure
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAEMDataFile::GetCCSDSAEMData(std::string &lff,
                                              CCSDSAEMObType *myOb)
{

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp1, dtemp2, dtemp3, dtemp4, dtemp5, dtemp6, dtemp7, dtemp8;
    std::string stemp;

    switch (myOb->ccsdsAEMMetaData->attitudeType)
    {
        case CCSDSObType::CCSDS_QUATERNION_ID:
        {
            CCSDSQuaternion *myQData = new CCSDSQuaternion;

            if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSObType::CCSDS_QUATERNION_FIRST_ID)
            {
                std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4))
                {
                    myQData->epoch = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->epoch,myOb->epoch)) return false;
                    myQData->qC = dtemp1;
                    myQData->q1 = dtemp2;
                    myQData->q2 = dtemp3;
                    myQData->q3 = dtemp4;

                    myOb->ccsdsQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSObType::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSObType::CCSDS_QUATERNION_LAST_ID)
            {
                std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4))
                {
                    myQData->epoch = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->epoch,myOb->epoch)) return false;
                    myQData->q1 = dtemp1;
                    myQData->q2 = dtemp2;
                    myQData->q3 = dtemp3;
                    myQData->qC = dtemp4;

                    myOb->ccsdsQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSObType::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else
                return false;
        }

        break;

        case CCSDSObType::CCSDS_QUATERNION_DERIVATIVE_ID:
        {
            CCSDSQuaternion *myQData = new CCSDSQuaternion;

            if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSObType::CCSDS_QUATERNION_FIRST_ID)
            {
                std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7,&dtemp8))
                {
                    myQData->epoch = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->epoch,myOb->epoch)) return false;
                    myQData->qC = dtemp1;
                    myQData->q1 = dtemp2;
                    myQData->q2 = dtemp3;
                    myQData->q3 = dtemp4;
                    myQData->qCDot = dtemp5;
                    myQData->q1Dot = dtemp6;
                    myQData->q2Dot = dtemp7;
                    myQData->q3Dot = dtemp8;

                    myOb->ccsdsQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSObType::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSObType::CCSDS_QUATERNION_LAST_ID)
            {
                std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7,&dtemp8))
                {
                    myQData->epoch = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->epoch,myOb->epoch)) return false;
                    myQData->q1 = dtemp1;
                    myQData->q2 = dtemp2;
                    myQData->q3 = dtemp3;
                    myQData->qC = dtemp4;
                    myQData->q1Dot = dtemp5;
                    myQData->q2Dot = dtemp6;
                    myQData->q3Dot = dtemp7;
                    myQData->qCDot = dtemp8;

                    myOb->ccsdsQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSObType::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else
                return false;
        }

        break;

        case CCSDSObType::CCSDS_QUATERNION_RATE_ID:
        {
            CCSDSQuaternion *myQData = new CCSDSQuaternion;

            if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSObType::CCSDS_QUATERNION_FIRST_ID)
            {
                std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7))
                {
                    myQData->epoch = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->epoch,myOb->epoch)) return false;
                    myQData->qC = dtemp1;
                    myQData->q1 = dtemp2;
                    myQData->q2 = dtemp3;
                    myQData->q3 = dtemp4;
                    myQData->xRate = dtemp5;
                    myQData->yRate = dtemp6;
                    myQData->zRate = dtemp7;

                    myOb->ccsdsQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSObType::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSObType::CCSDS_QUATERNION_LAST_ID)
            {
                std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7))
                {
                    myQData->epoch = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->epoch,myOb->epoch)) return false;
                    myQData->q1 = dtemp1;
                    myQData->q2 = dtemp2;
                    myQData->q3 = dtemp3;
                    myQData->qC = dtemp4;
                    myQData->xRate = dtemp5;
                    myQData->yRate = dtemp6;
                    myQData->zRate = dtemp7;

                    myOb->ccsdsQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSObType::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else
                return false;
        }

        break;

        case CCSDSObType::CCSDS_EULER_ANGLE_ID:
        {
            CCSDSEulerAngle *myEulerData = new CCSDSEulerAngle;

            std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")$";


            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                             &dtemp3))
            {
                myEulerData->epoch = stemp;

                if (!CCSDSTimeTag2A1Date(myEulerData->epoch,myOb->epoch)) return false;

                myEulerData->xAngle = dtemp1;
                myEulerData->yAngle = dtemp2;
                myEulerData->zAngle = dtemp3;

                myOb->ccsdsEulerAngle = myEulerData;
                myOb->ccsdsHeader->dataType = CCSDSObType::EULERANGLE_ID;

                return true;

            }

            return false;
        }

        break;

        case CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID:
        {
            CCSDSEulerAngle *myEulerData = new CCSDSEulerAngle;

            std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";


            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                             &dtemp3,&dtemp4,&dtemp5,
                                             &dtemp6))
            {
                myEulerData->epoch = stemp;

                if (!CCSDSTimeTag2A1Date(myEulerData->epoch,myOb->epoch)) return false;

                myEulerData->xAngle = dtemp1;
                myEulerData->yAngle = dtemp2;
                myEulerData->zAngle = dtemp3;
                myEulerData->xRate = dtemp4;
                myEulerData->yRate = dtemp5;
                myEulerData->zRate = dtemp6;

                myOb->ccsdsEulerAngle = myEulerData;
                myOb->ccsdsHeader->dataType = CCSDSObType::EULERANGLE_ID;

                return true;

            }

            return false;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_ID:
        {
            CCSDSSpinStabilized *mySpinData = new CCSDSSpinStabilized;

            std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";


            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4))
            {
                mySpinData->epoch = stemp;

                if (!CCSDSTimeTag2A1Date(mySpinData->epoch,myOb->epoch)) return false;

                mySpinData->spinAlpha = dtemp1;
                mySpinData->spinDelta = dtemp2;
                mySpinData->spinAngle = dtemp3;
                mySpinData->spinAngleVelocity = dtemp4;

                myOb->ccsdsSpinStabilized = mySpinData;
                myOb->ccsdsHeader->dataType = CCSDSObType::SPINSTABILIZED_ID;

                return true;
            }

            return false;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_NUTATION_ID:
        {
            CCSDSSpinStabilized *mySpinData = new CCSDSSpinStabilized;

            std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
                REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                REGEX_SCINUMBER + ")$";

            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                             &dtemp3,&dtemp4,&dtemp5,
                                             &dtemp6,&dtemp7))
            {
                mySpinData->epoch = stemp;

                if (!CCSDSTimeTag2A1Date(mySpinData->epoch,myOb->epoch)) return false;

                mySpinData->spinAlpha = dtemp1;
                mySpinData->spinDelta = dtemp2;
                mySpinData->spinAngle = dtemp3;
                mySpinData->spinAngleVelocity = dtemp4;
                mySpinData->nutation = dtemp5;
                mySpinData->nutationPeriod = dtemp6;
                mySpinData->nutationPhase = dtemp7;

                myOb->ccsdsSpinStabilized = mySpinData;
                myOb->ccsdsHeader->dataType = CCSDSObType::SPINSTABILIZED_ID;

                return true;
            }

            return false;
        }

        break;

        default:

            return false;

            break;

    }

}

//------------------------------------------------------------------------------
// bool GetCCSDSMetaData(std::string &lff,
//                       CCSDSAEMMetaData *myMetaData)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAEMDataFile::GetCCSDSMetaData(std::string &lff,
                                CCSDSAEMMetaData *myMetaData)
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
        else if (pcrecpp::RE("^REF_FRAME_A\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->frameA = stemp;
        }
        else if (pcrecpp::RE("^REF_FRAME_B\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->frameB = stemp;
        }
        else if (pcrecpp::RE("^ATTITUDE_DIR\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->direction = GetAttitudeDirID(stemp);
        }
        else if (pcrecpp::RE("^TIME_SYSTEM\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->timeSystem = stemp;
        }
        else if (pcrecpp::RE("^START_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->startEpoch = stemp;
        }
        else if (pcrecpp::RE("^STOP_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->stopEpoch = stemp;
        }
        else if (pcrecpp::RE("^USEABLE_START_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->useableStartEpoch = stemp;
        }
        else if (pcrecpp::RE("^USEABLE_STOP_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->useableStopEpoch = stemp;
        }
        else if (pcrecpp::RE("^ATTITUDE_TYPE\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->attitudeType = GetAttitudeTypeID(stemp);
        }
        else if (pcrecpp::RE("^QUATERNION_TYPE\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->quaternionType = GetQuaternionTypeID(stemp);
        }
        else if (pcrecpp::RE("^EULER_ROT_SEQ\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->eulerRotationSequence = stemp;
        }
        else if (pcrecpp::RE("^RATE_FRAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->rateFrame = GetRateFrameID(stemp);
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


//------------------------------------------------------------------------------
// bool WriteData(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes a CCSDS orbit ephemeris message to file
 *
 * @param <myAEM> the AEM data to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSAEMDataFile::WriteData(const ObType *myOb)
{
    if (myOb->GetTypeName() != "CCSDSAEMObType") return false;

    CCSDSAEMObType *theAEM = (CCSDSAEMObType*)myOb;
    WriteDataHeader(theAEM);
    WriteMetaData(theAEM);
    *theFile << theAEM;
    return true;
}