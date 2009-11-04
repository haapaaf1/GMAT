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
    if (!ProcessCCSDSDataFile::Initialize()) return false;

    requiredNumberMetaDataParameters = CountRequiredNumberAEMMetaDataParameters();

    // Test to see if we are reading or writing
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Construct an orbit parameter message obtype
        CCSDSAEMObType *myAEM = new CCSDSAEMObType;

        while (!IsEOF())
        {
            // The GetData function will attempt to populate the
            // AEM obtype variables
            if (GetData(myAEM))
                // Push this data point onto the obtype data stack
                theData.push_back(myAEM);
            else
                delete myAEM;

            // Allocate another struct in memory
            myAEM = new CCSDSAEMObType;
        }

        // Set data iterator to beginning of vector container
        i_theData = theData.begin();

        #ifdef DEBUG_CCSDSAEM_DATA

            fstream *outFile = new fstream;
            outFile->open("aem.output",ios::out);

            // Output to file to make sure all the data is properly stored
            for (ObTypeVector::iterator j=theData.begin(); j!=theData.end(); ++j)
		*outFile << (CCSDSAEMObType*)(*j) << std::endl;

            outFile->close();

        #endif

        MessageInterface::ShowMessage("Completed reading AEM data\n");

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
	currentCCSDSMetaData(NULL),
	lastMetaDataWritten(NULL),
        isMetaDataWritten(false),
        requiredNumberMetaDataParameters(0)
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
    currentCCSDSMetaData(CCSDSAEMdf.currentCCSDSMetaData),
    lastMetaDataWritten(CCSDSAEMdf.lastMetaDataWritten),
    isMetaDataWritten(CCSDSAEMdf.isMetaDataWritten),
    requiredNumberMetaDataParameters(CCSDSAEMdf.requiredNumberMetaDataParameters)
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
    lastMetaDataWritten = CCSDSAEMdf.lastMetaDataWritten;
    isMetaDataWritten = CCSDSAEMdf.isMetaDataWritten;
    requiredNumberMetaDataParameters = CCSDSAEMdf.requiredNumberMetaDataParameters;
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
    std::string line = ReadLineFromFile();

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_AEM_VERS.*").FullMatch(line))
    {
	if (GetCCSDSHeader(line,myAEM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSHeader = myAEM->ccsdsHeader;
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
	if (GetCCSDSMetaData(line,myAEM))
	{
	    // success so set currentHeader pointer to the
	    // one just processed
	    currentCCSDSMetaData = myAEM->ccsdsAEMMetaData;
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
        line = ReadLineFromFile();
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
// bool GetCCSDSAEMData(std::string &lff, CCSDSObType *myOb)
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
            CCSDSAEMQuaternion *myQData = new CCSDSAEMQuaternion;

            if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {  
                std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4))
                {
                    myQData->timeTag = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->timeTag,myOb->epoch)) return false;
                    myQData->qC = dtemp1;
                    myQData->q1 = dtemp2;
                    myQData->q2 = dtemp3;
                    myQData->q3 = dtemp4;
                    myQData->quaternionType = myOb->ccsdsAEMMetaData->quaternionType;
                    myQData->attitudeType = myOb->ccsdsAEMMetaData->attitudeType;

                    myOb->ccsdsAEMQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSHeader::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4))
                {
                    myQData->timeTag = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->timeTag,myOb->epoch)) return false;
                    myQData->q1 = dtemp1;
                    myQData->q2 = dtemp2;
                    myQData->q3 = dtemp3;
                    myQData->qC = dtemp4;
                    myQData->quaternionType = myOb->ccsdsAEMMetaData->quaternionType;
                    myQData->attitudeType = myOb->ccsdsAEMMetaData->attitudeType;

                    myOb->ccsdsAEMQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSHeader::QUATERNION_ID;

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
            CCSDSAEMQuaternion *myQData = new CCSDSAEMQuaternion;

            if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7,&dtemp8))
                {
                    myQData->timeTag = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->timeTag,myOb->epoch)) return false;
                    myQData->qC = dtemp1;
                    myQData->q1 = dtemp2;
                    myQData->q2 = dtemp3;
                    myQData->q3 = dtemp4;
                    myQData->qCDot = dtemp5;
                    myQData->q1Dot = dtemp6;
                    myQData->q2Dot = dtemp7;
                    myQData->q3Dot = dtemp8;
                    myQData->quaternionType = myOb->ccsdsAEMMetaData->quaternionType;
                    myQData->attitudeType = myOb->ccsdsAEMMetaData->attitudeType;

                    myOb->ccsdsAEMQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSHeader::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7,&dtemp8))
                {
                    myQData->timeTag = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->timeTag,myOb->epoch)) return false;
                    myQData->q1 = dtemp1;
                    myQData->q2 = dtemp2;
                    myQData->q3 = dtemp3;
                    myQData->qC = dtemp4;
                    myQData->q1Dot = dtemp5;
                    myQData->q2Dot = dtemp6;
                    myQData->q3Dot = dtemp7;
                    myQData->qCDot = dtemp8;
                    myQData->quaternionType = myOb->ccsdsAEMMetaData->quaternionType;
                    myQData->attitudeType = myOb->ccsdsAEMMetaData->attitudeType;

                    myOb->ccsdsAEMQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSHeader::QUATERNION_ID;

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
            CCSDSAEMQuaternion *myQData = new CCSDSAEMQuaternion;

            if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7))
                {
                    myQData->timeTag = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->timeTag,myOb->epoch)) return false;
                    myQData->qC = dtemp1;
                    myQData->q1 = dtemp2;
                    myQData->q2 = dtemp3;
                    myQData->q3 = dtemp4;
                    myQData->xRate = dtemp5;
                    myQData->yRate = dtemp6;
                    myQData->zRate = dtemp7;
                    myQData->quaternionType = myOb->ccsdsAEMMetaData->quaternionType;
                    myQData->attitudeType = myOb->ccsdsAEMMetaData->attitudeType;

                    myOb->ccsdsAEMQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSHeader::QUATERNION_ID;

                    return true;
                }

                return false;
            }
            else if (myOb->ccsdsAEMMetaData->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")$";

                if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4,&dtemp5,
                                                 &dtemp6,&dtemp7))
                {
                    myQData->timeTag = stemp;
                    if (!CCSDSTimeTag2A1Date(myQData->timeTag,myOb->epoch)) return false;
                    myQData->q1 = dtemp1;
                    myQData->q2 = dtemp2;
                    myQData->q3 = dtemp3;
                    myQData->qC = dtemp4;
                    myQData->xRate = dtemp5;
                    myQData->yRate = dtemp6;
                    myQData->zRate = dtemp7;
                    myQData->quaternionType = myOb->ccsdsAEMMetaData->quaternionType;
                    myQData->attitudeType = myOb->ccsdsAEMMetaData->attitudeType;

                    myOb->ccsdsAEMQuaternion = myQData;
                    myOb->ccsdsHeader->dataType = CCSDSHeader::QUATERNION_ID;

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
            CCSDSAEMEulerAngle *myEulerData = new CCSDSAEMEulerAngle;

            std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")$";


            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                             &dtemp3))
            {
                myEulerData->timeTag = stemp;

                if (!CCSDSTimeTag2A1Date(myEulerData->timeTag,myOb->epoch)) return false;

                myEulerData->xAngle = dtemp1;
                myEulerData->yAngle = dtemp2;
                myEulerData->zAngle = dtemp3;
                myEulerData->eulerAngleType = CCSDSObType::CCSDS_EULER_ANGLE_ID;

                myOb->ccsdsAEMEulerAngle = myEulerData;
                myOb->ccsdsHeader->dataType = CCSDSHeader::EULERANGLE_ID;

                return true;

            }

            return false;
        }

        break;

        case CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID:
        {
            CCSDSAEMEulerAngle *myEulerData = new CCSDSAEMEulerAngle;

            std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";


            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                             &dtemp3,&dtemp4,&dtemp5,
                                             &dtemp6))
            {
                myEulerData->timeTag = stemp;

                if (!CCSDSTimeTag2A1Date(myEulerData->timeTag,myOb->epoch)) return false;

                myEulerData->xAngle = dtemp1;
                myEulerData->yAngle = dtemp2;
                myEulerData->zAngle = dtemp3;
                myEulerData->xRate = dtemp4;
                myEulerData->yRate = dtemp5;
                myEulerData->zRate = dtemp6;
                myEulerData->eulerAngleType = CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID;

                myOb->ccsdsAEMEulerAngle = myEulerData;
                myOb->ccsdsHeader->dataType = CCSDSHeader::EULERANGLE_ID;

                return true;

            }

            return false;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_ID:
        {
            CCSDSAEMSpinStabilized *mySpinData = new CCSDSAEMSpinStabilized;

            std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                    REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";


            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                                 &dtemp3,&dtemp4))
            {
                mySpinData->timeTag = stemp;

                if (!CCSDSTimeTag2A1Date(mySpinData->timeTag,myOb->epoch)) return false;

                mySpinData->spinAlpha = dtemp1;
                mySpinData->spinDelta = dtemp2;
                mySpinData->spinAngle = dtemp3;
                mySpinData->spinAngleVelocity = dtemp4;
                mySpinData->attitudeType = CCSDSObType::CCSDS_SPIN_ID;

                myOb->ccsdsAEMSpinStabilized = mySpinData;
                myOb->ccsdsHeader->dataType = CCSDSHeader::SPINSTABILIZED_ID;

                return true;
            }

            return false;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_NUTATION_ID:
        {
            CCSDSAEMSpinStabilized *mySpinData = new CCSDSAEMSpinStabilized;

            std::string regex = "^(" + REGEX_CCSDS_DATE + ")\\s*(" +
                REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
                REGEX_SCINUMBER + ")$";

            if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp1,&dtemp2,
                                             &dtemp3,&dtemp4,&dtemp5,
                                             &dtemp6,&dtemp7))
            {
                mySpinData->timeTag = stemp;

                if (!CCSDSTimeTag2A1Date(mySpinData->timeTag,myOb->epoch)) return false;

                mySpinData->spinAlpha = dtemp1;
                mySpinData->spinDelta = dtemp2;
                mySpinData->spinAngle = dtemp3;
                mySpinData->spinAngleVelocity = dtemp4;
                mySpinData->nutation = dtemp5;
                mySpinData->nutationPeriod = dtemp6;
                mySpinData->nutationPhase = dtemp7;
                mySpinData->attitudeType = CCSDSObType::CCSDS_SPIN_NUTATION_ID;

                myOb->ccsdsAEMSpinStabilized = mySpinData;
                myOb->ccsdsHeader->dataType = CCSDSHeader::SPINSTABILIZED_ID;

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
// bool GetCCSDSMetaData(std::string &lff, CCSDSAEMObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSAEMDataFile::GetCCSDSMetaData(std::string &lff,
                                CCSDSAEMObType *myOb)
{
    CCSDSAEMMetaData *myMetaData = new CCSDSAEMMetaData;

    Integer requiredCount = 0;
    std::string keyword;

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;

        Integer keyID = myMetaData->GetKeywordID(keyword);

        if(myMetaData->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSAEMMetaData::CCSDS_AEM_METADATACOMMENTS_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp)) return false;
                myMetaData->comments.push_back(stemp);
                }
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_OBJECTNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->objectName))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_OBJECTID_ID:

                if (!GetCCSDSValue(lff,myMetaData->internationalDesignator))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_CENTERNAME_ID:

                if (!GetCCSDSValue(lff,myMetaData->refFrameOrigin))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_TIMESYSTEM_ID:

                if (!GetCCSDSValue(lff,myMetaData->timeSystem))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_STARTEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->startEpoch))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_STOPEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->stopEpoch))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_USEABLE_STARTEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->useableStartEpoch))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_USEABLE_STOPEPOCH_ID:

                if (!GetCCSDSValue(lff,myMetaData->useableStopEpoch))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_INTERPOLATION_ID:

                if (!GetCCSDSValue(lff,myMetaData->interpolationMethod))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_INTERPOLATIONDEGREE_ID:

                if (!GetCCSDSValue(lff,myMetaData->interpolationDegree))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_REFFRAMEA_ID:

                if (!GetCCSDSValue(lff,myMetaData->frameA))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_REFFRAMEB_ID:

                if (!GetCCSDSValue(lff,myMetaData->frameB))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_ATTITUDEDIR_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                    return false;
                myMetaData->direction = GetAttitudeDirID(stemp);
                }
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_ATTITUDETYPE_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                    return false;
                myMetaData->attitudeType = GetAttitudeTypeID(stemp);
                }
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID:
                {
                if (myMetaData->attitudeType != CCSDSObType::CCSDS_QUATERNION_ID
                 || myMetaData->attitudeType != CCSDSObType::CCSDS_QUATERNION_DERIVATIVE_ID
                 || myMetaData->attitudeType != CCSDSObType::CCSDS_QUATERNION_RATE_ID)
                    return false;
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                    return false;
                myMetaData->quaternionType = GetQuaternionTypeID(stemp);
                }
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_EULERROTSEQ_ID:

                if (myMetaData->attitudeType != CCSDSObType::CCSDS_EULER_ANGLE_ID
                 || myMetaData->attitudeType != CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID )
                    return false;
                if (!GetCCSDSValue(lff,myMetaData->eulerRotationSequence))
                    return false;
                break;

            case CCSDSAEMMetaData::CCSDS_AEM_RATEFRAME_ID:
                {
                std::string stemp;
                if (!GetCCSDSValue(lff,stemp))
                    return false;
                myMetaData->rateFrame = GetRateFrameID(stemp);
                }
                break;

            default:

                return false;
                break;

        }

        lff = ReadLineFromFile();
    }
    while(requiredCount < requiredNumberMetaDataParameters ||
          pcrecpp::RE("^DATA_START.*$").FullMatch(lff));

    myOb->ccsdsAEMMetaData = myMetaData;

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