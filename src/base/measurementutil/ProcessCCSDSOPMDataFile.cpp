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
    DataFile::Initialize();

    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        CCSDSOPMObType *myOPM = new CCSDSOPMObType;

        // Read the first line from file
	std::string line = Trim(ReadLineFromFile());

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
	    line = Trim(ReadLineFromFile());
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
	currentCCSDSMetaData(NULL)
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
    currentCCSDSMetaData(CCSDSOPMdf.currentCCSDSMetaData)
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
    std::string line = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_OPM_VERS.*").FullMatch(line))
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
        CCSDSOPMMetaData *myMetaData = new CCSDSOPMMetaData;

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
	myOPM->ccsdsHeader = currentCCSDSHeader;
        myOPM->ccsdsOPMMetaData = currentCCSDSMetaData;
	return GetCCSDSOPMData(line,myOPM);
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

    //std::string regex = "^" + REGEX_CCSDS_DATE + ")\\s*(" +
    //        REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")\\s*(" +
    //        REGEX_SCINUMBER + ")\\s*(" + REGEX_SCINUMBER + ")$";

    std::string regex = "^EPOCH\\s*=\\s*" + REGEX_SCINUMBER + ")$";
    std::string keyword;
    Real value;
    Integer count;

    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;
        
        CCSDSOPMStateVector *myOPMStateVector = new CCSDSOPMStateVector;

        do
        {

            GetCCSDSKeyValueData(lff,keyword,value);

            switch (myOb->GetKeywordID(keyword))
            {
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_EPOCH_ID:
                    myOPMStateVector->epoch = value;
                    if (!CCSDSTimeTag2A1Date(myOPMStateVector->epoch,
                                             myOb->epoch)) return false;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_X_ID:
                    myOPMStateVector->x = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_Y_ID:
                    myOPMStateVector->y = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_Z_ID:
                    myOPMStateVector->z = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_XDOT_ID:
                    myOPMStateVector->xDot = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_YDOT_ID:
                    myOPMStateVector->yDot = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_STATEVECTOR_ZDOT_ID:
                    myOPMStateVector->zDot = value;
                    count++;
                    break;
                default:
                    return false;
                    break;
            }

            lff = Trim(ReadLineFromFile());
        }
        while ( count < 7 );

        myOb->ccsdsOPMStateVector = myOPMStateVector;
    }

    regex = "^SEMI_MAJOR_AXIS\\s*=\\s*" + REGEX_SCINUMBER + ")$";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;
        
        CCSDSOPMKeplerianElements *myOPMKeplerianElements = 
                                                  new CCSDSOPMKeplerianElements;

        do
        {

            GetCCSDSKeyValueData(lff,keyword,value);

            switch (myOb->GetKeywordID(keyword))
            {
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:
                    myOPMKeplerianElements->semiMajorAxis = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_ECCENTRICITY_ID:
                    myOPMKeplerianElements->eccentricity = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_INCLINATION_ID:
                    myOPMKeplerianElements->inclination = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_RAAN_ID:
                    myOPMKeplerianElements->raan = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:
                    myOPMKeplerianElements->argumentOfPericenter = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_TRUEANOMALY_ID:
                    myOPMKeplerianElements->theAnomaly.Set(
                                         myOPMKeplerianElements->semiMajorAxis,
                                         myOPMKeplerianElements->eccentricity,
                                         value, Anomaly::TA);
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_MEANANOMALY_ID:
                    myOPMKeplerianElements->theAnomaly.Set(
                                                myOPMKeplerianElements->semiMajorAxis,
                                                myOPMKeplerianElements->eccentricity,
                                                value, Anomaly::MA);
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:
                    myOPMKeplerianElements->gravitationalCoefficient = value;
                    count++;
                    break;
                default:
                    return false;
                    break;
            }

            lff = Trim(ReadLineFromFile());
        }
        while ( count < 7 );

        myOb->ccsdsOPMKeplerianElements = myOPMKeplerianElements;

    }
    regex = "^MASS\\s*=\\s*" + REGEX_SCINUMBER + ")$";
    if (pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;

        CCSDSOPMSpacecraftParameters *myOPMSpacecraftParameters =
                                               new CCSDSOPMSpacecraftParameters;

        do
        {

            GetCCSDSKeyValueData(lff,keyword,value);

            switch (myOb->GetKeywordID(keyword))
            {
                case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_MASS_ID:
                    myOPMSpacecraftParameters->mass = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:
                    myOPMSpacecraftParameters->solarRadiationArea = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:
                    myOPMSpacecraftParameters->solarRadiationCoefficient = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGAREA_ID:
                    myOPMSpacecraftParameters->dragArea = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:
                    myOPMSpacecraftParameters->dragCoefficient = value;
                    count++;
                    break;
                default:
                    return false;
                    break;
            }

            lff = Trim(ReadLineFromFile());
        }
        while ( count < 5 );

        myOb->ccsdsOPMSpacecraftParameters = myOPMSpacecraftParameters;
    }

    regex = "^MAN_EPOCH_IGNITION\\s*=\\s*" + REGEX_SCINUMBER + ")$";
    while (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff))
    {
        count = 0;

        CCSDSOPMManeuver *myOPMManeuver = new CCSDSOPMManeuver;

        do
        {

            GetCCSDSKeyValueData(lff,keyword,value);

            switch (myOb->GetKeywordID(keyword))
            {
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_IGNITIONEPOCH_ID:
                    myOPMManeuver->ignitionEpoch = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DURATION_ID:
                    myOPMManeuver->duration = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAMASS_ID:
                    myOPMManeuver->deltaMass = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_REFFRAME_ID:
                    myOPMManeuver->refFrame = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAV1_ID:
                    myOPMManeuver->deltaV1 = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAV2_ID:
                    myOPMManeuver->deltaV2 = value;
                    count++;
                    break;
                case CCSDSOPMObType::CCSDS_OPM_MANUEVER_DELTAV3_ID:
                    myOPMManeuver->deltaV3 = value;
                    count++;
                    break;
                default:
                    return false;
                    break;
            }

            lff = Trim(ReadLineFromFile());
        }
        while ( count < 5 );

        myOb->ccsdsOPMManeuvers.push_back(myOPMManeuver);
    }

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
                                CCSDSOPMMetaData *myMetaData)
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
        else if (pcrecpp::RE("^REF_FRAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    myMetaData->refFrame = stemp;
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