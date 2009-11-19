#include "ProcessCCSDSDataFile.hpp"

//------------------------------------------------------------------------------
//  static data
//------------------------------------------------------------------------------

const std::string ProcessCCSDSDataFile::REGEX_CCSDS_DATE = MANDATORY_DIGITS + "-"
                             + MANDATORY_DIGITS + "[-]?" + OPTIONAL_DIGITS + "T"
                             + MANDATORY_DIGITS + ":" + MANDATORY_DIGITS + ":"
                             + REGEX_NUMBER + "[Z]?";
const std::string ProcessCCSDSDataFile::REGEX_CCSDS_DATE1 = MANDATORY_DIGITS + "-"
                             + MANDATORY_DIGITS + "T" + MANDATORY_DIGITS
                             + ":" + MANDATORY_DIGITS + ":"
                             + REGEX_NUMBER + "[Z]?";
const std::string ProcessCCSDSDataFile::REGEX_CCSDS_DATE2 = MANDATORY_DIGITS + "-"
                             + MANDATORY_DIGITS + "-" + MANDATORY_DIGITS
                             + "T" + MANDATORY_DIGITS + ":"
                             + MANDATORY_DIGITS + ":" + REGEX_NUMBER + "[Z]?";
const std::string ProcessCCSDSDataFile::REGEX_CCSDS_SAVETHEDATE1 = "(" + MANDATORY_DIGITS + ")-("
                             + MANDATORY_DIGITS + ")T(" + MANDATORY_DIGITS 
                             + "):(" + MANDATORY_DIGITS + "):("
                             + REGEX_NUMBER + ")([Z]?)";
const std::string ProcessCCSDSDataFile::REGEX_CCSDS_SAVETHEDATE2 = "(" + MANDATORY_DIGITS + ")-("
                             + MANDATORY_DIGITS + ")-(" + MANDATORY_DIGITS
                             + ")T(" + MANDATORY_DIGITS + "):("
                             + MANDATORY_DIGITS + "):(" + REGEX_NUMBER + ")([Z]?)";
const std::string ProcessCCSDSDataFile::REGEX_CCSDS_KEYWORD = "[\\w_]*";
const std::string ProcessCCSDSDataFile::REGEX_CCSDS_SAVETHEKEYWORD = "([\\w_]*)";

const std::string ProcessCCSDSDataFile::CCSDS_TIME_SYSTEM_REPS[EndCCSDSTimeSystemReps-8] =
{
    "TAI",
    "UTC",
    "UT1",
    "TDB",
    "TCB",
    "TT",
    "GMST",
    "GPS",
    "SCLK"
};

const std::string ProcessCCSDSDataFile::CCSDS_TIME_SYSTEM_DESCRIPTIONS[EndCCSDSTimeSystemReps-8] =
{
    "International Atomic Time",
    "Coordinated Universal Time",
    "Universal Time",
    "Barycentric Dynamical Time",
    "Barycentric Coordinated Time",
    "Terrestrial Time",
    "Greenwich Mean Sidereal Time",
    "Global Positioning System",
    "Spacecraft Clock (receiver)"
};

const std::string ProcessCCSDSDataFile::CCSDS_REF_FRAME_REPS[EndCCSDSRefFrameReps] =
{
    "EME2000",
    "ICRF",
    "ITRF2000",
    "ITRF-93",
    "ITRF-97",
    "TOD",
    "TRD",
    "GRC"
};

const std::string ProcessCCSDSDataFile::CCSDS_REF_FRAME_DESCRIPTIONS[EndCCSDSRefFrameReps] =
{
    "Earth Mean Equator and Equinox of J2000",
    "International Celestial Reference Frame",
    "Interantional Terrestrial Reference Frame 2000",
    "Interantional Terrestrial Reference Frame 1993",
    "Interantional Terrestrial Reference Frame 1997",
    "True of Date",
    "True of Date Rotating",
    "Greenwich Rotating Coordinate Frame"

};

//------------------------------------------------------------------------------
//  ProcessCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSDataFile::ProcessCCSDSDataFile(const std::string &itsType, 
			  	   const std::string &itsName) :
	DataFile (itsType, itsName),
	currentCCSDSHeader(NULL),
	lastHeaderWritten(NULL),
        isHeaderWritten(false),
        requiredNumberHeaderParameters(0),
        currentCCSDSMetaData(NULL),
        lastMetaDataWritten(NULL),
        isMetaDataWritten(false),
        requiredNumberMetaDataParameters(0),
        writingDataBlock(false)
{
    commentsAllowed = true;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSDataFile::ProcessCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Copy constructor for ProcessCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
ProcessCCSDSDataFile::ProcessCCSDSDataFile(const ProcessCCSDSDataFile &CCSDSdf) :
    DataFile      (CCSDSdf),
    currentCCSDSHeader(CCSDSdf.currentCCSDSHeader),
    lastHeaderWritten(CCSDSdf.lastHeaderWritten),
    isHeaderWritten(CCSDSdf.isHeaderWritten),
    requiredNumberHeaderParameters(CCSDSdf.requiredNumberHeaderParameters),
    currentCCSDSMetaData(CCSDSdf.currentCCSDSMetaData),
    lastMetaDataWritten(CCSDSdf.lastMetaDataWritten),
    isMetaDataWritten(CCSDSdf.isMetaDataWritten),
    requiredNumberMetaDataParameters(CCSDSdf.requiredNumberMetaDataParameters),
    writingDataBlock(CCSDSdf.writingDataBlock)
{
}


//------------------------------------------------------------------------------
//  ProcessCCSDSDataFile::ProcessCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Operator = constructor for ProcessCCSDSDataFile objects
 */
//------------------------------------------------------------------------------
const ProcessCCSDSDataFile& ProcessCCSDSDataFile::operator=(const ProcessCCSDSDataFile &CCSDSdf)
{
    if (&CCSDSdf == this)
	return *this;

    DataFile::operator=(CCSDSdf);

    currentCCSDSHeader = CCSDSdf.currentCCSDSHeader;
    lastHeaderWritten = CCSDSdf.lastHeaderWritten;
    isHeaderWritten = CCSDSdf.isHeaderWritten;
    requiredNumberHeaderParameters = CCSDSdf.requiredNumberHeaderParameters;
    currentCCSDSMetaData = CCSDSdf.currentCCSDSMetaData;
    lastMetaDataWritten = CCSDSdf.lastMetaDataWritten;
    isMetaDataWritten = CCSDSdf.isMetaDataWritten;
    requiredNumberMetaDataParameters = CCSDSdf.requiredNumberMetaDataParameters;
    writingDataBlock = CCSDSdf.writingDataBlock;

    return *this;
}

//------------------------------------------------------------------------------
// Integer Initialize() const
//------------------------------------------------------------------------------
/**
 * Initializes the CCSDSDataFile object.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::Initialize()
{
    if (!DataFile::Initialize()) return false;
    requiredNumberHeaderParameters = CountRequiredNumberHeaderDataParameters();
    return true;
}


//------------------------------------------------------------------------------
//  ProcessCCSDSDataFile::~ProcessCCSDSDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSDataFile::~ProcessCCSDSDataFile()
{
}

//------------------------------------------------------------------------------
//  CCSDSHeader* GetHeader()
//------------------------------------------------------------------------------
/**
 * Obtains the pointer to the current header.
 *
 * @return Pointer to the current header
 *
 */
//------------------------------------------------------------------------------
CCSDSHeader* ProcessCCSDSDataFile::GetHeader()
{
    return currentCCSDSHeader;
}

//------------------------------------------------------------------------------
//  void SetHeader(CCSDSHeader *myHeader)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the current header 
 *
 * @param <myHeader> Pointer to desired header.
 *
 */
//------------------------------------------------------------------------------
void ProcessCCSDSDataFile::SetHeader(CCSDSHeader *myHeader)
{
    currentCCSDSHeader = myHeader;
}

//------------------------------------------------------------------------------
// bool GetCCSDSHeader(std::string &lff, CCSDSObType* myOb)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the CCSDS data file
 *
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSHeader(std::string &lff, CCSDSObType *myOb)
{

    CCSDSHeader *myHeader = new CCSDSHeader;

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    std::string stemp;

    // The first line of any CCSDS data file must be the version keyword

    std::string regex = "^CCSDS_([A-Z]{3})_VERS\\s*=\\s*(" + REGEX_NUMBER + ")";

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp))
    {
        MessageInterface::ShowMessage("\nReading CCSDS " + stemp + " Data from file\n");

        myHeader->fileType = stemp;
	myHeader->ccsdsVersion = dtemp;
    }
    else
    {
	// Ill formed data
        MessageInterface::ShowMessage("\nCould not read CCSDS Version! Abort!\n");
	return false;	
    }

    // Since we already read in the first required parameter,
    // set the required count to 1
    Integer requiredCount = 1;
    std::string keyword;

    // Read in another line
    lff = ReadLineFromFile();

    do
    {

        if (!GetCCSDSKeyword(lff,keyword)) return false;
        Integer keyID = myHeader->GetKeywordID(keyword);
        if(myHeader->IsParameterRequired(keyID)) requiredCount++;

        switch (keyID)
        {

            case CCSDSHeader::CCSDS_HEADERCOMMENTS_ID:
                {
                std::string comment;

                if (GetCCSDSComment(lff,comment))
                    myHeader->comments.push_back(comment);
                else
                    return false;
                }
                break;

            case CCSDSHeader::CCSDS_CREATIONDATE_ID:

                if (!GetCCSDSValue(lff,myHeader->creationDate)) return false;
                break;

            case CCSDSHeader::CCSDS_ORIGINATOR_ID:

                if (!GetCCSDSValue(lff,myHeader->originator)) return false;
                break;

            default:

                MessageInterface::ShowMessage(keyword + " : This data not allowed in header!\n");
                return false;
                break;
        }

        // Read in another line
        lff = ReadLineFromFile();

    }
    while (requiredCount < requiredNumberHeaderParameters && !IsEOF());

    myOb->ccsdsHeader = myHeader;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSValue(const std::string &lff, Integer &value)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Key-Value pair
 * KEYWORD = VALUE
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSValue(const std::string &lff, Integer &value)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;

//    std::string regex = "^" + REGEX_CCSDS_KEYWORD + "\\s*=\\s*(" +
//                        REGEX_INTEGER + ")[\\s*\[(\\w+)\]]?.*";
    std::string regex = "^" + REGEX_CCSDS_KEYWORD + "\\s*=\\s*(" +
                        REGEX_INTEGER + ").*$";

    if (pcrecpp::RE(regex).FullMatch(lff,&itemp))
    {
       value = itemp;
       return true;
    }

    return false;
}


//------------------------------------------------------------------------------
// bool GetCCSDSValue(const std::string &lff, Real &value)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Key-Value pair
 * KEYWORD = VALUE
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSValue(const std::string &lff, Real &value)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    int itemp;

    std::string regex = "^" + REGEX_CCSDS_KEYWORD + "\\s*=\\s*(" +
                        REGEX_SCINUMBER + ").*$";

    if (pcrecpp::RE(regex).FullMatch(lff,&dtemp))
    {
       value = dtemp;
       return true;
    }

    // Catch integers here since the above tests for decimal points
    regex = "^" + REGEX_CCSDS_KEYWORD + "\\s*=\\s*(" + REGEX_INTEGER + ").*$";
    if (pcrecpp::RE(regex).FullMatch(lff,&itemp))
    {
       value = itemp;
       return true;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSValue(const std::string &lff, std::string &svalue)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Key-Value pair
 * KEYWORD = VALUE
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSValue(const std::string &lff,
                                                std::string &svalue)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    std::string stemp;

    std::string regex = "^" + REGEX_CCSDS_KEYWORD + "\\s*=\\s*(.*)$";

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp))
    {
       svalue = Trim(stemp);
       return true;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSKeyValueData(const std::string &lff, std::string &key,
//                           Real &value)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Key-Value pair
 * KEYWORD = VALUE
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSKeyword(const std::string &lff,
                                          std::string &key)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    std::string stemp;

    std::string regex = "^" + REGEX_CCSDS_SAVETHEKEYWORD + "\\s*=\\s*.*$";

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp))
    {
       key = Trim(stemp);
       return true;
    }

    regex = "^(COMMENT)\\s*.*$";

    if(pcrecpp::RE(regex).FullMatch(lff,&stemp))
    {
        key = Trim(stemp);
        return true;
    }

    return false;
}
//------------------------------------------------------------------------------
// bool GetCCSDSKeyEpochValueData(const std::string &lff, std::string &key,
//                           Real &value)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Key-Epoch-Value pair
 * KEYWORD = EPOCH VALUE
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSKeyEpochValueData(const std::string &lff,
                            std::string &timeTag, std::string &key, Real &value)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    std::string stemp, stemp2;

    std::string regex = "^.*=\\s*(" + REGEX_CCSDS_DATE + ")\\s*("+ REGEX_SCINUMBER + ").*$";
//    std::string regex = "^" + REGEX_CCSDS_SAVETHEKEYWORD + "\\s*=\\s*(" +
//            REGEX_CCSDS_DATE + ")\\s*("+ REGEX_SCINUMBER + ").*$";

    MessageInterface::ShowMessage("HERE\n");

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&dtemp))
    {
        //key = stemp;
        timeTag = stemp;
        value = dtemp;

        MessageInterface::ShowMessage("Key = "+key+"\n");
        MessageInterface::ShowMessage("TimeTag = "+timeTag+"\n");
        MessageInterface::ShowMessage("Value = %f\n",value);

        return true;
    }

    return false;
}

//------------------------------------------------------------------------------
// bool GetCCSDSComments(std::string &lff, StringArray &comments)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Comment block
 * COMMENT some text some text some text
 * @param <lff> A line from file
 * @param <comments> The string array of comments
 * @return Boolean success if comments found, false if no comments found
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSComments(std::string &lff,
                                            StringArray &comments)
{
    std::string stemp;
    comments.clear();

    bool found = false;

    std::string regex = "^COMMENT(.*)$";

    while (!IsEOF() && pcrecpp::RE(regex).FullMatch(lff,&stemp))
    {
       comments.push_back(Trim(stemp));
       found = true;
       lff = ReadLineFromFile();
    }

    return found;
}

//------------------------------------------------------------------------------
// bool GetCCSDSComment(std::string &lff, std::string &comment)
//------------------------------------------------------------------------------
/**
 * Extracts a CCSDS Comment line
 * COMMENT some text some text some text
 * @param <lff> A line from file
 * @param <comment> The string comment
 * @return Boolean success if comment found, false if no comment found
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSComment(std::string &lff,
                                           std::string &comment)
{
    std::string regex = "^COMMENT(.*)$";
    std::string stemp;
    
    if(pcrecpp::RE(regex).FullMatch(lff,&stemp))
    {
        comment = Trim(stemp);
        return true;
    }
    else return false;
}

//------------------------------------------------------------------------------
// bool CCSDSTimeTag2A1Date(std::string &timeTag, A1Date &myA1Date)
//------------------------------------------------------------------------------
/**
 * Converts the CCSDS time tag to the GMAT A1Date

//
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::A1Date2CCSDSTimeTag(A1Date &myA1Date,std::string &timeTag,
                                               Integer displayMode)
{
    return false;
}

*/

//------------------------------------------------------------------------------
// bool CCSDSTimeTag2A1Date(std::string &timeTag, A1Date &myA1Date)
//------------------------------------------------------------------------------
/**
 * Converts the CCSDS time tag to the GMAT A1Date
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::CCSDSTimeTag2A1Date(std::string &timeTag,
                                               A1Date &myA1Date)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp1,itemp2,itemp3,itemp4,itemp5;
    double dtemp;
    std::string stemp;

    // YYYY-MM-DDTHH:MM:SS.SSSSS

    std::string regex1 = "^"+REGEX_CCSDS_SAVETHEDATE1+"$";
    std::string regex2 = "^"+REGEX_CCSDS_SAVETHEDATE2+"$";
    std::string regex3 = "^"+REGEX_NUMBER+"$";

    if (pcrecpp::RE(regex2).FullMatch(timeTag,&itemp1,&itemp2,&itemp3,&itemp4,&itemp5,&dtemp,&stemp))
    {
        //std::string timeZone = stemp;
        Integer year = itemp1;
        Integer month = itemp2;
        Integer day = itemp3;
        Integer hour = itemp4;
        Integer minute = itemp5;
        Real seconds = dtemp;
        UtcDate myUTCDate = UtcDate(year,month,day,hour,minute,seconds);
        A1Mjd myA1MJDDate = myUTCDate.ToA1Mjd();
        myA1Date = myA1MJDDate.ToA1Date();
        return true;
    }

    // YYYY-DOYTHH:MM:SS.SSSSS
   
    else if (pcrecpp::RE(regex1).FullMatch(timeTag,&itemp1,&itemp2,&itemp3,&itemp4,&dtemp,&stemp))
    {
        //std::string timeZone = stemp;
        Integer year = itemp1;
        Integer doy = itemp2;
        Integer hour = itemp3;
        Integer minute = itemp4;
        Real seconds = dtemp;
        UtcDate myUTCDate = UtcDate(year,doy,hour,minute,seconds);
        A1Mjd myA1MJDDate = myUTCDate.ToA1Mjd();
        myA1Date = myA1MJDDate.ToA1Date();
        return true;
    }

    // Julian date
    // JJJJJJ.JJJJJJJJ
    
    else if (pcrecpp::RE(regex3).FullMatch(timeTag,&dtemp))
    {
        //std::string timeZone = stemp;

        // This is the GODDARD Modified Julian Date in UTC
        Real myGoddardMJD = dtemp -  GmatTimeUtil::JULIAN_DATE_OF_010541;
        A1Mjd newA1Mjd;
        Real myGoddardA1MJD = newA1Mjd.UtcMjdToA1Mjd(myGoddardMJD);
        A1Mjd myA1MJD(myGoddardA1MJD);
        myA1Date = myA1MJD.ToA1Date();
        return true;
    }
    else
        return false;

}

//------------------------------------------------------------------------------
// bool WriteDataHeader(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes CCSDS header data to file
 *
 * @param <myOb> the CCSDS ObType to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::WriteDataHeader(const ObType *myOb)
{
    // Make sure this is a CCSDS Obtype
    if (!pcrecpp::RE("^CCSDS[A-Z]{3}ObType.*").FullMatch(myOb->GetTypeName()))
        return false;

    CCSDSHeader *myHeader = ((CCSDSObType*)myOb)->ccsdsHeader;

    if (!isHeaderWritten)
    {
        *theFile << myHeader;

        lastHeaderWritten = myHeader;
        isHeaderWritten = true;
        writingDataBlock = false;
    }
    else if (isHeaderWritten && myHeader != lastHeaderWritten)
    {
        isHeaderWritten = false;
        lastHeaderWritten = NULL;

        *theFile << myHeader;

        lastHeaderWritten = myHeader;
        isHeaderWritten = true;
        writingDataBlock = false;
    }
    return true;
}


//------------------------------------------------------------------------------
// bool WriteMetaData(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes CCSDS metadata to file
 *
 * @param <myOb> the CCSDS metadata to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::WriteMetaData(const ObType *myOb)
{
    // Check to see if this ObType corresponds to this Data Format Type
    Integer fileTypeID = GetFileFormatID(((CCSDSObType*)myOb)->GetCCSDSObType());
    if (fileFormatID != fileTypeID)
        return false;

    CCSDSMetaData *myMetaData;

    switch(fileTypeID)
    {
        case DataFile::CCSDS_TDM_ID:
            myMetaData = ((CCSDSTDMObType*)myOb)->ccsdsMetaData;
            break;
        case DataFile::CCSDS_OPM_ID:
            myMetaData = ((CCSDSOPMObType*)myOb)->ccsdsMetaData;
            break;
        case DataFile::CCSDS_OEM_ID:
            myMetaData = ((CCSDSOEMObType*)myOb)->ccsdsMetaData;
            break;
        case DataFile::CCSDS_APM_ID:
            myMetaData = ((CCSDSAPMObType*)myOb)->ccsdsMetaData;
            break;
        case DataFile::CCSDS_AEM_ID:
            myMetaData = ((CCSDSAEMObType*)myOb)->ccsdsMetaData;
            break;
        default:
            return false;
    }

    if (!isMetaDataWritten)
    {

        switch(fileTypeID)
        {
            case DataFile::CCSDS_TDM_ID:
                *theFile << (CCSDSTDMMetaData*)myMetaData;
                break;
            case DataFile::CCSDS_OPM_ID:
                *theFile << (CCSDSOPMMetaData*)myMetaData;
                break;
            case DataFile::CCSDS_OEM_ID:
                *theFile << (CCSDSOEMMetaData*)myMetaData;
                break;
            case DataFile::CCSDS_APM_ID:
                *theFile << (CCSDSAPMMetaData*)myMetaData;
                *theFile << std::endl;
                break;
            case DataFile::CCSDS_AEM_ID:
                *theFile << (CCSDSAEMMetaData*)myMetaData;
                break;
            default:
                return false;
        }

        lastMetaDataWritten = myMetaData;
        isMetaDataWritten = true;
        writingDataBlock = false;

    }
    else if (isMetaDataWritten && myMetaData != lastMetaDataWritten)
    {
        isMetaDataWritten = false;
        lastMetaDataWritten = NULL;

        switch(fileTypeID)
        {
            case DataFile::CCSDS_TDM_ID:
                // Finalize last block of tracking data
                *theFile << "DATA_STOP" << std::endl;
                // Output a blank line to separate the new
                // metadata from the last block of data
                *theFile << std::endl;
                *theFile << (CCSDSTDMMetaData*)myMetaData;
                break;
            case DataFile::CCSDS_OPM_ID:
                // Output a blank line to separate the new
                // metadata from the last block of data
                *theFile << std::endl;
                *theFile << (CCSDSOPMMetaData*)myMetaData;
                break;
            case DataFile::CCSDS_OEM_ID:
                // Output a blank line to separate the new
                // metadata from the last block of data
                *theFile << std::endl;
                *theFile << (CCSDSOEMMetaData*)myMetaData;
                break;
            case DataFile::CCSDS_APM_ID:
                // Output a blank line to separate the new
                // metadata from the last block of data
                *theFile << std::endl;
                *theFile << (CCSDSAPMMetaData*)myMetaData;
                *theFile << std::endl;
                break;
            case DataFile::CCSDS_AEM_ID:
                // Finalize last block of ephemeris data
                *theFile << "DATA_STOP" << std::endl;
                // Output a blank line to separate the new
                // metadata from the last block of data
                *theFile << std::endl;
                *theFile << (CCSDSAEMMetaData*)myMetaData;
                break;
            default:
                return false;
        }

        lastMetaDataWritten = myMetaData;
        isMetaDataWritten = true;
        writingDataBlock = false;
    }

    return true;
}

//------------------------------------------------------------------------------
// bool WriteData(const ObType *myOb)
//------------------------------------------------------------------------------
/**
 * Writes CCSDS data to file
 *
 * @param <myOb> the CCSDS data to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::WriteData(const ObType *myOb)
{

    // Check to see if this ObType corresponds to this Data Format Type
    Integer fileTypeID = GetFileFormatID(((CCSDSObType*)myOb)->GetCCSDSObType());

    if (fileFormatID != fileTypeID)
    {
        MessageInterface::ShowMessage("ERROR: This observation does not correspond to the output file format. Abort!\n");
        return false;
    }

    if (!WriteDataHeader(myOb))
    {
        MessageInterface::ShowMessage("ERROR: Failed to write CCSDS Header. Abort!\n");
        return false;
    }

    if (!WriteMetaData(myOb))
    {
        MessageInterface::ShowMessage("ERROR: Failed to write CCSDS MetaData. Abort!\n");
        return false;
    }

    switch(fileTypeID)
    {
        case DataFile::CCSDS_TDM_ID:
            *theFile << (CCSDSTDMObType*)myOb;
            break;
        case DataFile::CCSDS_OPM_ID:
            *theFile << (CCSDSOPMObType*)myOb;
            break;
        case DataFile::CCSDS_OEM_ID:
            ((CCSDSOEMObType*)myOb)->commentsCurrentlyAllowed = !writingDataBlock;
            *theFile << (CCSDSOEMObType*)myOb;
            break;
        case DataFile::CCSDS_APM_ID:
            *theFile << (CCSDSAPMObType*)myOb;
            break;
        case DataFile::CCSDS_AEM_ID:
            ((CCSDSAEMObType*)myOb)->commentsCurrentlyAllowed = !writingDataBlock;
            *theFile << (CCSDSAEMObType*)myOb;
            break;
        default:
            MessageInterface::ShowMessage("ERROR: This CCSDS data type not recognized. Abort!\n");
            return false;
            break;
    }

    writingDataBlock = true;

    return true;
}