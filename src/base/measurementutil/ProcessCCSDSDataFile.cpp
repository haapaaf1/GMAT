#include "ProcessCCSDSDataFile.hpp"

//------------------------------------------------------------------------------
//  static data
//------------------------------------------------------------------------------
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
    "TOD"
};

const std::string ProcessCCSDSDataFile::CCSDS_REF_FRAME_DESCRIPTIONS[EndCCSDSRefFrameReps] =
{
    "Earth Mean Equator and Equinox of J2000",
    "International Celestial Reference Frame",
    "Interantional Terrestrial Reference Frame 2000",
    "Interantional Terrestrial Reference Frame 1993",
    "Interantional Terrestrial Reference Frame 1997",
    "True of Date"
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
        isHeaderWritten(false)
{
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
    isHeaderWritten(CCSDSdf.isHeaderWritten)
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
    return *this;
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
// bool GetCCSDSHeader(std::string line, CCSDSHeader* myHeader)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the CCSDS data file
 *
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSHeader(std::string line,
                                          CCSDSHeader* myHeader)
{
    
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    std::string stemp;

    if (pcrecpp::RE("^CCSDS_([A-Z]{3})_VERS\\s*=\\s*(\\d*[\\.\\d+]?)").FullMatch(line,&stemp,&dtemp))
    {
        myHeader->fileType = stemp;
	myHeader->ccsdsVersion = dtemp;
    }
    else
    {
	// Ill formed data
	return false;	
    }

    // Read in another line
    std::string nextline = Trim(ReadLineFromFile());

    // Read lines until we have encountered the first meta data start

    while (!pcrecpp::RE("^META_START.*").FullMatch(nextline))
    {
        if (pcrecpp::RE("^CREATION_DATE\\s*=(.*)").FullMatch(nextline,&stemp))
        {
	    myHeader->creationDate = Trim(stemp);
        }
        else if (pcrecpp::RE("^ORIGINATOR\\s*=(.*)").FullMatch(nextline,&stemp))
        {
	    myHeader->originator = Trim(stemp);
        }
        else if (pcrecpp::RE("^COMMENT\\s*(.*)").FullMatch(nextline,&stemp))
        {
	    myHeader->headerComments.push_back(Trim(stemp));
        }
	else
	{
	    // Ill formed data - these are the only keywords 
	    // allowed in the header
	    return false;		    
	}

        // Read in another line
        nextline = Trim(ReadLineFromFile());

    }

    // pass the first epoch header to the next subroutine
    line = nextline;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSData(std::string &lff, CCSDSData *myData,
//                CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSData(std::string &lff,
                                     CCSDSData *myData,
                                     CCSDSObType *myOb)
{
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    std::string stemp, stemp2;

    std::string regex = "^" + REGEX_CCSDS_KEYWORD + "\\s*=\\s*(" +
            REGEX_CCSDS_DATE + ")\\s*("+ REGEX_SCINUMBER + ")$";

    if (pcrecpp::RE(regex).FullMatch(lff,&stemp,&stemp2,&dtemp))
    {
       myData->keywordID = myOb->GetKeywordID(Trim(stemp));
       if(myData->keywordID >= 0)
       {
           myData->timeTag = stemp;
           if (!CCSDSTimeTag2A1Date(myData->timeTag,myOb->epoch)) return false;
           myData->measurement = dtemp;
           myOb->ccsdsData =  myData;
           return true;
       }
    }

    return false;
}

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
    std::string regex = "^"+REGEX_CCSDS_SAVETHEDATE1+"$";
    if (pcrecpp::RE(regex).FullMatch(timeTag,&itemp1,&itemp2,&itemp3,&itemp4,&itemp5,&dtemp,&stemp))
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
    regex = "^"+REGEX_CCSDS_SAVETHEDATE2+"$";
    if (pcrecpp::RE(regex).FullMatch(timeTag,&itemp1,&itemp2,&itemp3,&itemp4,&itemp5,&dtemp,&stemp))
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
    regex = "^"+REGEX_NUMBER+"$";
    if (pcrecpp::RE(regex).FullMatch(timeTag,&dtemp))
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

    return false;

}

//------------------------------------------------------------------------------
// bool WriteDataHeader(CCSDSHeader *myHeader)
//------------------------------------------------------------------------------
/**
 * Writes CCSDS header data to file
 *
 * @param <myHeader> the CCSDS header struct to be written to file
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::WriteDataHeader(ObType *myOb)
{
    if (!pcrecpp::RE("^CCSDS[A-Z]{3}ObType.*").FullMatch(myOb->GetTypeName()))
        return false;

    CCSDSHeader *myHeader = ((CCSDSObType*)myOb)->ccsdsHeader;

    if (!isHeaderWritten)
    {
        *theFile << myHeader;
        lastHeaderWritten = myHeader;
        isHeaderWritten = true;
    }
    else if (isHeaderWritten && myHeader != lastHeaderWritten)
    {
        isHeaderWritten = false;
        lastHeaderWritten = NULL;

        *theFile << myHeader;

        lastHeaderWritten = myHeader;
        isHeaderWritten = true;
    }
    return true;
}
