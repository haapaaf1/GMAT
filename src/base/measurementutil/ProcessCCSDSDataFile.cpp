#include "ProcessCCSDSDataFile.hpp"

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
	currentCCSDSHeader(NULL)
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
    currentCCSDSHeader(CCSDSdf.currentCCSDSHeader)
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
//  ccsds_header* GetHeader()
//------------------------------------------------------------------------------
/**
 * Obtains the pointer to the current header.
 *
 * @return Pointer to the current header
 *
 */
//------------------------------------------------------------------------------
CCSDSObtype::ccsds_header* ProcessCCSDSDataFile::GetHeader()
{
    return currentCCSDSHeader;
}

//------------------------------------------------------------------------------
//  void SetHeader(CCSDSObtype::ccsds_header *myHeader)
//------------------------------------------------------------------------------
/**
 * Sets the pointer to the current header 
 *
 * @param <myHeader> Pointer to desired header.
 *
 */
//------------------------------------------------------------------------------
void ProcessCCSDSDataFile::SetHeader(CCSDSObtype::ccsds_header *myHeader)
{
    currentCCSDSHeader = myHeader;
}

//------------------------------------------------------------------------------
// bool GetCCSDSHeader(std::string line, CCSDSObtype::ccsds_header* myHeader)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the CCSDS data file
 *
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSHeader(std::string line,
                                          CCSDSObtype::ccsds_header* myHeader)
{
    
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    std::string stemp;

    if (pcrecpp::RE("^CCSDS_TDM_VERS\\s*=\\s*(\\d*[\\.\\d+]?)").FullMatch(line,&dtemp))
    {
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
// bool GetCCSDSData(std::string &lff, CCSDSObtype::ccsds_data *myData,
//                CCSDSObtype *myOb)
//------------------------------------------------------------------------------
/**
 * Extracts the data from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSData(std::string &lff,
                                     CCSDSObtype::ccsds_data *myData,
                                     CCSDSObtype *myOb)
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
           myData->measurement = dtemp;
           myOb->ccsdsData =  myData;
           return true;
       }
    }

    return false;
}
