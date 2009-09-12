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
// bool GetCCSDSHeader(std::string firstline)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the CCSDS data file
 *
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSHeader(std::string firstline)
{
    
    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    double dtemp;
    std::string stemp;

    if (pcrecpp::RE("^CCSDS_TDM_VERS\\s*=\\s*(\\d*[\\.\\d+]?)").FullMatch(firstline,&dtemp))
    {
	currentCCSDSHeader->ccsdsVersion = dtemp;
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
	    currentCCSDSHeader->creationDate = Trim(stemp);
        }
        else if (pcrecpp::RE("^ORIGINATOR\\s*=(.*)").FullMatch(nextline,&stemp))
        {
	    currentCCSDSHeader->originator = Trim(stemp);
        }
        else if (pcrecpp::RE("^COMMENT\\s*(.*)").FullMatch(nextline,&stemp))
        {
	    currentCCSDSHeader->headerComments.push_back(Trim(stemp));
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
    firstline = nextline;

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSMetadata(std::string &lff)
//------------------------------------------------------------------------------
/**
 * Extracts metadata information from the CCSDS data file
 *
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSDataFile::GetCCSDSMetadata(std::string &lff)
{
    return false;
}