//$Header$
//------------------------------------------------------------------------------
//                             ProcessSP3cData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/22
//
/**
 *
 * Implements DataFile base class to read files written in the SP3c format.
 *
 */
//------------------------------------------------------------------------------

#include <ProcessSP3cData.hpp>
#include "RealUtilities.hpp"
#include "gmatdefs.hpp"
#include "StringUtil.hpp"           // for ToString()
#include "pcrecpp.h"

//---------------------------------
//  static data
//---------------------------------

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
bool ProcessSP3cData::Initialize()
{
    DataFile::Initialize();

    std::ifstream myFile;
    if(!OpenFile(myFile))
    {
	throw DataFileException("Unable to open data file: " + dataFileName);
	MessageInterface::ShowMessage("Unable to open data file: " + dataFileName);
    }

    // Make sure that the sp3cData vector has space reserved for
    // a minimum number of observations. This ensures that the
    // compiler does not unnecessarily reallocate the vector storage too-often.
    // The function reserve() will ensure that we have room for at least 1000
    // elements. If the vector already has room for the required number of elements,
    // reserve() does nothing. In other words, reserve() will grow the allocated
    // storage of the vector, if necessary, but will never shrink it.
    SP3cData.reserve(1000);
    SP3cHeader.reserve(5);

    // Parse the data file
    if (!GetData(myFile)) return false;

    // Set data iterator to beginning of vector container
    i = SP3cData.begin();

    // Set data iterator to beginning of vector container
    // We have to use i+1 to get past the begin marker but we don't want
    // to officially advance the pointer by using ++i
    i_p = (*(i+1))->position.begin();
    i_v = (*(i+1))->velocity.begin();
    i_ep = (*(i+1))->posClockCorrelation.begin();
    i_ev = (*(i+1))->velClockRateCorrelation.begin();

    // Reset the header iterator to the beginning of the vector container
    i_h = SP3cHeader.begin();

    FILE * outFile;
    outFile = fopen("SP3c.output","w");

    // Output to file to make sure all the data is properly stored
    for (std::vector<sp3c_obtype*>::const_iterator j=SP3cData.begin(); j!=SP3cData.end(); ++j)
    {

        // Output header record once because it's the same for everything
        if (j == SP3cData.begin())
        {
	    fprintf(outFile,"Velocity Data Flag = %s\n",(*(*j)->headerVectorIndex)->velFlag ? "true":"false");
	    fprintf(outFile,"Epoch Start Year = %d\n",(*(*j)->headerVectorIndex)->startYear);
	    fprintf(outFile,"Epoch Start Month = %d\n",(*(*j)->headerVectorIndex)->startMonth);
	    fprintf(outFile,"Epoch Start Day = %d\n",(*(*j)->headerVectorIndex)->startDay);
	    fprintf(outFile,"Epoch Start Hour = %d\n",(*(*j)->headerVectorIndex)->startHour);
	    fprintf(outFile,"Epoch Start Minute = %d\n",(*(*j)->headerVectorIndex)->startMinute);
	    fprintf(outFile,"Epoch Start Seconds = %16.8g\n",(*(*j)->headerVectorIndex)->startSeconds);
	    fprintf(outFile,"Number of Epochs = %ld\n",(*(*j)->headerVectorIndex)->numEpochs);
	    fprintf(outFile,"Data Used Indicator = %s\n",(*(*j)->headerVectorIndex)->dataUsed.c_str());
	    fprintf(outFile,"Coordinate System = %s\n",(*(*j)->headerVectorIndex)->coordSystem.c_str());
	    fprintf(outFile,"Orbit Type = %s\n",(*(*j)->headerVectorIndex)->orbitType.c_str());
	    fprintf(outFile,"Agency = %s\n",(*(*j)->headerVectorIndex)->agency.c_str());
	    fprintf(outFile,"GPS Week = %d\n",(*(*j)->headerVectorIndex)->gpsWeek);
	    fprintf(outFile,"Seconds of Week = %16.8g\n",(*(*j)->headerVectorIndex)->secondsOfWeek);
	    fprintf(outFile,"Epoch Interval = %16.8g\n",(*(*j)->headerVectorIndex)->epochInterval);
	    fprintf(outFile,"Modified Julian Day = %d\n",(*(*j)->headerVectorIndex)->modJulianDay);
	    fprintf(outFile,"Fraction of Day = %16.8g\n",(*(*j)->headerVectorIndex)->fractionOfDay);
	    fprintf(outFile,"Number of Sats = %d\n",(*(*j)->headerVectorIndex)->numSats);
            std::vector<std::string>::iterator iter;
            for( iter = (*(*j)->headerVectorIndex)->satIdList.begin(); iter != (*(*j)->headerVectorIndex)->satIdList.end(); iter++ )
            {
                fprintf(outFile,"Sat Id List = %s\n",(*iter).c_str());
            }
            std::vector<Integer>::iterator iter2;
            for( iter2 = (*(*j)->headerVectorIndex)->satAccuracyList.begin(); iter2 != (*(*j)->headerVectorIndex)->satAccuracyList.end(); iter2++ )
            {
                fprintf(outFile,"Sat Accuracy List = %d\n",(*iter2));
            }
	    fprintf(outFile,"File Type = %d\n",(*(*j)->headerVectorIndex)->fileType);
	    fprintf(outFile,"Time System = %d\n",(*(*j)->headerVectorIndex)->timeSystem);
	    fprintf(outFile,"Base PosVel Std Dev = %16.8g\n",(*(*j)->headerVectorIndex)->basePosVelStdDev);
	    fprintf(outFile,"Base Clock Rate Std Dev = %16.8g\n",(*(*j)->headerVectorIndex)->baseClkRateStdDev);
            std::vector<std::string>::iterator iter3;
            for( iter3 = (*(*j)->headerVectorIndex)->comments.begin(); iter3 != (*(*j)->headerVectorIndex)->comments.end(); iter3++ )
            {
                fprintf(outFile,"Comments = %s\n",(*iter3).c_str());
            }
	    fprintf(outFile,"\n-----------------------------\n");
        }

        // Output Epoch Data for this set of pos/vel data

        fprintf(outFile,"Epoch Year = %d\n",(*j)->year);
        fprintf(outFile,"Epoch Month = %d\n",(*j)->month);
        fprintf(outFile,"Epoch Day = %d\n",(*j)->day);
        fprintf(outFile,"Epoch Hour = %d\n",(*j)->hour);
        fprintf(outFile,"Epoch Minute = %d\n",(*j)->minute);
        fprintf(outFile,"Epoch Second = %16.8g\n",(*j)->seconds);

        fprintf(outFile,"\n*****************\n");

        // Output position data
        for (std::vector<sp3c_position*>::const_iterator k=(*j)->position.begin(); k!=(*j)->position.end(); ++k)
        {

            fprintf(outFile,"Vehicle ID = %s\n",(*k)->vehicleID.c_str());
            fprintf(outFile,"X = %16.8g\n",(*k)->x);
            fprintf(outFile,"Y = %16.8g\n",(*k)->y);
            fprintf(outFile,"Z = %16.8g\n",(*k)->z);
            fprintf(outFile,"Clock Value = %16.8g\n",(*k)->clockValue);
            fprintf(outFile,"Std Dev X = %16.8g\n",(*k)->stdDevX);
            fprintf(outFile,"Std Dev Y = %16.8g\n",(*k)->stdDevY);
            fprintf(outFile,"Std Dev Z = %16.8g\n",(*k)->stdDevZ);
            fprintf(outFile,"Std Dev Clock = %16.8g\n",(*k)->stdDevClock);
            fprintf(outFile,"Clock Event Flag = %s\n",(*k)->clockEventFlag ? "true":"false");
            fprintf(outFile,"Clock Prediction Flag = %s\n",(*k)->clockPredictionFlag ? "true":"false");
            fprintf(outFile,"Maneuver Flag = %s\n",(*k)->maneuverFlag ? "true":"false");
            fprintf(outFile,"Orbit Prediction Flag = %s\n",(*k)->orbitPredictFlag ? "true":"false");

        }

        // Output velocity data
        for (std::vector<sp3c_velocity*>::const_iterator k=(*j)->velocity.begin(); k!=(*j)->velocity.end(); ++k)
        {

            fprintf(outFile,"Vehicle ID = %s\n",(*k)->vehicleID.c_str());
            fprintf(outFile,"VX = %16.8g\n",(*k)->vx);
            fprintf(outFile,"VY = %16.8g\n",(*k)->vy);
            fprintf(outFile,"VZ = %16.8g\n",(*k)->vz);
            fprintf(outFile,"Clock Rate of Change = %16.8g\n",(*k)->clockRateOfChange);
            fprintf(outFile,"Std Dev VX = %16.8g\n",(*k)->stdDevVX);
            fprintf(outFile,"Std Dev VY = %16.8g\n",(*k)->stdDevVY);
            fprintf(outFile,"Std Dev VZ = %16.8g\n",(*k)->stdDevVZ);
            fprintf(outFile,"Std Dev Clock Rate = %16.8g\n",(*k)->stdDevClockRate);

        }


        fprintf(outFile,"\n******************************************************\n");

    }

    if (!CloseFile(myFile))
        return false;

    return true;
    
}

//------------------------------------------------------------------------------
//  ProcessSP3cData()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessSP3cData structures
 */
ProcessSP3cData::ProcessSP3cData(const std::string &itsName) :
	DataFile ("SP3cDataFile", itsName)
{
   objectTypeNames.push_back("SP3cDataFile");
   fileFormatName = "SP3c";
   fileFormatID = 3;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessSP3cData::~ProcessSP3cData()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessSP3cData::~ProcessSP3cData()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessSP3cData.
 *
 * @return clone of the ProcessSP3cData.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessSP3cData::Clone() const
{
   GmatBase *clone = new ProcessSP3cData(*this);
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
bool ProcessSP3cData::IsParameterReadOnly(const Integer id) const
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
bool ProcessSP3cData::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool AdvanceToNextOb()
//------------------------------------------------------------------------------
/**
 * Returns the next observation from the vector container.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::AdvanceToNextOb()
{
    // Advance position and optional velocity pointers and
    // optional correlation pointers if present
    ++i_p;
    if ((*i_h)->velFlag) ++i_v;
    if ((*i_h)->epFlag) ++i_ep;
    if ((*i_h)->evFlag) ++i_ev;

    // If we are at the end of this epoch data, then advance to new epoch record
    // We only test the position vector container because there are always
    // position records and if the velocity flag is set, there will be one
    // velocity record for each position vector.
    if (i_p==(*i)->position.end())
    {

        // Advance to new epoch
        ++i;
        if (i==SP3cData.end()) return false;
        
        // Set data iterator to beginning of vector container
        // Here we advance past begin marker because this function
        // is expected to return a valid pointer.
        i_p = ++(*i)->position.begin();
        i_v = ++(*i)->velocity.begin();
        i_ep = ++(*i)->posClockCorrelation.begin();
        i_ev = ++(*i)->velClockRateCorrelation.begin();
        
    }
    
    return true;

}

//------------------------------------------------------------------------------
//  bool CheckDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSP3cDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(SP3c_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// bool GetData(std::ifstream &theFile, sp3c_header *mySP3cheader, sp3c_obtype *mySP3cdata)
//------------------------------------------------------------------------------
/**
 * Obtains the header line of SP3c data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetData(std::ifstream &theFile)
{

    // Read a line from file
    std::string firstline = Trim(ReadLineFromFile(theFile));

    // Check to see if we encountered a new header record.
    while (!IsEOF(theFile) && pcrecpp::RE("^#c.*").FullMatch(firstline))
    {

        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        sp3c_header *mySP3cheader = new sp3c_header;

        // Push a pointer to the header record onto the stack
        SP3cHeader.push_back(mySP3cheader);

        if (SP3cHeader.size() == 1)
        {
            i_h = SP3cHeader.begin();
        }
        else
        {
            i_h++;
        }

        // Parse the header lines
        if (!GetSP3cHeader(firstline, theFile)) return false;

        // Parse the data records
        // firstline now contains the first epoch header record
        // which was encountered in GetSP3cHeader
        if (!GetSP3cData(firstline, theFile)) return false;

    }

    return true;

}

//------------------------------------------------------------------------------
// bool GetSP3cHeader(std::string firstline, std::ifstream &theFile)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the compact SP3c data format.
 *
 * Each data file has one header record (up to 22 lines) associated with it.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetSP3cHeader(std::string firstline, std::ifstream &theFile)
{

    // set SP3c type variable so that we know how
    // to process the rest of the data.
    if (pcrecpp::RE("^#cP.*").FullMatch(firstline))
    {
        (*i_h)->velFlag = false;
    }
    else if(pcrecpp::RE("^#cV.*").FullMatch(firstline))
    {
        (*i_h)->velFlag = true;
    }
    else
    {
        return false;
    }

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    double dtemp;

    // Check to make sure that the line length is at
    // least 60 characters long
    //if (firstline.size() < 60)
    //{
    //	return false;
    //}

    // Extract four digit year, two digit month, day, hour, minute,
    // and real seconds
    if (!from_string<int>((*i_h)->startYear,firstline.substr(3,4),std::dec)) return false;
    if (!from_string<int>((*i_h)->startMonth,firstline.substr(7,2),std::dec)) return false;
    if (!from_string<int>((*i_h)->startDay,firstline.substr(11,2),std::dec)) return false;
    if (!from_string<int>((*i_h)->startHour,firstline.substr(14,2),std::dec)) return false;
    if (!from_string<int>((*i_h)->startMinute,firstline.substr(17,2),std::dec)) return false;
    if (!from_string<double>(dtemp,firstline.substr(20,11),std::dec)) return false;
    (*i_h)->startSeconds = dtemp;

    // Number of epochs covered by this header
    if (!from_string<long int>((*i_h)->numEpochs,firstline.substr(32,4),std::dec)) return false;

    // These are all string variables so we just extract the substring.
    (*i_h)->dataUsed = firstline.substr(40,5);
    (*i_h)->coordSystem = firstline.substr(46,5);
    (*i_h)->orbitType = firstline.substr(52,3);
    (*i_h)->agency = firstline.substr(56,4);

    // Read in another line
    std::string nextline = Trim(ReadLineFromFile(theFile));

    // Read lines until we have encountered the first epoch data indicator "* "

    while (!pcrecpp::RE("^\\* .*").FullMatch(nextline))
    {
        // Line 2 of the SP3c data format
        if (pcrecpp::RE("^##.*").FullMatch(nextline))
        {
            if (!from_string<int>((*i_h)->gpsWeek,nextline.substr(3,4),std::dec)) return false;
            if (!from_string<double>(dtemp,nextline.substr(8,15),std::dec)) return false;
            (*i_h)->secondsOfWeek = dtemp;
            if (!from_string<double>(dtemp,nextline.substr(24,14),std::dec)) return false;
            (*i_h)->epochInterval = dtemp;
            if (!from_string<int>((*i_h)->modJulianDay,nextline.substr(39,5),std::dec)) return false;
            if (!from_string<double>(dtemp,nextline.substr(45,15),std::dec)) return false;
            (*i_h)->fractionOfDay = dtemp;
           
        }
        // Lines 3-7 of the SP3c data format
        else if (pcrecpp::RE("^+ .*").FullMatch(nextline))
        {
            if (pcrecpp::RE("^+\\s{3}\\d{2}.*").FullMatch(nextline))
            {
                if (!from_string<int>((*i_h)->numSats,nextline.substr(4,2),std::dec)) return false;
            }

            (*i_h)->satIdList.push_back(firstline.substr(9,3));
            (*i_h)->satIdList.push_back(firstline.substr(12,3));
            (*i_h)->satIdList.push_back(firstline.substr(15,3));
            (*i_h)->satIdList.push_back(firstline.substr(18,3));
            (*i_h)->satIdList.push_back(firstline.substr(21,3));
            (*i_h)->satIdList.push_back(firstline.substr(24,3));
            (*i_h)->satIdList.push_back(firstline.substr(27,3));
            (*i_h)->satIdList.push_back(firstline.substr(30,3));
            (*i_h)->satIdList.push_back(firstline.substr(33,3));
            (*i_h)->satIdList.push_back(firstline.substr(36,3));
            (*i_h)->satIdList.push_back(firstline.substr(39,3));
            (*i_h)->satIdList.push_back(firstline.substr(42,3));
            (*i_h)->satIdList.push_back(firstline.substr(45,3));
            (*i_h)->satIdList.push_back(firstline.substr(48,3));
            (*i_h)->satIdList.push_back(firstline.substr(51,3));
            (*i_h)->satIdList.push_back(firstline.substr(54,3));
            (*i_h)->satIdList.push_back(firstline.substr(57,3));

        }
        // Lines 8-12 of the SP3c data format
        else if (pcrecpp::RE("^++.*").FullMatch(nextline))
        {

            if (!from_string<int>(itemp,nextline.substr(9,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(12,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(15,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(18,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(21,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(24,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(27,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(30,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(33,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(36,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(39,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(42,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(45,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(48,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(51,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(54,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            if (!from_string<int>(itemp,nextline.substr(57,3),std::dec)) return false;
            (*i_h)->satAccuracyList.push_back(itemp);
            
        }
        // Lines 13-14 of the SP3c data format
        // Line 14 currently holds no data
        else if (pcrecpp::RE("^%c.*").FullMatch(nextline))
        {
            (*i_h)->fileType = GetFileTypeID(Trim(nextline.substr(3,2)));
            (*i_h)->timeSystem = GetTimeSystemID(Trim(nextline.substr(9,3)));
        }
        // Lines 15-16 of the SP3c data format
        // Line 16 currently holds no data
        else if (pcrecpp::RE("^%f.*").FullMatch(nextline))
        {
            if (!from_string<double>(dtemp,nextline.substr(3,10),std::dec)) return false;
            (*i_h)->basePosVelStdDev = dtemp;
            if (!from_string<double>(dtemp,nextline.substr(14,12),std::dec)) return false;
            (*i_h)->baseClkRateStdDev = dtemp;

        }
        // Lines 17-18 of the SP3c data format
        // Lines 17 & 18 currently hold no data so the
        // test is commented out
        //
        // else if (pcrecpp::RE("^%i.*").FullMatch(nextline))
        // {
        // }
        //
        // Lines 19-22 of the SP3c data format
        // These are the comment lines (up to 4 lines in the format spec)
        else if (pcrecpp::RE("^/\\*.*").FullMatch(nextline))
        {
            (*i_h)->comments.push_back(nextline.substr(3,57));
        }

        // Read in another line
        nextline = Trim(ReadLineFromFile(theFile));

    }

    // pass the first epoch header to the next subroutine
    firstline = nextline;

    return true;

}

//------------------------------------------------------------------------------
// bool GetSP3cData(std::string lff, SP3c_header, std::ifstream &theFile )
//------------------------------------------------------------------------------
/**
 * Converts the compact SP3c Normal Point Data format into usable numbers.
 *
 * Note that the SP3cType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------

bool ProcessSP3cData::GetSP3cData(std::string &lff, std::ifstream &theFile)
{
    // Construct a pointer to the SP3c data struct
    sp3c_obtype *mySP3cdata;

    // Test for end of file and whether we encounter another header
    // record (unlikely since most sp3c files only contain one header)
    while (!IsEOF(theFile) && pcrecpp::RE("^#c.*").FullMatch(lff))
    {

        // Remove any leading or trailing whitespace
        std::string lff2 = Trim(lff);

        // Temporary variables for string to number conversion.
        // This is needed because the from_string utility function
        // only supports the standard C++ types and does not
        // support the GMAT types Real and Integer. Therefore,
        // extraction is done into a temporary variable and then
        // assigned to the GMAT type via casting.
        int itemp;
        double dtemp;

        // Check for epoch header record indicator
        if (pcrecpp::RE("^* .*$").FullMatch(lff2))
        {

            // Initialize individual data struct
            // This needs new memory allocation because
            // we are storing pointers to this data
            mySP3cdata = new sp3c_obtype;

            // Associate this data point with the current header index
            mySP3cdata->headerVectorIndex = i_h;

            // Push this data point onto the stack.
            SP3cData.push_back(mySP3cdata);

            if (!from_string<int>(mySP3cdata->year,lff2.substr(3,4),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->month,lff2.substr(8,2),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->day,lff2.substr(11,2),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->hour,lff2.substr(14,2),std::dec)) return false;
            if (!from_string<int>(mySP3cdata->minute,lff2.substr(17,2),std::dec)) return false;
            if (!from_string<double>(dtemp,lff2.substr(20,11),std::dec)) return false;
            mySP3cdata->seconds = dtemp;
        
        }
        else if (pcrecpp::RE("^P.*$").FullMatch(lff2))
        {
            // initialize a new struct
            sp3c_position *mySP3c_position = new sp3c_position;

            // read data from file and assign to appropriate struct variables
            mySP3c_position->vehicleID = lff2.substr(1,3);
            if (!from_string<double>(dtemp,lff2.substr(4,14),std::dec)) return false;
            mySP3c_position->x = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(18,14),std::dec)) return false;
            mySP3c_position->y = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(32,14),std::dec)) return false;
            mySP3c_position->z = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(46,14),std::dec)) return false;
            mySP3c_position->clockValue = dtemp;
            if (!from_string<int>(itemp,lff2.substr(61,2),std::dec)) return false;
            mySP3c_position->stdDevX = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (!from_string<int>(itemp,lff2.substr(64,2),std::dec)) return false;
            mySP3c_position->stdDevY = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (!from_string<int>(itemp,lff2.substr(67,2),std::dec)) return false;
            mySP3c_position->stdDevZ = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (!from_string<int>(itemp,lff2.substr(70,2),std::dec)) return false;
            mySP3c_position->stdDevClock = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (pcrecpp::RE("^E$").FullMatch(lff2.substr(74,1)))
            {
                mySP3c_position->clockEventFlag = true;
            }
            else
            {
                mySP3c_position->clockEventFlag = false;
            }
            if (pcrecpp::RE("^P$").FullMatch(lff2.substr(75,1)))
            {
                mySP3c_position->clockPredictionFlag = true;
            }
            else
            {
                mySP3c_position->clockPredictionFlag = false;
            }
            if (pcrecpp::RE("^M$").FullMatch(lff2.substr(78,1)))
            {
                mySP3c_position->maneuverFlag = true;
            }
            else
            {
                mySP3c_position->maneuverFlag = false;
            }
            if (pcrecpp::RE("^P$").FullMatch(lff2.substr(79,1)))
            {
                mySP3c_position->orbitPredictFlag = true;
            }
            else
            {
                mySP3c_position->orbitPredictFlag = false;
            }

            // Push a pointer to the position record onto the stack
            mySP3cdata->position.push_back(mySP3c_position);

        }
        else if (pcrecpp::RE("^EP.*$").FullMatch(lff2))
        {

            sp3c_posClockCorrelation *mySP3c_posClockCorrelation = new sp3c_posClockCorrelation;

            if (!from_string<double>(dtemp,lff2.substr(4,4),std::dec)) return false;
            mySP3c_posClockCorrelation->highResolutionStdDevX = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(9,4),std::dec)) return false;
            mySP3c_posClockCorrelation->highResolutionStdDevY = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(14,4),std::dec)) return false;
            mySP3c_posClockCorrelation->highResolutionStdDevZ = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(19,7),std::dec)) return false;
            mySP3c_posClockCorrelation->highResolutionStdDevClock = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(27,8),std::dec)) return false;
            mySP3c_posClockCorrelation->xYCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(36,8),std::dec)) return false;
            mySP3c_posClockCorrelation->xZCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(45,8),std::dec)) return false;
            mySP3c_posClockCorrelation->xCCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(54,8),std::dec)) return false;
            mySP3c_posClockCorrelation->yZCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(63,8),std::dec)) return false;
            mySP3c_posClockCorrelation->yCCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(72,8),std::dec)) return false;
            mySP3c_posClockCorrelation->zCCorrelation = dtemp;

            // Push a pointer to the position record onto the stack
            mySP3cdata->posClockCorrelation.push_back(mySP3c_posClockCorrelation);

        }
        else if (pcrecpp::RE("^V.*$").FullMatch(lff2))
        {

            sp3c_velocity *mySP3c_velocity = new sp3c_velocity;

            // read data from file and assign to appropriate struct variables
            mySP3c_velocity->vehicleID = lff2.substr(1,3);
            if (!from_string<double>(dtemp,lff2.substr(4,14),std::dec)) return false;
            mySP3c_velocity->vx = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(18,14),std::dec)) return false;
            mySP3c_velocity->vy = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(32,14),std::dec)) return false;
            mySP3c_velocity->vz = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(46,14),std::dec)) return false;
            mySP3c_velocity->clockRateOfChange = dtemp;
            if (!from_string<int>(itemp,lff2.substr(61,2),std::dec)) return false;
            mySP3c_velocity->stdDevVX = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (!from_string<int>(itemp,lff2.substr(64,2),std::dec)) return false;
            mySP3c_velocity->stdDevVY = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (!from_string<int>(itemp,lff2.substr(67,2),std::dec)) return false;
            mySP3c_velocity->stdDevVZ = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);
            if (!from_string<int>(itemp,lff2.substr(70,2),std::dec)) return false;
            mySP3c_velocity->stdDevClockRate = GmatMathUtil::Pow((*i_h)->basePosVelStdDev,itemp);

            // Push a pointer to the position record onto the stack
            mySP3cdata->velocity.push_back(mySP3c_velocity);

        }
        else if (pcrecpp::RE("^EV.*$").FullMatch(lff2))
        {

            sp3c_velClockRateCorrelation *mySP3c_velClockRateCorrelation = new sp3c_velClockRateCorrelation;

            if (!from_string<double>(dtemp,lff2.substr(4,4),std::dec)) return false;
            mySP3c_velClockRateCorrelation->highResolutionStdDevVX = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(9,4),std::dec)) return false;
            mySP3c_velClockRateCorrelation->highResolutionStdDevVY = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(14,4),std::dec)) return false;
            mySP3c_velClockRateCorrelation->highResolutionStdDevVZ = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(19,7),std::dec)) return false;
            mySP3c_velClockRateCorrelation->highResolutionStdDevClockRate = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(27,8),std::dec)) return false;
            mySP3c_velClockRateCorrelation->vxVYCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(36,8),std::dec)) return false;
            mySP3c_velClockRateCorrelation->vxVZCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(45,8),std::dec)) return false;
            mySP3c_velClockRateCorrelation->vxCCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(54,8),std::dec)) return false;
            mySP3c_velClockRateCorrelation->vyVZCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(63,8),std::dec)) return false;
            mySP3c_velClockRateCorrelation->vyCCorrelation = dtemp;
            if (!from_string<double>(dtemp,lff2.substr(72,8),std::dec)) return false;
            mySP3c_velClockRateCorrelation->vzCCorrelation = dtemp;

            // Push a pointer to the position record onto the stack
            mySP3cdata->velClockRateCorrelation.push_back(mySP3c_velClockRateCorrelation);

        }
        else
        {
            return false;
        }

    }
    
    // Set some convenience flags if we encountered optional data records
    if ( mySP3cdata->posClockCorrelation.size() > 0 ) (*i_h)->epFlag = true;
    if ( mySP3cdata->velClockRateCorrelation.size() > 0 ) (*i_h)->evFlag = true;

    return true;

}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string ProcessSP3cData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
   {
      return SP3c_FILEFORMAT_DESCRIPTIONS[id];
   }
   return "";
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the unit text, given the input parameter ID.
 *
 * @param <id> Id for the requested unit text.
 *
 * @return unit text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string ProcessSP3cData::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
   {
      return SP3c_UNIT_DESCRIPTIONS[id];
   }
   return "";
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cDataReps; i++)
   {
      if (str == SP3c_FILEFORMAT_DESCRIPTIONS[i])
         return i;
   }

   return -1;
}

//------------------------------------------------------------------------------
//  Integer  GetFileTypeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the file type ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested file type.
 */
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetFileTypeID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cTypeReps; i++)
   {
      if (str == SP3c_TYPE_DESCRIPTIONS[i])
         return i;
   }

   return -1;
}

//------------------------------------------------------------------------------
//  Integer  GetTimeSystemID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the file type ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested file type.
 */
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetTimeSystemID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cTimeReps; i++)
   {
      if (str == SP3c_TIME_DESCRIPTIONS[i])
         return i;
   }

   return -1;
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType ProcessSP3cData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
      return SP3c_PARAMETER_TYPE[id];

   return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
// virtual Real GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetBoolDataParameter(const Integer id) const
{
    switch (id)
    {

        case SP3c_POSVELFLAG_ID:

            return (*(*i)->headerVectorIndex)->velFlag;

        case SP3c_CLOCKEVENTFLAG_ID:

            return (*i_p)->clockEventFlag;

        case SP3c_CLOCKPREDICTIONFLAG_ID:

            return (*i_p)->clockPredictionFlag;

        case SP3c_MANEUVERFLAG_ID:

            return (*i_p)->maneuverFlag;

        case SP3c_ORBITPREDICTFLAG_ID:

            return (*i_p)->orbitPredictFlag;

        default:

            return false;

    }

}

//------------------------------------------------------------------------------
// virtual Bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}


//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieve the string associated with a parameter.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return Text description for the type of the parameter, or the empty
 *         string ("").
 */
//---------------------------------------------------------------------------
std::string ProcessSP3cData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {

        case SP3c_STARTYEAR_ID:
            return (*(*i)->headerVectorIndex)->startYear;

        case SP3c_STARTMONTH_ID:

            return (*(*i)->headerVectorIndex)->startMonth;

        case SP3c_STARTDAY_ID:

            return (*(*i)->headerVectorIndex)->startDay;

        case SP3c_STARTHOUR_ID:

            return (*(*i)->headerVectorIndex)->startHour;

        case SP3c_STARTMINUTE_ID:

            return (*(*i)->headerVectorIndex)->startMinute;

        case SP3c_NUMEPOCHS_ID:

            return (*(*i)->headerVectorIndex)->numEpochs;

        case SP3c_GPSWEEK_ID:

            return (*(*i)->headerVectorIndex)->gpsWeek;

        case SP3c_EPOCHINTERVAL_ID:

            return (*(*i)->headerVectorIndex)->epochInterval;

        case SP3c_MODJULIANDAY_ID:

            return (*(*i)->headerVectorIndex)->modJulianDay;

        case SP3c_NUMSATS_ID:

            return (*(*i)->headerVectorIndex)->numSats;

        case SP3c_FILETYPE_ID:

            return (*(*i)->headerVectorIndex)->fileType;

        case SP3c_TIMESYSTEM_ID:

            return (*(*i)->headerVectorIndex)->timeSystem;

        case SP3c_YEAR_ID:

            return (*i)->year;

        case SP3c_MONTH_ID:

            return (*i)->month;

        case SP3c_DAY_ID:

            return (*i)->day;

        case SP3c_HOUR_ID:

            return (*i)->hour;

        case SP3c_MINUTE_ID:

            return (*i)->minute;

       default:

            return -123456789;

    }

}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer ProcessSP3cData::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string ProcessSP3cData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case SP3c_DATAUSED_ID:

            return (*(*i)->headerVectorIndex)->dataUsed;

        case SP3c_COORDSYS_ID:

            return (*(*i)->headerVectorIndex)->coordSystem;

        case SP3c_ORBITTYPE_ID:

            return (*(*i)->headerVectorIndex)->orbitType;

        case SP3c_AGENCY_ID:

            return (*(*i)->headerVectorIndex)->agency;

        case SP3c_VEHICLEID_ID:

            return (*i_p)->vehicleID;

        default:

            return "";

    }
}


//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string ProcessSP3cData::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual StringArray GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
StringArray ProcessSP3cData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case SP3c_SATIDLIST_ID:

            return (*(*i)->headerVectorIndex)->satIdList;

        case SP3c_COMMENTS_ID:

            return (*(*i)->headerVectorIndex)->comments;

        default:
            
            StringArray str;
            return str;

    }
}

//------------------------------------------------------------------------------
// virtual std::string GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
StringArray ProcessSP3cData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual IntegerArray GetIntegerArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
IntegerArray ProcessSP3cData::GetIntegerArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case SP3c_SATACCURACYLIST_ID:

            return (*(*i)->headerVectorIndex)->satAccuracyList;

        default:

            IntegerArray intarray;
            return intarray;

    }
}

//------------------------------------------------------------------------------
// virtual IntegerArray GetIntegerArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
IntegerArray ProcessSP3cData::GetIntegerArrayDataParameter(const std::string &label) const
{
   return GetIntegerArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real ProcessSP3cData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case SP3c_STARTSECOND_ID:
            return (*(*i)->headerVectorIndex)->startSeconds;

        case SP3c_SECONDSOFWEEK_ID:
            return (*(*i)->headerVectorIndex)->secondsOfWeek;

        case SP3c_FRACTIONOFDAY_ID:
            return (*(*i)->headerVectorIndex)->fractionOfDay;

        case SP3c_SATACCURACYLIST_ID:
            return (*(*i)->headerVectorIndex)->satAccuracyList[0];

        case SP3c_BASEPOSVELSTDDEV_ID:
            return (*(*i)->headerVectorIndex)->basePosVelStdDev;

        case SP3c_BASECLKRATESTDDEV_ID:
            return (*(*i)->headerVectorIndex)->baseClkRateStdDev;

        case SP3c_X_ID:
            return (*i_p)->x;

        case SP3c_Y_ID:
            return (*i_p)->y;

        case SP3c_Z_ID:
            return (*i_p)->z;

        case SP3c_VX_ID:
            return (*i_v)->vx;

        case SP3c_VY_ID:
            return (*i_v)->vy;

        case SP3c_VZ_ID:
            return (*i_v)->vz;

        case SP3c_CLOCKVALUE_ID:
            return (*i_p)->clockValue;

        case SP3c_STDDEV_X_ID:
            return (*i_p)->stdDevX;

        case SP3c_STDDEV_Y_ID:
            return (*i_p)->stdDevY;

        case SP3c_STDDEV_Z_ID:
            return (*i_p)->stdDevZ;

        case SP3c_STDDEV_VX_ID:
            return (*i_v)->stdDevVX;

        case SP3c_STDDEV_VY_ID:
            return (*i_v)->stdDevVY;

        case SP3c_STDDEV_VZ_ID:
            return (*i_v)->stdDevVZ;

        case SP3c_STDDEV_CLOCK_ID:
            return (*i_p)->stdDevClock;

        case SP3c_STDDEV_CLOCKRATE_ID:
            return (*i_v)->stdDevClockRate;

        case SP3c_XY_CORRELATION_ID:
            return (*i_ep)->xYCorrelation;

        case SP3c_XZ_CORRELATION_ID:
            return (*i_ep)->xZCorrelation;

        case SP3c_XC_CORRELATION_ID:
            return (*i_ep)->xCCorrelation;

        case SP3c_YZ_CORRELATION_ID:
            return (*i_ep)->yZCorrelation;

        case SP3c_YC_CORRELATION_ID:
            return (*i_ep)->yCCorrelation;

        case SP3c_ZC_CORRELATION_ID:
            return (*i_ep)->zCCorrelation;

        case SP3c_VXVY_CORRELATION_ID:
            return (*i_ev)->vxVYCorrelation;

        case SP3c_VXVZ_CORRELATION_ID:
            return (*i_ev)->vxVZCorrelation;

        case SP3c_VXC_CORRELATION_ID:
            return (*i_ev)->vxCCorrelation;

        case SP3c_VYVZ_CORRELATION_ID:
            return (*i_ev)->vyVZCorrelation;

        case SP3c_VYC_CORRELATION_ID:
            return (*i_ev)->vyCCorrelation;

        case SP3c_VZC_CORRELATION_ID:
            return (*i_ev)->vzCCorrelation;

        case SP3c_HIGHRESOLUTION_STDDEV_X_ID:
            return (*i_ep)->highResolutionStdDevX;

        case SP3c_HIGHRESOLUTION_STDDEV_Y_ID:
            return (*i_ep)->highResolutionStdDevY;

        case SP3c_HIGHRESOLUTION_STDDEV_Z_ID:
            return (*i_ep)->highResolutionStdDevZ;

        case SP3c_HIGHRESOLUTION_STDDEV_VX_ID:
            return (*i_ev)->highResolutionStdDevVX;

        case SP3c_HIGHRESOLUTION_STDDEV_VY_ID:
            return (*i_ev)->highResolutionStdDevVY;

        case SP3c_HIGHRESOLUTION_STDDEV_VZ_ID:
            return (*i_ev)->highResolutionStdDevVZ;

        case SP3c_HIGHRESOLUTION_STDDEV_CLOCK_ID:
            return (*i_ep)->highResolutionStdDevClock;

        case SP3c_HIGHRESOLUTION_STDDEV_CLOCKRATE_ID:
            return (*i_ev)->highResolutionStdDevClockRate;

        case SP3c_SECOND_ID:
            return (*i)->seconds;

        default:

            return -1234567.89;

    }

}


//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real ProcessSP3cData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}