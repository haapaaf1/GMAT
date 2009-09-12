//$Header$
//------------------------------------------------------------------------------
//                             ProcessSP3cDataFile
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

#include <ProcessSP3cDataFile.hpp>

//#define DEBUG_SP3C_DATA

//---------------------------------
//  static data
//---------------------------------

const std::string ProcessSP3cDataFile::DATATYPE_DESCRIPTIONS[EndSP3cTypeReps] =
{
    "GPS",
    "MIXED",
    "GLONASS",
    "LEO",
    "GALILEO"
};
    
const std::string ProcessSP3cDataFile::TIMESYSTEM_DESCRIPTIONS[EndSP3cTimeReps] =
{
    "GPS",
    "GLONASS",
    "GALILEO",
    "TAI",
    "UTC"
};

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
bool ProcessSP3cDataFile::Initialize()
{
    DataFile::Initialize();
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

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
        if (!GetData()) return false;

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

        #ifdef DEBUG_SP3C_DATA

            FILE * outFile;
            outFile = fopen("SP3c.output","w");

            // Output to file to make sure all the data is properly stored
            for (std::vector<SP3cObtype*>::const_iterator j=SP3cData.begin(); j!=SP3cData.end(); ++j)
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

            fclose(outFile);

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
//  ProcessSP3cDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessSP3cDataFile structures
 */
ProcessSP3cDataFile::ProcessSP3cDataFile(const std::string &itsName) :
	DataFile ("SP3cDataFile", itsName)
{
   objectTypeNames.push_back("SP3cDataFile");
   fileFormatName = "SP3c";
   fileFormatID = 3;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessSP3cDataFile::~ProcessSP3cDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessSP3cDataFile::~ProcessSP3cDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessSP3cDataFile.
 *
 * @return clone of the ProcessSP3cDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessSP3cDataFile::Clone() const
{
   GmatBase *clone = new ProcessSP3cDataFile(*this);
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
bool ProcessSP3cDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessSP3cDataFile::IsParameterReadOnly(const std::string &label) const
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
bool ProcessSP3cDataFile::AdvanceToNextOb()
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
//  bool BackUpToPreviousOb()
//------------------------------------------------------------------------------
/**
 * Decrements the vector container index
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
 bool DataFile::BackUpToPreviousOb()
{
    // Decrement position and optional velocity pointers and
    // optional correlation pointers if present
    --i_p;
    if ((*i_h)->velFlag) --i_v;
    if ((*i_h)->epFlag) --i_ep;
    if ((*i_h)->evFlag) --i_ev;

    // If we are at the beginning of this epoch data, then move to new epoch
    // record. We only test the position vector container because
    // there are always position records and if the velocity flag is set,
    // there will be one velocity record for each position vector.
    if (i_p==(*i)->position.begin())
    {

        // Move to previous epoch
        --i;
        
        if (i==SP3cData.begin()) return false;

        // Set data iterator to end of vector container
        // Here we move one position before the end marker because this function
        // is expected to return a valid pointer.
        i_p = --(*i)->position.end();
        i_v = --(*i)->velocity.end();
        i_ep = --(*i)->posClockCorrelation.end();
        i_ev = --(*i)->velClockRateCorrelation.end();

    }

    return true;
}

//------------------------------------------------------------------------------
// bool GetData()
//------------------------------------------------------------------------------
/**
 * Obtains the header line of SP3c data from file.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cDataFile::GetData()
{

    // Read a line from file
    std::string firstline = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^#c.*").FullMatch(firstline))
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
        if (!GetSP3cHeader(firstline)) return false;

        // Parse the data records
        // firstline now contains the first epoch header record
        // which was encountered in GetSP3cHeader
        if (!GetSP3cData(firstline)) return false;

    }

    return true;

}

//------------------------------------------------------------------------------
// bool GetSP3cHeader(std::string firstline)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the compact SP3c data format.
 *
 * Each data file has one header record (up to 22 lines) associated with it.
 */
//------------------------------------------------------------------------------
bool ProcessSP3cDataFile::GetSP3cHeader(std::string firstline)
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
    std::string nextline = Trim(ReadLineFromFile());

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
        nextline = Trim(ReadLineFromFile());

    }

    // pass the first epoch header to the next subroutine
    firstline = nextline;

    return true;

}

//------------------------------------------------------------------------------
// bool GetSP3cData(std::string lff, SP3c_header )
//------------------------------------------------------------------------------
/**
 * Converts the compact SP3c Normal Point Data format into usable numbers.
 *
 * Note that the SP3cType variable must be set prior to calling this routine!
 */
//
//------------------------------------------------------------------------------
bool ProcessSP3cDataFile::GetSP3cData(std::string &lff)
{
    // Construct a pointer to the SP3c data struct
    SP3cObtype *mySP3cdata;

    // Test for end of file and whether we encounter another header
    // record (unlikely since most sp3c files only contain one header)
    while (!IsEOF() && pcrecpp::RE("^#c.*").FullMatch(lff))
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
            mySP3cdata = new SP3cObtype;

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

