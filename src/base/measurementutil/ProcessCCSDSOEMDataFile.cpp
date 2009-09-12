//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOEMDataFile
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

#include <ProcessCCSDSOEMDataFile.hpp>

//#define DEBUG_CCSDSTDM_DATA

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
bool ProcessCCSDSOEMDataFile::Initialize()
{
    DataFile::Initialize();
    if (pcrecpp::RE("^[Rr].*").FullMatch(readWriteMode))
    {

        // Parse the data file
        if (!GetData()) return false;

        #ifdef DEBUG_CCSDSTDM_DATA

            FILE * outFile;
            outFile = fopen("ccsdsTDM.output","w");

            // Output to file to make sure all the data is properly stored
            for (std::vector<CCSDSTDMObtype*>::const_iterator j=ccsdsTDMData.begin(); j!=ccsdsTDMData.end(); ++j)
            {
/*
                // Output header record once because it's the same for everything
                if (j == ccsdsTDMData.begin())
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
*/
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
//  ProcessCCSDSOEMDataFile()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSOEMDataFile structures
 */
//------------------------------------------------------------------------------
ProcessCCSDSOEMDataFile::ProcessCCSDSOEMDataFile(const std::string &itsName) :
	ProcessCCSDSDataFile ("CCSDSTDMDataFile", itsName),
	currentCCSDSMetadata(NULL)
{
   objectTypeNames.push_back("CCSDSTDMDataFile");
   fileFormatName = "CCSDSTDM";
   fileFormatID = 9;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSOEMDataFile::~ProcessCCSDSOEMDataFile()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
//------------------------------------------------------------------------------
ProcessCCSDSOEMDataFile::~ProcessCCSDSOEMDataFile()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOEMDataFile.
 *
 * @return clone of the ProcessCCSDSOEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSOEMDataFile::Clone() const
{
   GmatBase *clone = new ProcessCCSDSOEMDataFile(*this);
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
bool ProcessCCSDSOEMDataFile::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSOEMDataFile::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool GetData()
//------------------------------------------------------------------------------
/**
 * Obtains the header line of TDM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetData()
{

    // Read a line from file
    std::string firstline = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_TDM_VERS.*").FullMatch(firstline))
    {
	
        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        currentCCSDSHeader = new CCSDSObtype::ccsds_header;

        // Parse the header lines
        if (!GetCCSDSHeader(firstline)) return false;

	// Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        currentCCSDSMetadata = new CCSDSOEMObtype::ccsds_oem_metadata;

	// Parse the data records
        // firstline now contains the first epoch header record
        // which was encountered in GetTDMHeader
        if (!GetCCSDSMetadata(firstline)) return false;

    }

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSMetadata(std::string &lff)
//------------------------------------------------------------------------------
/**
 * Extracts the metadata information from the tracking data message.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSOEMDataFile::GetCCSDSMetadata(std::string &lff)
{
    
    // Remove any leading or trailing whitespace
    lff = Trim(lff);
	
    // Test for end of file and whether we encounter another header
    // record (unlikely since most CCSDS files only contain one header)
    while (!IsEOF() && !pcrecpp::RE("^DATA_START.*").FullMatch(lff))
    {
	
        // Temporary variables for string to number conversion.
        // This is needed because the from_string utility function
        // only supports the standard C++ types and does not
        // support the GMAT types Real and Integer. Therefore,
        // extraction is done into a temporary variable and then
        // assigned to the GMAT type via casting.
        int itemp;
	std::string stemp;
	
        if (pcrecpp::RE("^COMMENT\\s*(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->metadataComments.push_back(stemp);
        }
        else if (pcrecpp::RE("^OBJECT_NAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->objectName = stemp;
        }
        else if (pcrecpp::RE("^OBJECT_ID\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->internationalDesignator = stemp;
        }
        else if (pcrecpp::RE("^CENTER_NAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->refFrameOrigin = stemp;
        }
        else if (pcrecpp::RE("^REF_FRAME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->refFrame = stemp;
        }
        else if (pcrecpp::RE("^TIME_SYSTEM\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->timeSystem = stemp;
        }
        else if (pcrecpp::RE("^START_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->startTime = stemp;
        }
        else if (pcrecpp::RE("^STOP_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->stopTime = stemp;
        }
        else if (pcrecpp::RE("^USEABLE_START_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->useableStartTime = stemp;
        }
        else if (pcrecpp::RE("^USEABLE_STOP_TIME\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->useableStopTime = stemp;
        }
        else if (pcrecpp::RE("^INTERPOLATION\\s*=(.*)").FullMatch(lff,&stemp))
        {
	    currentCCSDSMetadata->interpolationMethod = stemp;
        }
        else if (pcrecpp::RE("^INTERPOLATION_DEGREE\\s*=(.*)").FullMatch(lff,&itemp))
        {
	    currentCCSDSMetadata->interpolationDegree = itemp;
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