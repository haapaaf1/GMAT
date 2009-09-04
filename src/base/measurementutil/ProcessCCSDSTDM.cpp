//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSTDM
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

#include <ProcessCCSDSTDM.hpp>

//#define DEBUG_CCSDSTDM_DATA

//---------------------------------
//  static data
//---------------------------------
const std::string ProcessCCSDSTDM::DATATYPE_DESCRIPTIONS[EndTDMDataReps] =
{
    "Angle1",
    "Angle2",
    "CarrierPower",
    "ClockBias",
    "ClockDrift",
    "Comment",
    "DopplerInstantaneous",
    "DopplerIntegrated",
    "DOR",
    "PCN0",
    "PRN0",
    "Pressure",
    "Range",
    "ReceiveFrequency",
    "ReceiveFrequency1",
    "ReceiveFrequency2",
    "ReceiveFrequency3",
    "ReceiveFrequency4",
    "ReceiveFrequency5",
    "RelativeHumidity",
    "STEC",
    "Temperature",
    "TransmitFrequency1",
    "TransmitFrequency2",
    "TransmitFrequency3",
    "TransmitFrequency4",
    "TransmitFrequency5",
    "TransmitFrequencyRate1",
    "TransmitFrequencyRate2",
    "TransmitFrequencyRate3",
    "TransmitFrequencyRate4",
    "TransmitFrequencyRate5",
    "TropoDry",
    "TropoWet",
    "VLBIDelay"
};

const std::string ProcessCCSDSTDM::TIME_DESCRIPTIONS[EndTDMTimeReps] =
{
    "UTC",
    "TAI",
    "GPS",
    "SCLK"
};

const std::string ProcessCCSDSTDM::MODE_DESCRIPTIONS[EndTDMModeReps] =
{
    "SEQUENTIAL",
    "SINGLE_DIFF"
};

const std::string ProcessCCSDSTDM::TIMETAG_DESCRIPTIONS[EndTDMTimetagReps] =
{
    "TRANSMIT",
    "RECEIVE"
};

const std::string ProcessCCSDSTDM::INTEGRATION_DESCRIPTIONS[EndTDMIntegrationReps] =
{
    "START",
    "MIDDLE",
    "END"
};

const std::string ProcessCCSDSTDM::RANGEMODE_DESCRIPTIONS[EndTDMRangeModeReps] =
{
    "COHERENT",
    "CONSTANT",
    "ONE_WAY"
};

const std::string ProcessCCSDSTDM::RANGEUNIT_DESCRIPTIONS[EndTDMRangeUnitReps] =
{
    "km",
    "s",
    "RU"
};

const std::string ProcessCCSDSTDM::ANGLETYPE_DESCRIPTIONS[EndTDMAngleTypeReps] =
{
    "AZEL",
    "RADEC",
    "XEYN",
    "XSYE"
};

const std::string ProcessCCSDSTDM::DATAQUALITY_DESCRIPTIONS[EndTDMDataQualityReps] =
{
    "RAW",
    "VALIDATED",
    "DEGRADED"
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
bool ProcessCCSDSTDM::Initialize()
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
        ccsdsTDMData.reserve(1000);
        ccsdsTDMHeader.reserve(5);

        // Parse the data file
        if (!GetData()) return false;

        // Set data iterator to beginning of vector container
        i = ccsdsTDMData.begin();

        // Set data iterator to beginning of vector container
        // We have to use i+1 to get past the begin marker but we don't want
        // to officially advance the pointer by using ++i
        i_p = (*(i+1))->position.begin();
        i_v = (*(i+1))->velocity.begin();
        i_ep = (*(i+1))->posClockCorrelation.begin();
        i_ev = (*(i+1))->velClockRateCorrelation.begin();

        // Reset the header iterator to the beginning of the vector container
        i_h = ccsdsTDMHeader.begin();

        #ifdef DEBUG_SP3C_DATA

            FILE * outFile;
            outFile = fopen("SP3c.output","w");

            // Output to file to make sure all the data is properly stored
            for (std::vector<CCSDSTDMObtype*>::const_iterator j=ccsdsTDMData.begin(); j!=ccsdsTDMData.end(); ++j)
            {

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
//  ProcessCCSDSTDM()
//------------------------------------------------------------------------------
/**
 * Constructs base ProcessCCSDSTDM structures
 */
ProcessCCSDSTDM::ProcessCCSDSTDM(const std::string &itsName) :
	DataFile ("TDMDataFile", itsName)
{
   objectTypeNames.push_back("TDMDataFile");
   fileFormatName = "TDM";
   fileFormatID = 3;
   numLines = 1;
}

//------------------------------------------------------------------------------
//  ProcessCCSDSTDM::~ProcessCCSDSTDM()
//------------------------------------------------------------------------------
/**
 * Class destructor
 */
ProcessCCSDSTDM::~ProcessCCSDSTDM()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSTDM.
 *
 * @return clone of the ProcessCCSDSTDM.
 */
//------------------------------------------------------------------------------
GmatBase* ProcessCCSDSTDM::Clone() const
{
   GmatBase *clone = new ProcessCCSDSTDM(*this);
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
bool ProcessCCSDSTDM::IsParameterReadOnly(const Integer id) const
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
bool ProcessCCSDSTDM::IsParameterReadOnly(const std::string &label) const
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
bool ProcessCCSDSTDM::AdvanceToNextOb()
{
    ++i_angle1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_angle1==(*i)->angle1.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_angle2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_angle2 == (*i)->angle2.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_carrierPower;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_carrierPower == (*i)->carrierPower.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_clockBias;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_clockBias == (*i)->clockBias.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_clockDrift;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_clockDrift == (*i)->clockDrift.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_comment;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_comment==(*i)->comment.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_dopplerInstantaneous;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_dopplerInstantaneous == (*i)->dopplerInstantaneous.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_dopplerIntegrated;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_dopplerIntegrated == (*i)->dopplerIntegrated.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_dor;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_dor == (*i)->dor.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_pcn0;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_pcn0 == (*i)->pcn0.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_prn0;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_prn0 == (*i)->prn0.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_pressure;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_pressure == (*i)->pressure.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_range;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_range == (*i)->range.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_receiveFrequency;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency == (*i)->receiveFrequency.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_receiveFrequency1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency1 == (*i)->receiveFrequency1.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_receiveFrequency2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency2 == (*i)->receiveFrequency21.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_receiveFrequency3;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency3 == (*i)->receiveFrequency3.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_receiveFrequency4;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency4 == (*i)->receiveFrequency4.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_receiveFrequency5;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency5 == (*i)->receiveFrequency5.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_rHumidity;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_rHumidity == (*i)->realtiveHumidity.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_stec;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_stec == (*i)->stec.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_temperature;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_temperature == (*i)->temperature.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequency1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency1 == (*i)->transmitFrequency1.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequency2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency2 == (*i)->transmitFrequency2.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequency3;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency3 == (*i)->transmitFrequency3.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequency4;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency4 == (*i)->transmitFrequency4.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    } 
    
    ++i_transmitFrequency5;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency5 == (*i)->transmitFrequency5.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequencyRate1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate1 == (*i)->transmitFrequencyRate1.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequencyRate2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate2 == (*i)->transmitFrequencyRate2.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequencyRate3;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate3 == (*i)->transmitFrequencyRate3.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequencyRate4;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate4 == (*i)->transmitFrequencyRate4.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_transmitFrequencyRate5;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate5 == (*i)->transmitFrequencyRate5.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_tropoDry;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_tropoDry == (*i)->tropoDry.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_tropoWet;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_tropoWet == (*i)->tropoWet.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
    }
    
    ++i_vlbiDelay;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_vlbiDelay == (*i)->vlbiDelay.end())
    {
        // Advance to new epoch
        ++i;
        if (i==ccsdsTDMData.end()) return false;   
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
     
    --i_angle1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_angle1==(*i)->angle1.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_angle2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_angle2 == (*i)->angle2.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_carrierPower;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_carrierPower == (*i)->carrierPower.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_clockBias;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_clockBias == (*i)->clockBias.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_clockDrift;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_clockDrift == (*i)->clockDrift.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_comment;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_comment==(*i)->comment.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_dopplerInstantaneous;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_dopplerInstantaneous == (*i)->dopplerInstantaneous.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_dopplerIntegrated;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_dopplerIntegrated == (*i)->dopplerIntegrated.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_dor;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_dor == (*i)->dor.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_pcn0;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_pcn0 == (*i)->pcn0.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_prn0;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_prn0 == (*i)->prn0.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_pressure;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_pressure == (*i)->pressure.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_range;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_range == (*i)->range.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_receiveFrequency;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency == (*i)->receiveFrequency.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_receiveFrequency1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency1 == (*i)->receiveFrequency1.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_receiveFrequency2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency2 == (*i)->receiveFrequency21.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_receiveFrequency3;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency3 == (*i)->receiveFrequency3.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_receiveFrequency4;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency4 == (*i)->receiveFrequency4.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_receiveFrequency5;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_receiveFrequency5 == (*i)->receiveFrequency5.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_rHumidity;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_rHumidity == (*i)->realtiveHumidity.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_stec;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_stec == (*i)->stec.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_temperature;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_temperature == (*i)->temperature.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequency1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency1 == (*i)->transmitFrequency1.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequency2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency2 == (*i)->transmitFrequency2.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequency3;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency3 == (*i)->transmitFrequency3.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequency4;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency4 == (*i)->transmitFrequency4.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    } 
    
    --i_transmitFrequency5;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequency5 == (*i)->transmitFrequency5.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequencyRate1;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate1 == (*i)->transmitFrequencyRate1.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequencyRate2;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate2 == (*i)->transmitFrequencyRate2.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequencyRate3;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate3 == (*i)->transmitFrequencyRate3.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequencyRate4;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate4 == (*i)->transmitFrequencyRate4.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_transmitFrequencyRate5;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_transmitFrequencyRate5 == (*i)->transmitFrequencyRate5.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_tropoDry;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_tropoDry == (*i)->tropoDry.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_tropoWet;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_tropoWet == (*i)->tropoWet.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
    }
    
    --i_vlbiDelay;
    
    // If we are at the end of this epoch data, then advance to new epoch record
    if (i_vlbiDelay == (*i)->vlbiDelay.begin())
    {
        // Advance to new epoch
        --i;
        if (i==ccsdsTDMData.begin()) return false;   
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
bool ProcessCCSDSTDM::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSTDMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(DATATYPE_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// bool GetData()
//------------------------------------------------------------------------------
/**
 * Obtains the header line of TDM data from file.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSTDM::GetData()
{

    // Read a line from file
    std::string firstline = Trim(ReadLineFromFile());

    // Check to see if we encountered a new header record.
    while (!IsEOF() && pcrecpp::RE("^CCSDS_TDM_VERS.*").FullMatch(firstline))
    {

        // Initialize individual data struct
        // This needs new memory allocation because
        // we are storing pointers to this data
        ccsds_header *myheader = new ccsds_header;

        // Push a pointer to the header record onto the stack
        ccsdsHeader.push_back(myheader);

        if (ccsdsHeader.size() == 1)
        {
            i_h = ccsdsHeader.begin();
        }
        else
        {
            i_h++;
        }

        // Parse the header lines
        if (!GetCCSDSHeader(firstline)) return false;

        // Parse the data records
        // firstline now contains the first epoch header record
        // which was encountered in GetTDMHeader
        if (!GetTDMData(firstline)) return false;

    }

    return true;

}

//------------------------------------------------------------------------------
// bool GetCCSDSHeader(std::string firstline)
//------------------------------------------------------------------------------
/**
 * Extracts header information from the compact SP3c data format.
 *
 * Each data file has one header record (up to 22 lines) associated with it.
 */
//------------------------------------------------------------------------------
bool ProcessCCSDSTDM::GetCCSDSHeader(std::string firstline)
{
    

    // Temporary variables for string to number conversion.
    // This is needed because the from_string utility function
    // only supports the standard C++ types and does not
    // support the GMAT types Real and Integer. Therefore,
    // extraction is done into a temporary variable and then
    // assigned to the GMAT type via casting.
    int itemp;
    double dtemp;
    std::string stemp;

    if (pcrecpp::RE("^CCSDS_TDM_VERS\s*=\s*(\d*[\.\d+]?)").FullMatch(firstline,&dtemp))
    {
	myTDMData->ccsdsVersion = dtemp;
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
        if (pcrecpp::RE("^CREATION_DATE\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_h)->creationDate = stemp;
        }
        else if (pcrecpp::RE("^ORIGINATOR\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_h)->originator = stemp;
        }
        else if (pcrecpp::RE("^COMMENT\s*(.*)").FullMatch(nextline,stemp))
        {
	    (*i_h)->headerComments.push_back(stemp);
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
// bool GetTDMData(std::string lff, ccsds_header )
//------------------------------------------------------------------------------
/**
 * Converts the tracking data message format into usable numbers.
 */
//
//------------------------------------------------------------------------------
bool ProcessCCSDSTDM::GetTDMData(std::string &lff)
{
    // Construct a pointer to the SP3c data struct
    CCSDSTDMObtype *myTDMdata;

    // Test for end of file and whether we encounter another header
    // record (unlikely since most sp3c files only contain one header)
    while (!IsEOF() && !pcrecpp::RE("^DATA_START.*").FullMatch(nextline))
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
	std::string stemp;
	
        if (pcrecpp::RE("^COMMENT\s*(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->metadataComments.push_back(stemp);
        }
        else if (pcrecpp::RE("^TIME_SYSTEM\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->timeSystem = stemp;
        }
        else if (pcrecpp::RE("^START_TIME\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->startTime = stemp;
        }
        else if (pcrecpp::RE("^STOP_TIME\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->stopTime = stemp;
        }
	else if (pcrecpp::RE("^PARTICIPANT_1\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->participant[0] = stemp;
        }
	else if (pcrecpp::RE("^PARTICIPANT_2\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->participant[1] = stemp;
        }
	else if (pcrecpp::RE("^PARTICIPANT_3\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->participant[2] = stemp;
        }
	else if (pcrecpp::RE("^PARTICIPANT_4\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->participant[3] = stemp;
        }
	else if (pcrecpp::RE("^PARTICIPANT_5\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->participant[4] = stemp;
        }
        else if (pcrecpp::RE("^MODE\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->mode = stemp;
        }
        else if (pcrecpp::RE("^PATH\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->path[0] = stemp;
        }
        else if (pcrecpp::RE("^PATH_1\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->path[1] = stemp;
        }
        else if (pcrecpp::RE("^PATH_2\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->path[2] = stemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_BAND\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->transmitBand = stemp;
        }
        else if (pcrecpp::RE("^RECEIVE_BAND\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->receiveBand = stemp;
        }
        else if (pcrecpp::RE("^TIMETAG_REF\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->turnaroundNumerator = stemp;
        }
        else if (pcrecpp::RE("^INTEGRATION_INTERVAL\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->integrationInterval = stemp;
        }
        else if (pcrecpp::RE("^INTEGRATION_REF\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->integrationRef = stemp;
        }
        else if (pcrecpp::RE("^FREQ_OFFSET\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->frequencyOffset = stemp;
        }
        else if (pcrecpp::RE("^RANGE_MODE\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->rangeMode = stemp;
        }
        else if (pcrecpp::RE("^RANGE_MODULUS\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->rangeModulus = dtemp;
        }
        else if (pcrecpp::RE("^RANGE_UNITS\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->rangeUnits = stemp;
        }
        else if (pcrecpp::RE("^ANGLE_TYPE\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->angleType = stemp;
        }
        else if (pcrecpp::RE("^REFERENCE_FRAME\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->referenceFrame = stemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_1\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->transmitDelay[0] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_2\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->transmitDelay[1] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_3\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->transmitDelay[2] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_4\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->transmitDelay[3] = dtemp;
        }
        else if (pcrecpp::RE("^TRANSMIT_DELAY_5\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->transmitDelay[4] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_1\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->receiveDelay[0] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_2\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->receiveDelay[1] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_3\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->receiveDelay[2] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_4\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->receiveDelay[3] = dtemp;
        }
        else if (pcrecpp::RE("^RECEIVE_DELAY_5\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->receiveDelay[4] = dtemp;
        }
        else if (pcrecpp::RE("^DATA_QUALITY\s*=(.*)").FullMatch(nextline,stemp))
        {
	    (*i_m)->dataQuality = stemp;
        }
        else if (pcrecpp::RE("^CORRECTION_ANGLE_1\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->correctionAngle1 = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_ANGLE_2\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->correctionAngle2 = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_DOPPLER\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->correctionDoppler = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTION_RECEIVE\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->correctionReceive = dtemp;
        }
	else if (pcrecpp::RE("^CORRECTION_TRANSMIT\s*=(.*)").FullMatch(nextline,dtemp))
        {
	    (*i_m)->correctionTransmit = dtemp;
        }
        else if (pcrecpp::RE("^CORRECTIONS_APPLIED\s*=(.*)").FullMatch(nextline,stemp))
        {
	    if (pcrecpp::RE("^YES$").FullMatch(trim(stemp)))
		(*i_m)->correctionsApplied = true;
	    else
		(*i_m)->correctionsApplied = false;
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
std::string ProcessCCSDSTDM::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMDataReps))
   {
      return CCSDS_TDM_FILEFORMAT_DESCRIPTIONS[id];
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
std::string ProcessCCSDSTDM::GetDataUnits(const Integer id) const
{
//   if ((id >= 0) && (id < EndCCSDSTDMDataReps))
//   {
//      return CCSDS_TDM_UNIT_DESCRIPTIONS[id];
//   }
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
Integer ProcessCCSDSTDM::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndCCSDSTDMDataReps; i++)
   {
      if (str == CCSDS_TDM_FILEFORMAT_DESCRIPTIONS[i])
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
Integer ProcessCCSDSTDM::GetFileTypeID(const std::string &str) const
{
   for (Integer i = 0; i < EndCCSDSTDMTypeReps; i++)
   {
      if (str == CCSDS_TDM_TYPE_DESCRIPTIONS[i])
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
Integer ProcessCCSDSTDM::GetTimeSystemID(const std::string &str) const
{
   for (Integer i = 0; i < EndTDMTimeReps; i++)
   {
      if (str == TDM_TIME_DESCRIPTIONS[i])
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
Gmat::ParameterType ProcessCCSDSTDM::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSTDMDataReps))
      return SP3c_PARAMETER_TYPE[id];

   return GmatBase::GetParameterType(id);
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
std::string ProcessCCSDSTDM::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}
//------------------------------------------------------------------------------
// bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
bool ProcessCCSDSTDM::GetBoolDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_TDM_CORRECTIONAPPLIED_ID:

	    return (*(*i)->metadataVectorIndex)->correctionsApplied;

        default:

            return false;

    }

}

//------------------------------------------------------------------------------
// Bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool ProcessSP3cData::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer ProcessCCSDSTDM::GetIntegerDataParameter(const Integer id) const
{

    switch (id)
    {

        case CCSDS_TDM_TURNAROUNDNUMERATOR_ID:
            return (*(*i)->metadataVectorIndex)->turnaroundNumerator;

        case CCSDS_TDM_TURNAROUNDDENOMINATOR_ID:
            return (*(*i)->metadataVectorIndex)->turnaroundDenominator;

       default:

            return -123456789;

    }

}


//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer ProcessCCSDSTDM::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string ProcessCCSDSTDM::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_CREATIONDATE_ID:
            return (*(*i)->headerVectorIndex)->creationDate;
	
	case CCSDS_TDM_ORIGINATOR_ID:
            return (*(*i)->headerVectorIndex)->originator;
	
	case CCSDS_TDM_TIMESYSTEM_ID:
            return (*(*i)->metadataVectorIndex)->timeSystem;
	
	case CCSDS_TDM_STARTTIME_ID:
            return (*(*i)->metadataVectorIndex)->startTime;
	
	case CCSDS_TDM_STOPTIME_ID:
            return (*(*i)->metadataVectorIndex)->stopTime;
	
	case CCSDS_TDM_PARTICIPANT1_ID:
            return (*(*i)->metadataVectorIndex)->participants[0];
	
	case CCSDS_TDM_PARTICIPANT2_ID:
            return (*(*i)->metadataVectorIndex)->participants[1];
	
	case CCSDS_TDM_PARTICIPANT3_ID:
            return (*(*i)->metadataVectorIndex)->participants[2];
	
	case CCSDS_TDM_PARTICIPANT4_ID:
            return (*(*i)->metadataVectorIndex)->participants[3];
	
	case CCSDS_TDM_PARTICIPANT5_ID:
            return (*(*i)->metadataVectorIndex)->participants[4];
	
	case CCSDS_TDM_MODE_ID:
            return (*(*i)->metadataVectorIndex)->mode;
	
	case CCSDS_TDM_PATH_ID:
            return (*(*i)->metadataVectorIndex)->path[0];
	
	case CCSDS_TDM_PATH1_ID:
            return (*(*i)->metadataVectorIndex)->path[1];
	
	case CCSDS_TDM_PATH2_ID:
            return (*(*i)->metadataVectorIndex)->path[2];
	
	case CCSDS_TDM_TRANSMITBAND_ID:
            return (*(*i)->metadataVectorIndex)->transmitBand;
	
	case CCSDS_TDM_RECEIVEBAND_ID:
            return (*(*i)->metadataVectorIndex)->receiveBand;
	
	case CCSDS_TDM_TIMETAGREF_ID:
            return (*(*i)->metadataVectorIndex)->timeTagRef;
	
	case CCSDS_TDM_INTEGRATIONREF_ID:
            return (*(*i)->metadataVectorIndex)->integrationRef;
	
	case CCSDS_TDM_RANGEMODE_ID:
            return (*(*i)->metadataVectorIndex)->rangeMode;
	
	case CCSDS_TDM_RANGEUNITS_ID:
            return (*(*i)->metadataVectorIndex)->rangeUnits;
	
	case CCSDS_TDM_ANGLETYPE_ID:	
            return (*(*i)->metadataVectorIndex)->angleType;
	
	case CCSDS_TDM_REFERENCEFRAME_ID:
            return (*(*i)->metadataVectorIndex)->referenceFrame;
	
	case CCSDS_TDM_DATAQUALITY_ID:
            return (*(*i)->metadataVectorIndex)->dataQuality;
	
	case CCSDS_TDM_ANGLE1_TIMETAG_ID:
            return (*i_angle1)->timeTag;
	
	case CCSDS_TDM_ANGLE1_UNITS_ID:
            return (*i_angle1)->units;
	
	case CCSDS_TDM_ANGLE2_TIMETAG_ID:
            return (*i_angle2)->timeTag;
	
	case CCSDS_TDM_ANGLE2_UNITS_ID:
            return (*i_angle2)->units;
	
	case CCSDS_TDM_CARRIERPOWER_TIMETAG_ID:
            return (*i_carrierPower)->timeTag;
	
	case CCSDS_TDM_CARRIERPOWER_UNITS_ID:
            return (*i_carrierPower)->units;
	
	case CCSDS_TDM_CLOCKBIAS_TIMETAG_ID:
            return (*i_clockBias)->timeTag;
	
	case CCSDS_TDM_CLOCKBIAS_UNITS_ID:
            return (*i_clockBias)->units;
	
	case CCSDS_TDM_CLOCKDRIFT_TIMETAG_ID:
            return (*i_clockDrift)->timeTag;
	
	case CCSDS_TDM_CLOCKDRIFT_UNITS_ID:
            return (*i_clockDrift)->units;
	
	case CCSDS_TDM_DOPPLERINSTANTANEOUS_TIMETAG_ID:
            return (*i_dopplerInstantaneous)->timeTag;
	
	case CCSDS_TDM_DOPPLERINSTANTANEOUS_UNITS_ID:
            return (*i_dopplerInstantaneous)->units;
	
	case CCSDS_TDM_DOPPLERINTEGRATED_TIMETAG_ID:
            return (*i_dopplerIntegrated)->timeTag;
	
	case CCSDS_TDM_DOPPLERINTEGRATED_UNITS_ID:
            return (*i_dopplerIntegrated)->units;
	
	case CCSDS_TDM_DOR_TIMETAG_ID:
            return (*i_dor)->timeTag;
	
	case CCSDS_TDM_DOR_UNITS_ID:
            return (*i_dor)->units;
	
	case CCSDS_TDM_PCN0_TIMETAG_ID:
            return (*i_pcn0)->timeTag;
	
	case CCSDS_TDM_PCN0_UNITS_ID:
            return (*i_pcn0)->units;
	
	case CCSDS_TDM_PRN0_TIMETAG_ID:
            return (*i_prn0)->timeTag;
	
	case CCSDS_TDM_PRN0_UNITS_ID:
            return (*i_prn0)->units;
	
	case CCSDS_TDM_PRESSURE_TIMETAG_ID:
            return (*i_pressure)->timeTag;
	
	case CCSDS_TDM_PRESSURE_UNITS_ID:
            return (*i_pressure)->units;
	
	case CCSDS_TDM_RANGE_TIMETAG_ID:
            return (*i_range)->timeTag;
	
	case CCSDS_TDM_RANGE_UNITS_ID:
            return (*i_range)->units;
	
	case CCSDS_TDM_RECEIVEFREQ_TIMETAG_ID:
            return (*i_receiveFrequency)->timeTag;
	
	case CCSDS_TDM_RECEIVEFREQ_UNITS_ID:
            return (*i_receiveFrequency)->units;
	
	case CCSDS_TDM_RECEIVEFREQ1_TIMETAG_ID:
            return (*i_receiveFrequency1)->timeTag;
	
	case CCSDS_TDM_RECEIVEFREQ1_UNITS_ID:
            return (*i_receiveFrequency1)->units;
	
	case CCSDS_TDM_RECEIVEFREQ2_TIMETAG_ID:
            return (*i_receiveFrequency2)->timeTag;
	
	case CCSDS_TDM_RECEIVEFREQ2_UNITS_ID:
            return (*i_receiveFrequency2)->units;
	
	case CCSDS_TDM_RECEIVEFREQ3_TIMETAG_ID:
            return (*i_receiveFrequency3)->timeTag;
	
	case CCSDS_TDM_RECEIVEFREQ3_UNITS_ID:
            return (*i_receiveFrequency3)->units;
	
	case CCSDS_TDM_RECEIVEFREQ4_TIMETAG_ID:
            return (*i_receiveFrequency4)->timeTag;
	
	case CCSDS_TDM_RECEIVEFREQ4_UNITS_ID:
            return (*i_receiveFrequency4)->units;
	
	case CCSDS_TDM_RECEIVEFREQ5_TIMETAG_ID:
            return (*i_receiveFrequency5)->timeTag;
	
	case CCSDS_TDM_RECEIVEFREQ5_UNITS_ID:
            return (*i_receiveFrequency5)->units;
	
	case CCSDS_TDM_RHUMIDITY_TIMETAG_ID:
            return (*i_relativeHumidity)->timeTag;
	
	case CCSDS_TDM_RHUMIDITY_UNITS_ID:
            return (*i_relativeHumidity)->units;
	
	case CCSDS_TDM_STEC_TIMETAG_ID:
            return (*i_stec)->timeTag;
	
	case CCSDS_TDM_STEC_UNITS_ID:
            return (*i_stec)->units;
	
	case CCSDS_TDM_TEMPERATURE_TIMETAG_ID:
            return (*i_temperature)->timeTag;
	
	case CCSDS_TDM_TEMPERATURE_UNITS_ID:
            return (*i_temperature)->units;
	
	case CCSDS_TDM_TRANSMITFREQ1_TIMETAG_ID:
            return (*i_transmitFrequency1)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQ1_UNITS_ID:
            return (*i_transmitFrequency1)->units;
	
	case CCSDS_TDM_TRANSMITFREQ2_TIMETAG_ID:
            return (*i_transmitFrequency2)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQ2_UNITS_ID:
            return (*i_transmitFrequency2)->units;
	
	case CCSDS_TDM_TRANSMITFREQ3_TIMETAG_ID:
            return (*i_transmitFrequency3)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQ3_UNITS_ID:
            return (*i_transmitFrequency3)->units;
	
	case CCSDS_TDM_TRANSMITFREQ4_TIMETAG_ID:
            return (*i_transmitFrequency4)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQ4_UNITS_ID:
            return (*i_transmitFrequency4)->units;
	
	case CCSDS_TDM_TRANSMITFREQ5_TIMETAG_ID:
            return (*i_transmitFrequency5)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQ5_UNITS_ID:
            return (*i_transmitFrequency5)->units;
	
	case CCSDS_TDM_TRANSMITFREQRATE1_TIMETAG_ID:
            return (*i_transmitFrequencyRate1)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQRATE1_UNITS_ID:
            return (*i_transmitFrequencyRate1)->units;
	
	case CCSDS_TDM_TRANSMITFREQRATE2_TIMETAG_ID:
            return (*i_transmitFrequencyRate2)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQRATE2_UNITS_ID:
            return (*i_transmitFrequencyRate2)->units;
	
	case CCSDS_TDM_TRANSMITFREQRATE3_TIMETAG_ID:
            return (*i_transmitFrequencyRate3)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQRATE3_UNITS_ID:
            return (*i_transmitFrequencyRate3)->units;
	
	case CCSDS_TDM_TRANSMITFREQRATE4_TIMETAG_ID:
            return (*i_transmitFrequencyRate4)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQRATE4_UNITS_ID:
            return (*i_transmitFrequencyRate4)->units;
	
	case CCSDS_TDM_TRANSMITFREQRATE5_TIMETAG_ID:
            return (*i_transmitFrequencyRate5)->timeTag;
	
	case CCSDS_TDM_TRANSMITFREQRATE5_UNITS_ID:
            return (*i_transmitFrequencyRate5)->units;
	
	case CCSDS_TDM_TROPODRY_TIMETAG_ID:
            return (*i_tropoDry)->timeTag;
	
	case CCSDS_TDM_TROPODRY_UNITS_ID:
            return (*i_tropoDry)->units;
	
	case CCSDS_TDM_TROPOWET_TIMETAG_ID:
            return (*i_tropoWet)->timeTag;
	
	case CCSDS_TDM_TROPOWET_UNITS_ID:
            return (*i_tropoWet)->units;
	
	case CCSDS_TDM_VLBIDELAY_TIMETAG_ID:
            return (*i_vlbiDelay)->timeTag;
	
	case CCSDS_TDM_VLBIDELAY_UNITS_ID:
            return (*i_vlbiDelay)->units;

        default:

            return "";

    }
}


//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string ProcessCCSDSTDM::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
StringArray ProcessCCSDSTDM::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_TDM_HEADERCOMMENTS_ID:
            return (*(*i)->headerVectorIndex)->headerComments;
	    
	case CCSDS_TDM_METADATACOMMENTS_ID:
            return (*(*i)->metadataVectorIndex)->metadataComments;

        default:
            
            StringArray str;
            return str;

    }
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
StringArray ProcessCCSDSTDM::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}


//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real ProcessCCSDSTDM::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_TDM_VERSION_ID:
            return (*(*i)->headerVectorIndex)->ccsdsVersion;

        case CCSDS_TDM_INTEGRATIONINTERVAL_ID:
            return (*(*i)->metadataVectorIndex)->integrationInterval;

        case CCSDS_TDM_FREQUENCYOFFSET_ID:
            return (*(*i)->metadataVectorIndex)->frequencyOffset;

        case CCSDS_TDM_RANGEMODULUS_ID:
            return (*(*i)->metadataVectorIndex)->rangeModulus;

        case CCSDS_TDM_TRANSMITDELAY1_ID:
            return (*(*i)->metadataVectorIndex)->transmitDelay[0];

        case CCSDS_TDM_TRANSMITDELAY2_ID:
            return (*(*i)->metadataVectorIndex)->transmitDelay[1];
	    
        case CCSDS_TDM_TRANSMITDELAY3_ID:
            return (*(*i)->metadataVectorIndex)->transmitDelay[2];
	    
        case CCSDS_TDM_TRANSMITDELAY4_ID:
            return (*(*i)->metadataVectorIndex)->transmitDelay[3];
	    
        case CCSDS_TDM_TRANSMITDELAY5_ID:
            return (*(*i)->metadataVectorIndex)->transmitDelay[4];

        case CCSDS_TDM_RECEIVEDELAY1_ID:
            return (*(*i)->metadataVectorIndex)->receiveDelay[0];

        case CCSDS_TDM_RECEIVEDELAY2_ID:
            return (*(*i)->metadataVectorIndex)->receiveDelay[1];
	    
        case CCSDS_TDM_RECEIVEDELAY3_ID:
            return (*(*i)->metadataVectorIndex)->receiveDelay[2];
	    
        case CCSDS_TDM_RECEIVEDELAY4_ID:
            return (*(*i)->metadataVectorIndex)->receiveDelay[3];
	    
        case CCSDS_TDM_RECEIVEDELAY5_ID:
            return (*(*i)->metadataVectorIndex)->receiveDelay[4];

        case CCSDS_TDM_CORRECTIONANGLE1_ID:
            return (*(*i)->metadataVectorIndex)->correctionAngle1;

        case CCSDS_TDM_CORRECTIONANGLE2_ID:
            return (*(*i)->metadataVectorIndex)->correctionAngle2;

	case CCSDS_TDM_CORRECTIONDOPPLER_ID:
            return (*(*i)->metadataVectorIndex)->correctionDoppler;

        case CCSDS_TDM_CORRECTIONRANGE_ID:
            return (*(*i)->metadataVectorIndex)->correctionRange;

        case CCSDS_TDM_CORRECTIONRECEIVE_ID:        
	    return (*(*i)->metadataVectorIndex)->correctionReceive;

	case CCSDS_TDM_CORRECTIONTRANSMIT_ID:
            return (*(*i)->metadataVectorIndex)->correctionTransmit;
	
	case CCSDS_TDM_ANGLE1_MEASUREMENT_ID:
	    return (*i_angle1)->measurement;
	
	case CCSDS_TDM_ANGLE2_MEASUREMENT_ID:
	    return (*i_angle2)->measurement;
	
	case CCSDS_TDM_CARRIERPOWER_MEASUREMENT_ID:
	    return (*i_carrierPower)->measurement;
	
	case CCSDS_TDM_CLOCKBIAS_MEASUREMENT_ID:
	    return (*i_clockBias)->measurement;
	
	case CCSDS_TDM_CLOCKDRIFT_MEASUREMENT_ID:
	    return (*i_clockDrift)->measurement;
	
	case CCSDS_TDM_DOPPLERINSTANTANEOUS_MEASUREMENT_ID:
	    return (*i_dopplerInstantaneous)->measurement;
	
	case CCSDS_TDM_DOPPLERINTEGRATED_MEASUREMENT_ID:
	    return (*i_dopplerIntegrated)->measurement;
	
	case CCSDS_TDM_DOR_MEASUREMENT_ID:
	    return (*i_dor)->measurement;
	
	case CCSDS_TDM_PCN0_MEASUREMENT_ID:
	    return (*i_pcn0)->measurement;
	
	case CCSDS_TDM_PRN0_MEASUREMENT_ID:
	    return (*i_prn0)->measurement;
	
	case CCSDS_TDM_PRESSURE_MEASUREMENT_ID:
	    return (*i_pressure)->measurement;
	
	case CCSDS_TDM_RANGE_MEASUREMENT_ID:
	    return (*i_range)->measurement;
	
	case CCSDS_TDM_RECEIVEFREQ_MEASUREMENT_ID:
	    return (*i_receiveFrequency)->measurement;
	
	case CCSDS_TDM_RECEIVEFREQ1_MEASUREMENT_ID:
	    return (*i_receiveFrequency1)->measurement;
	
	case CCSDS_TDM_RECEIVEFREQ2_MEASUREMENT_ID:
	    return (*i_receiveFrequency2)->measurement;
	
	case CCSDS_TDM_RECEIVEFREQ3_MEASUREMENT_ID:
	    return (*i_receiveFrequency3)->measurement;
	
	case CCSDS_TDM_RECEIVEFREQ4_MEASUREMENT_ID:
	    return (*i_receiveFrequency4)->measurement;
	
	case CCSDS_TDM_RECEIVEFREQ5_MEASUREMENT_ID:
	    return (*i_receiveFrequency5)->measurement;
	
	case CCSDS_TDM_RHUMIDITY_MEASUREMENT_ID:
	    return (*i_relativeHumidity)->measurement;
	
	case CCSDS_TDM_STEC_MEASUREMENT_ID:
	    return (*i_stec)->measurement;
	
	case CCSDS_TDM_TEMPERATURE_MEASUREMENT_ID:
	    return (*i_temperature)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQ1_MEASUREMENT_ID:
	    return (*i_transmitFrequency1)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQ2_MEASUREMENT_ID:
	    return (*i_transmitFrequency2)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQ3_MEASUREMENT_ID:
	    return (*i_transmitFrequency3)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQ4_MEASUREMENT_ID:
	    return (*i_transmitFrequency4)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQ5_MEASUREMENT_ID:
	    return (*i_transmitFrequency5)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQRATE1_MEASUREMENT_ID:
	    return (*i_transmitFrequencyRate1)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQRATE2_MEASUREMENT_ID:
	    return (*i_transmitFrequencyRate2)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQRATE3_MEASUREMENT_ID:
	    return (*i_transmitFrequencyRate3)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQRATE4_MEASUREMENT_ID:
	    return (*i_transmitFrequencyRate4)->measurement;
	
	case CCSDS_TDM_TRANSMITFREQRATE5_MEASUREMENT_ID:
	    return (*i_transmitFrequencyRate5)->measurement;
	
	case CCSDS_TDM_TROPODRY_MEASUREMENT_ID:
	    return (*i_tropoDry)->measurement;
	
	case CCSDS_TDM_TROPOWET_MEASUREMENT_ID:
	    return (*i_tropoWet)->measurement;

	case CCSDS_TDM_VLBIDELAY_MEASUREMENT_ID:
	    return (*i_vlbiDelay)->measurement;

        default:

            return -1234567.89;

    }

}


//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real ProcessCCSDSTDM::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetDataTypes() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable data types.
 *
 * @return String array of all data types.
 *
 */
//------------------------------------------------------------------------------
const std::string* ProcessCCSDSTDM::GetDataTypes() const
{
   return DATATYPE_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetDataTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type text corresponding to a ID
 *
 * @param <id> Integer ID associated with the data type
 * @return The string description of the data type
 *
 */
//------------------------------------------------------------------------------
std::string ProcessCCSDSTDM::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndTDMTypeReps))
   {
      return DATATYPE_DESCRIPTIONS[id];
   }

   return "INVALID";
}

//------------------------------------------------------------------------------
// Integer GetDataTypeID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type ID
 *
 * @param <label> The string label associated with the data type
 * @return The integer data type ID
 *
 */
//------------------------------------------------------------------------------
Integer ProcessCCSDSTDM::GetDataTypeID(const std::string &label)
{
    return -1;
}

//------------------------------------------------------------------------------
// const StringArray GetTimeSystems() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable time systems.
 *
 * @return String array of all time systems
 *
 */
//------------------------------------------------------------------------------
const std::string* ProcessCCSDSTDM::GetTimeSystems() const
{
   return TIMESYSTEM_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetTimeSystemText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the time system text corresponding to a ID
 *
 * @param <id> Integer ID associated with the time system
 * @return The string description of the time system
 *
 */
//------------------------------------------------------------------------------
std::string ProcessCCSDSTDM::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndTDMTimeReps))
   {
      return TIMESYSTEM_DESCRIPTIONS[id];
   }

   return "INVALID";
}

//------------------------------------------------------------------------------
// Integer GetTimeSystemID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the time system ID
 *
 * @param <label> The string label associated with the time system
 * @return The integer time system ID
 *
 */
//------------------------------------------------------------------------------
Integer ProcessCCSDSTDM::GetTimeSystemID(const std::string &label)
{
    return -1;
}