#include "SP3cObtype.hpp"

//---------------------------------
//  data
//---------------------------------

const std::string SP3cObtype::SP3c_DATATYPE_DESCRIPTIONS[EndSP3cTypeReps] =
{
    "TwoWayTimeOfFlight"
};
    
const std::string SP3cObtype::SP3c_TIMESYSTEM_DESCRIPTIONS[EndSP3cTimeReps] =
{
    "UTC",
    "GPS",
    "BIPM",
    "BIH"
};

const Gmat::ParameterType SP3c_PARAMETER_TYPE[EndSP3cDataReps] =
{
        Gmat::BOOLEAN_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::STRING_TYPE,
        Gmat::STRING_TYPE,
        Gmat::STRING_TYPE,
        Gmat::STRING_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::STRINGARRAY_TYPE,
        Gmat::INTARRAY_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::STRINGARRAY_TYPE,
        Gmat::STRING_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::BOOLEAN_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::REAL_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::INTEGER_TYPE,
        Gmat::REAL_TYPE,
};

const std::string SP3c_FILEFORMAT_DESCRIPTIONS[EndSP3cDataReps] =
{
        "PosVelFlag",
        "StartYear",
        "StartMonth",
        "StartHour",
        "StartMinute",
        "StartSecond",
        "NumEpochs",
        "DataUsed",
        "CoordSys",
        "OrbitType",
        "Agency",
        "GpsWeek",
        "SecondsOfWeek",
        "EpochInterval",
        "ModJulianDay",
        "FractionOfDay",
        "NumSats",
        "SatIdList",
        "satAccuracyList",
        "FileType",
        "TimeSystem",
        "BasePosVelStdDev",
        "BaseClkRateStdDev",
        "Comments",
        "VehicleID",
        "X",
        "Y",
        "Z",
        "VX",
        "VY",
        "VZ",
        "ClockValue",
        "StdDevX",
        "StdDevY",
        "StdDevZ",
        "StdDevVX",
        "StdDevVY",
        "StdDevVZ",
        "StdDevClock",
        "StdDevClockRate",
        "ClockEventFlag",
        "ClockPredictionFlag",
        "ManeuverFlag",
        "OrbitPredictFlag",
        "XYCorrelation",
        "XZCorrelation",
        "XCCorrelation",
        "YZCorrelation",
        "YCCorrelation",
        "ZCCorrelation",
        "VXVYCorrelation",
        "VXVZCorrelation",
        "VXCCorrelation",
        "VYVZCorrelation",
        "VYCCorrelation",
        "VZCCorrelation",
        "HighResolutionStdDevX",
        "HighResolutionStdDevY",
        "HighResolutionStdDevZ",
        "HighResolutionStdDevVX",
        "HighResolutionStdDevVY",
        "HighResolutionStdDevVZ",
        "HighResolutionStdDevClock",
        "HighResolutionStdDevClockRate",
        "Year",
        "Month",
        "Hour",
        "Minute",
        "Second"
};

const std::string SP3c_UNIT_DESCRIPTIONS[EndSP3cDataReps] =
{
        "",
        "years",
        "months",
        "hrs",
        "min",
        "sec",
        "",
        "",
        "",
        "",
        "",
        "",
        "sec",
        "sec",
        "",
        "FractionOfDay",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "km",
        "km",
        "km",
        "dm/sec",
        "dm/sec",
        "dm/sec",
        "microsec",
        "mm",
        "mm",
        "mm",
        "mm/sec",
        "mm/sec",
        "mm/sec",
        "picosec",
        "picosec/sec",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "years",
        "months",
        "hrs",
        "min",
        "sec"
};

//------------------------------------------------------------------------------
//  SP3cObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SP3cObtype::SP3cObtype() :
        headerVectorIndex(NULL),
	timeOfLaserFiring(0),
	twoWayTimeOfFlight(0),
	binRMSRange(0),
	surfacePressure(0),
	surfaceTemp(0),
	relativeHumidity(0),
	numRawRanges(0),
	dataReleaseFlag(0),
	rawRangeFactor(0),
	normalPointWindowIndicator2(0),
	signalToNoiseRatio(0),
        burstCalSysDelay(0),
	signalStrength(0),
        angleOriginIndicator(0),
        az(0),
        el(0),
{
}

//------------------------------------------------------------------------------
//  ~SP3cObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
SP3cObtype::~SP3cObtype()
{
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  bool CheckDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool SP3cObtype::CheckDataAvailability(const std::string str) const
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
std::string SP3cObtype::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
   {
      return SP3c_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
std::string SP3cObtype::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
   {
      return SP3c_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer SP3cObtype::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cDataReps; i++)
   {
      if (str == SP3c_FILEFORMAT_DESCRIPTIONS[i])
         return i;
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
Integer SP3cObtype::GetFileTypeID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cTypeReps; i++)
   {
      if (str == SP3c_DATATYPE_DESCRIPTIONS[i])
         return i;
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
Integer SP3cObtype::GetTimeSystemID(const std::string &str) const
{
   for (Integer i = 0; i < EndSP3cTimeReps; i++)
   {
      if (str == SP3c_TIME_DESCRIPTIONS[i])
         return i;
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
Gmat::ParameterType SP3cObtype::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSP3cDataReps))
      return SP3c_PARAMETER_TYPE[id];

   return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
// bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
bool SP3cObtype::GetBoolDataParameter(const Integer id) const
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
// Bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
bool SP3cObtype::GetBoolDataParameter(const std::string &label) const
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
std::string SP3cObtype::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer SP3cObtype::GetIntegerDataParameter(const Integer id) const
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

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;

    }

}


//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer SP3cObtype::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string SP3cObtype::GetStringDataParameter(const Integer id) const
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

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }
}


//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string SP3cObtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
StringArray SP3cObtype::GetStringArrayDataParameter(const Integer id) const
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
// std::string GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
StringArray SP3cObtype::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// IntegerArray GetIntegerArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
IntegerArray SP3cObtype::GetIntegerArrayDataParameter(const Integer id) const
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
// IntegerArray GetIntegerArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
IntegerArray SP3cObtype::GetIntegerArrayDataParameter(const std::string &label) const
{
   return GetIntegerArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real SP3cObtype::GetRealDataParameter(const Integer id) const
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

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}


//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real SP3cObtype::GetRealDataParameter(const std::string &label) const
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
const std::string* SP3cObtype::GetDataTypes() const
{
   return SP3c_DATATYPE_DESCRIPTIONS;
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
std::string SP3cObtype::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndSP3cTypeReps))
   {
      return SP3c_DATATYPE_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer SP3cObtype::GetDataTypeID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
const std::string* SP3cObtype::GetTimeSystems() const
{
   return SP3c_TIMESYSTEM_DESCRIPTIONS;
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
std::string SP3cObtype::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndSP3cTimeReps))
   {
      return SP3c_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer SP3cObtype::GetTimeSystemID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}