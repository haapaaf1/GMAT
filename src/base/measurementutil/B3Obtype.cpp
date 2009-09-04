#include "B3Obtype.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string B3_FILEFORMAT_DESCRIPTIONS[EndB3DataReps] =
{
    "B3Type",
    "SecurityClassification",
    "SatelliteID",
    "SensorID",
    "Year",
    "DayOfYear",
    "Hour",
    "Minute",
    "Seconds",
    "Elevation",
    "Declination",
    "RightAscension",
    "Azimuth",
    "Range",
    "RangeRate",
    "Ecf_X",
    "Ecf_Y",
    "Ecf_Z"
};

const std::string B3_UNIT_DESCRIPTIONS[EndB3DataReps] =
{
    "",
    "",
    "",
    "",
    "year",
    "DayOfYear",
    "hrs",
    "min",
    "sec",
    "deg",
    "deg",
    "deg",
    "deg",
    "km",
    "km/sec",
    "km",
    "km",
    "km"
};

const Gmat::ParameterType B3_PARAMETER_TYPE[EndB3DataReps] =
{
    Gmat::INTEGER_TYPE,
    Gmat::STRING_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
    Gmat::INTEGER_TYPE,
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
};

const bool B3_IS_REQUIRED[EndB3DataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false
};

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
   {
      return B3_FILEFORMAT_DESCRIPTIONS[id];
   }
   return "";
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
   {
      return B3_UNIT_DESCRIPTIONS[id];
   }
   return "";
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
Integer B3Obtype::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndB3DataReps; i++)
   {
      if (str == B3_FILEFORMAT_DESCRIPTIONS[i])
         return i;
   }

   return -1;
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
Gmat::ParameterType B3Obtype::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
      return B3_PARAMETER_TYPE[id];

   return GmatBase::GetParameterType(id);
}



//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see DataFile
 */
//---------------------------------------------------------------------------
std::string B3Obtype::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//---------------------------------------------------------------------------
Integer B3Obtype::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case B3_TYPE_ID:

            return (*i)->b3Type;

        case B3_SATELLITE_ID:

            return (*i)->satelliteID;

        case B3_SENSORID_ID:

            return (*i)->sensorID;

        case B3_YEAR_ID:

            return (*i)->year;

        case B3_DAYOFYEAR_ID:

            return (*i)->dayOfYear;

        case B3_HOUR_ID:

            return (*i)->hour;

        case B3_MINUTE_ID:

            return (*i)->minute;

        default:

            return -123456789;

    }

}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
Integer B3Obtype::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case B3_SECURITYCLASSIFICATION_ID:

            return (*i)->securityClassification;

        default:

            return "";

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
Real B3Obtype::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

       case B3_SECONDS_ID:

            return (*i)->seconds;

        case B3_ELEVATION_ID:

            return (*i)->elevation;

        case B3_DECLINATION_ID:

            return (*i)->declination;

        case B3_RIGHTASCENSION_ID:

            return (*i)->rightAscension;

        case B3_AZIMUTH_ID:

            return (*i)->azimuth;

        case B3_RANGE_ID:

            return (*i)->range;

        case B3_RANGERATE_ID:

            return (*i)->rangeRate;

        case B3_ECFX_ID:

            return (*i)->ecf_X;

        case B3_ECFY_ID:

            return (*i)->ecf_Y;

        case B3_ECFZ_ID:

            return (*i)->ecf_Z;

        default:

            return -1234567.89;

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see DataFile
 */
//------------------------------------------------------------------------------
Real B3Obtype::GetRealDataParameter(const std::string &label) const
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
const std::string* B3Obtype::GetDataTypes() const
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
std::string B3Obtype::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndB3TypeReps))
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
Integer B3Obtype::GetDataTypeID(const std::string &label)
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
const std::string* B3Obtype::GetTimeSystems() const
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
std::string B3Obtype::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndB3TimeReps))
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
Integer B3Obtype::GetTimeSystemID(const std::string &label)
{
    return -1;
}

//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default)
 */
//---------------------------------------------------------------------------
bool B3Obtype::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndB3DataReps)
	return B3_IS_REQUIRED[id];
else
	return false;
}