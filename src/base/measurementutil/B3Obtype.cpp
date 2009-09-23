#include "B3Obtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string B3Obtype::B3_DATATYPE_DESCRIPTIONS[EndB3TypeReps] =
{
    "Range rate only",
    "Azimuth & elevation",
    "Range, azimuth, & elevation",
    "Range, azimuth, elevation, & range rate",
    "Range, azimuth, elevation, & range rate (extra measurements for azimuth rate, elevation rate, etc are ignored)",
    "Right Ascension & Declination",
    "Range only",
    "Azimuth, elevation, sometimes range and ECF position of the sensor",
    "Right ascension, declination, sometimes range and ECF position of the sensor",
};

const std::string B3Obtype::B3_FILEFORMAT_DESCRIPTIONS[EndB3DataReps] =
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

const std::string B3Obtype::B3_UNIT_DESCRIPTIONS[EndB3DataReps] =
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

const Gmat::ParameterType B3Obtype::B3_PARAMETER_TYPE[EndB3DataReps] =
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
    Gmat::REAL_TYPE
};

const bool B3Obtype::B3_IS_REQUIRED[EndB3DataReps] =
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
    
const std::string B3Obtype::B3_TIMESYSTEM_DESCRIPTIONS[EndB3TimeReps] =
{
    "UTC"
};

//------------------------------------------------------------------------------
//  B3Obtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the B3Obtype class
 */
//------------------------------------------------------------------------------
B3Obtype::B3Obtype() : Obtype(),
    b3Type(0),
    securityClassification("U"),
    satID(0),
    sensID(0),
    year(0),
    dayOfYear(0),
    hour(0),
    minute(0),
    seconds(0),
    elevation(0),
    declination(0),
    rightAscension(0),
    azimuth(0),
    range(0),
    rangeRate(0),
    ecf_X(0),
    ecf_Y(0),
    ecf_Z(0)
{
}

//------------------------------------------------------------------------------
//  ~B3Obtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the B3Obtype class
 */
//------------------------------------------------------------------------------
B3Obtype::~B3Obtype()
{
}

//---------------------------------------------------------------------------
//  B3Obtype(const B3Obtype &b3Ob);
//---------------------------------------------------------------------------
/**
 * Constructs base Obtype structures used in derived classes, by copying
 * the input instance (copy constructor).
 *
 * @param <b3Ob>  Obtype instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
B3Obtype::B3Obtype(const B3Obtype &b3Ob) :
    Obtype(b3Ob),
    b3Type(b3Ob.b3Type),
    securityClassification(b3Ob.securityClassification),
    satID(b3Ob.satID),
    sensID(b3Ob.sensID),
    year(b3Ob.year),
    dayOfYear(b3Ob.dayOfYear),
    hour(b3Ob.hour),
    minute(b3Ob.minute),
    seconds(b3Ob.seconds),
    elevation(b3Ob.elevation),
    declination(b3Ob.declination),
    rightAscension(b3Ob.rightAscension),
    azimuth(b3Ob.azimuth),
    range(b3Ob.range),
    rangeRate(b3Ob.rangeRate),
    ecf_X(b3Ob.ecf_X),
    ecf_Y(b3Ob.ecf_Y),
    ecf_Z(b3Ob.ecf_Z)
{
}

//---------------------------------------------------------------------------
//  B3Obtype& operator=(const B3Obtype &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Obtype structures.
 *
 * @param <b3Ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const B3Obtype& B3Obtype::operator=(const B3Obtype &b3Ob)
{
   if (&b3Ob == this)
      return *this;

    b3Type = b3Ob.b3Type;
    securityClassification = b3Ob.securityClassification;
    satID = b3Ob.satID;
    sensID = b3Ob.sensID;
    year = b3Ob.year;
    dayOfYear = b3Ob.dayOfYear;
    hour = b3Ob.hour;
    minute = b3Ob.minute;
    seconds = b3Ob.seconds;
    elevation = b3Ob.elevation;
    declination = b3Ob.declination;
    rightAscension = b3Ob.rightAscension;
    azimuth = b3Ob.azimuth;
    range = b3Ob.range;
    rangeRate = b3Ob.rangeRate;
    ecf_X = b3Ob.ecf_X;
    ecf_Y = b3Ob.ecf_Y;
    ecf_Z = b3Ob.ecf_Z;
    
   return *this;
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
   {
      return B3_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
   {
      return B3_UNIT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Integer B3Obtype::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";
    
    for (Integer i = 0; i < EndB3DataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(B3_FILEFORMAT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }
      
   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
 */
//------------------------------------------------------------------------------
Gmat::ParameterType B3Obtype::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
      return B3_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see Obtype
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
 * @see Obtype
 */
//---------------------------------------------------------------------------
Integer B3Obtype::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case B3_TYPE_ID:

            return b3Type;

        case B3_SATELLITE_ID:

            return satelliteID;

        case B3_SENSORID_ID:

            return sensorID;

        case B3_YEAR_ID:

            return year;

        case B3_DAYOFYEAR_ID:

            return dayOfYear;

        case B3_HOUR_ID:

            return hour;

        case B3_MINUTE_ID:

            return minute;

        default:

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
std::string B3Obtype::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case B3_SECURITYCLASSIFICATION_ID:

            return securityClassification;

        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
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
 * @see Obtype
 */
//------------------------------------------------------------------------------
Real B3Obtype::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

       case B3_SECONDS_ID:

            return seconds;

        case B3_ELEVATION_ID:

            return elevation;

        case B3_DECLINATION_ID:

            return declination;

        case B3_RIGHTASCENSION_ID:

            return rightAscension;

        case B3_AZIMUTH_ID:

            return azimuth;

        case B3_RANGE_ID:

            return range;

        case B3_RANGERATE_ID:

            return rangeRate;

        case B3_ECFX_ID:

            return ecf_X;

        case B3_ECFY_ID:

            return ecf_Y;

        case B3_ECFZ_ID:

            return ecf_Z;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see Obtype
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
   return B3_DATATYPE_DESCRIPTIONS;
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
      return B3_DATATYPE_DESCRIPTIONS[id];
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
Integer B3Obtype::GetDataTypeID(const std::string &label)
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
const std::string* B3Obtype::GetTimeSystems() const
{
   return B3_TIMESYSTEM_DESCRIPTIONS;
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
      return B3_TIMESYSTEM_DESCRIPTIONS[id];
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
Integer B3Obtype::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = 0; i < EndB3TimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(B3_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}
    
    }
      
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
 
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

//------------------------------------------------------------------------------
//  bool CheckDataAvailability(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return true if successfull
 */
//------------------------------------------------------------------------------
bool B3Obtype::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndB3DataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(B3_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const B3Obtype &myB3)
//------------------------------------------------------------------------------
/**
 * Formats B3Obtype value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myB3>    B3 observation to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const B3Obtype *myB3) 
{
   using namespace std;
   
   output.setf(std::ios::showpoint);
   output.setf(std::ios::scientific);

   output << "Class = " << myB3->securityClassification << std::endl;
   output << "Satnum = " << myB3->satelliteID << std::endl;
   output << "Sensor ID = " << myB3->sensorID << std::endl;
   Integer year;
   if (myB3->year < 57)
   {
       year = myB3->year+2000;
   }
   else
   {
       year = myB3->year+1900;   
   }
   output << "Year = " << year << std::endl;   
   output << "Day of Year = " << myB3->dayOfYear << std::endl;
   output << "Hour = " << myB3->hour << std::endl;
   output << "Minutes = " << myB3->minute << std::endl;
   output << "Seconds = " << myB3->seconds << std::endl;
   output << "Elevation = " << myB3->elevation << std::endl;
   output << "Azimuth = " << myB3->azimuth << std::endl;
   output << "Declination = " << myB3->declination << std::endl;
   output << "Right Ascension = " << myB3->rightAscension << std::endl;
   output << "Range = " << myB3->range << std::endl;
   output << "Range Rate = " << myB3->rangeRate << std::endl;
   output << "ECF X = " << myB3->ecf_X << std::endl;
   output << "ECF Y = " << myB3->ecf_Y << std::endl;
   output << "ECF Z = " << myB3->ecf_Z << std::endl;
   output << "******************************************************" << std::endl;
   //output << setw(w) << setprecision(p) << prefix << a[i];
   
   return output;
}