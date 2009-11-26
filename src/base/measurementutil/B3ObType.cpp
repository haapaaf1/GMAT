#include "B3ObType.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string B3ObType::B3_DATATYPE_DESCRIPTIONS[EndB3TypeReps] =
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

const std::string B3ObType::B3_FILEFORMAT_DESCRIPTIONS[EndB3DataReps] =
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

const std::string B3ObType::B3_UNIT_DESCRIPTIONS[EndB3DataReps] =
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

const Gmat::ParameterType B3ObType::B3_PARAMETER_TYPE[EndB3DataReps] =
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

const bool B3ObType::B3_IS_REQUIRED[EndB3DataReps] =
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
    
const std::string B3ObType::B3_TIMESYSTEM_DESCRIPTIONS[EndB3TimeReps] =
{
    "UTC"
};

//------------------------------------------------------------------------------
//  B3ObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the B3ObType class
 */
//------------------------------------------------------------------------------
B3ObType::B3ObType() : ObType("B3ObType",""),
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
    objectTypeNames.push_back("B3ObType");
}

//------------------------------------------------------------------------------
//  ~B3ObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the B3ObType class
 */
//------------------------------------------------------------------------------
B3ObType::~B3ObType()
{
}

//---------------------------------------------------------------------------
//  B3ObType(const B3ObType &b3Ob);
//---------------------------------------------------------------------------
/**
 * Constructs base ObType structures used in derived classes, by copying
 * the input instance (copy constructor).
 *
 * @param <b3Ob>  ObType instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
B3ObType::B3ObType(const B3ObType &b3Ob) :
    ObType(b3Ob),
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
//  B3ObType& operator=(const B3ObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <b3Ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const B3ObType& B3ObType::operator=(const B3ObType &b3Ob)
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
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the B3DataFile.
 *
 * @return clone of the B3DataFile.
 */
//------------------------------------------------------------------------------
GmatBase* B3ObType::Clone() const
{
   GmatBase *clone = new B3ObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string B3ObType::GetDataParameterText(const Integer id) const
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
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string B3ObType::GetDataUnits(const Integer id) const
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
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer B3ObType::GetDataParameterID(const std::string &str) const
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
 * @see ObType
 */
//------------------------------------------------------------------------------
Gmat::ParameterType B3ObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndB3DataReps))
      return B3_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string B3ObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer B3ObType::GetIntegerDataParameter(const Integer id) const
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
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer B3ObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string B3ObType::GetStringDataParameter(const Integer id) const
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
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string B3ObType::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real B3ObType::GetRealDataParameter(const Integer id) const
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
 * @see ObType
 */
//------------------------------------------------------------------------------
Real B3ObType::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Real &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool B3ObType::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

       case B3_SECONDS_ID:

            seconds = value;
            return true;

        case B3_ELEVATION_ID:

            elevation = value;
            return true;

        case B3_DECLINATION_ID:

            declination = value;
            return true;

        case B3_RIGHTASCENSION_ID:

            rightAscension = value;
            return true;

        case B3_AZIMUTH_ID:

            azimuth = value;
            return true;

        case B3_RANGE_ID:

            range = value;
            return true;

        case B3_RANGERATE_ID:

            rangeRate = value;
            return true;

        case B3_ECFX_ID:

            ecf_X = value;
            return true;

        case B3_ECFY_ID:

            ecf_Y = value;
            return true;

        case B3_ECFZ_ID:

            ecf_Z = value;
            return true;

        default:

            return false;

    }

}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const Real &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Real parameter.
 *
 * @param <label> String label identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool B3ObType::SetDataParameter(const std::string &label, const Real &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const Integer &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Integer parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool B3ObType::SetDataParameter(const Integer id, const Integer &value)
{
    switch (id)
    {
        case B3_TYPE_ID:

            b3Type = value;
            return true;

        case B3_SATELLITE_ID:

            satelliteID = value;
            return true;

        case B3_SENSORID_ID:

            sensorID = value;
            return true;

        case B3_YEAR_ID:

            year = value;
            return true;

        case B3_DAYOFYEAR_ID:

            dayOfYear = value;
            return true;

        case B3_HOUR_ID:

            hour = value;
            return true;

        case B3_MINUTE_ID:

            minute = value;
            return true;

        default:

            return false;

    }

}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const Integer &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a Integer parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool B3ObType::SetDataParameter(const std::string &label, const Integer &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const std::string &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a std::string parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool B3ObType::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {
        case B3_SECURITYCLASSIFICATION_ID:

            securityClassification = value;
            return true;

        default:

            return false;

    }

}


//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const std::string &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a std::string parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool B3ObType::SetDataParameter(const std::string &label, const std::string &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
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
const std::string* B3ObType::GetDataTypes() const
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
std::string B3ObType::GetDataTypeText(const Integer &id) const
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
Integer B3ObType::GetDataTypeID(const std::string &label)
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
const std::string* B3ObType::GetTimeSystems() const
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
std::string B3ObType::GetTimeSystemText(const Integer &id) const
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
Integer B3ObType::GetTimeSystemID(const std::string &label)
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
bool B3ObType::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndB3DataReps)
	return B3_IS_REQUIRED[id];
    else
	return false;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const B3ObType &myB3)
//------------------------------------------------------------------------------
/**
 * Formats B3ObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myB3>    B3 observation to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const B3ObType *myB3)
{
   using namespace std;
   
   output.setf(std::ios::showpoint);
   output.setf(std::ios::scientific);


    // Verify the type of the b3 observation
    // Possible ObType values and their meaning
    // 0 - Range rate only
    // 1 - Azimuth and elevation
    // 2 - Range, azimuth and elevation
    // 3 - Range, azimuth, elevation, and range rate
    // 4 - Range, azimuth, eelcation, and range rate
    //    (extra measurements for azimuth rate, elevation rate, etc are ignored)
    // 5 - Right Ascension and Declination
    // 6 - Range only
    // 8 - Azimuth, elevation, sometimes range and ECF position of the sensor
    // 9 - Right ascension, declination, sometimes range and
    //     ECF position of the sensor

    output << setw(1) << myB3->securityClassification;
    output << setw(5) << right << myB3->satelliteID;
    output << setw(3) << right << myB3->sensorID;
    output << setw(2) << setfill('0') << myB3->year;
    output << setw(3) << setfill('0') << myB3->dayOfYear;
    output << setw(2) << setfill('0') << myB3->hour;
    output << setw(2) << setfill('0') << myB3->minute;
    int sec = myB3->seconds*1e3 + 0.5;
    output << setw(5) << setfill('0') << sec;

    switch (myB3->b3Type)
    {
	case B3ObType::RANGERATEONLY_ID:
        {

            output << setw(23) << setfill(' ') << right << " ";

            if (myB3->rangeRate > 0)
            {
                int rangeRate = myB3->rangeRate*1e5 + 0.5;
                output << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3->rangeRate*1e5;
                output << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            output << setw(21) << right << setfill(' ') << myB3->b3Type;

            break;
        }
	case B3ObType::AZEL_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->elevation > 0)
	    {
                int elevation = myB3->elevation*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3->elevation*1e4;
		output << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3->azimuth*1e4 + 0.5;
	    output << " " << setw(7) << right << setfill('0') << azimuth;

            output << setw(38) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	case B3ObType::RAZEL_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->elevation > 0)
	    {
                int elevation = myB3->elevation*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3->elevation*1e4;
		output << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3->azimuth*1e4 + 0.5;
	    output << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                output << endl;
                return output;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                output << endl;
                return output;
            }
            if (exponent < 0 || exponent >= 5)
            {
                output << endl;
                return output;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            output << " " << setw(7) << right << mantissa;
            output << setw(1) << exponent;

            output << setw(29) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	case B3ObType::B3ObType::RAZELRR_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->elevation > 0)
	    {
                int elevation = myB3->elevation*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3->elevation*1e4;
		output << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3->azimuth*1e4 + 0.5;
	    output << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                output << endl;
                return output;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                output << endl;
                return output;
            }
            if (exponent < 0 || exponent >= 5)
            {
                output << endl;
                return output;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            output << " " << setw(7) << right << mantissa;
            output << setw(1) << exponent;

	    // For a negative range rate value, there is one less
	    // decimal place of precision
            // width is actually 7 but have to place at position 47

            if (myB3->rangeRate > 0)
            {
                int rangeRate = myB3->rangeRate*1e5 + 0.5;
                output << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3->rangeRate*1e5;
                output << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            output << setw(21) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	case B3ObType::B3ObType::RAZELRR2_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->elevation > 0)
	    {
                int elevation = myB3->elevation*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3->elevation*1e4;
		output << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3->azimuth*1e4 + 0.5;
	    output << " " << setw(7) << right << setfill('0') << azimuth;

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                output << endl;
                return output;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                output << endl;
                return output;
            }
            if (exponent < 0 || exponent >= 5)
            {
                output << endl;
                return output;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            output << " " << setw(7) << right << mantissa;
            output << setw(1) << exponent;

	    // For a negative range rate value, there is one less
	    // decimal place of precision
            // width is actually 7 but have to place at position 47
            if (myB3->rangeRate > 0)
            {
                int rangeRate = myB3->rangeRate*1e5 + 0.5;
                output << " " << setw(7) << right << setfill('0') << rangeRate;
            }
            else
            {
                int rangeRate = -myB3->rangeRate*1e5;
                output << " -" << setw(6) << right << setfill('0') << rangeRate;
            }

            output << setw(21) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
        case B3ObType::RADEC_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->declination > 0)
	    {
                int declination = myB3->declination*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << declination;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int declination = myB3->declination*1e4;
                output << setw(6) << right << setfill('0') << Overpunch(declination);
            }

	    // Find right ascension in hours, minutes, and seconds
            int hr = myB3->rightAscension;
            double minutes = (myB3->rightAscension - hr)*60.0;
            int mn = minutes;
            double seconds = (minutes-mn)*60.0;
            // we want SSS but have SS.SSSSSS
            // so mutiply by 10 and truncate
            int sec = seconds*10 + 0.5;

            output << " " << setw(2) << right << setfill('0') << hr;
            output << setw(2) << right << setfill('0') << mn;
            output << setw(3) << right << setfill('0') << sec;

            output << setw(38) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	case B3ObType::RANGEONLY_ID:
        {
            output << setw(14) << right << setfill(' ') << " ";

	    // Decompose range into RRRRRRR*10^E
            // ouptut integer range and range exponent separately
            ostringstream temp;
            char buffer[25];
            sprintf(buffer,"%010.7E",myB3->range);
            temp << buffer;
            int found = temp.str().find("E");
            std::string mantemp = temp.str().substr(0,found);
            std::string exptemp = temp.str().substr(found+1,temp.str().length());

	    // Find range mantissa
            double mantissa1;
	    if (!from_string<double>(mantissa1,mantemp,std::dec))
            {
                output << endl;
                return output;
            }
            int mantissa = mantissa1*1e6+0.5;

	    // Find range exponent
            int exponent;
	    if (!from_string<int>(exponent,exptemp,std::dec))
            {
                output << endl;
                return output;
            }
            if (exponent < 0 || exponent >= 5)
            {
                output << endl;
                return output;
            }

            // Range is formated RR.RRRRRR*10^E
            // traditional scientific notation is R.RRRRRRR*10^E
            // So we must account for the movement of the decimal place
            // to display to correct exponent according to the B3 format
            exponent -= 1;

            output << " " << setw(7) << right << mantissa;
            output << setw(1) << exponent;

            output << setw(29) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	case B3ObType::AZELSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->elevation > 0)
	    {
                int elevation = myB3->elevation*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << elevation;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int elevation = myB3->elevation*1e4;
		output << setw(6) << right << setfill('0') << Overpunch(elevation);
            }

	    // Find azimuth
            // account for extra blank space between el and az
            int azimuth = myB3->azimuth*1e4 + 0.5;
	    output << " " << setw(7) << right << setfill('0') << azimuth;

            // If range is defined, then output range
            if (myB3->range > 0)
            {
                // Decompose range into RRRRRRR*10^E
                // ouptut integer range and range exponent separately
                ostringstream temp;
                char buffer[25];
                sprintf(buffer,"%010.7E",myB3->range);
                temp << buffer;
                int found = temp.str().find("E");
                std::string mantemp = temp.str().substr(0,found);
                std::string exptemp = temp.str().substr(found+1,temp.str().length());

                // Find range mantissa
                double mantissa1;
                if (!from_string<double>(mantissa1,mantemp,std::dec))
                {
                    output << endl;
                    return output;
                }
                int mantissa = mantissa1*1e6+0.5;

                // Find range exponent
                int exponent;
                if (!from_string<int>(exponent,exptemp,std::dec))
                {
                    output << endl;
                    return output;
                }
                if (exponent < 0 || exponent >= 5)
                {
                    output << endl;
                    return output;
                }

                // Range is formated RR.RRRRRR*10^E
                // traditional scientific notation is R.RRRRRRR*10^E
                // So we must account for the movement of the decimal place
                // to display to correct exponent according to the B3 format
                exponent -= 1;

                output << " " << setw(7) << right << mantissa;
                output << setw(1) << exponent;

                // Find sensor position in meters
                int ecf_X = myB3->ecf_X * 1e3 + 0.5;
                output << setw(9) << right << ecf_X;
                int ecf_Y = myB3->ecf_Y * 1e3 + 0.5;
                output << setw(9) << right << ecf_Y;
                int ecf_Z = myB3->ecf_Z * 1e3 + 0.5;
                output << setw(9) << right << ecf_Z;

            }
            else
            {

                // Find sensor position in meters
                int ecf_X = myB3->ecf_X * 1e3 + 0.5;
                output << setw(18) << right << ecf_X;
                int ecf_Y = myB3->ecf_Y * 1e3 + 0.5;
                output << setw(9) << right << ecf_Y;
                int ecf_Z = myB3->ecf_Z * 1e3 + 0.5;
                output << setw(9) << right << ecf_Z;

            }

            output << " " << setw(1) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	case B3ObType::RADECSENSORPOS_ID:
        {
	    // Negative elevation values are formatted as Overpunched values
	    if (myB3->declination > 0)
	    {
                int declination = myB3->declination*1e4 + 0.5;
		output << setw(6) << right << setfill('0') << declination;
	    }
	    else
	    {
		// Figure out the Overpunch code and output to file
                int declination = myB3->declination*1e4;
                output << setw(6) << right << setfill('0') << Overpunch(declination);
            }

	    // Find right ascension in hours, minutes, and seconds
            int hr = myB3->rightAscension;
            double minutes = (myB3->rightAscension - hr)*60.0;
            int mn = minutes;
            double seconds = (minutes-mn)*60.0;
            // we want SSS but have SS.SSSSSS
            // so mutiply by 10 and truncate
            int sec = seconds*10 + 0.5;

            output << " " << setw(2) << right << setfill('0') << hr;
            output << setw(2) << right << setfill('0') << mn;
            output << setw(3) << right << setfill('0') << sec;

            // If range is defined, then output range
            // in addition to the sensor coordinates
            if (myB3->range > 0)
            {
                // Decompose range into RRRRRRR*10^E
                // ouptut integer range and range exponent separately
                ostringstream temp;
                char buffer[25];
                sprintf(buffer,"%010.7E",myB3->range);
                temp << buffer;
                int found = temp.str().find("E");
                std::string mantemp = temp.str().substr(0,found);
                std::string exptemp = temp.str().substr(found+1,temp.str().length());

                // Find range mantissa
                double mantissa1;
                if (!from_string<double>(mantissa1,mantemp,std::dec))
                {
                    output << endl;
                    return output;
                }
                int mantissa = mantissa1*1e6+0.5;

                // Find range exponent
                int exponent;
                if (!from_string<int>(exponent,exptemp,std::dec))
                {
                    output << endl;
                    return output;
                }
                if (exponent < 0 || exponent >= 5)
                {
                    output << endl;
                    return output;
                }

                // Range is formated RR.RRRRRR*10^E
                // traditional scientific notation is R.RRRRRRR*10^E
                // So we must account for the movement of the decimal place
                // to display to correct exponent according to the B3 format
                exponent -= 1;

                output << " " << setw(7) << right << mantissa;
                output << setw(1) << exponent;

                // Find sensor position in meters
                int ecf_X = myB3->ecf_X * 1e3 + 0.5;
                output << setw(9) << right << ecf_X;
                int ecf_Y = myB3->ecf_Y * 1e3 + 0.5;
                output << setw(9) << right << ecf_Y;
                int ecf_Z = myB3->ecf_Z * 1e3 + 0.5;
                output << setw(9) << right << ecf_Z;
            }
            else
            {

                // Find sensor position in meters
                int ecf_X = myB3->ecf_X * 1e3 + 0.5;
                output << setw(18) << right << ecf_X;
                int ecf_Y = myB3->ecf_Y * 1e3 + 0.5;
                output << setw(9) << right << ecf_Y;
                int ecf_Z = myB3->ecf_Z * 1e3 + 0.5;
                output << setw(9) << right << ecf_Z;
            }

            output << " " << setw(1) << right << setfill(' ') << myB3->b3Type;

	    break;
        }
	default:

	    // Not a recognized B3 data format. All stop.
	    return output;

    }

    output << endl;
   
   return output;
}