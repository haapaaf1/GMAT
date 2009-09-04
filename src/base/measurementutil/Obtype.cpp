#include <Obtype.hpp>

//---------------------------------
//  static data
//---------------------------------
const std::string Obtype::OBTYPE_KEYWORDS[EndObtypeReps] =
{
	"Range",
	"RangeRate",
	"Azimuth",
	"AzimuthRate",
	"Elevation",
	"ElevationRate",
	"RightAscension",
	"RightAscensionRate",
	"Declination",
	"DeclinationRate",
	"TwoWayTimeOfFlight",
	"CartesianState",
	"X",
	"VX",
	"Y",
	"VY",
	"Z",
	"VZ",
	"OrbitElementState",
	"SemiMajorAxis",
	"Eccentricity",
	"Inclination",
	"ArgumentOfPerigee",
	"RAAN",
	"TrueAnomaly"
};

//------------------------------------------------------------------------------
// A1Date GetEpoch()
//------------------------------------------------------------------------------
/**
 * Code used to obtain the epoch
 *
 * @return The epoch in GMAT A1 time
 *
 */
//------------------------------------------------------------------------------
A1Date Obtype::GetEpoch()
{
   return epoch;
}

//------------------------------------------------------------------------------
// Integer GetSatelliteID()
//------------------------------------------------------------------------------
/**
 * Code used to obtain the satellite ID
 *
 * @return The satellite ID corresponding to the NORAD catalog
 *
 */
//------------------------------------------------------------------------------
Integer Obtype::GetSatelliteID()
{
   return satelliteID;
}

//------------------------------------------------------------------------------
// std::string GetInternationalDesignator()
//------------------------------------------------------------------------------
/**
 * Code used to obtain the international designator
 *
 * @return The international designator
 *
 */
//------------------------------------------------------------------------------
std::string Obtype::GetInternationalDesignator()
{
   return internationalDesignator;
}

//------------------------------------------------------------------------------
// Integer GetSensorID()
//------------------------------------------------------------------------------
/**
 * Code used to obtain the sensor ID
 *
 * @return The sensorID
 *
 */
//------------------------------------------------------------------------------
Integer Obtype::GetSensorID()
{
   return sensorID;
}

//------------------------------------------------------------------------------
//  const std::string* GetObtypeKeywords()
//------------------------------------------------------------------------------
/**
 * Returns the list of valid obtype keywords
 *
 * @return String array of obtype keywords
 */
//------------------------------------------------------------------------------
const std::string* Obtype::GetObtypeKeywords()
{
    return OBTYPE_KEYWORDS;
}

//------------------------------------------------------------------------------
//  Integer GetObtypeID(const std::string keyword)
//------------------------------------------------------------------------------
/**
 * Obtains the obtype ID associated with a given keyword.
 *
 * @param <keyword> Keyword associated with a particular obtype
 * @return Integer ID corresponding to desired obtype; -1 if not found
 */
//------------------------------------------------------------------------------
Integer Obtype::GetObtypeID(const std::string keyword)
{
    std::string regex = "^" + keyword + "$";
    
    for (Integer i = 0; i < EndObtypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(OBTYPE_KEYWORDS[i]))
        {
            return i;
        }
    }

   return -1;
}

//------------------------------------------------------------------------------
//  std::string GetObtypeKeyword(Integer myID)
//------------------------------------------------------------------------------
/**
 * Obtains the obtype keyword associated with a given ID.
 *
 * @param <myID> ID associated with a particular obtype
 * @return String keyword corresponding to desired obtype; "" if not found
 */
//------------------------------------------------------------------------------
std::string Obtype::GetObtypeKeyword(Integer myID)
{
    if(myID >= 0 && myID < EndObtypeReps)
    {
        return OBTYPE_KEYWORDS[myID];
    }
    else
    {
	return "";
    }
}

//------------------------------------------------------------------------------
// std::string Ilrs2Cospar(std::string ilrsSatnum)
//------------------------------------------------------------------------------
/**
 * Convert ILRS Satellite Number to COSPAR International Designator
 *
 * ILRS Satellite Identifier - 7 digit number based on COSPAR
 * Note: COSPAR ID to ILRS Satellite Identification Algorithm
 *
 * COSPAR ID Format: (YYYY-XXXA)
 *
 * YYYY is the four digit year when the launch vehicle was put in orbit
 * XXX is the sequential launch vehicle number for that year
 * A is the alpha numeric sequence number within a launch
 * Example: LAGEOS-1 COSPAR ID is 1976-039A
 * Explanation: LAGEOS-1 launch vehicle wasplaced in orbit in 1976;
 * was the 39th launch in that year; and LAGEOS-1 was the first object
 * injected into orbit from this launch.
 *
 * ILRS Satellite Identification Format: (YYXXXAA), based on the COSPAR ID
 * Where YY is the two digit year when the launch vehicle was put in orbit
 * Where XXX is the sequential launch vehicle number for that year
 * AA is the numeric sequence number within a launch
 * Example: LAGEOS-1 ILRS Satellite ID is 7603901
 */
//------------------------------------------------------------------------------
std::string Obtype::Ilrs2Cospar(std::string ilrsSatnum)
{

    int year;

    from_string<int>(year,ilrsSatnum.substr(0,2),std::dec);

    if ( year < 50 )
    {
	year += 2000;
    } else {
	year += 1900;
    }

    std::string launchAlpha;

    int index;
    from_string<int>(index,ilrsSatnum.substr(5,2),std::dec);

    static const char alpha[26] = {'A','B','C','D','E','F','G','H','I','J','K',
                  'L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};

    if (index <= 26)
    {
	// Account for zero indexed array so subtract 1
	launchAlpha = alpha[index-1];
    }
    else
    {
	int index2 = -1;
	while (index > 26)
	{
	    index -= 26;
	    index2++;
	}
	launchAlpha = alpha[index2] + alpha[index];
    }

    return GmatStringUtil::ToString(year,2) + ilrsSatnum.substr(2,3) + launchAlpha;

}

//------------------------------------------------------------------------------
// std::string Cospar2Ilrs(std::string cosparSatnum)
//------------------------------------------------------------------------------
/**
 * Convert COSPAR International Designator to ILRS Satellite Number
 *
 * ILRS Satellite Identifier - 7 digit number based on COSPAR
 * Note: COSPAR ID to ILRS Satellite Identification Algorithm
 *
 * COSPAR ID Format: (YYYY-XXXA)
 *
 * YYYY is the four digit year when the launch vehicle was put in orbit
 * XXX is the sequential launch vehicle number for that year
 * A is the alpha numeric sequence number within a launch
 * Example: LAGEOS-1 COSPAR ID is 1976-039A
 * Explanation: LAGEOS-1 launch vehicle wasplaced in orbit in 1976;
 * was the 39th launch in that year; and LAGEOS-1 was the first object
 * injected into orbit from this launch.
 *
 * ILRS Satellite Identification Format: (YYXXXAA), based on the COSPAR ID
 * Where YY is the two digit year when the launch vehicle was put in orbit
 * Where XXX is the sequential launch vehicle number for that year
 * AA is the numeric sequence number within a launch
 * Example: LAGEOS-1 ILRS Satellite ID is 7603901
 */
//------------------------------------------------------------------------------
std::string Obtype::Cospar2Ilrs(std::string cosparSatnum)
{

    int year;

    from_string<int>(year,cosparSatnum.substr(0,4),std::dec);

    if ( year >= 2000 )
    {
	year -= 2000;
    } else {
	year -= 1900;
    }
    
    static const char alpha[26] = {'A','B','C','D','E','F','G','H','I','J','K',
                  'L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'};
    
    int launchInt;
    std::string launchAlpha = cosparSatnum.substr(7,cosparSatnum.length()-6);
    Integer n1 = 0;
    Integer n2 = 0;
    
    if (launchAlpha.length() == 1)
    {
	for (Integer i=0; i < 26; i++)
	{
	    if (launchAlpha[0] == alpha[i])
	    {
		launchInt = i+1;
		break;
	    }
	}
    }
    else if (launchAlpha.length() == 2)
    {
	for (Integer i=0; i < 26; i++)
	{
	    if (launchAlpha[0] == alpha[i])
	    {
		n1 = i+1;
		break;
	    }
	}
	if (n1 > 4)
	{
	    launchInt = 99;
	}
	else
	{	    
	    for (Integer i=0; i < 26; i++)
	    {
		if (launchAlpha[1] == alpha[i])
		{
		    n2 = i+1;
		    break;
		}
	    }
	    launchInt = n1*26 + n2;	
	}
    }
    else if (launchAlpha.length() > 2)
    {
	    // Technically only 7 digits allowed in ILRS
	    launchInt = 99;
    }

    return GmatStringUtil::ToString(year,2) + cosparSatnum.substr(6,3)
	   + GmatStringUtil::ToString(launchInt,2);

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
std::string Obtype::GetDataParameterText(const Integer id) const
{
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
std::string Obtype::GetDataUnits(const Integer id) const
{
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
Integer Obtype::GetDataParameterID(const std::string &str) const
{
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
Gmat::ParameterType Obtype::GetDataParameterType(const Integer id) const
{
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
std::string Obtype::GetDataParameterTypeString(const Integer id) const
{
   return "";
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves Integer data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The Integer data parameter
 *
 */
//------------------------------------------------------------------------------
Integer Obtype::GetIntegerDataParameter(const Integer id) const
{
    return -123456789;
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves Integer data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The Integer data parameter
 *
 */
//------------------------------------------------------------------------------
Integer Obtype::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves Real data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The Real data parameter
 *
 */
//------------------------------------------------------------------------------
Real Obtype::GetRealDataParameter(const Integer id) const
{
    return -1234567.89;
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves Real data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The Real data parameter
 *
 */
//------------------------------------------------------------------------------
Real Obtype::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool Obtype::GetBoolDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool Obtype::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves string data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The string data parameter
 *
 */
//------------------------------------------------------------------------------
std::string Obtype::GetStringDataParameter(const Integer id) const
{
    return "";
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves string data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The string data parameter
 *
 */
//------------------------------------------------------------------------------
std::string Obtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual StringArray GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves string array data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The string array data parameter
 *
 */
//------------------------------------------------------------------------------
StringArray Obtype::GetStringArrayDataParameter(const Integer id) const
{
    StringArray str;
    return str;
}

//------------------------------------------------------------------------------
// virtual StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves string array data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The string array data parameter
 *
 */
//------------------------------------------------------------------------------
StringArray Obtype::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool Obtype::IsParameterRequired(const Integer id) const
{
   return false;
}


//---------------------------------------------------------------------------
//  bool IsParameterRequired(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by data format
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is required, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool Obtype::IsParameterRequired(const std::string &label) const
{
   return IsParameterRequired(GetParameterID(label));
}