//$Header$
//------------------------------------------------------------------------------
//                             CCSDSObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/09/02
//
/**
 *
 * This class specifies the base observation data type from which the
 * various data format observation types flow.
 *
 */
//------------------------------------------------------------------------------

#include <ObType.hpp>

//---------------------------------
//  static data
//---------------------------------
const std::string ObType::OBTYPES[EndObTypeReps] =
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
//  ObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
ObType::ObType(const std::string &type, const std::string &name) :
   GmatBase       (Gmat::OBTYPE, type, name),
    epoch(1950,1,1,0,0,0),
    satelliteID(0),
    internationalDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
    sensorID(0)
{
   objectTypes.push_back(Gmat::OBTYPE);
   objectTypeNames.push_back("ObType");
}

//------------------------------------------------------------------------------
//  ~ObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
ObType::~ObType()
{
}

//---------------------------------------------------------------------------
//  ObType(const ObType &ob);
//---------------------------------------------------------------------------
/**
 * Constructs base ObType structures used in derived classes, by copying
 * the input instance (copy constructor).
 *
 * @param <ob>  ObType instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
ObType::ObType(const ObType &ob) : GmatBase(ob),
    epoch(ob.epoch),
    satelliteID(ob.satelliteID),
    internationalDesignator(ob.internationalDesignator),
    sensorID(ob.sensorID)
{
}

//---------------------------------------------------------------------------
//  ObType& operator=(const ObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const ObType& ObType::operator=(const ObType &ob)
{
   if (&ob == this)
      return *this;

   epoch = ob.epoch;
   satelliteID = ob.satelliteID;
   internationalDesignator = ob.internationalDesignator;
   sensorID = ob.sensorID;
    
   return *this;
}

//------------------------------------------------------------------------------
// A1Date& GetEpoch()
//------------------------------------------------------------------------------
/**
 * Code used to obtain the epoch
 *
 * @return The epoch in GMAT A1 time
 *
 */
//------------------------------------------------------------------------------
A1Date& ObType::GetEpoch()
{
   return epoch;
}

//------------------------------------------------------------------------------
// Integer GetSatID()
//------------------------------------------------------------------------------
/**
 * Code used to obtain the satellite ID
 *
 * @return The satellite ID corresponding to the NORAD catalog
 *
 */
//------------------------------------------------------------------------------
Integer ObType::GetSatID()
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
std::string ObType::GetInternationalDesignator()
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
Integer ObType::GetSensorID()
{
   return sensorID;
}

//------------------------------------------------------------------------------
//  const std::string* GetObTypes()
//------------------------------------------------------------------------------
/**
 * Returns the list of valid obtype keywords
 *
 * @return String array of obtype keywords
 */
//------------------------------------------------------------------------------
const std::string* ObType::GetObTypes()
{
    return OBTYPES;
}

//------------------------------------------------------------------------------
//  Integer GetObTypeID(const std::string keyword)
//------------------------------------------------------------------------------
/**
 * Obtains the obtype ID associated with a given keyword.
 *
 * @param <keyword> Keyword associated with a particular obtype
 * @return Integer ID corresponding to desired obtype; -1 if not found
 */
//------------------------------------------------------------------------------
Integer ObType::GetObTypeID(const std::string keyword)
{
    std::string regex = "^" + keyword + "$";
    
    for (Integer i = 0; i < EndObTypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(OBTYPES[i]))
        {
            return i;
        }
    }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;;
}

//------------------------------------------------------------------------------
//  std::string GetObType(Integer myID)
//------------------------------------------------------------------------------
/**
 * Obtains the obtype keyword associated with a given ID.
 *
 * @param <myID> ID associated with a particular obtype
 * @return String keyword corresponding to desired obtype; "" if not found
 */
//------------------------------------------------------------------------------
std::string ObType::GetObType(Integer myID)
{
    if(myID >= 0 && myID < EndObTypeReps)
    {
        return OBTYPES[myID];
    }
    else
    {
	return GmatBase::STRING_PARAMETER_UNDEFINED;;
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
std::string Ilrs2Cospar(std::string ilrsSatnum)
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
std::string Cospar2Ilrs(std::string cosparSatnum)
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
std::string ObType::GetDataParameterText(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;;
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
std::string ObType::GetDataUnits(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;;
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
Integer ObType::GetDataParameterID(const std::string &str) const
{
   return GmatBase::INTEGER_PARAMETER_UNDEFINED;;
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
Gmat::ParameterType ObType::GetDataParameterType(const Integer id) const
{
   return Gmat::UNKNOWN_PARAMETER_TYPE;
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
std::string ObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer ObType::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;;
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
Integer ObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual IntegerArray GetIntegerArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
IntegerArray ObType::GetIntegerArrayDataParameter(const Integer id) const
{

    return IntegerArray(1,GmatBase::INTEGER_PARAMETER_UNDEFINED);
}


//------------------------------------------------------------------------------
// virtual IntegerArray GetIntegerArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
IntegerArray ObType::GetIntegerArrayDataParameter(const std::string &label) const
{
   return GetIntegerArrayDataParameter(GetDataParameterID(label));
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
Real ObType::GetRealDataParameter(const Integer id) const
{
    return GmatBase::REAL_PARAMETER_UNDEFINED;;
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
Real ObType::GetRealDataParameter(const std::string &label) const
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
bool ObType::GetBoolDataParameter(const Integer id) const
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
bool ObType::GetBoolDataParameter(const std::string &label) const
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
std::string ObType::GetStringDataParameter(const Integer id) const
{
    return GmatBase::STRING_PARAMETER_UNDEFINED;;
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
std::string ObType::GetStringDataParameter(const std::string &label) const
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
StringArray ObType::GetStringArrayDataParameter(const Integer id) const
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
StringArray ObType::GetStringArrayDataParameter(const std::string &label) const
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
bool ObType::IsParameterRequired(const Integer id) const
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
bool ObType::IsParameterRequired(const std::string &label) const
{
   return IsParameterRequired(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Bool Validate() const
//------------------------------------------------------------------------------
/**
 * Code used to determine if the data is valid
 *
 * @return Boolean true or false
 *
 */
//------------------------------------------------------------------------------
bool ObType::Validate() const
{
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
 bool ObType::CheckDataAvailability(const std::string str) const
{
   return false;
}
 
//------------------------------------------------------------------------------
// std::string Overpunch(const Real &number )
//------------------------------------------------------------------------------
/**
 * Converts numeric value to overpunch code version for output in text file.
 */
//------------------------------------------------------------------------------
std::string Overpunch(const Real &number )
{
    ostringstream numstr;
    int digit;

    if (number < 0)
    {

        // convert number to stringstream
        // making sure to strip the negative sign
        numstr << -number << flush;

        // extract last digit to determine which overpunch value to use
        if (!from_string<int>(digit,numstr.str().substr(numstr.str().length()-1,1),std::dec))
        {
            return "";
        };

        // Remove last digit to be replaced with overpunched character
        numstr.str().erase(numstr.str().length() - 1, 1);

        if (digit == 0)
        {
            numstr << "}";
        }
        else if (digit == 1)
        {
            numstr << "J";
        }
        else if (digit == 2)
        {
            numstr << "K";
        }
        else if (digit == 3)
        {
            numstr << "L";
        }
        else if (digit == 4)
        {
            numstr << "M";
        }
        else if (digit == 5)
        {
            numstr << "N";
        }
        else if (digit == 6)
        {
            numstr << "O";
        }
        else if (digit == 7)
        {
            numstr << "P";
        }
        else if (digit == 8)
        {
            numstr << "Q";
        }
        else if (digit == 9)
        {
            numstr << "R";
        }
        else
        {
            return "";
        }
    }
    else
    {

        // conver number to stringstream
        numstr << number << flush;

        // extract last digit to determine which overpunch value to use
        if (!from_string<int>(digit,numstr.str().substr(numstr.str().length()-1,1),std::dec))
        {
            return "";
        };

        // Remove last digit to be replaced with overpunched character
        numstr.str().erase(numstr.str().length() - 1, 1);

        if (digit == 0)
        {
            numstr << "{";
        }
        else if (digit == 1)
        {
            numstr << "A";
        }
        else if (digit == 2)
        {
            numstr << "B";
        }
        else if (digit == 3)
        {
            numstr << "C";
        }
        else if (digit == 4)
        {
            numstr << "D";
        }
        else if (digit == 5)
        {
            numstr << "E";
        }
        else if (digit == 6)
        {
            numstr << "F";
        }
        else if (digit == 7)
        {
            numstr << "G";
        }
        else if (digit == 8)
        {
            numstr << "H";
        }
        else if (digit == 9)
        {
            numstr << "I";
        }
        else
        {
            return "";
        }
    }

    return numstr.str();
}
