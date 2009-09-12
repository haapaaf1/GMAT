#include "TLEObtype.hpp"

//---------------------------------
//   data
//---------------------------------
const std::string TLEObtype::DATATYPE_DESCRIPTIONS[EndTLETypeReps] =
{
    "Bstar",
    "Inclination",
    "RAAN",
    "Eccentricity",
    "ArgumentOfPerigee",
    "MeanAnomaly",
    "KozaiMeanMotion"
};
    
const std::string TLEObtype::TIMESYSTEM_DESCRIPTIONS[EndTLETimeReps] =
{
    "UT"
};

const std::string TLEObtype::TLE_FILEFORMAT_DESCRIPTIONS[EndTLEDataReps] =
{
	"Satnum",
	"SecurityClassification",
	"IntlDesignator",
	"EpochYear",
	"EpochDayOfYear",
	"Ndotby2",
	"Nddotby6",
	"Bstar",
	"EphemerisType",
	"ElementNum",
	"Inclination",
	"Raan",
	"Eccentricity",
	"ArgPerigee",
	"MeanAnomaly",
	"KozaiMeanMotion",
	"RevolutionNum"
};

const std::string TLEObtype::TLE_UNIT_DESCRIPTIONS[EndTLEDataReps] =
{
	"",
	"",
	"",
	"years",
	"DayOfYear",
	"rev/sec^2",
	"rad/sec^3",
	"1/EarthRadii",
	"",
	"",
	"deg",
	"deg",
	"",
	"deg",
	"deg",
	"rev/day",
	""
};

const Gmat::ParameterType TLEObtype::TLE_PARAMETER_TYPE[EndTLEDataReps] =
{
	Gmat::INTEGER_TYPE,
	Gmat::STRING_TYPE,
	Gmat::STRING_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::INTEGER_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::REAL_TYPE,
	Gmat::INTEGER_TYPE
};

const bool TLEObtype::TLE_IS_REQUIRED[EndTLEDataReps] =
{
    true,
    true,
    false,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    false
};

//------------------------------------------------------------------------------
//  TLEObtype()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
TLEObtype::TLEObtype() :
	satnum(0),
	securityClassification("U"),
	intlDesignator(GmatBase::STRING_PARAMETER_UNDEFINED),
	epochYear(0),
	epochDayOfYear(0),
	ndotby2(0),
	nddotby6(0),
	bstar(0),
	ephemerisType(0),
	elementNum(0),
	inclination(0),
	raan(0),
	eccentricity(0),
	argPerigee(0),
	meanAnomaly(0),
	meanMotion(0),
	revolutionNum(0)
{
}

//------------------------------------------------------------------------------
//  ~TLEObtype()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
TLEObtype::~TLEObtype()
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
 bool TLEObtype::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";
    
    for (Integer i = 0; i < EndTLEDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(TLE_FILEFORMAT_DESCRIPTIONS[i]))
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
std::string TLEObtype::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndTLEDataReps))
   {
      return TLE_FILEFORMAT_DESCRIPTIONS[id];
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
std::string TLEObtype::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndTLEDataReps))
   {
      return TLE_UNIT_DESCRIPTIONS[id];
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
Integer TLEObtype::GetDataParameterID(const std::string &str) const
{
   for (Integer i = 0; i < EndTLEDataReps; i++)
   {
      if (str == TLE_FILEFORMAT_DESCRIPTIONS[i])
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
Gmat::ParameterType TLEObtype::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndTLEDataReps))
      return TLE_PARAMETER_TYPE[id];

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
std::string TLEObtype::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer TLEObtype::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case TLE_SATNUM_ID:

            return satnum;

        case TLE_EPOCHYEAR_ID:

            return epochYear;

        case TLE_EPHEMERISTYPE_ID:

            return ephemerisType;

        case TLE_ELEMENTNUM_ID:

            return elementNum;

        case TLE_REVOLUTIONNUM_ID:

            return revolutionNum;

        default:

            return GmatBase::INTEGER_PARAMETER_UNDEFINED;
    }
   
}


//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Integer TLEObtype::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string TLEObtype::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case TLE_SECURITYCLASSIFICATION_ID:

            return securityClassification;

        case TLE_INTLDESIGNATOR_ID:

            return intlDesignator;

        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
std::string TLEObtype::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real TLEObtype::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

       case TLE_EPOCHDAYOFYEAR_ID:

            return epochDayOfYear;

        case TLE_NDOTBY2_ID:

            return ndotby2;

        case TLE_NDDOTBY6_ID:

            return nddotby6;

        case TLE_BSTAR_ID:

            return bstar;

        case TLE_INCLINATION_ID:

            return inclination;

        case TLE_RAAN_ID:

            return raan;

        case TLE_ECCENTRICITY_ID:

            return eccentricity;

        case TLE_ARGPERIGEE_ID:

            return argPerigee;

        case TLE_MEANANOMALY_ID:

            return meanAnomaly;

        case TLE_MEANMOTION_ID:

            return meanMotion;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}


//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see GmatBase
 */
//------------------------------------------------------------------------------
Real TLEObtype::GetRealDataParameter(const std::string &label) const
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
const std::string* TLEObtype::GetDataTypes() const
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
std::string TLEObtype::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndTLETypeReps))
   {
      return DATATYPE_DESCRIPTIONS[id];
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
Integer TLEObtype::GetDataTypeID(const std::string &label)
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
const std::string* TLEObtype::GetTimeSystems() const
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
std::string TLEObtype::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndTLETimeReps))
   {
      return TIMESYSTEM_DESCRIPTIONS[id];
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
Integer TLEObtype::GetTimeSystemID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// bool AdvanceToNextOb()
//------------------------------------------------------------------------------
/**
 * Returns the next observation from the vector container.
 */
//------------------------------------------------------------------------------
//bool ProcessTLEData::AdvanceToNextOb() 
//{
//    ++i;
//    if ( i == tleData.end() ) return false;
//    return true;
//}

//------------------------------------------------------------------------------
// bool BackUpToPreviousOb()
//------------------------------------------------------------------------------
/**
 * Returns the previous observation from the vector container.
 */
//------------------------------------------------------------------------------
//bool ProcessTLEData::BackUpToPreviousOb() 
//{
//    --i;
//    if ( i == tleData.begin() ) return false;
//    return true;
//}