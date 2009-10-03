#include "TLEObType.hpp"

//---------------------------------
//   data
//---------------------------------
const std::string TLEObType::DATATYPE_DESCRIPTIONS[EndTLETypeReps] =
{
    "Bstar",
    "Inclination",
    "RAAN",
    "Eccentricity",
    "ArgumentOfPerigee",
    "MeanAnomaly",
    "KozaiMeanMotion"
};
    
const std::string TLEObType::TIMESYSTEM_DESCRIPTIONS[EndTLETimeReps] =
{
    "UT"
};

const std::string TLEObType::TLE_FILEFORMAT_DESCRIPTIONS[EndTLEDataReps] =
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

const std::string TLEObType::TLE_UNIT_DESCRIPTIONS[EndTLEDataReps] =
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

const Gmat::ParameterType TLEObType::TLE_PARAMETER_TYPE[EndTLEDataReps] =
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

const bool TLEObType::TLE_IS_REQUIRED[EndTLEDataReps] =
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
//  TLEObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the ObType class
 */
//------------------------------------------------------------------------------
TLEObType::TLEObType() : ObType("TLEObType",""),
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
    objectTypeNames.push_back("TLEObType");
}

//---------------------------------------------------------------------------
//  TLEObType(const TLEObType &tleOb);
//---------------------------------------------------------------------------
/**
 * Constructs base ObType structures used in derived classes, by copying
 * the input instance (copy constructor).
 *
 * @param <TLEob>  ObType instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
TLEObType::TLEObType(const TLEObType &tleOb) :
    ObType(tleOb),
    satnum(tleOb.satnum),
    securityClassification(tleOb.securityClassification),
    intlDesignator(tleOb.intlDesignator),
    epochYear(tleOb.epochYear),
    epochDayOfYear(tleOb.epochDayOfYear),
    ndotby2(tleOb.ndotby2),
    nddotby6(tleOb.nddotby6),
    bstar(tleOb.bstar),
    ephemerisType(tleOb.ephemerisType),
    elementNum(tleOb.elementNum),
    inclination(tleOb.inclination),
    raan(tleOb.raan),
    eccentricity(tleOb.eccentricity),
    argPerigee(tleOb.argPerigee),
    meanAnomaly(tleOb.meanAnomaly),
    meanMotion(tleOb.meanMotion),
    revolutionNum(tleOb.revolutionNum)
{
}

//---------------------------------------------------------------------------
//  TLEObType& operator=(const TLEObType &tleOb)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <tleOb> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const TLEObType& TLEObType::operator=(const TLEObType &tleOb)
{
   if (&tleOb == this)
      return *this;

    satnum = tleOb.satnum;
    securityClassification = tleOb.securityClassification;
    intlDesignator = tleOb.intlDesignator;
    epochYear = tleOb.epochYear;
    epochDayOfYear = tleOb.epochDayOfYear;
    ndotby2 = tleOb.ndotby2;
    nddotby6 = tleOb.nddotby6;
    bstar = tleOb.bstar;
    ephemerisType = tleOb.ephemerisType;
    elementNum = tleOb.elementNum;
    inclination = tleOb.inclination;
    raan = tleOb.raan;
    eccentricity = tleOb.eccentricity;
    argPerigee = tleOb.argPerigee;
    meanAnomaly = tleOb.meanAnomaly;
    meanMotion = tleOb.meanMotion;
    revolutionNum = tleOb.revolutionNum;
    
   return *this;
}

//------------------------------------------------------------------------------
//  ~TLEObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the ObType class
 */
//------------------------------------------------------------------------------
TLEObType::~TLEObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessTLEDataFile.
 *
 * @return clone of the ProcessTLEDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* TLEObType::Clone() const
{
   GmatBase *clone = new TLEObType(*this);
   return (clone);
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
 bool TLEObType::CheckDataAvailability(const std::string str) const
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
std::string TLEObType::GetDataParameterText(const Integer id) const
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
std::string TLEObType::GetDataUnits(const Integer id) const
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
Integer TLEObType::GetDataParameterID(const std::string &str) const
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
Gmat::ParameterType TLEObType::GetDataParameterType(const Integer id) const
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
std::string TLEObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer TLEObType::GetIntegerDataParameter(const Integer id) const
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
Integer TLEObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string TLEObType::GetStringDataParameter(const Integer id) const
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
std::string TLEObType::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
Real TLEObType::GetRealDataParameter(const Integer id) const
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
Real TLEObType::GetRealDataParameter(const std::string &label) const
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
const std::string* TLEObType::GetDataTypes() const
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
std::string TLEObType::GetDataTypeText(const Integer &id) const
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
Integer TLEObType::GetDataTypeID(const std::string &label)
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
const std::string* TLEObType::GetTimeSystems() const
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
std::string TLEObType::GetTimeSystemText(const Integer &id) const
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
Integer TLEObType::GetTimeSystemID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const TLEObType &myTLE)
//------------------------------------------------------------------------------
/**
 * Formats TLEObType value and sends to output stream.
 *
 * @param  output  Output stream
 * @param  myTLE   TLE observation to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const TLEObType *myTLE)
{
    // TLE's have a maximum length of 68 characters plus a 1 character checksum
    ostringstream buffer;

    // junk variable
    ostringstream temp;

    buffer << setw(2) << "1 ";
    buffer << setw(5) << setfill('0') << right << myTLE->satnum;
    buffer << setw(1) << myTLE->securityClassification;
    buffer << " " << setw(8) << left << setfill(' ') << myTLE->intlDesignator;
    buffer << " " << setw(2) << right << setfill('0') << myTLE->epochYear;
    buffer.precision(11);
    buffer << setw(12) << left << setfill(' ') << myTLE->epochDayOfYear;

    // Format first time derivative of the mean motion
    if (myTLE->ndotby2 == 0)
    {
        buffer << " " << setw(10) << "+.00000000";
    }
    else
    {
        char buffer2[15];
        sprintf(buffer2,"%.8f",myTLE->ndotby2);
        string str = buffer2;
        pcrecpp::RE("0\\.").GlobalReplace(".", &str);
        temp.str(std::string());
        temp << str;
        if (pcrecpp::RE("^-.*").FullMatch(temp.str()))
        {
            buffer << " " << setw(10) << setfill(' ') << right << temp.str();
        }
        else
        {
            buffer << " +" << setw(9) << left << temp.str();
        }
    }

    if (myTLE->nddotby6 == 0)
    {
        buffer << " " << setw(8) << "+00000-0";
    }
    else
    {
        char buffer2[15];
        // TODO: Find a better way to move decimal point
        // The multiplication by 10 is a lazy way
        sprintf(buffer2,"%+.4E",myTLE->nddotby6*10);
        string str = buffer2;
        pcrecpp::RE("\\.").GlobalReplace("", &str);
        pcrecpp::RE("E").GlobalReplace("", &str);
        pcrecpp::RE("-0").GlobalReplace("-", &str);
        pcrecpp::RE("+0").GlobalReplace("+", &str);
        temp.str(std::string());
        temp << str;
        buffer << " " << setw(8) << right << temp.str();
    }

    if (myTLE->bstar == 0)
    {
        buffer << " " << setw(8) << "+00000-0";
    }
    else
    {
        char buffer2[15];
        // TODO: Find a better way to move decimal point
        // The multiplication by 10 is a lazy way
        sprintf(buffer2,"%+.4E",myTLE->bstar*10);
        string str = buffer2;
        pcrecpp::RE("\\.").GlobalReplace("", &str);
        pcrecpp::RE("E").GlobalReplace("", &str);
        pcrecpp::RE("-0").GlobalReplace("-", &str);
        pcrecpp::RE("+0").GlobalReplace("+", &str);
        temp.str(std::string());
        temp << str;
        buffer << " " << setw(8) << right << temp.str();
    }

    buffer << " " << setw(1) << myTLE->ephemerisType;
    buffer << " " << setw(4) << right << myTLE->elementNum;

    output << buffer.str();
    output << setw(1) << TLECheckSum(buffer.str()) << endl;

    // Start output of line 2
    buffer.str(std::string());
    buffer << setw(2) << "2 ";
    buffer << setw(5) << setfill('0') << right << myTLE->satnum;
    char buffer3[15];
    sprintf(buffer3,"%08.4f",myTLE->inclination);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%08.4f",myTLE->raan);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%.7f",myTLE->eccentricity);
    string str = buffer3;
    pcrecpp::RE("0\\.").GlobalReplace("", &str);
    buffer << " " << setw(7) << right << str;
    sprintf(buffer3,"%08.4f",myTLE->argPerigee);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%08.4f",myTLE->meanAnomaly);
    buffer << " " << setw(8) << right << buffer3;
    sprintf(buffer3,"%011.8f",myTLE->meanMotion);
    buffer << " " << setw(11) << right << buffer3;
    buffer << setw(5) << setfill(' ') << right << myTLE->revolutionNum;

    output << buffer.str();
    output << setw(1) << TLECheckSum(buffer.str()) << endl;

    output << myTLE->tleCommentLine << endl;
   
   return output;
}

//------------------------------------------------------------------------------
//  Integer  TLECheckSum(const std::string &str)
//------------------------------------------------------------------------------
/**
 * This method computes the checksum for a string of TLE data
 *
 * @param <str> String of data
 *
 * @return Integer checksum modulo 10
 */
//------------------------------------------------------------------------------
Integer TLECheckSum(const std::string &str)
{

    Integer checksum = 0;
    int num = 0 ;

    for ( Integer pos = 0; pos < (Integer)str.length(); ++pos )
    {
        // If it's a number, add the number to the checksum
        // if it's a minus sign, add +1
        // ignore everything else
        if (pcrecpp::RE("(\\d)").FullMatch(str.substr(pos,1),&num))
        {
            checksum += num;
            num = 0;
        }
        else if (pcrecpp::RE("-").FullMatch(str.substr(pos,1)))
        {
            checksum += 1;
        }
    }

    return GmatMathUtil::Mod(checksum,10);

}