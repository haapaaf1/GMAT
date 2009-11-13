#include "CCSDSData.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSData::CCSDS_RATE_FRAME[EndCCSDSRateFrameReps] =
{
    "REF_FRAME_A",
    "REF_FRAME_B"
};

const std::string CCSDSData::CCSDS_ATTITUDE_TYPE[EndCCSDSAttitudeTypeReps] =
{
    "QUATERNION",
    "QUATERNION/DERIVATIVE",
    "QUATERNION/RATE",
    "EULER_ANGLE",
    "EULER_ANGLE/RATE",
    "SPIN",
    "SPIN/NUTATION"
};

const std::string CCSDSData::CCSDS_ATTITUDE_DIR[EndCCSDSAttitudeDirReps] =
{
    "A2B",
    "B2A"
};

//------------------------------------------------------------------------------
//  CCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::CCSDSData()
{
}

//------------------------------------------------------------------------------
//  CCSDSData(const CCSDSData &data)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::CCSDSData(const CCSDSData &data)
{
}

//---------------------------------------------------------------------------
//  CCSDSData& operator=(const CCSDSData &data)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSData structures.
 *
 * @param <aemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSData& CCSDSData::operator=(const CCSDSData &data)
{
    if (&data == this)
        return *this;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::~CCSDSData()
{
}

//------------------------------------------------------------------------------
//  bool GetBooleanDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
bool CCSDSData::GetBooleanDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
//  bool GetBooleanDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
bool CCSDSData::GetBooleanDataParameter(const std::string &label) const
{
   return GetBooleanDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
//  Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Real CCSDSData::GetRealDataParameter(const Integer id) const
{
    return GmatBase::REAL_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
//  Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSData::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSData::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
//  std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetStringDataParameter(const Integer id) const
{
    return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSData::GetStringArrayDataParameter(const Integer id) const
{
    return GmatBase::STRINGARRAY_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, bool value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSData::IsParameterDefined(const Integer id, bool value) const
{
    if (GetDataParameterType(id) == Gmat::BOOLEAN_TYPE)
    {
        if (&value == NULL)
            return false;
        else
            return true;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, StringArray value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSData::IsParameterDefined(const Integer id, StringArray value) const
{
    if (GetDataParameterType(id) == Gmat::STRINGARRAY_TYPE)
    {
        if (&value == NULL || value == GmatBase::STRINGARRAY_PARAMETER_UNDEFINED)
            return false;
        else
            return true;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, std::string value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSData::IsParameterDefined(const Integer id, std::string value) const
{
    if (GetDataParameterType(id) == Gmat::STRING_TYPE)
    {
        if (&value == NULL || value == GmatBase::STRING_PARAMETER_UNDEFINED)
            return false;
        else
            return true;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, Real value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSData::IsParameterDefined(const Integer id, Real value) const
{
    if (GetDataParameterType(id) == Gmat::REAL_TYPE)
    {
        if (&value == NULL || value == GmatBase::REAL_PARAMETER_UNDEFINED)
            return false;
        else
            return true;
    }
    else
        return false;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(const Integer id, Integer value) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is defined.
 *
 * @param <id> ID of the parameter.
 * @param <value> Value of the parameter.
 *
 * @return true if the parameter is defined, false if not (the default)
 */
//---------------------------------------------------------------------------
bool CCSDSData::IsParameterDefined(const Integer id, Integer value) const
{
    if (GetDataParameterType(id) == Gmat::INTEGER_TYPE)
    {
        if (&value == NULL || value == GmatBase::INTEGER_PARAMETER_UNDEFINED)
            return false;
        else
            return true;
    }
    else
        return false;
}

//------------------------------------------------------------------------------
//  std::string  GetAttitudeDirText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the attitude type keyword for a specific ID
 *
 * @param <id> The attitude direction id
 * @return The attitude direction keyword
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetAttitudeDirText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAttitudeDirReps))
   {
      return CCSDS_ATTITUDE_DIR[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetAttitudeDirID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an attitude type keyword
 *
 * @param <str> The attitude direction keyword
 * @return The attitude direction id
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSData::GetAttitudeDirID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAttitudeDirReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_ATTITUDE_DIR[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetAttitudeTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the attitude type keyword for a specific ID
 *
 * @param <id> The attitude type id
 * @return The attitude type keyword
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetAttitudeTypeText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAttitudeTypeReps))
   {
      return CCSDS_ATTITUDE_TYPE[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetAttitudeTypeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an attitude type keyword
 *
 * @param <str> The attitude type keyword
 * @return The attitude type id
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSData::GetAttitudeTypeID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAttitudeTypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_ATTITUDE_TYPE[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetRateFrameText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the attitude type keyword for a specific ID
 *
 * @param <id> The rate frame type id
 * @return The rate frame type keyword
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSData::GetRateFrameText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSRateFrameReps))
   {
      return CCSDS_RATE_FRAME[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetRateFrameID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an rate frame keyword
 *
 * @param <str> The rate frame keyword
 * @return The rate frame id
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSData::GetRateFrameID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSRateFrameReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_RATE_FRAME[i]))
	{
	    return i;
	}
   }

   return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}
