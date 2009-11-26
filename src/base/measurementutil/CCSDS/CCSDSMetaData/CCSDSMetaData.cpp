//$Header$
//------------------------------------------------------------------------------
//                             CCSDSMetaData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/11/04
//
/**
 *
 * This class specifies the CCSDS MetaData base class from which the individual
 * meta data constructs for each of the CCSDS data formats is derived
 *
 */
//------------------------------------------------------------------------------

#include "CCSDSMetaData.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSMetaData::CCSDS_RATE_FRAME[EndCCSDSRateFrameReps] =
{
    "REF_FRAME_A",
    "REF_FRAME_B"
};

const std::string CCSDSMetaData::CCSDS_QUATERNION_TYPE[EndCCSDSQuaternionTypeReps] =
{
    "FIRST",
    "LAST"
};

const std::string CCSDSMetaData::CCSDS_ATTITUDE_TYPE[EndCCSDSAttitudeTypeReps] =
{
    "QUATERNION",
    "QUATERNION/DERIVATIVE",
    "QUATERNION/RATE",
    "EULER_ANGLE",
    "EULER_ANGLE/RATE",
    "SPIN",
    "SPIN/NUTATION"
};

const std::string CCSDSMetaData::CCSDS_ATTITUDE_DIR[EndCCSDSAttitudeDirReps] =
{
    "A2B",
    "B2A"
};

//------------------------------------------------------------------------------
//  CCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSMetaData class
 */
//------------------------------------------------------------------------------
CCSDSMetaData::CCSDSMetaData()
{
}

//------------------------------------------------------------------------------
//  CCSDSMetaData(const CCSDSMetaData &md)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSMetaData class
 */
//------------------------------------------------------------------------------
CCSDSMetaData::CCSDSMetaData(const CCSDSMetaData &md)
{
}

//---------------------------------------------------------------------------
//  CCSDSMetaData& operator=(const CCSDSMetaData &md)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSMetaData structures.
 *
 * @param <aemMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSMetaData& CCSDSMetaData::operator=(const CCSDSMetaData &md)
{
    if (&md == this)
        return *this;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSMetaData class
 */
//------------------------------------------------------------------------------
CCSDSMetaData::~CCSDSMetaData()
{
}

//------------------------------------------------------------------------------
//  bool GetBooleanDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
bool CCSDSMetaData::GetBooleanDataParameter(const Integer id) const
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
bool CCSDSMetaData::GetBooleanDataParameter(const std::string &label) const
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
Real CCSDSMetaData::GetRealDataParameter(const Integer id) const
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
Real CCSDSMetaData::GetRealDataParameter(const std::string &label) const
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
Integer CCSDSMetaData::GetIntegerDataParameter(const Integer id) const
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
Integer CCSDSMetaData::GetIntegerDataParameter(const std::string &label) const
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
std::string CCSDSMetaData::GetStringDataParameter(const Integer id) const
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
std::string CCSDSMetaData::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSMetaData::GetStringArrayDataParameter(const Integer id) const
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
StringArray CCSDSMetaData::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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
bool CCSDSMetaData::SetDataParameter(const Integer id, const Real &value)
{
    return false;
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
bool CCSDSMetaData::SetDataParameter(const std::string &label, const Real &value)
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
bool CCSDSMetaData::SetDataParameter(const Integer id, const Integer &value)
{
    return false;
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
bool CCSDSMetaData::SetDataParameter(const std::string &label, const Integer &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const bool &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a boolean parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//---------------------------------------------------------------------------
bool CCSDSMetaData::SetDataParameter(const Integer id, const bool &value)
{
    return false;
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const bool &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a boolean parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool CCSDSMetaData::SetDataParameter(const std::string &label, const bool &value)
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
bool CCSDSMetaData::SetDataParameter(const Integer id, const std::string &value)
{
    return false;
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
bool CCSDSMetaData::SetDataParameter(const std::string &label, const std::string &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const Integer id, const StringArray &value) const
//------------------------------------------------------------------------------
/**
 * Method to set the value of a StringArray parameter.
 *
 * @param <id> Integer ID identifying the parameter to be set
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 */
//------------------------------------------------------------------------------
bool CCSDSMetaData::SetDataParameter(const Integer id, const StringArray &value)
{
    return false;
}

//------------------------------------------------------------------------------
// bool SetDataParameter(const std::string &label, const StringArray &value)
//------------------------------------------------------------------------------
/**
 * Method to set the value of a StringArray parameter.
 *
 * @param <label> String label identifying the parameter to be set.
 * @param <value> The desired value to be set
 * @return Boolean success or failure
 *
 */
//------------------------------------------------------------------------------
bool CCSDSMetaData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(bool value) const
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
bool CCSDSMetaData::IsParameterDefined(bool value) const
{
    if (&value == NULL)
        return false;
    else
        return true;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(StringArray value) const
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
bool CCSDSMetaData::IsParameterDefined(StringArray value) const
{
    if (&value == NULL || value == GmatBase::STRINGARRAY_PARAMETER_UNDEFINED)
        return false;
    else
        return true;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(std::string value) const
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
bool CCSDSMetaData::IsParameterDefined(std::string value) const
{
    if (&value == NULL || value == GmatBase::STRING_PARAMETER_UNDEFINED)
        return false;
    else
        return true;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(Real value) const
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
bool CCSDSMetaData::IsParameterDefined(Real value) const
{
    if (&value == NULL || value == GmatBase::REAL_PARAMETER_UNDEFINED)
        return false;
    else
        return true;
}

//---------------------------------------------------------------------------
//  bool IsParameterDefined(Integer value) const
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
bool CCSDSMetaData::IsParameterDefined(Integer value) const
{
    if (&value == NULL || value == GmatBase::INTEGER_PARAMETER_UNDEFINED)
        return false;
    else
        return true;
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
std::string CCSDSMetaData::GetAttitudeDirText(const Integer id) const
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
Integer CCSDSMetaData::GetAttitudeDirID(const std::string &str) const
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
//  std::string  GetQuaternionTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the quaternion type keyword for a specific ID
 *
 * @param <id> The quaternion type id
 * @return The quaternion type keyword
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSMetaData::GetQuaternionTypeText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSQuaternionTypeReps))
       return CCSDS_QUATERNION_TYPE[id];
   else
       return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  Integer  GetQuaternionTypeID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Function to obtain the ID associated with an quaternion type keyword
 *
 * @param <str> The quaternion type keyword
 * @return The quaternion type id
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSMetaData::GetQuaternionTypeID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSQuaternionTypeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_QUATERNION_TYPE[i]))
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
std::string CCSDSMetaData::GetAttitudeTypeText(const Integer id) const
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
Integer CCSDSMetaData::GetAttitudeTypeID(const std::string &str) const
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
std::string CCSDSMetaData::GetRateFrameText(const Integer id) const
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
Integer CCSDSMetaData::GetRateFrameID(const std::string &str) const
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
