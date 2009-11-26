//$Header$
//------------------------------------------------------------------------------
//                             SpacecraftInertiaCCSDSData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the Spacecraft Inertia data construct that is used
 * by the CCSDS Attitude Parameter Message format.
 *
 */
//------------------------------------------------------------------------------

#include "SpacecraftInertiaCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string SpacecraftInertiaCCSDSData::CCSDS_SPACECRAFTINERTIA_KEYWORDS[EndSpacecraftInertiaCCSDSDataDataReps] =
{
    "INERTIA_REF_FRAME",
    "I11",
    "I22",
    "I33",
    "I12",
    "I13",
    "I23",
    "COMMENT"
};

const std::string SpacecraftInertiaCCSDSData::CCSDS_UNIT_DESCRIPTIONS[EndSpacecraftInertiaCCSDSDataDataReps] =
{
    "",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "",
};

const std::string SpacecraftInertiaCCSDSData::CCSDS_FILEFORMAT_DESCRIPTIONS[EndSpacecraftInertiaCCSDSDataDataReps] =
{
    "Spacecraft Inertia Ref Frame",
    "Spacecraft Inertia Component (1,1)",
    "Spacecraft Inertia Component (2,2)",
    "Spacecraft Inertia Component (3,3)",
    "Spacecraft Inertia Component (1,2)",
    "Spacecraft Inertia Component (1,3)",
    "Spacecraft Inertia Component (2,3)",
    "Spacecraft Inertia Comments"
};

const bool SpacecraftInertiaCCSDSData::CCSDS_IS_REQUIRED[EndSpacecraftInertiaCCSDSDataDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType SpacecraftInertiaCCSDSData::CCSDS_PARAMETER_TYPE[EndSpacecraftInertiaCCSDSDataDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  SpacecraftInertiaCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SpacecraftInertiaCCSDSData::SpacecraftInertiaCCSDSData() : CCSDSData(),
    inertiaRefFrame(GmatBase::STRING_PARAMETER_UNDEFINED),
    i11(GmatBase::REAL_PARAMETER_UNDEFINED),
    i22(GmatBase::REAL_PARAMETER_UNDEFINED),
    i33(GmatBase::REAL_PARAMETER_UNDEFINED),
    i12(GmatBase::REAL_PARAMETER_UNDEFINED),
    i13(GmatBase::REAL_PARAMETER_UNDEFINED),
    i23(GmatBase::REAL_PARAMETER_UNDEFINED),
    comments()
{
}

//------------------------------------------------------------------------------
//  SpacecraftInertiaCCSDSData(const SpacecraftInertiaCCSDSData &si)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
SpacecraftInertiaCCSDSData::SpacecraftInertiaCCSDSData(const SpacecraftInertiaCCSDSData &si) :
    CCSDSData(si),
    inertiaRefFrame(si.inertiaRefFrame),
    i11(si.i11),
    i22(si.i22),
    i33(si.i33),
    i12(si.i12),
    i13(si.i13),
    i23(si.i23),
    comments(si.comments)
{
}

//---------------------------------------------------------------------------
//  SpacecraftInertiaCCSDSData& operator=(const SpacecraftInertiaCCSDSData &si)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <si> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const SpacecraftInertiaCCSDSData& SpacecraftInertiaCCSDSData::operator=(const SpacecraftInertiaCCSDSData &si)
{
   if (&si == this)
      return *this;

   SpacecraftInertiaCCSDSData::operator=(si);

    inertiaRefFrame = si.inertiaRefFrame;
    i11 = si.i11;
    i22 = si.i22;
    i33 = si.i33;
    i12 = si.i12;
    i13 = si.i13;
    i23 = si.i23;
    comments = si.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~SpacecraftInertiaCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
SpacecraftInertiaCCSDSData::~SpacecraftInertiaCCSDSData()
{
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
std::string SpacecraftInertiaCCSDSData::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndSpacecraftInertiaCCSDSDataDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;;
}

//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer SpacecraftInertiaCCSDSData::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSpacecraftInertiaCCSDSDataDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
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
Gmat::ParameterType SpacecraftInertiaCCSDSData::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndSpacecraftInertiaCCSDSDataDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return Gmat::UNKNOWN_PARAMETER_TYPE;
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string SpacecraftInertiaCCSDSData::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;;
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real SpacecraftInertiaCCSDSData::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPACECRAFTINERTIA_I11_ID:

            return i11;

	case CCSDS_SPACECRAFTINERTIA_I22_ID:

            return i22;

	case CCSDS_SPACECRAFTINERTIA_I33_ID:

            return i33;

	case CCSDS_SPACECRAFTINERTIA_I12_ID:

            return i12;

	case CCSDS_SPACECRAFTINERTIA_I13_ID:

            return i13;

	case CCSDS_SPACECRAFTINERTIA_I23_ID:

            return i23;

        default:

            return GmatBase::REAL_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real SpacecraftInertiaCCSDSData::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string SpacecraftInertiaCCSDSData::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPACECRAFTINERTIA_INERTIAREFFRAME_ID:

	    return inertiaRefFrame;

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
std::string SpacecraftInertiaCCSDSData::GetStringDataParameter(const std::string &label) const
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
StringArray SpacecraftInertiaCCSDSData::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPACECRAFTINERTIA_COMMENTS_ID:

	    return comments;

        default:

            return GmatBase::STRINGARRAY_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray SpacecraftInertiaCCSDSData::GetStringArrayDataParameter(const std::string &label) const
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
bool SpacecraftInertiaCCSDSData::SetDataParameter(const Integer id, const Real &value)
{
    switch (id)
    {

	case CCSDS_SPACECRAFTINERTIA_I11_ID:

            i11 = value;
            return true;

	case CCSDS_SPACECRAFTINERTIA_I22_ID:

            i22 = value;
            return true;

	case CCSDS_SPACECRAFTINERTIA_I33_ID:

            i33 = value;
            return true;

	case CCSDS_SPACECRAFTINERTIA_I12_ID:

            i12 = value;
            return true;

	case CCSDS_SPACECRAFTINERTIA_I13_ID:

            i13 = value;
            return true;

	case CCSDS_SPACECRAFTINERTIA_I23_ID:

            i23 = value;
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
bool SpacecraftInertiaCCSDSData::SetDataParameter(const std::string &label, const Real &value)
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
bool SpacecraftInertiaCCSDSData::SetDataParameter(const Integer id, const std::string &value)
{
    switch (id)
    {

	case CCSDS_SPACECRAFTINERTIA_INERTIAREFFRAME_ID:

	    inertiaRefFrame = value;
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
bool SpacecraftInertiaCCSDSData::SetDataParameter(const std::string &label, const std::string &value)
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
bool SpacecraftInertiaCCSDSData::SetDataParameter(const Integer id, const StringArray &value)
{
    switch (id)
    {

	case CCSDS_SPACECRAFTINERTIA_COMMENTS_ID:

	    comments = value;
            return true;

        default:

            return false;

    }

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
bool SpacecraftInertiaCCSDSData::SetDataParameter(const std::string &label, const StringArray &value)
{
    return SetDataParameter(GetDataParameterID(label),value);
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS APM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* SpacecraftInertiaCCSDSData::GetKeywords() const
{
   return CCSDS_SPACECRAFTINERTIA_KEYWORDS;
}

//------------------------------------------------------------------------------
//  const Integer GetKeywordID(const std::string str) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
const Integer SpacecraftInertiaCCSDSData::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndSpacecraftInertiaCCSDSDataDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_SPACECRAFTINERTIA_KEYWORDS[i]))
            return i;
    }

   return -1;

}

//------------------------------------------------------------------------------
//  std::string GetUnits(const Integer &id) const
//------------------------------------------------------------------------------
/**
 * Checks to see if data is available in a given data format
 *
 * @return ID associated with a keyword
 */
//------------------------------------------------------------------------------
std::string SpacecraftInertiaCCSDSData::GetUnits(const Integer &id) const
{
    if (id >= 0 && id <= EndSpacecraftInertiaCCSDSDataDataReps)
        return CCSDS_UNIT_DESCRIPTIONS[id];
    else
        return GmatBase::STRING_PARAMETER_UNDEFINED;
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
bool SpacecraftInertiaCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndSpacecraftInertiaCCSDSDataDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberSpacecraftInertiaParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberSpacecraftInertiaParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < SpacecraftInertiaCCSDSData::EndSpacecraftInertiaCCSDSDataDataReps; id++)
        if (SpacecraftInertiaCCSDSData::CCSDS_IS_REQUIRED[id])
            num++;

    return num;
}

//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool SpacecraftInertiaCCSDSData::Validate() const
{

    for (unsigned int i = 0; i < EndSpacecraftInertiaCCSDSDataDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::REAL_TYPE:
                    if (!IsParameterDefined(GetRealDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Real parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRING_TYPE:
                    if (!IsParameterDefined(GetStringDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    if (!IsParameterDefined(GetStringArrayDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                default:
                    return false;
                    break;
            }
        }
    }

    return true;
}

// std::ostream& operator<< (std::ostream &output,
//                            const SpacecraftInertiaCCSDSData *mySpacecraftInertia)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySpacecraftInertia>    CCSDS spacecraft inertia data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const SpacecraftInertiaCCSDSData *mySpacecraftInertia)
{
   using namespace std;

   if (!mySpacecraftInertia->Validate()) return output;

   unsigned int i;
   for (i = 0; i < mySpacecraftInertia->comments.size(); i++ )
   {
       output << "COMMENT " << mySpacecraftInertia->comments[i] << endl;
   }
   if (i > 0) output << endl;

   output << "INERTIA_REF_FRAME = " << mySpacecraftInertia->inertiaRefFrame << endl;
   output << "I11 = " << mySpacecraftInertia->i11 << endl;
   output << "I22 = " << mySpacecraftInertia->i22 << endl;
   output << "I33 = " << mySpacecraftInertia->i33 << endl;
   output << "I12 = " << mySpacecraftInertia->i12 << endl;
   output << "I13 = " << mySpacecraftInertia->i13 << endl;
   output << "I23 = " << mySpacecraftInertia->i23 << endl;

   output << endl;

   return output;
}