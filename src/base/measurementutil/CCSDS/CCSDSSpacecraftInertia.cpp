#include "CCSDSSpacecraftInertia.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSSpacecraftInertia::CCSDS_SPACECRAFTINERTIA_KEYWORDS[EndCCSDSSpacecraftInertiaDataReps] =
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

const std::string CCSDSSpacecraftInertia::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSSpacecraftInertiaDataReps] =
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

const std::string CCSDSSpacecraftInertia::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSSpacecraftInertiaDataReps] =
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

const bool CCSDSSpacecraftInertia::CCSDS_IS_REQUIRED[EndCCSDSSpacecraftInertiaDataReps] =
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

const Gmat::ParameterType CCSDSSpacecraftInertia::CCSDS_PARAMETER_TYPE[EndCCSDSSpacecraftInertiaDataReps] =
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
//  CCSDSSpacecraftInertia()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia::CCSDSSpacecraftInertia() : CCSDSData(),
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
//  CCSDSSpacecraftInertia(const CCSDSSpacecraftInertia &si)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia::CCSDSSpacecraftInertia(const CCSDSSpacecraftInertia &si) :
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
//  CCSDSSpacecraftInertia& operator=(const CCSDSSpacecraftInertia &si)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <si> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSSpacecraftInertia& CCSDSSpacecraftInertia::operator=(const CCSDSSpacecraftInertia &si)
{
   if (&si == this)
      return *this;

   CCSDSData::operator=(si);

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
//  ~CCSDSSpacecraftInertia()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia::~CCSDSSpacecraftInertia()
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
std::string CCSDSSpacecraftInertia::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSSpacecraftInertiaDataReps))
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
Integer CCSDSSpacecraftInertia::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpacecraftInertiaDataReps; i++)
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
Gmat::ParameterType CCSDSSpacecraftInertia::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSSpacecraftInertiaDataReps))
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
std::string CCSDSSpacecraftInertia::GetDataParameterTypeString(const Integer id) const
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
Real CCSDSSpacecraftInertia::GetRealDataParameter(const Integer id) const
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
Real CCSDSSpacecraftInertia::GetRealDataParameter(const std::string &label) const
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
std::string CCSDSSpacecraftInertia::GetStringDataParameter(const Integer id) const
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
std::string CCSDSSpacecraftInertia::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSSpacecraftInertia::GetStringArrayDataParameter(const Integer id) const
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
StringArray CCSDSSpacecraftInertia::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
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
const std::string* CCSDSSpacecraftInertia::GetKeywords() const
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
const Integer CCSDSSpacecraftInertia::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpacecraftInertiaDataReps; i++)
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
std::string CCSDSSpacecraftInertia::GetUnits(const Integer &id) const
{
    if (id >= 0 && id <= EndCCSDSSpacecraftInertiaDataReps)
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
bool CCSDSSpacecraftInertia::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSSpacecraftInertiaDataReps)
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

    for (Integer id = 0; id < CCSDSSpacecraftInertia::EndCCSDSSpacecraftInertiaDataReps; id++)
        if (CCSDSSpacecraftInertia::CCSDS_IS_REQUIRED[id])
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
bool CCSDSSpacecraftInertia::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSSpacecraftInertiaDataReps; i++ )
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
//                            const CCSDSSpacecraftInertia *mySpacecraftInertia)
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
                          const CCSDSSpacecraftInertia *mySpacecraftInertia)
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