#include "CCSDSSpacecraftParameters.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSSpacecraftParameters::CCSDS_SPACECRAFTPARAMETERS_KEYWORDS[EndCCSDSSpacecraftParametersDataReps] =
{
    "MASS",
    "SOLAR_RAD_AREA",
    "SOLAR_RAD_COEFF",
    "DRAG_AREA",
    "DRAG_COEFF",
    "COMMENT"
};

const std::string CCSDSSpacecraftParameters::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSSpacecraftParametersDataReps] =
{
    "kg",
    "m^2",
    "",
    "m^2",
    "",
    ""
};

const std::string CCSDSSpacecraftParameters::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSSpacecraftParametersDataReps] =
{
    "Spacecraft Parameters Mass",
    "Spacecraft Parameters Solar Radiation Area",
    "Spacecraft Parameters Solar Radiation Coefficient",
    "Spacecraft Parameters Drag Area",
    "Spacecraft Parameters Drag Coefficient",
    "Spacecraft Parameters Comments"
};

const bool CCSDSSpacecraftParameters::CCSDS_IS_REQUIRED[EndCCSDSSpacecraftParametersDataReps] =
{
    true,
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSSpacecraftParameters::CCSDS_PARAMETER_TYPE[EndCCSDSSpacecraftParametersDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSSpacecraftParameters()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftParameters::CCSDSSpacecraftParameters() : CCSDSData(),
    mass(0),
    solarRadiationArea(0),
    solarRadiationCoefficient(0),
    dragArea(0),
    dragCoefficient(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSSpacecraftParameters(const CCSDSSpacecraftParameters &sp)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftParameters::CCSDSSpacecraftParameters
               (const CCSDSSpacecraftParameters &sp) :
    CCSDSData(sp),
    mass(sp.mass),
    solarRadiationArea(sp.solarRadiationArea),
    solarRadiationCoefficient(sp.solarRadiationCoefficient),
    dragArea(sp.dragArea),
    dragCoefficient(sp.dragCoefficient),
    comments(sp.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSSpacecraftParameters& operator=
//                                   (const CCSDSSpacecraftParameters &sp)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSSpacecraftParameters structures.
 *
 * @param <SP> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSSpacecraftParameters& CCSDSSpacecraftParameters::operator=
                                     (const CCSDSSpacecraftParameters &sp)
{
    if (&sp == this)
        return *this;

    CCSDSData::operator=(sp);

    mass = sp.mass;
    solarRadiationArea = sp.solarRadiationArea;
    solarRadiationCoefficient = sp.solarRadiationCoefficient;
    dragArea = sp.dragArea;
    dragCoefficient = sp.dragCoefficient;
    comments = sp.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSSpacecraftParameters()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftParameters::~CCSDSSpacecraftParameters()
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
std::string CCSDSSpacecraftParameters::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSSpacecraftParametersDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetUnits(const Integer &id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSSpacecraftParameters::GetUnits(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSSpacecraftParametersDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
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
Integer CCSDSSpacecraftParameters::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpacecraftParametersDataReps; i++)
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
Gmat::ParameterType CCSDSSpacecraftParameters::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSSpacecraftParametersDataReps))
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
std::string CCSDSSpacecraftParameters::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSSpacecraftParameters::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_SPACECRAFTPARAMETERS_MASS_ID:

            return mass;

	case CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:

            return solarRadiationArea;

	case CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:

            return solarRadiationCoefficient;

	case CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID:

            return dragArea;

	case CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:

            return dragCoefficient;

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
Real CCSDSSpacecraftParameters::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSSpacecraftParameters::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_SPACECRAFTPARAMETERS_COMMENTS_ID:

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
StringArray CCSDSSpacecraftParameters::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS OPM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSSpacecraftParameters::GetKeywords() const
{
   return CCSDS_SPACECRAFTPARAMETERS_KEYWORDS;
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
const Integer CCSDSSpacecraftParameters::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSSpacecraftParametersDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_SPACECRAFTPARAMETERS_KEYWORDS[i]))
            return i;
    }

   return -1;

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
bool CCSDSSpacecraftParameters::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSSpacecraftParametersDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  Integer CountRequiredNumberSpacecraftParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberSpacecraftParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSSpacecraftParameters::EndCCSDSSpacecraftParametersDataReps; id++)
        if (CCSDSSpacecraftParameters::CCSDS_IS_REQUIRED[id])
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
bool CCSDSSpacecraftParameters::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSSpacecraftParametersDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::REAL_TYPE:
                    {
                    Real rvalue = GetRealDataParameter(i);
                    if (&rvalue == NULL ||
                        rvalue == GmatBase::REAL_PARAMETER_UNDEFINED)
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    {
                    StringArray savalue = GetStringArrayDataParameter(i);
                    if (&savalue == NULL ||
                        savalue == GmatBase::STRINGARRAY_PARAMETER_UNDEFINED)
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                      const CCSDSSpacecraftParameters *mySpacecraftParameters)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySpacecraftParameters> CCSDS spacecraft parameter data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                        const CCSDSSpacecraftParameters *mySpacecraftParameters)
{
   using namespace std;

    unsigned int i;
    for (i = 0; i < mySpacecraftParameters->comments.size(); i++ )
    {
        output << "COMMENT " << mySpacecraftParameters->comments[i] << endl;
    }
    if (i > 0) output << endl;

   output << "MASS = " << mySpacecraftParameters->mass << endl;
   output << "SOLAR_RAD_AREA = " << mySpacecraftParameters->solarRadiationArea << endl;
   output << "SOLAR_RAD_COEFF = " << mySpacecraftParameters->solarRadiationCoefficient << endl;
   output << "DRAG_AREA = " << mySpacecraftParameters->dragArea << endl;
   output << "DRAG_COEFF = " << mySpacecraftParameters->dragCoefficient << endl;

   output << endl;

   return output;
}
