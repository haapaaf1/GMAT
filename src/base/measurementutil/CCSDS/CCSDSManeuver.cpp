#include "CCSDSManeuver.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSManeuver::CCSDS_MANEUVER_KEYWORDS[EndCCSDSManeuverDataReps] =
{
    "MAN_EPOCH_IGNITION",
    "MAN_DURATION",
    "MAN_DELTA_MASS",
    "MAN_REF_FRAME",
    "MAN_DV_1",
    "MAN_DV_2",
    "MAN_DV_3",
    "COMMENT"
};

const std::string CCSDSManeuver::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSManeuverDataReps] =
{
    "",
    "s",
    "kg",
    "",
    "km/s",
    "km/s",
    "km/s",
    ""
};

const std::string CCSDSManeuver::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSManeuverDataReps] =
{
    "Maneuver Ref Ignition Epoch",
    "Maneuver Duration",
    "Maneuver Ref Delta Mass",
    "Maneuver Ref Frame",
    "Maneuver Ref DeltaV1",
    "Maneuver Ref DeltaV2",
    "Maneuver Ref DeltaV3",
    "Maneuver Comments"
};

const bool CCSDSManeuver::CCSDS_IS_REQUIRED[EndCCSDSManeuverDataReps] =
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

const Gmat::ParameterType CCSDSManeuver::CCSDS_PARAMETER_TYPE[EndCCSDSManeuverDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSManeuver()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSManeuver class
 */
//------------------------------------------------------------------------------
CCSDSManeuver::CCSDSManeuver() : CCSDSData(),
    ignitionEpoch(std::string("")),
    duration(0),
    deltaMass(0),
    refFrame(std::string("")),
    deltaV1(0),
    deltaV2(0),
    deltaV3(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSManeuver(const CCSDSManeuver &man)
//------------------------------------------------------------------------------
/**
 * Constructor for the Maneuver class
 */
//------------------------------------------------------------------------------
CCSDSManeuver::CCSDSManeuver(const CCSDSManeuver &man) :
    CCSDSData(man),
    ignitionEpoch(man.ignitionEpoch),
    duration(man.duration),
    deltaMass(man.deltaMass),
    refFrame(man.refFrame),
    deltaV1(man.deltaV1),
    deltaV2(man.deltaV2),
    deltaV3(man.deltaV3),
    comments(man.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSManeuver& operator= (const CCSDSManeuver &man)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Maneuver structures.
 *
 * @param <M> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSManeuver& CCSDSManeuver::operator=(const CCSDSManeuver &man)
{
    if (&man == this)
        return *this;

    CCSDSData::operator=(man);

    ignitionEpoch = man.ignitionEpoch;
    duration = man.duration;
    deltaMass = man.deltaMass;
    refFrame = man.refFrame;
    deltaV1 = man.deltaV1;
    deltaV2 = man.deltaV2;
    deltaV3 = man.deltaV3;
    comments = man.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSManeuver()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSManeuver class
 */
//------------------------------------------------------------------------------
CCSDSManeuver::~CCSDSManeuver()
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
std::string CCSDSManeuver::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSManeuverDataReps))
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
std::string CCSDSManeuver::GetUnits(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSManeuverDataReps))
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
Integer CCSDSManeuver::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSManeuverDataReps; i++)
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
Gmat::ParameterType CCSDSManeuver::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSManeuverDataReps))
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
std::string CCSDSManeuver::GetDataParameterTypeString(const Integer id) const
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
Real CCSDSManeuver::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_MANUEVER_DURATION_ID:

            return duration;

        case CCSDS_MANUEVER_DELTAMASS_ID:

            return deltaMass;

        case CCSDS_MANUEVER_DELTAV1_ID:

            return deltaV1;

        case CCSDS_MANUEVER_DELTAV2_ID:

            return deltaV2;

        case CCSDS_MANUEVER_DELTAV3_ID:

            return deltaV3;

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
Real CCSDSManeuver::GetRealDataParameter(const std::string &label) const
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
std::string CCSDSManeuver::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_MANUEVER_IGNITIONEPOCH_ID:

	    return ignitionEpoch;

        case CCSDS_MANUEVER_REFFRAME_ID:

	    return refFrame;

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
std::string CCSDSManeuver::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSManeuver::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_MANUEVER_COMMENTS_ID:

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
StringArray CCSDSManeuver::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSManeuver::GetKeywords() const
{
   return CCSDS_MANEUVER_KEYWORDS;
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
const Integer CCSDSManeuver::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSManeuverDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_MANEUVER_KEYWORDS[i]))
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
bool CCSDSManeuver::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSManeuverDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}

//---------------------------------------------------------------------------
//  Integer CountRequiredNumberManeuverParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberManeuverParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSManeuver::EndCCSDSManeuverDataReps; id++)
        if (CCSDSManeuver::CCSDS_IS_REQUIRED[id])
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
bool CCSDSManeuver::Validate() const
{

    for (unsigned int i = 0; i < EndCCSDSManeuverDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::REAL_TYPE:
                    if (!IsParameterDefined(GetRealDataParameter(i)))
                        return false;
                    break;
                case Gmat::STRING_TYPE:
                    if (!IsParameterDefined(GetStringDataParameter(i)))
                        return false;
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    if (!IsParameterDefined(GetStringArrayDataParameter(i)))
                        return false;
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
// std::ostream& operator<< (std::ostream &output, const CCSDSManeuver *myManeuver)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output streaman.
 *
 * @param  <output>  Output stream
 * @param  <mymyManeuver>    CCSDS maneuver data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSManeuver *myManeuver)
{
    using namespace std;

    if (!myManeuver->Validate()) return output;

    unsigned int i;
    for (i = 0; i < myManeuver->comments.size(); i++ )
    {
        output << "COMMENT " << myManeuver->comments[i] << endl;
    }
    if (i > 0) output << endl;

    output << "MAN_EPOCH_IGNITION = " << myManeuver->ignitionEpoch << endl;
    output << "MAN_DURATION = " << myManeuver->duration << endl;
    output << "MAN_DELTA_MASS = " << myManeuver->deltaMass << endl;
    output << "MAN_REF_FRAME = " << myManeuver->refFrame << endl;
    output << "MAN_DV_1 = " << myManeuver->deltaV1 << endl;
    output << "MAN_DV_2 = " << myManeuver->deltaV2 << endl;
    output << "MAN_DV_3 = " << myManeuver->deltaV3 << endl;
    output << endl;
    
   return output;
}