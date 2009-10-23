#include "CCSDSOPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string CCSDSOPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  CCSDSOPMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::CCSDSOPMObType() : CCSDSObType("CCSDSOPMObType", ""),
	ccsdsOPMMetaData(NULL),
        ccsdsOPMStateVector(NULL),
        ccsdsOPMKeplerianElements(NULL),
        ccsdsOPMSpacecraftParameters(NULL),
        ccsdsOPMManeuvers(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMObType(const CCSDSOPMObType &opm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::CCSDSOPMObType(const CCSDSOPMObType &opm) : CCSDSObType(opm),
	ccsdsOPMMetaData(opm.ccsdsOPMMetaData),
        ccsdsOPMStateVector(opm.ccsdsOPMStateVector),
        ccsdsOPMKeplerianElements(opm.ccsdsOPMKeplerianElements),
        ccsdsOPMSpacecraftParameters(opm.ccsdsOPMSpacecraftParameters),
        ccsdsOPMManeuvers(opm.ccsdsOPMManeuvers)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMObType& operator=(const CCSDSOPMObType &opm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <OPM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMObType& CCSDSOPMObType::operator=(const CCSDSOPMObType &opm)
{
    if (&opm == this)
        return *this;

    CCSDSObType::operator=(opm);

    ccsdsOPMMetaData = opm.ccsdsOPMMetaData;
    ccsdsOPMStateVector = opm.ccsdsOPMStateVector;
    ccsdsOPMKeplerianElements = opm.ccsdsOPMKeplerianElements;
    ccsdsOPMSpacecraftParameters = opm.ccsdsOPMSpacecraftParameters;
    ccsdsOPMManeuvers = opm.ccsdsOPMManeuvers;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSOPMObType::~CCSDSOPMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSOPMDataFile.
 *
 * @return clone of the ProcessCCSDSOPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOPMObType::Clone() const
{
   GmatBase *clone = new CCSDSOPMObType(*this);
   return (clone);
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
std::string CCSDSOPMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOPMDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataParameterText(id);
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSOPMObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOPMDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataUnits(id);
}


//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSOPMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOPMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
	{
	    return i;
	}
   }

   return CCSDSObType::GetDataParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetDataParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Gmat::ParameterType CCSDSOPMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSOPMDataReps))
      return CCSDS_PARAMETER_TYPE[id];

   return CCSDSObType::GetDataParameterType(id);
}

//---------------------------------------------------------------------------
//  std::string GetDataParameterTypeString(const Integer id) const
//---------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
std::string CCSDSOPMObType::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObType::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSOPMObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_OPM_STATEVECTOR_X_ID:

            return ccsdsOPMStateVector->x;

	case CCSDS_OPM_STATEVECTOR_Y_ID:

            return ccsdsOPMStateVector->y;

	case CCSDS_OPM_STATEVECTOR_Z_ID:

            return ccsdsOPMStateVector->z;

	case CCSDS_OPM_STATEVECTOR_XDOT_ID:

            return ccsdsOPMStateVector->xDot;

        case CCSDS_OPM_STATEVECTOR_YDOT_ID:

            return ccsdsOPMStateVector->yDot;

	case CCSDS_OPM_STATEVECTOR_ZDOT_ID:

            return ccsdsOPMStateVector->zDot;

	case CCSDS_OPM_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:

            return ccsdsOPMKeplerianElements->semiMajorAxis;

	case CCSDS_OPM_KEPLERIANELEMENTS_ECCENTRICITY_ID:

            return ccsdsOPMKeplerianElements->eccentricity;

	case CCSDS_OPM_KEPLERIANELEMENTS_INCLINATION_ID:

            return ccsdsOPMKeplerianElements->inclination;

	case CCSDS_OPM_KEPLERIANELEMENTS_RAAN_ID:

            return ccsdsOPMKeplerianElements->raan;

	case CCSDS_OPM_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:

            return ccsdsOPMKeplerianElements->argumentOfPericenter;

	case CCSDS_OPM_KEPLERIANELEMENTS_TRUEANOMALY_ID:

            return ccsdsOPMKeplerianElements->theAnomaly.GetTrueAnomaly();

	case CCSDS_OPM_KEPLERIANELEMENTS_MEANANOMALY_ID:

            return ccsdsOPMKeplerianElements->theAnomaly.GetMeanAnomaly();

	case CCSDS_OPM_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:

            return ccsdsOPMKeplerianElements->gravitationalCoefficient;

	case CCSDS_OPM_SPACECRAFTPARAMETERS_MASS_ID:

            return ccsdsOPMSpacecraftParameters->mass;

	case CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:

            return ccsdsOPMSpacecraftParameters->solarRadiationArea;

	case CCSDS_OPM_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:

            return ccsdsOPMSpacecraftParameters->solarRadiationCoefficient;

	case CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGAREA_ID:

            return ccsdsOPMSpacecraftParameters->dragArea;

	case CCSDS_OPM_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:

            return ccsdsOPMSpacecraftParameters->dragCoefficient;

        case CCSDS_OPM_MANUEVER_DURATION_ID:

            return (*i_ccsdsOPMManeuvers)->duration;

        case CCSDS_OPM_MANUEVER_DELTAMASS_ID:

            return (*i_ccsdsOPMManeuvers)->deltaMass;

        case CCSDS_OPM_MANUEVER_DELTAV1_ID:

            return (*i_ccsdsOPMManeuvers)->deltaV1;

        case CCSDS_OPM_MANUEVER_DELTAV2_ID:

            return (*i_ccsdsOPMManeuvers)->deltaV2;

        case CCSDS_OPM_MANUEVER_DELTAV3_ID:

            return (*i_ccsdsOPMManeuvers)->deltaV3;

        default:

            return CCSDSObType::GetRealDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSOPMObType::GetRealDataParameter(const std::string &label) const
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
std::string CCSDSOPMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_OPM_STATEVECTOR_EPOCH_ID:

	    return ccsdsOPMStateVector->epoch;

        case CCSDS_OPM_MANUEVER_IGNITIONEPOCH_ID:

	    return (*i_ccsdsOPMManeuvers)->ignitionEpoch;

        case CCSDS_OPM_MANUEVER_REFFRAME_ID:

	    return (*i_ccsdsOPMManeuvers)->refFrame;

        default:

            return CCSDSObType::GetStringDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSOPMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSOPMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_OPM_STATEVECTOR_COMMENTS_ID:

	    return ccsdsOPMStateVector->comments;

        case CCSDS_OPM_KEPLERIANELEMENTS_COMMENTS_ID:

	    return ccsdsOPMKeplerianElements->comments;

        case CCSDS_OPM_SPACECRAFTPARAMETERS_COMMENTS_ID:

	    return ccsdsOPMSpacecraftParameters->comments;

        case CCSDS_OPM_MANUEVER_COMMENTS_ID:

	    return (*i_ccsdsOPMManeuvers)->comments;

        default:

            return CCSDSObType::GetStringArrayDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// StringArray GetStringArrayDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSOPMObType::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSOPMObType::GetKeywords() const
{
   return CCSDS_OPM_KEYWORDS;
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
const Integer CCSDSOPMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOPMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_OPM_KEYWORDS[i]))
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
std::string CCSDSOPMObType::GetUnits(const Integer &id) const
{
   return CCSDS_UNIT_DESCRIPTIONS[id];
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
const std::string* CCSDSOPMObType::GetTimeSystems() const
{
   return CCSDS_TIMESYSTEM_DESCRIPTIONS;
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
std::string CCSDSOPMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSOPMTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return CCSDSObType::GetTimeSystemText(id);
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
Integer CCSDSOPMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSOPMTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}

    }

    return CCSDSObType::GetTimeSystemID(label);

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
bool CCSDSOPMObType::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndCCSDSOPMDataReps)
    return CCSDS_IS_REQUIRED[id];
else
    return CCSDSObType::IsParameterRequired(id);
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
bool CCSDSOPMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSOPMDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return CCSDSObType::CheckDataAvailability(str);

}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMObType *myOPM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSOPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS OPM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSOPMObType *myOPM)
{
    if(myOPM->ccsdsOPMStateVector != NULL)
        output << myOPM->ccsdsOPMStateVector;
    else
        return output;

    if(myOPM->ccsdsOPMKeplerianElements != NULL)
        output << myOPM->ccsdsOPMKeplerianElements;

    if(myOPM->ccsdsOPMSpacecraftParameters != NULL)
        output << myOPM->ccsdsOPMSpacecraftParameters;

    for (std::vector<CCSDSOPMManeuver*>::const_iterator
         j=myOPM->ccsdsOPMManeuvers.begin();
         j!=myOPM->ccsdsOPMManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            output << (*j);
    }

    return output;
}

