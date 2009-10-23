#include "CCSDSAPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------

const std::string CCSDSAPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  CCSDSAPMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::CCSDSAPMObType() : CCSDSObType("CCSDSAPMObType", ""),
	ccsdsAPMMetaData(NULL),
        ccsdsAPMQuaternion(NULL),
        ccsdsAPMEulerAngle(NULL),
        ccsdsAPMSpinStabilized(NULL),
        ccsdsAPMSpacecraftInertia(NULL),
        ccsdsAPMAttitudeManeuvers(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMObType(const CCSDSAPMObType &apm)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::CCSDSAPMObType(const CCSDSAPMObType &apm) : CCSDSObType(apm),
	ccsdsAPMMetaData(apm.ccsdsAPMMetaData),
        ccsdsAPMQuaternion(apm.ccsdsAPMQuaternion),
        ccsdsAPMEulerAngle(apm.ccsdsAPMEulerAngle),
        ccsdsAPMSpinStabilized(apm.ccsdsAPMSpinStabilized),
        ccsdsAPMSpacecraftInertia(apm.ccsdsAPMSpacecraftInertia),
        ccsdsAPMAttitudeManeuvers(apm.ccsdsAPMAttitudeManeuvers)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMObType& operator=(const CCSDSAPMObType &apm)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apm> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMObType& CCSDSAPMObType::operator=(const CCSDSAPMObType &apm)
{
   if (&apm == this)
      return *this;

    CCSDSObType::operator=(apm);

    ccsdsAPMMetaData = apm.ccsdsAPMMetaData;
    ccsdsAPMQuaternion = apm.ccsdsAPMQuaternion;
    ccsdsAPMEulerAngle = apm.ccsdsAPMEulerAngle;
    ccsdsAPMSpinStabilized = apm.ccsdsAPMSpinStabilized;
    ccsdsAPMSpacecraftInertia = apm.ccsdsAPMSpacecraftInertia;
    ccsdsAPMAttitudeManeuvers = apm.ccsdsAPMAttitudeManeuvers;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMObType::~CCSDSAPMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAPMDataFile.
 *
 * @return clone of the ProcessCCSDSAPMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSAPMObType::Clone() const
{
   GmatBase *clone = new CCSDSAPMObType(*this);
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
std::string CCSDSAPMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAPMDataReps))
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
std::string CCSDSAPMObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAPMDataReps))
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
Integer CCSDSAPMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAPMDataReps; i++)
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
Gmat::ParameterType CCSDSAPMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAPMDataReps))
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
std::string CCSDSAPMObType::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObType::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSAPMObType::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_APM_QUATERNION_TYPE_ID:

	    return ccsdsAPMQuaternion->quaternionType;

        case CCSDS_APM_SPINSTABILIZED_ATTITUDETYPE_ID:

	    return ccsdsAPMSpinStabilized->attitudeType;

     default:

        return CCSDSObType::GetIntegerDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSAPMObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSAPMObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_APM_ATTITUDEMANUEVER_DURATION_ID:

            return (*i_ccsdsAPMAttitudeManeuvers)->duration;

        case CCSDS_APM_ATTITUDEMANUEVER_TOR1_ID:

            return (*i_ccsdsAPMAttitudeManeuvers)->tor1;

        case CCSDS_APM_ATTITUDEMANUEVER_TOR2_ID:

            return (*i_ccsdsAPMAttitudeManeuvers)->tor2;

        case CCSDS_APM_ATTITUDEMANUEVER_TOR3_ID:

            return (*i_ccsdsAPMAttitudeManeuvers)->tor3;

	case CCSDS_APM_SPACECRAFTINERTIA_I11_ID:

            return ccsdsAPMSpacecraftInertia->i11;

	case CCSDS_APM_SPACECRAFTINERTIA_I22_ID:

            return ccsdsAPMSpacecraftInertia->i22;

	case CCSDS_APM_SPACECRAFTINERTIA_I33_ID:

            return ccsdsAPMSpacecraftInertia->i33;

	case CCSDS_APM_SPACECRAFTINERTIA_I12_ID:

            return ccsdsAPMSpacecraftInertia->i12;

	case CCSDS_APM_SPACECRAFTINERTIA_I13_ID:

            return ccsdsAPMSpacecraftInertia->i13;

	case CCSDS_APM_SPACECRAFTINERTIA_I23_ID:

            return ccsdsAPMSpacecraftInertia->i23;

	case CCSDS_APM_QUATERNION_Q1_ID:

            return ccsdsAPMQuaternion->q1;

	case CCSDS_APM_QUATERNION_Q2_ID:

            return ccsdsAPMQuaternion->q2;

	case CCSDS_APM_QUATERNION_Q3_ID:

            return ccsdsAPMQuaternion->q3;

	case CCSDS_APM_QUATERNION_QC_ID:

            return ccsdsAPMQuaternion->qC;

	case CCSDS_APM_QUATERNION_Q1DOT_ID:

            return ccsdsAPMQuaternion->q1Dot;

        case CCSDS_APM_QUATERNION_Q2DOT_ID:

            return ccsdsAPMQuaternion->q2Dot;

        case CCSDS_APM_QUATERNION_Q3DOT_ID:

            return ccsdsAPMQuaternion->q3Dot;

        case CCSDS_APM_QUATERNION_QCDOT_ID:

            return ccsdsAPMQuaternion->qCDot;

        case CCSDS_APM_EULERANGLE_XANGLE_ID:

            return ccsdsAPMEulerAngle->xAngle;

        case CCSDS_APM_EULERANGLE_YANGLE_ID:

            return ccsdsAPMEulerAngle->yAngle;

        case CCSDS_APM_EULERANGLE_ZANGLE_ID:

            return ccsdsAPMEulerAngle->zAngle;

        case CCSDS_APM_EULERANGLE_XRATE_ID:

            return ccsdsAPMEulerAngle->xRate;

        case CCSDS_APM_EULERANGLE_YRATE_ID:

            return ccsdsAPMEulerAngle->yRate;

        case CCSDS_APM_EULERANGLE_ZRATE_ID:

            return ccsdsAPMEulerAngle->zRate;

	case CCSDS_APM_SPINSTABILIZED_SPINALPHA_ID:

            return ccsdsAPMSpinStabilized->spinAlpha;

	case CCSDS_APM_SPINSTABILIZED_SPINDELTA_ID:

            return ccsdsAPMSpinStabilized->spinDelta;

	case CCSDS_APM_SPINSTABILIZED_SPINANGLE_ID:

            return ccsdsAPMSpinStabilized->spinAngle;

	case CCSDS_APM_SPINSTABILIZED_SPINANGLEVEOCITY_ID:

            return ccsdsAPMSpinStabilized->spinAngleVelocity;

	case CCSDS_APM_SPINSTABILIZED_NUTATION_ID:

            return ccsdsAPMSpinStabilized->nutation;

	case CCSDS_APM_SPINSTABILIZED_NUTATIONPERIOD_ID:

            return ccsdsAPMSpinStabilized->nutationPeriod;

	case CCSDS_APM_SPINSTABILIZED_NUTATIONPHASE_ID:

            return ccsdsAPMSpinStabilized->nutationPhase;
            
        default:

            return CCSDSObType::GetRealDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSAPMObType::GetRealDataParameter(const std::string &label) const
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
std::string CCSDSAPMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_APM_SPACECRAFTINERTIA_INERTIAREFFRAME_ID:

	    return ccsdsAPMSpacecraftInertia->inertiaRefFrame;

        case CCSDS_APM_ATTITUDEMANUEVER_EPOCHSTART_ID:

	    return (*i_ccsdsAPMAttitudeManeuvers)->epochStart;

        case CCSDS_APM_ATTITUDEMANUEVER_REFFRAME_ID:

	    return (*i_ccsdsAPMAttitudeManeuvers)->refFrame;

        case CCSDS_APM_QUATERNION_FRAMEA_ID:

	    return ccsdsAPMQuaternion->frameA;

	case CCSDS_APM_QUATERNION_FRAMEB_ID:

	    return ccsdsAPMQuaternion->frameB;

	case CCSDS_APM_QUATERNION_DIRECTION_ID:

	    return ccsdsAPMQuaternion->direction;

	case CCSDS_APM_EULERANGLE_FRAMEA_ID:

	    return ccsdsAPMEulerAngle->frameA;

	case CCSDS_APM_EULERANGLE_FRAMEB_ID:

	    return ccsdsAPMEulerAngle->frameB;

        case CCSDS_APM_EULERANGLE_DIRECTION_ID:

	    return ccsdsAPMEulerAngle->direction;

        case CCSDS_APM_EULERANGLE_ROTATIONSEQUENCE_ID:

	    return ccsdsAPMEulerAngle->rotationSequence;

        case CCSDS_APM_EULERANGLE_RATEFRAME_ID:

	    return ccsdsAPMEulerAngle->rateFrame;

	case CCSDS_APM_SPINSTABILIZED_FRAMEA_ID:

	    return ccsdsAPMSpinStabilized->frameA;

	case CCSDS_APM_SPINSTABILIZED_FRAMEB_ID:

	    return ccsdsAPMSpinStabilized->frameB;

	case CCSDS_APM_SPINSTABILIZED_DIRECTION_ID:

	    return ccsdsAPMSpinStabilized->direction;

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
std::string CCSDSAPMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAPMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_APM_QUATERNION_COMMENTS_ID:

	    return ccsdsAPMQuaternion->comments;

        case CCSDS_APM_EULERANGLE_COMMENTS_ID:

	    return ccsdsAPMEulerAngle->comments;

        case CCSDS_APM_SPINSTABILIZED_COMMENTS_ID:

	    return ccsdsAPMSpinStabilized->comments;

        case CCSDS_APM_ATTITUDEMANUEVER_COMMENTS_ID:

	    return (*i_ccsdsAPMAttitudeManeuvers)->comments;

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
StringArray CCSDSAPMObType::GetStringArrayDataParameter(const std::string &label) const
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
const std::string* CCSDSAPMObType::GetKeywords() const
{
   return CCSDS_APM_KEYWORDS;
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
const Integer CCSDSAPMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAPMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_APM_KEYWORDS[i]))
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
std::string CCSDSAPMObType::GetUnits(const Integer &id) const
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
const std::string* CCSDSAPMObType::GetTimeSystems() const
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
std::string CCSDSAPMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSAPMTimeReps))
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
Integer CCSDSAPMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSAPMTimeReps; i++)
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
bool CCSDSAPMObType::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndCCSDSAPMDataReps)
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
bool CCSDSAPMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAPMDataReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSAPMObType *myAPM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAPMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS APM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAPMObType *myAPM)
{
    if (myAPM->ccsdsAPMQuaternion != NULL);
        output << myAPM->ccsdsAPMQuaternion;

    if (myAPM->ccsdsAPMEulerAngle != NULL)
        output << myAPM->ccsdsAPMEulerAngle;

    if (myAPM->ccsdsAPMSpinStabilized != NULL)
        output << myAPM->ccsdsAPMSpinStabilized;

    for (std::vector<CCSDSAPMAttitudeManeuver*>::const_iterator 
         j=myAPM->ccsdsAPMAttitudeManeuvers.begin();
         j!=myAPM->ccsdsAPMAttitudeManeuvers.end(); ++j)
    {
        if((*j) != NULL)
            output << (*j);
    }
    return output;
}
