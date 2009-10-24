#include "CCSDSAEMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAEMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

//------------------------------------------------------------------------------
//  CCSDSAEMObType()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::CCSDSAEMObType() : CCSDSObType("CCSDSAEMObType", ""),
	ccsdsAEMMetaData(NULL),
        ccsdsAEMQuaternion(NULL),
        ccsdsAEMEulerAngle(NULL),
        ccsdsAEMSpinStabilized(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMObType(const CCSDSAEMObType &aem)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::CCSDSAEMObType(const CCSDSAEMObType &aem) : CCSDSObType(AEM),
	ccsdsAEMMetaData(aem.ccsdsAEMMetaData),
        ccsdsAEMQuaternion(aem.ccsdsAEMQuaternion),
        ccsdsAEMEulerAngle(aem.ccsdsAEMEulerAngle),
        ccsdsAEMSpinStabilized(aem.ccsdsAEMSpinStabilized)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMObType& operator=(const CCSDSAEMObType &aem)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <AEM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMObType& CCSDSAEMObType::operator=(const CCSDSAEMObType &aem)
{
   if (&aem == this)
      return *this;

   CCSDSObType::operator=(aem);

   ccsdsAEMMetaData = aem.ccsdsAEMMetaData;

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMObType::~CCSDSAEMObType()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the ProcessCCSDSAEMDataFile.
 *
 * @return clone of the ProcessCCSDSAEMDataFile.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSAEMObType::Clone() const
{
   GmatBase *clone = new CCSDSAEMObType(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// Measurement Data Access functions
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAEMObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAEMDataReps))
   {
      return CCSDS_UNIT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataUnits(id);
}

//------------------------------------------------------------------------------
//  std::string  GetDataParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAEMObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAEMDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return CCSDSObType::GetDataParameterText(id);
}

//------------------------------------------------------------------------------
//  Integer  GetDataParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSAEMObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAEMDataReps; i++)
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
Gmat::ParameterType CCSDSAEMObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSAEMDataReps))
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
std::string CCSDSAEMObType::GetDataParameterTypeString(const Integer id) const
{
   return CCSDSObType::GetDataParameterTypeString(id);
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSAEMObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_AEM_QUATERNION_Q1_ID:

            return ccsdsAEMQuaternion->q1;

	case CCSDS_AEM_QUATERNION_Q2_ID:

            return ccsdsAEMQuaternion->q2;

	case CCSDS_AEM_QUATERNION_Q3_ID:

            return ccsdsAEMQuaternion->q3;

	case CCSDS_AEM_QUATERNION_QC_ID:

            return ccsdsAEMQuaternion->qC;

	case CCSDS_AEM_QUATERNION_Q1DOT_ID:

            return ccsdsAEMQuaternion->q1Dot;

        case CCSDS_AEM_QUATERNION_Q2DOT_ID:

            return ccsdsAEMQuaternion->q2Dot;

        case CCSDS_AEM_QUATERNION_Q3DOT_ID:

            return ccsdsAEMQuaternion->q3Dot;

        case CCSDS_AEM_QUATERNION_QCDOT_ID:

            return ccsdsAEMQuaternion->qCDot;

        case CCSDS_AEM_EULERANGLE_XANGLE_ID:

            return ccsdsAEMEulerAngle->xAngle;

        case CCSDS_AEM_EULERANGLE_YANGLE_ID:

            return ccsdsAEMEulerAngle->yAngle;

        case CCSDS_AEM_EULERANGLE_ZANGLE_ID:

            return ccsdsAEMEulerAngle->zAngle;

        case CCSDS_AEM_EULERANGLE_XRATE_ID:

            return ccsdsAEMEulerAngle->xRate;

        case CCSDS_AEM_EULERANGLE_YRATE_ID:

            return ccsdsAEMEulerAngle->yRate;

        case CCSDS_AEM_EULERANGLE_ZRATE_ID:

            return ccsdsAEMEulerAngle->zRate;

	case CCSDS_AEM_SPINSTABILIZED_SPINALPHA_ID:

            return ccsdsAEMSpinStabilized->spinAlpha;

	case CCSDS_AEM_SPINSTABILIZED_SPINDELTA_ID:

            return ccsdsAEMSpinStabilized->spinDelta;

	case CCSDS_AEM_SPINSTABILIZED_SPINANGLE_ID:

            return ccsdsAEMSpinStabilized->spinAngle;

	case CCSDS_AEM_SPINSTABILIZED_SPINANGLEVEOCITY_ID:

            return ccsdsAEMSpinStabilized->spinAngleVelocity;

	case CCSDS_AEM_SPINSTABILIZED_NUTATION_ID:

            return ccsdsAEMSpinStabilized->nutation;

	case CCSDS_AEM_SPINSTABILIZED_NUTATIONPERIOD_ID:

            return ccsdsAEMSpinStabilized->nutationPeriod;

	case CCSDS_AEM_SPINSTABILIZED_NUTATIONPHASE_ID:

            return ccsdsAEMSpinStabilized->nutationPhase;

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
Real CCSDSAEMObType::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSAEMObType::GetIntegerDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_AEM_QUATERNION_TYPE_ID:

	    return ccsdsAEMQuaternion->quaternionType;

        case CCSDS_AEM_SPINSTABILIZED_ATTITUDETYPE_ID:

	    return ccsdsAEMSpinStabilized->attitudeType;

        default:

            return CCSDSObType::GetIntegerDataParameter(id);

    }

}

//------------------------------------------------------------------------------
// virtual Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSAEMObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSAEMObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {

        case CCSDS_AEM_QUATERNION_FRAMEA_ID:

	    return ccsdsAEMQuaternion->frameA;

	case CCSDS_AEM_QUATERNION_FRAMEB_ID:

	    return ccsdsAEMQuaternion->frameB;

	case CCSDS_AEM_QUATERNION_DIRECTION_ID:

	    return ccsdsAEMQuaternion->direction;

	case CCSDS_AEM_EULERANGLE_FRAMEA_ID:

	    return ccsdsAEMEulerAngle->frameA;

	case CCSDS_AEM_EULERANGLE_FRAMEB_ID:

	    return ccsdsAEMEulerAngle->frameB;

        case CCSDS_AEM_EULERANGLE_DIRECTION_ID:

	    return ccsdsAEMEulerAngle->direction;

        case CCSDS_AEM_EULERANGLE_ROTATIONSEQUENCE_ID:

	    return ccsdsAEMEulerAngle->rotationSequence;

        case CCSDS_AEM_EULERANGLE_RATEFRAME_ID:

	    return ccsdsAEMEulerAngle->rateFrame;

	case CCSDS_AEM_SPINSTABILIZED_FRAMEA_ID:

	    return ccsdsAEMSpinStabilized->frameA;

	case CCSDS_AEM_SPINSTABILIZED_FRAMEB_ID:

	    return ccsdsAEMSpinStabilized->frameB;

	case CCSDS_AEM_SPINSTABILIZED_DIRECTION_ID:

	    return ccsdsAEMSpinStabilized->direction;

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
std::string CCSDSAEMObType::GetStringDataParameter(const std::string &label) const
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
StringArray CCSDSAEMObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_AEM_QUATERNION_COMMENTS_ID:

	    return ccsdsAEMQuaternion->comments;

        case CCSDS_AEM_EULERANGLE_COMMENTS_ID:

	    return ccsdsAEMEulerAngle->comments;

        case CCSDS_AEM_SPINSTABILIZED_COMMENTS_ID:

	    return ccsdsAEMSpinStabilized->comments;

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
StringArray CCSDSAEMObType::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS AEM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSAEMObType::GetKeywords() const
{
   return CCSDS_AEM_KEYWORDS;
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
const Integer CCSDSAEMObType::GetKeywordID(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAEMDataReps; i++)
    {
        if (pcrecpp::RE(regex).FullMatch(CCSDS_AEM_KEYWORDS[i]))
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
std::string CCSDSAEMObType::GetUnits(const Integer &id) const
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
const std::string* CCSDSAEMObType::GetTimeSystems() const
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
std::string CCSDSAEMObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= EndCCSDSTimeReps) && (id < EndCCSDSAEMTimeReps))
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
Integer CCSDSAEMObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";

    for (Integer i = EndCCSDSTimeReps; i < EndCCSDSAEMTimeReps; i++)
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
bool CCSDSAEMObType::IsParameterRequired(const Integer id) const
{
    if (id > 0 && id <= EndCCSDSAEMDataReps)
	return CCSDS_IS_REQUIRED[id];
    else
	return false;
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
bool CCSDSAEMObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSAEMDataReps; i++)
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
// std::ostream& operator<< (std::ostream &output, const CCSDSAEMObType *myAEM)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSAEMObType value and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myMetadata>    CCSDS AEM metadata to write out
 *
 * return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAEMObType *myAEM)
{
    switch (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_ATTITUDETYPE_ID))
    {
        case CCSDSObType::CCSDS_QUATERNION_ID:
        {
            if (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID) == CCSDSObType::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEM->ccsdsAEMQuaternion->epoch 
                       << myAEM->ccsdsAEMQuaternion->qC
                       << myAEM->ccsdsAEMQuaternion->q1
                       << myAEM->ccsdsAEMQuaternion->q2
                       << myAEM->ccsdsAEMQuaternion->q3 << endl;

                return output;
            }
            else if (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID) == CCSDSObType::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEM->ccsdsAEMQuaternion->epoch 
                       << myAEM->ccsdsAEMQuaternion->q1
                       << myAEM->ccsdsAEMQuaternion->q2
                       << myAEM->ccsdsAEMQuaternion->q3
                       << myAEM->ccsdsAEMQuaternion->qC << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSObType::CCSDS_QUATERNION_DERIVATIVE_ID:
        {
            if (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID) == CCSDSObType::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEM->ccsdsAEMQuaternion->epoch 
                       << myAEM->ccsdsAEMQuaternion->qC
                       << myAEM->ccsdsAEMQuaternion->q1
                       << myAEM->ccsdsAEMQuaternion->q2 
                       << myAEM->ccsdsAEMQuaternion->q3
                       << myAEM->ccsdsAEMQuaternion->qCDot
                       << myAEM->ccsdsAEMQuaternion->q1Dot
                       << myAEM->ccsdsAEMQuaternion->q2Dot
                       << myAEM->ccsdsAEMQuaternion->q3Dot << endl;

                return output;
            }
            else if (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID) == CCSDSObType::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEM->ccsdsAEMQuaternion->epoch 
                       << myAEM->ccsdsAEMQuaternion->q1
                       << myAEM->ccsdsAEMQuaternion->q2
                       << myAEM->ccsdsAEMQuaternion->q3 
                       << myAEM->ccsdsAEMQuaternion->qC
                       << myAEM->ccsdsAEMQuaternion->q1Dot
                       << myAEM->ccsdsAEMQuaternion->q2Dot
                       << myAEM->ccsdsAEMQuaternion->q3Dot
                       << myAEM->ccsdsAEMQuaternion->qCDot << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSObType::CCSDS_QUATERNION_RATE_ID:
        {
            if (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID) == CCSDSObType::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEM->ccsdsAEMQuaternion->epoch 
                       << myAEM->ccsdsAEMQuaternion->qC
                       << myAEM->ccsdsAEMQuaternion->q1
                       << myAEM->ccsdsAEMQuaternion->q2 
                       << myAEM->ccsdsAEMQuaternion->q3
                       << myAEM->ccsdsAEMQuaternion->xRate
                       << myAEM->ccsdsAEMQuaternion->yRate
                       << myAEM->ccsdsAEMQuaternion->zRate << endl;

                return output;
            }
            else if (myAEM->ccsdsAEMMetaData->GetIntegerDataParameter(CCSDSAEMMetaData::CCSDS_AEM_QUATERNIONTYPE_ID) == CCSDSObType::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEM->ccsdsAEMQuaternion->epoch 
                       << myAEM->ccsdsAEMQuaternion->q1
                       << myAEM->ccsdsAEMQuaternion->q2
                       << myAEM->ccsdsAEMQuaternion->q3 
                       << myAEM->ccsdsAEMQuaternion->qC
                       << myAEM->ccsdsAEMQuaternion->xRate
                       << myAEM->ccsdsAEMQuaternion->yRate
                       << myAEM->ccsdsAEMQuaternion->zRate << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSObType::CCSDS_EULER_ANGLE_ID:
        {
            output << myAEM->ccsdsAEMEulerAngle->epoch
                   << myAEM->ccsdsAEMEulerAngle->xAngle
                   << myAEM->ccsdsAEMEulerAngle->yAngle
                   << myAEM->ccsdsAEMEulerAngle->zAngle << endl;

            return output;
        }

        break;

        case CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID:
        {
            output << myAEM->ccsdsAEMEulerAngle->epoch
                   << myAEM->ccsdsAEMEulerAngle->xAngle
                   << myAEM->ccsdsAEMEulerAngle->yAngle
                   << myAEM->ccsdsAEMEulerAngle->zAngle
                   << myAEM->ccsdsAEMEulerAngle->xRate
                   << myAEM->ccsdsAEMEulerAngle->yRate
                   << myAEM->ccsdsAEMEulerAngle->zRate << endl;

            return output;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_ID:
        {
            output << myAEM->ccsdsAEMSpinStabilized->epoch
                   << myAEM->ccsdsAEMSpinStabilized->spinAlpha
                   << myAEM->ccsdsAEMSpinStabilized->spinDelta
                   << myAEM->ccsdsAEMSpinStabilized->spinAngle
                   << myAEM->ccsdsAEMSpinStabilized->spinAngleVelocity << endl;
            return output;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_NUTATION_ID:
        {
            output << myAEM->ccsdsAEMSpinStabilized->epoch
                   << myAEM->ccsdsAEMSpinStabilized->spinAlpha
                   << myAEM->ccsdsAEMSpinStabilized->spinDelta
                   << myAEM->ccsdsAEMSpinStabilized->spinAngle
                   << myAEM->ccsdsAEMSpinStabilized->spinAngleVelocity
                   << myAEM->ccsdsAEMSpinStabilized->nutation
                   << myAEM->ccsdsAEMSpinStabilized->nutationPeriod
                   << myAEM->ccsdsAEMSpinStabilized->nutationPhase
                   << endl;

            return output;
        }
        break;

        default:

            return output;
            break;

    }

    return output;
}