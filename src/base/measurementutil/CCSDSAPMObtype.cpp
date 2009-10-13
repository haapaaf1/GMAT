#include "CCSDSAPMObtype.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMObType::CCSDS_APM_KEYWORDS[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "TIME_SYSTEM",
    "COMMENT",
    "Quaternion Type",
    "EPOCH",
    "Q_FRAME_A",
    "Q_FRAME_B",
    "Q_DIR",
    "Q1",
    "Q2",
    "Q3",
    "QC",
    "Q1_DOT",
    "Q2_DOT",
    "Q3_DOT",
    "QC_DOT",
    "X_RATE",
    "Y_RATE",
    "Z_RATE",
    "COMMENT",
    "Euler Angle Type",
    "EPOCH",
    "EULER_FRAME_A",
    "EULER_FRAME_B",
    "EULER_DIR",
    "EULER_ROT_SEQ",
    "RATE_FRAME",
    "X_ANGLE",
    "Y_ANGLE",
    "Z_ANGLE",
    "X_RATE",
    "Y_RATE",
    "Z_RATE",
    "COMMENT",
    "Spin Stabilized Attitude Type",
    "EPOCH",
    "SPIN_FRAME_A",
    "SPIN_FRAME_B",
    "SPIN_DIR",
    "SPIN_ALPHA",
    "SPIN_DELTA",
    "SPIN_ANGLE",
    "SPIN_ANGLE_VEL",
    "NUTATION",
    "NUTATION_PER",
    "NUTATION_PHASE",
    "COMMENT",
    "INERTIA_REF_FRAME",
    "I11",
    "I22",
    "I33",
    "I12",
    "I13",
    "I23",
    "COMMENT",
    "MAN_EPOCH_START",
    "MAN_DURATION",
    "MAN_REF_FRAME",
    "MAN_TOR1",
    "MAN_TOR2",
    "MAN_TOR3",
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "1/s",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "deg",
    "deg",
    "deg",
    "deg/s",
    "deg/s",
    "deg/s",
    "",
    "",
    "",
    "",
    "",
    "",
    "deg",
    "deg",
    "deg",
    "deg/s",
    "deg",
    "s",
    "deg",
    "",
    "",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "",
    "",
    "s",
    "",
    "N m",
    "N m",
    "N m",
    ""
};

const std::string CCSDSAPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Time System",
    "Comments",
    "Quaternion Type",
    "Quaternion Epoch",
    "Quaternion Frame A",
    "Quaternion Frame B",
    "Quaternion Direction",
    "Quaternion Q1",
    "Quaternion Q2",
    "Quaternion Q3",
    "Quaternion QC",
    "Quaternion Q1 Dot",
    "Quaternion Q2 Dot",
    "Quaternion Q3 Dot",
    "Quaternion QC Dot",
    "Quaternion X Rate",
    "Quaternion Y Rate",
    "Quaternion Z Rate",
    "Quaternion Comments",
    "Euler Angle Type",
    "Euler Angle Epoch",
    "Euler Angle Frame A",
    "Euler Angle Frame B",
    "Euler Angle Direction",
    "Euler Angle Rotation Sequence",
    "Euler Angle Rate Frame",
    "Euler Angle X Angle",
    "Euler Angle Y Angle",
    "Euler Angle Z Angle",
    "Euler Angle X Rate",
    "Euler Angle Y Rate",
    "Euler Angle Z Rate",
    "Euler Angle Comments",
    "Spin Stabilized Attitude Type",
    "Spin Stabilized Epoch",
    "Spin Stabilized Frame A",
    "Spin Stabilized Frame B",
    "Spin Stabilized Direction",
    "Spin Stabilized Spin Alpha",
    "Spin Stabilized Spin Delta",
    "Spin Stabilized Spin Angle",
    "Spin Stabilized Spin Angle Velocity",
    "Spin Stabilized Nutation",
    "Spin Stabilized Nutation Period",
    "Spin Stabilized Nutation Phase",
    "Spin Stabilized Comments",
    "Spacecraft Inertia Ref Frame",
    "Spacecraft Inertia Component (1,1)",
    "Spacecraft Inertia Component (2,2)",
    "Spacecraft Inertia Component (3,3)",
    "Spacecraft Inertia Component (1,2)",
    "Spacecraft Inertia Component (1,3)",
    "Spacecraft Inertia Component (2,3)",
    "Spacecraft Inertia Comments",
    "Attitude Maneuver Epoch Start",
    "Attitude Maneuver Duration",
    "Attitude Maneuver Ref Frame",
    "Attitude Maneuver TOR1",
    "Attitude Maneuver TOR2",
    "Attitude Maneuver TOR3",
    "Attitude Maneuver Comments"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    true,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSAPMDataReps-EndCCSDSDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAPMDataReps))
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAPMDataReps))
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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAPMDataReps; i++)
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSAPMDataReps))
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

	case CCSDS_APM_TIMESYSTEM_ID:

            return ccsdsAPMMetaData->timeSystem;

	case CCSDS_APM_CENTERNAME_ID:

            return ccsdsAPMMetaData->refFrameOrigin;

	case CCSDS_APM_OBJECTID_ID:

            return ccsdsAPMMetaData->internationalDesignator;

        case CCSDS_APM_OBJECTNAME_ID:

            return ccsdsAPMMetaData->objectName;

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

        case CCSDS_APM_METADATACOMMENTS_ID:

	    return ccsdsAPMMetaData->comments;

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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAPMDataReps; i++)
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
if (id > EndCCSDSDataReps && id <= EndCCSDSAPMDataReps)
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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSAPMDataReps; i++)
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAPMMetaData *myMetadata)
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
std::ostream& operator<< (std::ostream &output, const CCSDSAPMMetaData *myMetadata)
{

   //output.setf(std::ios::showpoint);
   //output.setf(std::ios::scientific);

   output << "META_START" << std::endl;

   for (unsigned int i = 0; i < myMetadata->comments.size(); i++ )
   {
       output << "COMMENT " << myMetadata->comments[i] << std::endl;
   }
   output << "OBJECT_NAME = " << myMetadata->objectName << std::endl;
   output << "OBJECT_ID = " << myMetadata->internationalDesignator << std::endl;
   output << "CENTER_NAME = " << myMetadata->refFrameOrigin << std::endl;
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << std::endl;


   output << "META_STOP" << std::endl << std::endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAPMAttitudeManeuver *myAttitudeManeuver)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mymyManeuver>    CCSDS maneuver data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAPMAttitudeManeuver *myAttitudeManeuver)
{
    using namespace std;

    output << "MAN_EPOCH_START = " << myAttitudeManeuver->epochStart << endl;
    output << "MAN_DURATION = " << myAttitudeManeuver->duration << endl;
    output << "MAN_REF_FRAME = " << myAttitudeManeuver->refFrame << endl;
    output << "MAN_TOR_1 = " << myAttitudeManeuver->tor1 << endl;
    output << "MAN_TOR_2 = " << myAttitudeManeuver->tor2 << endl;
    output << "MAN_TOR_3 = " << myAttitudeManeuver->tor3 << endl;

    return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                            const CCSDSAPMSpacecraftInertia *mySpacecraftInertia)
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
                          const CCSDSAPMSpacecraftInertia *mySpacecraftInertia)
{
   using namespace std;

   output << "INERTIA_REF_FRAME = " << mySpacecraftInertia->inertiaRefFrame << endl;
   output << "I11 = " << mySpacecraftInertia->i11 << endl;
   output << "I22 = " << mySpacecraftInertia->i22 << endl;
   output << "I33 = " << mySpacecraftInertia->i33 << endl;
   output << "I12 = " << mySpacecraftInertia->i12 << endl;
   output << "I13 = " << mySpacecraftInertia->i13 << endl;
   output << "I23 = " << mySpacecraftInertia->i23 << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                       const ccsdsAPMSpinStabilized *myCCSDSAPMSpinStabilized)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myCCSDSAPMSpinStabilized>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                         const CCSDSAPMSpinStabilized *myCCSDSAPMSpinStabilized)
{
   using namespace std;

   switch(myCCSDSAPMSpinStabilized->attitudeType)
   {
       case CCSDSObType::CCSDS_QUATERNION_ID:

           break;
       default:
           break;
   }

   for (unsigned int i = 0; i < myCCSDSAPMSpinStabilized->comments.size(); i++)
   {
       output << "COMMENT " << myCCSDSAPMSpinStabilized->comments[i] << endl;
   }
   output << "SPIN_FRAME_A = " << myCCSDSAPMSpinStabilized->frameA << endl;
   output << "SPIN_FRAME_B = " << myCCSDSAPMSpinStabilized->frameB << endl;
   output << "SPIN_DIR = " << myCCSDSAPMSpinStabilized->direction << endl;
   output << "SPIN_ALPHA = " << myCCSDSAPMSpinStabilized->spinAlpha << endl;
   output << "SPIN_DELTA = " << myCCSDSAPMSpinStabilized->spinDelta << endl;
   output << "SPIN_ANGLE = " << myCCSDSAPMSpinStabilized->spinAngle << endl;
   output << "SPIN_ANGLE_VEL = " << myCCSDSAPMSpinStabilized->spinAngleVelocity << endl;
   output << "NUTATION = " << myCCSDSAPMSpinStabilized->nutation << endl;
   output << "NUTATION_PER = " << myCCSDSAPMSpinStabilized->nutationPeriod << endl;
   output << "NUTATION_PHASE = " << myCCSDSAPMSpinStabilized->nutationPhase << endl;

   return output;
}


//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAPMEulerAngle *myAPMEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAPMEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAPMEulerAngle *myAPMEulerAngle)
{
   using namespace std;

   output << "EULER_FRAME_A = " << myAPMEulerAngle->frameA << endl;
   output << "EULER_FRAME_B = " << myAPMEulerAngle->frameB << endl;
   output << "EULER_DIR = " << myAPMEulerAngle->direction << endl;
   output << "EULER_ROT_SEQ = " << myAPMEulerAngle->rotationSequence << endl;
   output << "RATE_FRAME = " << myAPMEulerAngle->rateFrame << endl;
   output << "X_ANGLE = " << myAPMEulerAngle->xAngle << endl;
   output << "Y_ANGLE = " << myAPMEulerAngle->yAngle << endl;
   output << "Z_ANGLE = " << myAPMEulerAngle->zAngle << endl;
   output << "X_RATE = " << myAPMEulerAngle->xRate << endl;
   output << "Y_RATE = " << myAPMEulerAngle->yRate << endl;
   output << "Z_RATE = " << myAPMEulerAngle->zRate << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAPMQuaternion *myAPMQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAPMQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAPMQuaternion *myAPMQuaternion)
{
   using namespace std;

   output << "Quaternion Type = " << myAPMQuaternion->quaternionType << endl;
   output << "Q_FRAME_A = " << myAPMQuaternion->frameA << endl;
   output << "Q_FRAME_B = " << myAPMQuaternion->frameB << endl;
   output << "Q_DIR = " << myAPMQuaternion->direction << endl;
   output << "Q1 = " << myAPMQuaternion->q1 << endl;
   output << "Q2 = " << myAPMQuaternion->q2 << endl;
   output << "Q3 = " << myAPMQuaternion->q3 << endl;
   output << "QC = " << myAPMQuaternion->qC << endl;
   output << "Q1_DOT = " << myAPMQuaternion->q1Dot << endl;
   output << "Q2_DOT = " << myAPMQuaternion->q2Dot << endl;
   output << "Q3_DOT = " << myAPMQuaternion->q3Dot << endl;
   output << "QC_DOT = " << myAPMQuaternion->qCDot << endl;

   return output;
}