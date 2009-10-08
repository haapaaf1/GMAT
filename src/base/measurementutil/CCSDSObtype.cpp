#include "CCSDSObType.hpp";
//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string CCSDSObType::CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps] =
{
    "Quaternion",
    "Euler Angle",
    "Spin Stabilized",
    "State Vector",
    "Keplerian Elements",
    "Spacecraft Parameters",
    "Maneuver",
    "Attitude Maneuver",
    "Generic Data Type"
};

const std::string CCSDSObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

const std::string CCSDSObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSDataReps] =
{
    "CCSDS Version",
    "Creation Date",
    "Originator",
    "Header Comments",
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
    "State Vector Epoch",
    "State Vector X",
    "State Vector Y",
    "State Vector Z",
    "State Vector X Dot",
    "State Vector Y Dot",
    "State Vector Z Dot",
    "State Vector Comments",
    "Keplerian Elements Epoch",
    "Keplerian Elements Semimajor Axis",
    "Keplerian Elements Eccentricity",
    "Keplerian Elements Inclination",
    "Keplerian Elements Right Ascension of the Ascending Node",
    "Keplerian Elements Argument of Pericenter",
    "Keplerian Elements True Anomaly",
    "Keplerian Elements Mean Anomaly",
    "Keplerian Elements Gravitational Coefficient",
    "Keplerian Elements Comments",
    "Spacecraft Parameters Epoch",
    "Spacecraft Parameters Mass",
    "Spacecraft Parameters Solar Radiation Area",
    "Spacecraft Parameters Solar Radiation Coefficient",
    "Spacecraft Parameters Drag Area",
    "Spacecraft Parameters Drag Coefficient",
    "Spacecraft Parameters Inertia Ref Frame",
    "Spacecraft Parameters Inertia Component (1,1)",
    "Spacecraft Parameters Inertia Component (2,2)",
    "Spacecraft Parameters Inertia Component (3,3)",
    "Spacecraft Parameters Inertia Component (1,2)",
    "Spacecraft Parameters Inertia Component (1,3)",
    "Spacecraft Parameters Inertia Component (2,3)",
    "Spacecraft Parameters Comments",
    "Maneuver Ref Ignition Epoch",
    "Maneuver Duration",
    "Maneuver Ref Delta Mass",
    "Maneuver Ref Frame",
    "Maneuver Ref DeltaV1",
    "Maneuver Ref DeltaV2",
    "Maneuver Ref DeltaV3",
    "Maneuver Comments",
    "Attitude Maneuver Epoch Start",
    "Attitude Maneuver Duration",
    "Attitude Maneuver Ref Frame",
    "Attitude Maneuver TOR1",
    "Attitude Maneuver TOR2",
    "Attitude Maneuver TOR3",
    "Attitude Maneuver Comments",
    "General Data Keyword",
    "General Data TimeTag",
    "General Data Measurement",
    "General Data Comments"
};

const std::string CCSDSObType::CCSDS_KEYWORDS[EndCCSDSDataReps] =
{
    "CCSDS_VERSION",
    "CREATION_DATE",
    "ORIGINATOR",
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
    "EPOCH",
    "X",
    "Y",
    "Z",
    "X_DOT",
    "Y_DOT",
    "Z_DOT",
    "COMMENT",
    "EPOCH",
    "SEMI_MAJ_AXIS",
    "ECCENTRICITY",
    "INCLINATION",
    "RA_OF_ASC_NODE",
    "ARG_OF_PERICENTER",
    "TRUE_ANOMALY",
    "MEAN_ANOMALY",
    "GM",
    "COMMENT",
    "EPOCH",
    "MASS",
    "SOLAR_RAD_AREA",
    "SOLAR_RAD_COEFF",
    "DRAG_AREA",
    "DRAG_COEFF",
    "INERTIA_REF_FRAME",
    "I11",
    "I22",
    "I33",
    "I12",
    "I13",
    "I23",
    "COMMENT",
    "MAN_EPOCH_IGNITION",
    "MAN_DURATION",
    "MAN_DELTA_MASS",
    "MAN_REF_FRAME",
    "MAN_DV_1",
    "MAN_DV_2",
    "MAN_DV_3",
    "COMMENT",
    "MAN_EPOCH_START",
    "MAN_DURATION",
    "MAN_REF_FRAME",
    "MAN_TOR1",
    "MAN_TOR2",
    "MAN_TOR3",
    "COMMENT",
    "",
    "",
    "",
    "COMMENTS"
};

const bool CCSDSObType::CCSDS_IS_REQUIRED[EndCCSDSDataReps] =
{
    true,
    true,
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    false,
    false,
    false,
    true,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    true,
    true,
    true,
    true,
    true,
    true,
    false,
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSObType::CCSDS_PARAMETER_TYPE[EndCCSDSDataReps] =
{
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
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
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
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
    Gmat::STRING_TYPE,
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
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

const std::string CCSDSObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSDataReps] =
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
    "km",
    "km",
    "km",
    "km/s",
    "km/s",
    "km/s",
    "",
    "",
    "km",
    "",
    "deg",
    "deg",
    "deg",
    "deg",
    "deg",
    "km^3/s^2",
    "",
    "",
    "kg",
    "m^2",
    "",
    "m^2",
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
    "",
    "kg",
    "s",
    "km/s",
    "km/s",
    "km/s",
    "",
    "",
    "s",
    "",
    "N m",
    "N m",
    "N m",
    "",
    "",
    "",
    "",
    "",
};

//------------------------------------------------------------------------------
//  CCSDSObType(const std::string &type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::CCSDSObType(const std::string &type, const std::string &name) :
   ObType(type, name),
    ccsdsHeader(NULL),
    ccsdsData(NULL),
    ccsdsQuaternion(NULL),
    ccsdsEulerAngle(NULL),
    ccsdsSpinStabilized(NULL),
    ccsdsStateVector(NULL),
    ccsdsKeplerianElements(NULL),
    ccsdsSpacecraftParameters(NULL),
    ccsdsManeuver(NULL)
{
}

//------------------------------------------------------------------------------
//  CCSDSObType(const CCSDSObType &ob)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::CCSDSObType(const CCSDSObType &ob) : ObType(ob),
    ccsdsHeader(ob.ccsdsHeader),
    ccsdsData(ob.ccsdsData),
    ccsdsQuaternion(ob.ccsdsQuaternion),
    ccsdsEulerAngle(ob.ccsdsEulerAngle),
    ccsdsSpinStabilized(ob.ccsdsSpinStabilized),
    ccsdsStateVector(ob.ccsdsStateVector),
    ccsdsKeplerianElements(ob.ccsdsKeplerianElements),
    ccsdsSpacecraftParameters(ob.ccsdsSpacecraftParameters),
    ccsdsManeuver(ob.ccsdsManeuver)
{
}

//---------------------------------------------------------------------------
//  CCSDSObType& operator=(const CCSDSObType &ob)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <ob> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSObType& CCSDSObType::operator=(const CCSDSObType &ob)
{
   if (&ob == this)
      return *this;

    ccsdsHeader = ob.ccsdsHeader;
    ccsdsData = ob.ccsdsData;
    ccsdsQuaternion = ob.ccsdsQuaternion;
    ccsdsEulerAngle = ob.ccsdsEulerAngle;
    ccsdsSpinStabilized = ob.ccsdsSpinStabilized;
    ccsdsStateVector = ob.ccsdsStateVector;
    ccsdsKeplerianElements = ob.ccsdsKeplerianElements;
    ccsdsSpacecraftParameters = ob.ccsdsSpacecraftParameters;
    ccsdsManeuver = ob.ccsdsManeuver;
    
   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSObType()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSObType::~CCSDSObType()
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
std::string CCSDSObType::GetDataParameterText(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
   {
      return CCSDS_FILEFORMAT_DESCRIPTIONS[id];
   }
   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
//  std::string  GetDataUnits(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetDataUnits(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
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
Integer CCSDSObType::GetDataParameterID(const std::string &str) const
{
    std::string regex = "^" + str + "$";
    
    for (Integer i = 0; i < EndCCSDSDataReps; i++)
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
Gmat::ParameterType CCSDSObType::GetDataParameterType(const Integer id) const
{
   if ((id >= 0) && (id < EndCCSDSDataReps))
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
std::string CCSDSObType::GetDataParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetDataParameterType(id)];
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//---------------------------------------------------------------------------
Integer CCSDSObType::GetIntegerDataParameter(const Integer id) const
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetIntegerDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Integer CCSDSObType::GetIntegerDataParameter(const std::string &label) const
{
   return GetIntegerDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <id> Integer ID associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool CCSDSObType::GetBoolDataParameter(const Integer id) const
{
    return false;
}

//------------------------------------------------------------------------------
// virtual bool GetBoolDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieves boolean data parameter
 *
 * @param <label> String associated with data parameter
 *
 * @return The boolean data parameter
 *
 */
//------------------------------------------------------------------------------
bool CCSDSObType::GetBoolDataParameter(const std::string &label) const
{
   return GetBoolDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringArrayDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
StringArray CCSDSObType::GetStringArrayDataParameter(const Integer id) const
{
    switch (id)
    {
	case CCSDS_HEADERCOMMENTS_ID:

	    return ccsdsHeader->comments;

	case CCSDS_QUATERNION_COMMENTS_ID:

	    return ccsdsQuaternion->comments;

        case CCSDS_EULERANGLE_COMMENTS_ID:

	    return ccsdsEulerAngle->comments;

        case CCSDS_SPINSTABILIZED_COMMENTS_ID:

	    return ccsdsSpinStabilized->comments;

        case CCSDS_STATEVECTOR_COMMENTS_ID:

	    return ccsdsStateVector->comments;

        case CCSDS_KEPLERIANELEMENTS_COMMENTS_ID:

	    return ccsdsKeplerianElements->comments;

        case CCSDS_SPACECRAFTPARAMETERS_COMMENTS_ID:

	    return ccsdsSpacecraftParameters->comments;

        case CCSDS_MANUEVER_COMMENTS_ID:

	    return ccsdsManeuver->comments;

        case CCSDS_ATTITUDEMANUEVER_COMMENTS_ID:

	    return ccsdsAttitudeManeuver->comments;

        case CCSDS_GENERALDATA_COMMENTS_ID:

	    return ccsdsData->comments;

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
StringArray CCSDSObType::GetStringArrayDataParameter(const std::string &label) const
{
   return GetStringArrayDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetStringDataParameter(const Integer id) const
{
    switch (id)
    {
        case CCSDS_QUATERNION_TYPE_ID:

	    return ccsdsQuaternion->quarternionType;

        case CCSDS_QUATERNION_FRAMEA_ID:

	    return ccsdsQuaternion->frameA;

	case CCSDS_QUATERNION_FRAMEB_ID:

	    return ccsdsQuaternion->frameB;

	case CCSDS_QUATERNION_DIRECTION_ID:

	    return ccsdsQuaternion->direction;

        case CCSDS_EULERANGLE_TYPE_ID:

	    return ccsdsEulerAngle->eulerAngleType;

	case CCSDS_EULERANGLE_FRAMEA_ID:

	    return ccsdsEulerAngle->frameA;

	case CCSDS_EULERANGLE_FRAMEB_ID:

	    return ccsdsEulerAngle->frameB;

        case CCSDS_EULERANGLE_DIRECTION_ID:

	    return ccsdsEulerAngle->direction;

        case CCSDS_EULERANGLE_ROTATIONSEQUENCE_ID:

	    return ccsdsEulerAngle->rotationSequence;

        case CCSDS_EULERANGLE_RATEFRAME_ID:

	    return ccsdsEulerAngle->rateFrame;

        case CCSDS_SPINSTABILIZED_ATTITUDETYPE_ID:

	    return ccsdsSpinStabilized->attitudeType;

	case CCSDS_SPINSTABILIZED_FRAMEA_ID:

	    return ccsdsSpinStabilized->frameA;

	case CCSDS_SPINSTABILIZED_FRAMEB_ID:

	    return ccsdsSpinStabilized->frameB;

	case CCSDS_SPINSTABILIZED_DIRECTION_ID:

	    return ccsdsSpinStabilized->direction;

	case CCSDS_STATEVECTOR_EPOCH_ID:

	    return ccsdsStateVector->epoch;

	case CCSDS_SPACECRAFTPARAMETERS_INERTIAREFFRAME_ID:

	    return ccsdsSpacecraftParameters->inertiaRefFrame;

        case CCSDS_MANUEVER_IGNITIONEPOCH_ID:

	    return ccsdsManeuver->ignitionEpoch;

        case CCSDS_MANUEVER_REFFRAME_ID:

	    return ccsdsManeuver->refFrame;

        case CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID:

	    return ccsdsAttitudeManeuver->epochStart;

        case CCSDS_ATTITUDEMANUEVER_REFFRAME_ID:

	    return ccsdsAttitudeManeuver->refFrame;

        case CCSDS_GENERALDATA_TIMETAG_ID:

	    return ccsdsData->timeTag;

	case CCSDS_CREATIONDATE_ID:

	    return ccsdsHeader->creationDate;

	case CCSDS_ORIGINATOR_ID:

	    return ccsdsHeader->originator;
	    
        default:

            return GmatBase::STRING_PARAMETER_UNDEFINED;

    }

}

//------------------------------------------------------------------------------
// std::string GetStringDataParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetStringDataParameter(const std::string &label) const
{
   return GetStringDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// Real GetRealDataParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * @see ObType
 */
//------------------------------------------------------------------------------
Real CCSDSObType::GetRealDataParameter(const Integer id) const
{
    switch (id)
    {

	case CCSDS_QUATERNION_Q1_ID:

            return ccsdsQuaternion->q1;

	case CCSDS_QUATERNION_Q2_ID:

            return ccsdsQuaternion->q2;

	case CCSDS_QUATERNION_Q3_ID:

            return ccsdsQuaternion->q3;

	case CCSDS_QUATERNION_QC_ID:

            return ccsdsQuaternion->qC;

	case CCSDS_QUATERNION_Q1DOT_ID:

            return ccsdsQuaternion->q1Dot;

        case CCSDS_QUATERNION_Q2DOT_ID:

            return ccsdsQuaternion->q2Dot;

        case CCSDS_QUATERNION_Q3DOT_ID:

            return ccsdsQuaternion->q3Dot;

        case CCSDS_QUATERNION_QCDOT_ID:

            return ccsdsQuaternion->qCDot;

        case CCSDS_EULERANGLE_XANGLE_ID:

            return ccsdsEulerAngle->xAngle;

        case CCSDS_EULERANGLE_YANGLE_ID:

            return ccsdsEulerAngle->yAngle;

        case CCSDS_EULERANGLE_ZANGLE_ID:

            return ccsdsEulerAngle->zAngle;

        case CCSDS_EULERANGLE_XRATE_ID:

            return ccsdsEulerAngle->xRate;

        case CCSDS_EULERANGLE_YRATE_ID:

            return ccsdsEulerAngle->yRate;

        case CCSDS_EULERANGLE_ZRATE_ID:

            return ccsdsEulerAngle->zRate;

	case CCSDS_SPINSTABILIZED_SPINALPHA_ID:

            return ccsdsSpinStabilized->spinAlpha;

	case CCSDS_SPINSTABILIZED_SPINDELTA_ID:

            return ccsdsSpinStabilized->spinDelta;

	case CCSDS_SPINSTABILIZED_SPINANGLE_ID:

            return ccsdsSpinStabilized->spinAngle;

	case CCSDS_SPINSTABILIZED_SPINANGLEVEOCITY_ID:

            return ccsdsSpinStabilized->spinAngleVelocity;

	case CCSDS_SPINSTABILIZED_NUTATION_ID:

            return ccsdsSpinStabilized->nutation;

	case CCSDS_SPINSTABILIZED_NUTATIONPERIOD_ID:

            return ccsdsSpinStabilized->nutationPeriod;

	case CCSDS_SPINSTABILIZED_NUTATIONPHASE_ID:

            return ccsdsSpinStabilized->nutationPhase;

	case CCSDS_STATEVECTOR_X_ID:

            return ccsdsStateVector->x;

	case CCSDS_STATEVECTOR_Y_ID:

            return ccsdsStateVector->y;

	case CCSDS_STATEVECTOR_Z_ID:

            return ccsdsStateVector->z;

	case CCSDS_STATEVECTOR_XDOT_ID:

            return ccsdsStateVector->xDot;

        case CCSDS_STATEVECTOR_YDOT_ID:

            return ccsdsStateVector->yDot;

	case CCSDS_STATEVECTOR_ZDOT_ID:

            return ccsdsStateVector->zDot;

	case CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID:

            return ccsdsKeplerianElements->semiMajorAxis;

	case CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID:

            return ccsdsKeplerianElements->eccentricity;

	case CCSDS_KEPLERIANELEMENTS_INCLINATION_ID:

            return ccsdsKeplerianElements->inclination;

	case CCSDS_KEPLERIANELEMENTS_RAAN_ID:

            return ccsdsKeplerianElements->raan;

	case CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID:

            return ccsdsKeplerianElements->argumentOfPericenter;

	case CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID:

            return ccsdsKeplerianElements->trueAnomaly;

	case CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID:

            return ccsdsKeplerianElements->meanAnomaly;

	case CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID:

            return ccsdsKeplerianElements->gravitationalCoefficient;

	case CCSDS_SPACECRAFTPARAMETERS_MASS_ID:

            return ccsdsSpacecraftParameters->mass;

	case CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID:

            return ccsdsSpacecraftParameters->solarRadiationArea;

	case CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID:

            return ccsdsSpacecraftParameters->solarRadiationCoefficient;

	case CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID:

            return ccsdsSpacecraftParameters->dragArea;

	case CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID:

            return ccsdsSpacecraftParameters->dragCoefficient;

	case CCSDS_SPACECRAFTPARAMETERS_I11_ID:

            return ccsdsSpacecraftParameters->i11;

	case CCSDS_SPACECRAFTPARAMETERS_I22_ID:

            return ccsdsSpacecraftParameters->i22;

	case CCSDS_SPACECRAFTPARAMETERS_I33_ID:

            return ccsdsSpacecraftParameters->i33;

	case CCSDS_SPACECRAFTPARAMETERS_I12_ID:

            return ccsdsSpacecraftParameters->i12;

	case CCSDS_SPACECRAFTPARAMETERS_I13_ID:

            return ccsdsSpacecraftParameters->i13;

	case CCSDS_SPACECRAFTPARAMETERS_I23_ID:

            return ccsdsSpacecraftParameters->i23;

        case CCSDS_MANUEVER_DURATION_ID:

            return ccsdsManeuver->duration;

        case CCSDS_MANUEVER_DELTAMASS_ID:

            return ccsdsManeuver->deltaMass;

        case CCSDS_MANUEVER_DELTAV1_ID:

            return ccsdsManeuver->deltaV1;

        case CCSDS_MANUEVER_DELTAV2_ID:

            return ccsdsManeuver->deltaV2;

        case CCSDS_MANUEVER_DELTAV3_ID:

            return ccsdsManeuver->deltaV3;

        case CCSDS_ATTITUDEMANUEVER_DURATION_ID:

            return ccsdsAttitudeManeuver->duration;

        case CCSDS_ATTITUDEMANUEVER_TOR1_ID:

            return ccsdsAttitudeManeuver->tor1;

        case CCSDS_ATTITUDEMANUEVER_TOR2_ID:

            return ccsdsAttitudeManeuver->tor2;

        case CCSDS_ATTITUDEMANUEVER_TOR3_ID:

            return ccsdsAttitudeManeuver->tor3;

        case CCSDS_GENERALDATA_MEASUREMENT_ID:

            return ccsdsData->measurement;

        case CCSDS_VERSION_ID:

            return ccsdsHeader->ccsdsVersion;

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
Real CCSDSObType::GetRealDataParameter(const std::string &label) const
{
   return GetRealDataParameter(GetDataParameterID(label));
}

//------------------------------------------------------------------------------
// const std::string* GetDataTypes() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable data types.
 *
 * @return String array of all data types.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSObType::GetDataTypes() const
{
   return CCSDS_DATATYPE_DESCRIPTIONS;
}

//------------------------------------------------------------------------------
// std::string GetDataTypeText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type text corresponding to a ID
 *
 * @param <id> Integer ID associated with the data type
 * @return The string description of the data type
 *
 */
//------------------------------------------------------------------------------
std::string CCSDSObType::GetDataTypeText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTypeReps))
   {
      return CCSDS_DATATYPE_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
}

//------------------------------------------------------------------------------
// Integer GetDataTypeID(const std::string &label)
//------------------------------------------------------------------------------
/**
 * Code used to obtain the data type ID
 *
 * @param <label> The string label associated with the data type
 * @return The integer data type ID
 *
 */
//------------------------------------------------------------------------------
Integer CCSDSObType::GetDataTypeID(const std::string &label)
{
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
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
const std::string* CCSDSObType::GetTimeSystems() const
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
std::string CCSDSObType::GetTimeSystemText(const Integer &id) const
{
   if ((id >= 0) && (id < EndCCSDSTimeReps))
   {
      return CCSDS_TIMESYSTEM_DESCRIPTIONS[id];
   }

   return GmatBase::STRING_PARAMETER_UNDEFINED;
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
Integer CCSDSObType::GetTimeSystemID(const std::string &label)
{

    std::string regex = "^" + label + "$";
    
    for (Integer i = 0; i < EndCCSDSTimeReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_TIMESYSTEM_DESCRIPTIONS[i]))
        {
	    return i;
	}
    
    }
      
    return GmatBase::INTEGER_PARAMETER_UNDEFINED;
 
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
bool CCSDSObType::IsParameterRequired(const Integer id) const
{
if (id > 0 && id <= EndCCSDSDataReps)
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
bool CCSDSObType::CheckDataAvailability(const std::string str) const
{

    std::string regex = "^" + str + "$";

    for (Integer i = 0; i < EndCCSDSDataReps; i++)
    {
        if (pcrecpp::RE(regex,pcrecpp::RE_Options().set_caseless(true)
                                          .set_extended(true)
                       ).FullMatch(CCSDS_FILEFORMAT_DESCRIPTIONS[i]))
        {
            return true;
        }
    }

   return false;

}

//------------------------------------------------------------------------------
// const std::string* GetKeywords() const
//------------------------------------------------------------------------------
/**
 * Returns the string array of allowable CCSDS TDM keywords
 *
 * @return String array of keywords.
 *
 */
//------------------------------------------------------------------------------
const std::string* CCSDSObType::GetKeywords() const
{
   return CCSDS_KEYWORDS;
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
const Integer CCSDSObType::GetKeywordID(const std::string str) const
{
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
std::string CCSDSObType::GetUnits(const Integer &id) const
{
   return std::string("");
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSQuaternion *myQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSQuaternion *myQuaternion)
{
   using namespace std;

   output << "Quaternion Type = " << myQuaternion->quarternionType << endl;
   output << "Q_FRAME_A = " << myQuaternion->frameA << endl;
   output << "Q_FRAME_B = " << myQuaternion->frameB << endl;
   output << "Q_DIR = " << myQuaternion->direction << endl;
   output << "Q1 = " << myQuaternion->q1 << endl;
   output << "Q2 = " << myQuaternion->q2 << endl;
   output << "Q3 = " << myQuaternion->q3 << endl;
   output << "QC = " << myQuaternion->qC << endl;
   output << "Q1_DOT = " << myQuaternion->q1Dot << endl;
   output << "Q2_DOT = " << myQuaternion->q2Dot << endl;
   output << "Q3_DOT = " << myQuaternion->q3Dot << endl;
   output << "QC_DOT = " << myQuaternion->qCDot << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSEulerAngle *myEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSEulerAngle *myEulerAngle)
{
   using namespace std;

   output << "Euler Angle Type = " << myEulerAngle->eulerAngleType << endl;
   output << "EULER_FRAME_A = " << myEulerAngle->frameA << endl;
   output << "EULER_FRAME_B = " << myEulerAngle->frameB << endl;
   output << "EULER_DIR = " << myEulerAngle->direction << endl;
   output << "EULER_ROT_SEQ = " << myEulerAngle->rotationSequence << endl;
   output << "RATE_FRAME = " << myEulerAngle->rateFrame << endl;
   output << "X_ANGLE = " << myEulerAngle->xAngle << endl;
   output << "Y_ANGLE = " << myEulerAngle->yAngle << endl;
   output << "Z_ANGLE = " << myEulerAngle->zAngle << endl;
   output << "X_RATE = " << myEulerAngle->xRate << endl;
   output << "Y_RATE = " << myEulerAngle->yRate << endl;
   output << "Z_RATE = " << myEulerAngle->zRate << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSSpinStabilized *mySpinStabilized)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySpinStabilized>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSSpinStabilized *mySpinStabilized)
{
   using namespace std;

   output << "Attitude type = " << mySpinStabilized->attitudeType << endl;
   for (Integer i = 0; i < mySpinStabilized->comments.size(); i++)
   {
       output << "COMMENT " << mySpinStabilized->comments[i] << endl;
   }
   output << "SPIN_FRAME_A = " << mySpinStabilized->frameA << endl;
   output << "SPIN_FRAME_B = " << mySpinStabilized->frameB << endl;
   output << "SPIN_DIR = " << mySpinStabilized->direction << endl;
   output << "SPIN_ALPHA = " << mySpinStabilized->spinAlpha << endl;
   output << "SPIN_DELTA = " << mySpinStabilized->spinDelta << endl;
   output << "SPIN_ANGLE = " << mySpinStabilized->spinAngle << endl;
   output << "SPIN_ANGLE_VEL = " << mySpinStabilized->spinAngleVelocity << endl;
   output << "NUTATION = " << mySpinStabilized->nutation << endl;
   output << "NUTATION_PER = " << mySpinStabilized->nutationPeriod << endl;
   output << "NUTATION_PHASE = " << mySpinStabilized->nutationPhase << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSStateVector *myStateVector)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myStateVector>    CCSDS state vector data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSStateVector *myStateVector)
{
   using namespace std;

   output << "EPOCH = " << myStateVector->epoch << endl;
   output << "X = " << myStateVector->x << endl;
   output << "Y = " << myStateVector->y << endl;
   output << "Z = " << myStateVector->z << endl;
   output << "X_DOT = " << myStateVector->xDot << endl;
   output << "Y_DOT = " << myStateVector->yDot << endl;
   output << "Z_DOT = " << myStateVector->zDot << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSObType *myOb)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myOb>    CCSDS data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSKeplerianElements *myKeplerianElements)
{
   using namespace std;

   output << "SEMI_MAJOR_AXIS = " << myKeplerianElements->semiMajorAxis << endl;
   output << "ECCENTRICITY = " << myKeplerianElements->eccentricity << endl;
   output << "INCLINATION = " << myKeplerianElements->inclination << endl;
   output << "RA_OF_ASC_NODE = " << myKeplerianElements->raan << endl;
   output << "ARG_OF_PERICENTER = " << myKeplerianElements->argumentOfPericenter << endl;
   output << "TRUE_ANOMALY = " << myKeplerianElements->trueAnomaly << endl;
   output << "MEAN_ANOMALY = " << myKeplerianElements->meanAnomaly << endl;
   output << "GM = " << myKeplerianElements->gravitationalCoefficient << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSSpacecraftParameters *mySpacecraftParameters)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySpacecraftParameters>    CCSDS spacecraft parameter data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSSpacecraftParameters *mySpacecraftParameters)
{
   using namespace std;

   output << "MASS = " << mySpacecraftParameters->mass << endl;
   output << "SOLAR_RAD_AREA = " << mySpacecraftParameters->solarRadiationArea << endl;
   output << "SOLAR_RAD_COEFF = " << mySpacecraftParameters->solarRadiationCoefficient << endl;
   output << "DRAG_AREA = " << mySpacecraftParameters->dragArea << endl;
   output << "DRAG_COEFF = " << mySpacecraftParameters->dragCoefficient << endl;
   output << "INERTIA_REF_FRAME = " << mySpacecraftParameters->inertiaRefFrame << endl;
   output << "I11 = " << mySpacecraftParameters->i11 << endl;
   output << "I22 = " << mySpacecraftParameters->i22 << endl;
   output << "I33 = " << mySpacecraftParameters->i33 << endl;
   output << "I12 = " << mySpacecraftParameters->i12 << endl;
   output << "I13 = " << mySpacecraftParameters->i13 << endl;
   output << "I23 = " << mySpacecraftParameters->i23 << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSData *myData)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myData>    CCSDS data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSData *myData)
{
    using namespace std;

   output << myData->keywordID << " = " << myData->timeTag << " " << myData->measurement << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myHeader)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myHeader>    CCSDS header data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myHeader)
{
    using namespace std;

    output << "CCSDS_" << myHeader->fileType << "_VERS = " << myHeader->ccsdsVersion << endl;
    for (Integer i = 0; i < myHeader->comments.size(); i++)
    {
        output << "COMMENT " << myHeader->comments[i] << endl;
    }
    output << "CREATION_DATE = " << myHeader->creationDate << endl;
    output << "ORIGINATOR = " << myHeader->originator << endl;

    return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSManeuver *myManeuver)
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
std::ostream& operator<< (std::ostream &output, const CCSDSManeuver *myManeuver)
{
    using namespace std;

    output << "MAN_EPOCH_IGNITION = " << myManeuver->ignitionEpoch << endl;
    output << "MAN_DURATION = " << myManeuver->duration << endl;
    output << "MAN_DELTA_MASS = " << myManeuver->deltaMass << endl;
    output << "MAN_REF_FRAME = " << myManeuver->refFrame << endl;
    output << "MAN_DV_1 = " << myManeuver->deltaV1 << endl;
    output << "MAN_DV_2 = " << myManeuver->deltaV2 << endl;
    output << "MAN_DV_3 = " << myManeuver->deltaV3 << endl;

   return output;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, 
//                           const CCSDSAttitudeManeuver *myAttitudeManeuver)
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
                          const CCSDSAttitudeManeuver *myAttitudeManeuver)
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