#include "CCSDSOPMObtype.hpp"

//------------------------------------------------------------------------------
//  CCSDSOPMSpacecraftParameters()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSOPMSpacecraftParameters::CCSDSOPMSpacecraftParameters() :
    mass(0),
    solarRadiationArea(0),
    solarRadiationCoefficient(0),
    dragArea(0),
    dragCoefficient(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMSpacecraftParameters(const CCSDSOPMSpacecraftParameters &opmSP)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSOPMSpacecraftParameters::CCSDSOPMSpacecraftParameters
               (const CCSDSOPMSpacecraftParameters &opmSP) :
    mass(opmSP.mass),
    solarRadiationArea(opmSP.solarRadiationArea),
    solarRadiationCoefficient(opmSP.solarRadiationCoefficient),
    dragArea(opmSP.dragArea),
    dragCoefficient(opmSP.dragCoefficient),
    comments(opmSP.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMSpacecraftParameters& operator=
//                                   (const CCSDSOPMSpacecraftParameters &opmSP)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSOPMSpacecraftParameters structures.
 *
 * @param <opmSP> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMSpacecraftParameters& CCSDSOPMSpacecraftParameters::operator=
                                     (const CCSDSOPMSpacecraftParameters &opmSP)
{
    if (&opmSP == this)
        return *this;

    mass = opmSP.mass;
    solarRadiationArea = opmSP.solarRadiationArea;
    solarRadiationCoefficient = opmSP.solarRadiationCoefficient;
    dragArea = opmSP.dragArea;
    dragCoefficient = opmSP.dragCoefficient;
    comments = opmSP.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMSpacecraftParameters()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOPMSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSOPMSpacecraftParameters::~CCSDSOPMSpacecraftParameters()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMKeplerianElements()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMKeplerianElements class
 */
//------------------------------------------------------------------------------
CCSDSOPMKeplerianElements::CCSDSOPMKeplerianElements() :
    semiMajorAxis(0),
    eccentricity(0),
    inclination(0),
    raan(0),
    argumentOfPericenter(0),
    theAnomaly(0,0,0,Anomaly::TA,false),
    gravitationalCoefficient(0),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMKeplerianElements(const CCSDSOPMKeplerianElements &opmKE)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMKeplerianElements class
 */
//------------------------------------------------------------------------------
CCSDSOPMKeplerianElements::CCSDSOPMKeplerianElements
               (const CCSDSOPMKeplerianElements &opmKE) :
    semiMajorAxis(opmKE.semiMajorAxis),
    eccentricity(opmKE.eccentricity),
    inclination(opmKE.inclination),
    raan(opmKE.raan),
    argumentOfPericenter(opmKE.argumentOfPericenter),
    theAnomaly(opmKE.theAnomaly),
    gravitationalCoefficient(opmKE.gravitationalCoefficient),
    comments(opmKE.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMKeplerianElements& operator=
//                                   (const CCSDSOPMKeplerianElements &opmKE)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSOPMKeplerianElements structures.
 *
 * @param <opmKE> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMKeplerianElements& CCSDSOPMKeplerianElements::operator=
                                     (const CCSDSOPMKeplerianElements &opmKE)
{
    if (&opmKE == this)
        return *this;

    semiMajorAxis = opmKE.semiMajorAxis;
    eccentricity = opmKE.eccentricity;
    inclination = opmKE.inclination;
    raan = opmKE.raan;
    argumentOfPericenter = opmKE.argumentOfPericenter;
    theAnomaly = opmKE.theAnomaly;
    gravitationalCoefficient = opmKE.gravitationalCoefficient;
    comments = opmKE.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMKeplerianElements()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOPMKeplerianElements class
 */
//------------------------------------------------------------------------------
CCSDSOPMKeplerianElements::~CCSDSOPMKeplerianElements()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMStateVector()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMStateVector class
 */
//------------------------------------------------------------------------------
CCSDSOPMStateVector::CCSDSOPMStateVector() : CCSDSStateVector()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMStateVector(const CCSDSOPMStateVector &opmSV)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSOPMStateVector::CCSDSOPMStateVector
               (const CCSDSOPMStateVector &opmSV) :
    CCSDSStateVector(opmSV)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMStateVector& operator=
//                                   (const CCSDSOPMStateVector &opmSV)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <opmSV> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMStateVector& CCSDSOPMStateVector::operator=
                                     (const CCSDSOPMStateVector &opmSV)
    
{
    if (&opmSV == this)
        return *this;

    CCSDSStateVector::operator=(opmSV);

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMStateVector()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOPMStateVector class
 */
//------------------------------------------------------------------------------
CCSDSOPMStateVector::~CCSDSOPMStateVector()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMManeuver()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMManeuver class
 */
//------------------------------------------------------------------------------
CCSDSOPMManeuver::CCSDSOPMManeuver() :
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
//  CCSDSOPMManeuver(const CCSDSOPMManeuver &opmM)
//------------------------------------------------------------------------------
/**
 * Constructor for the Maneuver class
 */
//------------------------------------------------------------------------------
CCSDSOPMManeuver::CCSDSOPMManeuver
               (const CCSDSOPMManeuver &opmM) :
    ignitionEpoch(opmM.ignitionEpoch),
    duration(opmM.duration),
    deltaMass(opmM.deltaMass),
    refFrame(opmM.refFrame),
    deltaV1(opmM.deltaV1),
    deltaV2(opmM.deltaV2),
    deltaV3(opmM.deltaV3),
    comments(opmM.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMManeuver& operator= (const CCSDSOPMManeuver &opmM)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Maneuver structures.
 *
 * @param <opmM> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMManeuver& CCSDSOPMManeuver::operator=
                                     (const CCSDSOPMManeuver &opmM)
{
    if (&opmM == this)
        return *this;

    ignitionEpoch = opmM.ignitionEpoch;
    duration = opmM.duration;
    deltaMass = opmM.deltaMass;
    refFrame = opmM.refFrame;
    deltaV1 = opmM.deltaV1;
    deltaV2 = opmM.deltaV2;
    deltaV3 = opmM.deltaV3;
    comments = opmM.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMManeuver()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOPMManeuver class
 */
//------------------------------------------------------------------------------
CCSDSOPMManeuver::~CCSDSOPMManeuver()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMMetaData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOPMMetaData::CCSDSOPMMetaData() :
    objectName(std::string("")),
    internationalDesignator(std::string("")),
    refFrameOrigin(std::string("")),
    refFrame(std::string("")),
    timeSystem(std::string("")),
    comments()
{
}

//------------------------------------------------------------------------------
//  CCSDSOPMMetaData(const CCSDSOPMMetaData &opmMD)
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOPMMetaData::CCSDSOPMMetaData
               (const CCSDSOPMMetaData &opmMD) :
    objectName(opmMD.objectName),
    internationalDesignator(opmMD.internationalDesignator),
    refFrameOrigin(opmMD.refFrameOrigin),
    refFrame(opmMD.refFrame),
    timeSystem(opmMD.timeSystem),
    comments(opmMD.comments)
{
}

//---------------------------------------------------------------------------
//  CCSDSOPMMetaData& operator= (const CCSDSOPMMetaData &opmMD)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CCSDSOPMMetaData structures.
 *
 * @param <opmMD> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOPMMetaData& CCSDSOPMMetaData::operator=
                                     (const CCSDSOPMMetaData &opmMD)
{
    if (&opmMD == this)
        return *this;

    objectName = opmMD.objectName;
    internationalDesignator = opmMD.internationalDesignator;
    refFrameOrigin = opmMD.refFrameOrigin;
    refFrame = opmMD.refFrame;
    timeSystem = opmMD.timeSystem;
    comments = opmMD.comments;

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOPMMetaData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOPMMetaData class
 */
//------------------------------------------------------------------------------
CCSDSOPMMetaData::~CCSDSOPMMetaData()
{
}

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOPMObType::CCSDS_OPM_KEYWORDS[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    "OBJECT_NAME",
    "OBJECT_ID",
    "CENTER_NAME",
    "REF_FRAME",
    "TIME_SYSTEM",
    "COMMENT",
    "EPOCH",
    "X",
    "Y",
    "Z",
    "X_DOT",
    "Y_DOT",
    "Z_DOT",
    "",
    "SEMI_MAJ_AXIS",
    "ECCENTRICITY",
    "INCLINATION",
    "RA_OF_ASC_NODE",
    "ARG_OF_PERICENTER",
    "TRUE_ANOMALY",
    "MEAN_ANOMALY",
    "GM",
    "",
    "MASS",
    "SOLAR_RAD_AREA",
    "SOLAR_RAD_COEFF",
    "DRAG_AREA",
    "DRAG_COEFF",
    "",
    "MAN_EPOCH_IGNITION",
    "MAN_DURATION",
    "MAN_DELTA_MASS",
    "MAN_REF_FRAME",
    "MAN_DV_1",
    "MAN_DV_2",
    "MAN_DV_3",
    ""
};

const std::string CCSDSOPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "km",
    "km",
    "km",
    "km/s",
    "km/s",
    "km/s",
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
    "kg",
    "m^2",
    "",
    "m^2",
    "",
    "",
    "",
    "kg",
    "s",
    "km/s",
    "km/s",
    "km/s",
    ""
};

const std::string CCSDSOPMObType::CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps-EndCCSDSTimeReps] =
{
    "UTC",
    "TAI",
    "TT",
    "GPS",
    "TDB",
    "TCB"
};

const std::string CCSDSOPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    "Object Name"
    "Object ID",
    "Reference Frame Origin",
    "Reference Frame",
    "Time System",
    "Comments",
    "State Vector Epoch",
    "State Vector X",
    "State Vector Y",
    "State Vector Z",
    "State Vector X Dot",
    "State Vector Y Dot",
    "State Vector Z Dot",
    "State Vector Comments",
    "Keplerian Elements Semimajor Axis",
    "Keplerian Elements Eccentricity",
    "Keplerian Elements Inclination",
    "Keplerian Elements Right Ascension of the Ascending Node",
    "Keplerian Elements Argument of Pericenter",
    "Keplerian Elements True Anomaly",
    "Keplerian Elements Mean Anomaly",
    "Keplerian Elements Gravitational Coefficient",
    "Keplerian Elements Comments",
    "Spacecraft Parameters Mass",
    "Spacecraft Parameters Solar Radiation Area",
    "Spacecraft Parameters Solar Radiation Coefficient",
    "Spacecraft Parameters Drag Area",
    "Spacecraft Parameters Drag Coefficient",
    "Spacecraft Parameters Comments",
    "Maneuver Ref Ignition Epoch",
    "Maneuver Duration",
    "Maneuver Ref Delta Mass",
    "Maneuver Ref Frame",
    "Maneuver Ref DeltaV1",
    "Maneuver Ref DeltaV2",
    "Maneuver Ref DeltaV3",
    "Maneuver Comments"
};

const bool CCSDSOPMObType::CCSDS_IS_REQUIRED[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
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
    false,
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
    true,
    false
};

const Gmat::ParameterType CCSDSOPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSOPMDataReps-EndCCSDSDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOPMDataReps))
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOPMDataReps))
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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOPMDataReps; i++)
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
   if ((id >= EndCCSDSDataReps) && (id < EndCCSDSOPMDataReps))
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

        case CCSDS_OPM_TIMESYSTEM_ID:

            return ccsdsOPMMetaData->timeSystem;

	case CCSDS_OPM_REFFRAME_ID:

            return ccsdsOPMMetaData->refFrame;

	case CCSDS_OPM_CENTERNAME_ID:

            return ccsdsOPMMetaData->refFrameOrigin;

	case CCSDS_OPM_OBJECTID_ID:

            return ccsdsOPMMetaData->internationalDesignator;

        case CCSDS_OPM_OBJECTNAME_ID:

            return ccsdsOPMMetaData->objectName;

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

        case CCSDS_OPM_METADATACOMMENTS_ID:

	    return ccsdsOPMMetaData->comments;

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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOPMDataReps; i++)
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
if (id > EndCCSDSDataReps && id <= EndCCSDSOPMDataReps)
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

    for (Integer i = EndCCSDSDataReps; i < EndCCSDSOPMDataReps; i++)
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

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMMetaData *myMetadata)
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
std::ostream& operator<< (std::ostream &output, const CCSDSOPMMetaData *myMetadata)
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
   output << "REF_FRAME = " << myMetadata->refFrame << std::endl;
   output << "TIME_SYSTEM = " << myMetadata->timeSystem << std::endl;


   output << "META_STOP" << std::endl << std::endl;

   return output;
}


//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSOPMManeuver *myManeuver)
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
std::ostream& operator<< (std::ostream &output, const CCSDSOPMManeuver *myManeuver)
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
//                      const CCSDSOPMSpacecraftParameters *mySpacecraftParameters)
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
                        const CCSDSOPMSpacecraftParameters *mySpacecraftParameters)
{
   using namespace std;

   output << "MASS = " << mySpacecraftParameters->mass << endl;
   output << "SOLAR_RAD_AREA = " << mySpacecraftParameters->solarRadiationArea << endl;
   output << "SOLAR_RAD_COEFF = " << mySpacecraftParameters->solarRadiationCoefficient << endl;
   output << "DRAG_AREA = " << mySpacecraftParameters->dragArea << endl;
   output << "DRAG_COEFF = " << mySpacecraftParameters->dragCoefficient << endl;

   return output;
}


//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSOPMStateVector *myOPMStateVector)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myOPMStateVector>    CCSDS state vector data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSOPMStateVector *myOPMStateVector)
{
   using namespace std;

   output << "EPOCH = " << myOPMStateVector->epoch << endl;
   output << "X = " << myOPMStateVector->x << endl;
   output << "Y = " << myOPMStateVector->y << endl;
   output << "Z = " << myOPMStateVector->z << endl;
   output << "X_DOT = " << myOPMStateVector->xDot << endl;
   output << "Y_DOT = " << myOPMStateVector->yDot << endl;
   output << "Z_DOT = " << myOPMStateVector->zDot << endl;

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
std::ostream& operator<< (std::ostream &output, const CCSDSOPMKeplerianElements *myKeplerianElements)
{
   using namespace std;

   output << "SEMI_MAJOR_AXIS = " << myKeplerianElements->semiMajorAxis << endl;
   output << "ECCENTRICITY = " << myKeplerianElements->eccentricity << endl;
   output << "INCLINATION = " << myKeplerianElements->inclination << endl;
   output << "RA_OF_ASC_NODE = " << myKeplerianElements->raan << endl;
   output << "ARG_OF_PERICENTER = " << myKeplerianElements->argumentOfPericenter << endl;
   output << "TRUE_ANOMALY = " << myKeplerianElements->theAnomaly.GetTrueAnomaly() << endl;
   output << "MEAN_ANOMALY = " << myKeplerianElements->theAnomaly.GetMeanAnomaly() << endl;
   output << "GM = " << myKeplerianElements->gravitationalCoefficient << endl;

   return output;
}