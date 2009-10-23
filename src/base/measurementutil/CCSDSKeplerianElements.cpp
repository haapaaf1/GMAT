#include "CCSDSKeplerianElements.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOPMObType::CCSDS_OPM_KEYWORDS[EndCCSDSOPMDataReps] =
{
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

const std::string CCSDSOPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOPMDataReps] =
{
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

const std::string CCSDSOPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMDataReps] =
{
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

const bool CCSDSOPMObType::CCSDS_IS_REQUIRED[EndCCSDSOPMDataReps] =
{
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

const Gmat::ParameterType CCSDSOPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSOPMDataReps] =
{
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
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSKeplerianElements.
 *
 * @return clone of the CCSDSKeplerianElements.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSKeplerianElements::Clone() const
{
   GmatBase *clone = new CCSDSKeplerianElements(*this);
   return (clone);
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