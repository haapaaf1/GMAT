#include "CCSDSManeuver.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOPMObType::CCSDS_OPM_KEYWORDS[EndCCSDSManeuverDataReps] =
{
    "MAN_EPOCH_IGNITION",
    "MAN_DURATION",
    "MAN_DELTA_MASS",
    "MAN_REF_FRAME",
    "MAN_DV_1",
    "MAN_DV_2",
    "MAN_DV_3",
    ""
};

const std::string CCSDSOPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSManeuverDataReps] =
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

const std::string CCSDSOPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSManeuverDataReps] =
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

const bool CCSDSOPMObType::CCSDS_IS_REQUIRED[EndCCSDSManeuverDataReps] =
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

const Gmat::ParameterType CCSDSOPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSManeuverDataReps] =
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
CCSDSManeuver::CCSDSManeuver() :
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
CCSDSManeuver::CCSDSManeuver
               (const CCSDSManeuver &man) :
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
const CCSDSManeuver& CCSDSManeuver::operator=
                                     (const CCSDSManeuver &man)
{
    if (&man == this)
        return *this;

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
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSManeuver.
 *
 * @return clone of the CCSDSManeuver.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSManeuver::Clone() const
{
   GmatBase *clone = new CCSDSManeuver(*this);
   return (clone);
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

    output << "MAN_EPOCH_IGNITION = " << myManeuver->ignitionEpoch << endl;
    output << "MAN_DURATION = " << myManeuver->duration << endl;
    output << "MAN_DELTA_MASS = " << myManeuver->deltaMass << endl;
    output << "MAN_REF_FRAME = " << myManeuver->refFrame << endl;
    output << "MAN_DV_1 = " << myManeuver->deltaV1 << endl;
    output << "MAN_DV_2 = " << myManeuver->deltaV2 << endl;
    output << "MAN_DV_3 = " << myManeuver->deltaV3 << endl;

   return output;
}