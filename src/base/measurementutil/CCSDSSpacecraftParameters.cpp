#include "CCSDSSpacecraftParameters.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSOPMObType::CCSDS_OPM_KEYWORDS[EndCCSDSOPMDataReps] =
{
    "MASS",
    "SOLAR_RAD_AREA",
    "SOLAR_RAD_COEFF",
    "DRAG_AREA",
    "DRAG_COEFF",
    "",
};

const std::string CCSDSOPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOPMDataReps] =
{
    "kg",
    "m^2",
    "",
    "m^2",
    "",
    ""
};

const std::string CCSDSOPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMDataReps] =
{
    "Spacecraft Parameters Mass",
    "Spacecraft Parameters Solar Radiation Area",
    "Spacecraft Parameters Solar Radiation Coefficient",
    "Spacecraft Parameters Drag Area",
    "Spacecraft Parameters Drag Coefficient",
    "Spacecraft Parameters Comments"
};

const bool CCSDSOPMObType::CCSDS_IS_REQUIRED[EndCCSDSOPMDataReps] =
{
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
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSSpacecraftParameters()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSSpacecraftParameters class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftParameters::CCSDSSpacecraftParameters() :
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
    mass(SP.mass),
    solarRadiationArea(SP.solarRadiationArea),
    solarRadiationCoefficient(SP.solarRadiationCoefficient),
    dragArea(SP.dragArea),
    dragCoefficient(SP.dragCoefficient),
    comments(SP.comments)
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

    mass = SP.mass;
    solarRadiationArea = SP.solarRadiationArea;
    solarRadiationCoefficient = SP.solarRadiationCoefficient;
    dragArea = SP.dragArea;
    dragCoefficient = SP.dragCoefficient;
    comments = SP.comments;

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
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSSpacecraftParameters.
 *
 * @return clone of the CCSDSSpacecraftParameters.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSSpacecraftParameters::Clone() const
{
   GmatBase *clone = new CCSDSSpacecraftParameters(*this);
   return (clone);
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

   output << "MASS = " << mySpacecraftParameters->mass << endl;
   output << "SOLAR_RAD_AREA = " << mySpacecraftParameters->solarRadiationArea << endl;
   output << "SOLAR_RAD_COEFF = " << mySpacecraftParameters->solarRadiationCoefficient << endl;
   output << "DRAG_AREA = " << mySpacecraftParameters->dragArea << endl;
   output << "DRAG_COEFF = " << mySpacecraftParameters->dragCoefficient << endl;

   return output;
}
