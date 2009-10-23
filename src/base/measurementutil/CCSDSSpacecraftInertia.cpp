#include "CCSDSSpacecraftInertia.hpp"
//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSAPMObType::CCSDS_QUATERNION_KEYWORDS[EndCCSDSQuaternionDataReps] =
{
    "INERTIA_REF_FRAME",
    "I11",
    "I22",
    "I33",
    "I12",
    "I13",
    "I23",
    "COMMENT"
};

const std::string CCSDSAPMObType::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
    "",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "kg m^2",
    "",
};

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSQuaternionDataReps] =
{
    "Spacecraft Inertia Ref Frame",
    "Spacecraft Inertia Component (1,1)",
    "Spacecraft Inertia Component (2,2)",
    "Spacecraft Inertia Component (3,3)",
    "Spacecraft Inertia Component (1,2)",
    "Spacecraft Inertia Component (1,3)",
    "Spacecraft Inertia Component (2,3)",
    "Spacecraft Inertia Comments"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSQuaternionDataReps] =
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

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSQuaternionDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSSpacecraftInertia()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia::CCSDSSpacecraftInertia() : CCSDSObtype(),
{
}

//------------------------------------------------------------------------------
//  CCSDSSpacecraftInertia(const CCSDSSpacecraftInertia &si)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia::CCSDSSpacecraftInertia(const CCSDSSpacecraftInertia &si) : CCSDSObtype(si),
{
}

//---------------------------------------------------------------------------
//  CCSDSSpacecraftInertia& operator=(const CCSDSSpacecraftInertia &si)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <si> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSSpacecraftInertia& CCSDSSpacecraftInertia::operator=(const CCSDSSpacecraftInertia &si)
{
   if (&si == this)
      return *this;

   CCSDSObtype::operator=(si);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSSpacecraftInertia()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSSpacecraftInertia::~CCSDSSpacecraftInertia()
{
}

// std::ostream& operator<< (std::ostream &output,
//                            const CCSDSSpacecraftInertia *mySpacecraftInertia)
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
                          const CCSDSSpacecraftInertia *mySpacecraftInertia)
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