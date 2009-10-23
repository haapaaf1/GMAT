#include "CCSDSStateVector.hpp"

//---------------------------------
//  static data
//---------------------------------
const std::string CCSDSStateVector::CCSDS_STATEVECTOR_KEYWORDS[EndCCSDSStateVectorDataReps] =
{
    "EPOCH",
    "X",
    "Y",
    "Z",
    "X_DOT",
    "Y_DOT",
    "Z_DOT",
    "COMMENT"
};

const std::string CCSDSStateVector::CCSDS_UNIT_DESCRIPTIONS[EndCCSDSStateVectorDataReps] =
{
    "",
    "km",
    "km",
    "km",
    "km/s",
    "km/s",
    "km/s",
    ""
};

const std::string CCSDSStateVector::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSStateVectorDataReps] =
{
    "State Vector Epoch",
    "State Vector X",
    "State Vector Y",
    "State Vector Z",
    "State Vector X Dot",
    "State Vector Y Dot",
    "State Vector Z Dot",
    "State Vector Comments"
};

const bool CCSDSStateVector::CCSDS_IS_REQUIRED[EndCCSDSStateVectorDataReps] =
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

const Gmat::ParameterType CCSDSStateVector::CCSDS_PARAMETER_TYPE[EndCCSDSStateVectorDataReps] =
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
//  CCSDSStateVector()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSStateVector class
 */
//------------------------------------------------------------------------------
CCSDSStateVector::CCSDSStateVector() : CCSDSStateVector()
{
}

//------------------------------------------------------------------------------
//  CCSDSStateVector(const CCSDSStateVector &sv)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSStateVector::CCSDSStateVector
               (const CCSDSStateVector &sv) :
    CCSDSStateVector(sv)
{
}

//---------------------------------------------------------------------------
//  CCSDSStateVector& operator=
//                                   (const CCSDSStateVector &sv)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <sv> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSStateVector& CCSDSStateVector::operator=
                                     (const CCSDSStateVector &sv)

{
    if (&sv == this)
        return *this;

    CCSDSObType::operator=(sv);

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSStateVector()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSStateVector class
 */
//------------------------------------------------------------------------------
CCSDSStateVector::~CCSDSStateVector()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSStateVector.
 *
 * @return clone of the CCSDSStateVector.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSStateVector::Clone() const
{
   GmatBase *clone = new CCSDSStateVector(*this);
   return (clone);
}
