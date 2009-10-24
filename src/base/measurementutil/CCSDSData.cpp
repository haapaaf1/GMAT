#include "CCSDSData.hpp"
//---------------------------------
//  static data
//---------------------------------

const std::string CCSDSAPMObType::CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSGenericDataReps] =
{
    "Keyword",
    "Epoch",
    "Measurement",
    "Comment"
};

const bool CCSDSAPMObType::CCSDS_IS_REQUIRED[EndCCSDSGenericDataReps] =
{
    true,
    true,
    true,
    false
};

const Gmat::ParameterType CCSDSAPMObType::CCSDS_PARAMETER_TYPE[EndCCSDSGenericDataReps] =
{
    Gmat::STRING_TYPE,
    Gmat::STRING_TYPE,
    Gmat::REAL_TYPE,
    Gmat::STRINGARRAY_TYPE
};

//------------------------------------------------------------------------------
//  CCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::CCSDSData() : CCSDSObType()
{
}

//------------------------------------------------------------------------------
//  CCSDSData(const CCSDSData &data) : CCSDSData(data)
//------------------------------------------------------------------------------
/**
 * Constructor for the generic CCSDS Data class
 */
//------------------------------------------------------------------------------
CCSDSData::CCSDSData(const CCSDSData &data) : CCSDSObType(data)
{
}

//---------------------------------------------------------------------------
//  CCSDSData& operator=(const CCSDSData &data)
//---------------------------------------------------------------------------
/**
 * Assignment operator for Data structures.
 *
 * @param <data> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSData& CCSDSData::operator=(const CCSDSData &data)

{
    if (&data == this)
        return *this;

    CCSDSObType::operator=(data);

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSData class
 */
//------------------------------------------------------------------------------
CCSDSData::~CCSDSData()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSData.
 *
 * @return clone of the CCSDSData.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSData::Clone() const
{
   GmatBase *clone = new CCSDSData(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSData *myCCSDSData)
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
std::ostream& operator<< (std::ostream &output,
                          const CCSDSData *myCCSDSData)
{
    using namespace std;

    output << myCCSDSData->keywordID << " = " << myCCSDSData->timeTag
           << " " << myCCSDSData->measurement << endl;

    return output;
}