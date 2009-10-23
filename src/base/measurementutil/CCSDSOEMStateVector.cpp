#include "CCSDSOEMStateVector.hpp"
//------------------------------------------------------------------------------
//  CCSDSOEMStateVector()
//------------------------------------------------------------------------------
/**
 * Constructor for the CCSDSOEMStateVector class
 */
//------------------------------------------------------------------------------
CCSDSOEMStateVector::CCSDSOEMStateVector() : CCSDSStateVector()
{
}

//------------------------------------------------------------------------------
//  CCSDSOEMStateVector(const CCSDSOEMStateVector &oemSV)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
CCSDSOEMStateVector::CCSDSOEMStateVector
               (const CCSDSOEMStateVector &oemSV) :
    CCSDSStateVector(oemSV)
{
}

//---------------------------------------------------------------------------
//  CCSDSOEMStateVector& operator=
//                                   (const CCSDSOEMStateVector &oemSV)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <oemSV> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSOEMStateVector& CCSDSOEMStateVector::operator=
                                     (const CCSDSOEMStateVector &oemSV)

{
    if (&oemSV == this)
        return *this;

    CCSDSStateVector::operator=(oemSV);

    return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSOEMStateVector()
//------------------------------------------------------------------------------
/**
 * Destructor for the CCSDSOEMStateVector class
 */
//------------------------------------------------------------------------------
CCSDSOEMStateVector::~CCSDSOEMStateVector()
{
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSOEMStateVector.
 *
 * @return clone of the CCSDSOEMStateVector.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOEMStateVector::Clone() const
{
   GmatBase *clone = new CCSDSOEMStateVector(*this);
   return (clone);
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSOEMStateVector *myStateVector)
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
std::ostream& operator<< (std::ostream &output,
                          const CCSDSOEMStateVector *myOEMStateVector)
{
   using namespace std;

   output << myOEMStateVector->epoch << myOEMStateVector->x
           << myOEMStateVector->y << myOEMStateVector->xDot
           << myOEMStateVector->yDot << myOEMStateVector->zDot << endl;

   return output;
}