#include "CCSDSOPMStateVector.hpp"

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
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the CCSDSOPMStateVector.
 *
 * @return clone of the CCSDSOPMStateVector.
 */
//------------------------------------------------------------------------------
GmatBase* CCSDSOPMStateVector::Clone() const
{
   GmatBase *clone = new CCSDSOPMStateVector(*this);
   return (clone);
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