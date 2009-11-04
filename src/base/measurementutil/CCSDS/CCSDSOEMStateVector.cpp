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
   
   //if (ProcessCCSDSOEMDataFile::scientific)
   // output.setf(std::ios::scientific);

   //if (ProcessCCSDSOEMDataFile::showPoint)
   //    output << showpoint;

   //output.precision(ProcessCCSDSOEMDataFile::precision);

   for (unsigned int i = 0; i < myOEMStateVector->comments.size(); i++)
       output << myOEMStateVector->comments[i] << endl;

   output << endl;

   output << myOEMStateVector->timeTag;
   output << " " << myOEMStateVector->x;
   output << " " << myOEMStateVector->y;
   output << " " << myOEMStateVector->z;
   output << " " << myOEMStateVector->xDot;
   output << " " << myOEMStateVector->yDot;
   output << " " << myOEMStateVector->zDot;
   output << endl;

   //if (ProcessCCSDSOEMDataFile::scientific)
   //    output.unsetf(std::ios_base::floatfield);

   //if (ProcessCCSDSOEMDataFile::showPoint)
   //    output << noshowpoint;

   return output;
}