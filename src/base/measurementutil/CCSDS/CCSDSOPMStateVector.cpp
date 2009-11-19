//$Header$
//------------------------------------------------------------------------------
//                             CCSDSOPMStateVector
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the CCSDS Orbit Parameter message format
 * implementation of the State Vector construct.
 *
 */
//------------------------------------------------------------------------------

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

   if (!myOPMStateVector->Validate()) return output;

   unsigned int i;
   for (i = 0; i < myOPMStateVector->comments.size(); i++ )
   {
       output << "COMMENT " << myOPMStateVector->comments[i] << std::endl;
   }
   if (i > 0) output << std::endl;

   output << "EPOCH = " << myOPMStateVector->timeTag << endl;
   output << "X = " << myOPMStateVector->x << endl;
   output << "Y = " << myOPMStateVector->y << endl;
   output << "Z = " << myOPMStateVector->z << endl;
   output << "X_DOT = " << myOPMStateVector->xDot << endl;
   output << "Y_DOT = " << myOPMStateVector->yDot << endl;
   output << "Z_DOT = " << myOPMStateVector->zDot << endl;

   output << std::endl;

   return output;
}