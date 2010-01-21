//$Header$
//------------------------------------------------------------------------------
//                             OPMStateVectorCCSDSData
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

#include "OPMStateVectorCCSDSData.hpp"

//------------------------------------------------------------------------------
//  OPMStateVectorCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the OPMStateVectorCCSDSData class
 */
//------------------------------------------------------------------------------
OPMStateVectorCCSDSData::OPMStateVectorCCSDSData() : StateVectorCCSDSData()
{
}

//------------------------------------------------------------------------------
//  OPMStateVectorCCSDSData(const OPMStateVectorCCSDSData &opmSV)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
OPMStateVectorCCSDSData::OPMStateVectorCCSDSData
               (const OPMStateVectorCCSDSData &opmSV) :
    StateVectorCCSDSData(opmSV)
{
}

//---------------------------------------------------------------------------
//  OPMStateVectorCCSDSData& operator=
//                                   (const OPMStateVectorCCSDSData &opmSV)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <opmSV> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const OPMStateVectorCCSDSData& OPMStateVectorCCSDSData::operator=
                                     (const OPMStateVectorCCSDSData &opmSV)

{
    if (&opmSV == this)
        return *this;

    StateVectorCCSDSData::operator=(opmSV);

    return *this;
}

//------------------------------------------------------------------------------
//  ~OPMStateVectorCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the OPMStateVectorCCSDSData class
 */
//------------------------------------------------------------------------------
OPMStateVectorCCSDSData::~OPMStateVectorCCSDSData()
{
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const OPMStateVectorCCSDSData *myOPMStateVector)
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
                          const OPMStateVectorCCSDSData *myOPMStateVector)
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