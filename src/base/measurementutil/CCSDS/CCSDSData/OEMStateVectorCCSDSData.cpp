//$Header$
//------------------------------------------------------------------------------
//                             OEMStateVectorCCSDSData
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
 * This class specifies the CCSDS Orbit Ephemeris message format
 * implementation of the State Vector construct.
 *
 */
//------------------------------------------------------------------------------

#include "OEMStateVectorCCSDSData.hpp"

//------------------------------------------------------------------------------
//  OEMStateVectorCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the OEMStateVectorCCSDSData class
 */
//------------------------------------------------------------------------------
OEMStateVectorCCSDSData::OEMStateVectorCCSDSData() : StateVectorCCSDSData()
{
}

//------------------------------------------------------------------------------
//  OEMStateVectorCCSDSData(const OEMStateVectorCCSDSData &oemSV)
//------------------------------------------------------------------------------
/**
 * Constructor for the StateVector class
 */
//------------------------------------------------------------------------------
OEMStateVectorCCSDSData::OEMStateVectorCCSDSData
               (const OEMStateVectorCCSDSData &oemSV) :
    StateVectorCCSDSData(oemSV)
{
}

//---------------------------------------------------------------------------
//  OEMStateVectorCCSDSData& operator=
//                                   (const OEMStateVectorCCSDSData &oemSV)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StateVector structures.
 *
 * @param <oemSV> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const OEMStateVectorCCSDSData& OEMStateVectorCCSDSData::operator=
                                     (const OEMStateVectorCCSDSData &oemSV)

{
    if (&oemSV == this)
        return *this;

    StateVectorCCSDSData::operator=(oemSV);

    return *this;
}

//------------------------------------------------------------------------------
//  ~OEMStateVectorCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the OEMStateVectorCCSDSData class
 */
//------------------------------------------------------------------------------
OEMStateVectorCCSDSData::~OEMStateVectorCCSDSData()
{
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const OEMStateVectorCCSDSData *myStateVector)
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
                          const OEMStateVectorCCSDSData *myOEMStateVector)
{
    using namespace std;

    if (!myOEMStateVector->Validate()) return output;
   
    //if (ProcessCCSDSOEMDataFile::scientific)
    // output.setf(std::ios::scientific);

    //if (ProcessCCSDSOEMDataFile::showPoint)
    //    output << showpoint;

    //output.precision(ProcessCCSDSOEMDataFile::precision);
  
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