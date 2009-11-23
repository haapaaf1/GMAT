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

#ifndef _OEMStateVectorCCSDSData_HPP
#define	_OEMStateVectorCCSDSData_HPP

#include "StateVectorCCSDSData.hpp"

class OEMStateVectorCCSDSData : public StateVectorCCSDSData
{

public:

    OEMStateVectorCCSDSData();
    OEMStateVectorCCSDSData(const OEMStateVectorCCSDSData &oemSV);
    const OEMStateVectorCCSDSData& OEMStateVectorCCSDSData::operator=(const OEMStateVectorCCSDSData &oemSV);
    virtual ~OEMStateVectorCCSDSData();

    friend class ProcessCCSDSOEMDataFile;
    
    friend std::ostream& operator<< (std::ostream &output,
                              const OEMStateVectorCCSDSData *myOEMStateVectorCCSDSData);
};

#endif	/* _OEMStateVectorCCSDSData_HPP */

