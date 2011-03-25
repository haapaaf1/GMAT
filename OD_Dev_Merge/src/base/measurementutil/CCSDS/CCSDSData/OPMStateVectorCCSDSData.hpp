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

#ifndef _OPMStateVectorCCSDSData_HPP
#define	_OPMStateVectorCCSDSData_HPP

#include "StateVectorCCSDSData.hpp"

class OPMStateVectorCCSDSData : public StateVectorCCSDSData
{

public:

    OPMStateVectorCCSDSData();
    OPMStateVectorCCSDSData(const OPMStateVectorCCSDSData &opmSV);
    const OPMStateVectorCCSDSData& OPMStateVectorCCSDSData::operator=(const OPMStateVectorCCSDSData &opmSV);
    virtual ~OPMStateVectorCCSDSData();

    friend class ProcessCCSDSOPMDataFile;

    friend std::ostream& operator<< (std::ostream &output,
                              const OPMStateVectorCCSDSData *myOPMStateVectorCCSDSData);

};

#endif	/* _OPMStateVectorCCSDSData_HPP */

