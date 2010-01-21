//$Header$
//------------------------------------------------------------------------------
//                             AEMQuaternionCCSDSData
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
 * This class specifies the CCSDS Attitude Ephemeris implementation of
 * the Quaternion data construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAEMQUATERNION_HPP
#define	_CCSDSAEMQUATERNION_HPP

#include "QuaternionCCSDSData.hpp"

class AEMQuaternionCCSDSData : public QuaternionCCSDSData
{

public:

    AEMQuaternionCCSDSData();
    AEMQuaternionCCSDSData(const AEMQuaternionCCSDSData &aemQ);
    const AEMQuaternionCCSDSData& AEMQuaternionCCSDSData::operator=(const AEMQuaternionCCSDSData &aemQ);
    virtual ~AEMQuaternionCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                                const AEMQuaternionCCSDSData *myAEMQuaternionCCSDSData);

    friend class ProcessCCSDSAEMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAEMQuaternionParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndQuaternionCCSDSDataReps];

    
};

#endif	/* _CCSDSAEMQUATERNION_HPP */