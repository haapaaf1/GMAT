//$Header$
//------------------------------------------------------------------------------
//                             AEMSpinStabilizedCCSDSData
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
 * the Spin Stabilized data construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAEMSPINSTABILIZED_HPP
#define	_CCSDSAEMSPINSTABILIZED_HPP

#include "SpinStabilizedCCSDSData.hpp"

class AEMSpinStabilizedCCSDSData : public SpinStabilizedCCSDSData
{

public:

    AEMSpinStabilizedCCSDSData();
    AEMSpinStabilizedCCSDSData(const AEMSpinStabilizedCCSDSData &aemSS);
    const AEMSpinStabilizedCCSDSData& AEMSpinStabilizedCCSDSData::operator=(const AEMSpinStabilizedCCSDSData &aemSS);
    virtual ~AEMSpinStabilizedCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                        const AEMSpinStabilizedCCSDSData *myAEMSpinStabilizedCCSDSData);

    friend class ProcessCCSDSAEMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAEMSpinStabilizedParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndSpinStabilizedCCSDSDataDataReps];
    
};

#endif	/* _CCSDSAEMSPINSTABILIZED_HPP */