//$Header$
//------------------------------------------------------------------------------
//                             AEMEulerAngleCCSDSData
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
 * the Euler Angle data construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAEMEULERANGLE_HPP
#define	_CCSDSAEMEULERANGLE_HPP

#include "EulerAngleCCSDSData.hpp"

class AEMEulerAngleCCSDSData : public EulerAngleCCSDSData
{

public:

    AEMEulerAngleCCSDSData();
    AEMEulerAngleCCSDSData(const AEMEulerAngleCCSDSData &aemEA);
    const AEMEulerAngleCCSDSData& AEMEulerAngleCCSDSData::operator=(const AEMEulerAngleCCSDSData &aemEA);
    virtual ~AEMEulerAngleCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                                const AEMEulerAngleCCSDSData *myAEMEulerAngleCCSDSData);

    friend class ProcessCCSDSAEMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAEMEulerAngleParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndEulerAngleCCSDSDataDataReps];

};

#endif	/* _CCSDSAEMEULERANGLE_HPP */