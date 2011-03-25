//$Header$
//------------------------------------------------------------------------------
//                             APMEulerAngleCCSDSData
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
 * This class specifies the CCSDS Attitude Parameter implementation of
 * the Euler Angle data construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAPMEULERANGLE_HPP
#define	_CCSDSAPMEULERANGLE_HPP

#include "EulerAngleCCSDSData.hpp"

class APMEulerAngleCCSDSData : public EulerAngleCCSDSData
{

public:

    APMEulerAngleCCSDSData();
    APMEulerAngleCCSDSData(const APMEulerAngleCCSDSData &apmEA);
    const APMEulerAngleCCSDSData& APMEulerAngleCCSDSData::operator=(const APMEulerAngleCCSDSData &apmEA);
    virtual ~APMEulerAngleCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                                const APMEulerAngleCCSDSData *myAPMEulerAngleCCSDSData);

    friend class ProcessCCSDSAPMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMEulerAngleParameters();
    bool Validate() const;

protected:
    
    static const bool CCSDS_IS_REQUIRED[EndEulerAngleCCSDSDataDataReps];

};

#endif	/* _CCSDSAPMEULERANGLE_HPP */