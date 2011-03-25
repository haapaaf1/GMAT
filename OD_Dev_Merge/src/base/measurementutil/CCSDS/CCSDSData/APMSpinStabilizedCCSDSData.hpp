//$Header$
//------------------------------------------------------------------------------
//                             APMSpinStabilizedCCSDSData
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
 * the Spin Stabilized data construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAPMSPINSTABILIZED_HPP
#define	_CCSDSAPMSPINSTABILIZED_HPP

#include "SpinStabilizedCCSDSData.hpp"

class APMSpinStabilizedCCSDSData : public SpinStabilizedCCSDSData
{

public:

    APMSpinStabilizedCCSDSData();
    APMSpinStabilizedCCSDSData(const APMSpinStabilizedCCSDSData &apmSS);
    const APMSpinStabilizedCCSDSData& APMSpinStabilizedCCSDSData::operator=(const APMSpinStabilizedCCSDSData &apmSS);
    virtual ~APMSpinStabilizedCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                        const APMSpinStabilizedCCSDSData *myAPMSpinStabilizedCCSDSData);

    friend class ProcessCCSDSAPMDataFile;
    
    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMSpinStabilizedParameters();
    bool Validate() const;
    
protected:

    static const bool CCSDS_IS_REQUIRED[EndSpinStabilizedCCSDSDataDataReps];


};

#endif	/* _CCSDSAPMSPINSTABILIZED_HPP */