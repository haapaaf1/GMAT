//$Header$
//------------------------------------------------------------------------------
//                             APMQuaternionCCSDSData
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
 * the Quaternion data construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAPMQUATERNION_HPP
#define	_CCSDSAPMQUATERNION_HPP

#include "QuaternionCCSDSData.hpp"

class APMQuaternionCCSDSData : public QuaternionCCSDSData
{

public:

    APMQuaternionCCSDSData();
    APMQuaternionCCSDSData(const APMQuaternionCCSDSData &apmQ);
    const APMQuaternionCCSDSData& APMQuaternionCCSDSData::operator=(const APMQuaternionCCSDSData &apmQ);
    virtual ~APMQuaternionCCSDSData();
    
    friend std::ostream& operator<< (std::ostream &output,
                                const APMQuaternionCCSDSData *myAPMQuaternionCCSDSData);

    friend class APMCCSDSDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMQuaternionParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndQuaternionCCSDSDataReps];
    
};

#endif	/* _CCSDSAPMQUATERNION_HPP */