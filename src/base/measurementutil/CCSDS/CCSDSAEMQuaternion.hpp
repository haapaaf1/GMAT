//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAEMQuaternion
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

#include "CCSDSQuaternion.hpp"

class CCSDSAEMQuaternion : public CCSDSQuaternion
{

public:

    CCSDSAEMQuaternion();
    CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ);
    const CCSDSAEMQuaternion& CCSDSAEMQuaternion::operator=(const CCSDSAEMQuaternion &aemQ);
    virtual ~CCSDSAEMQuaternion();

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAEMQuaternion *myCCSDSAEMQuaternion);

    friend class ProcessCCSDSAEMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAEMQuaternionParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndCCSDSQuaternionDataReps];

    
};

#endif	/* _CCSDSAEMQUATERNION_HPP */