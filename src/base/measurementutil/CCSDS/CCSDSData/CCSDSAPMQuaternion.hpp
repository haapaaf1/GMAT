//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAPMQuaternion
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

#include "CCSDSQuaternion.hpp"

class CCSDSAPMQuaternion : public CCSDSQuaternion
{

public:

    CCSDSAPMQuaternion();
    CCSDSAPMQuaternion(const CCSDSAPMQuaternion &apmQ);
    const CCSDSAPMQuaternion& CCSDSAPMQuaternion::operator=(const CCSDSAPMQuaternion &apmQ);
    virtual ~CCSDSAPMQuaternion();
    
    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAPMQuaternion *myCCSDSAPMQuaternion);

    friend class ProcessCCSDSAPMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMQuaternionParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndCCSDSQuaternionDataReps];
    
};

#endif	/* _CCSDSAPMQUATERNION_HPP */