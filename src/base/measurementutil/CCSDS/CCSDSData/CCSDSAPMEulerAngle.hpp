//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAPMEulerAngle
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

#include "CCSDSEulerAngle.hpp"

class CCSDSAPMEulerAngle : public CCSDSEulerAngle
{

public:

    CCSDSAPMEulerAngle();
    CCSDSAPMEulerAngle(const CCSDSAPMEulerAngle &apmEA);
    const CCSDSAPMEulerAngle& CCSDSAPMEulerAngle::operator=(const CCSDSAPMEulerAngle &apmEA);
    virtual ~CCSDSAPMEulerAngle();

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAPMEulerAngle *myCCSDSAPMEulerAngle);

    friend class ProcessCCSDSAPMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMEulerAngleParameters();
    bool Validate() const;

protected:
    
    static const bool CCSDS_IS_REQUIRED[EndCCSDSEulerAngleDataReps];

};

#endif	/* _CCSDSAPMEULERANGLE_HPP */