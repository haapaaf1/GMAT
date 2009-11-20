//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAEMEulerAngle
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

#include "CCSDSEulerAngle.hpp"

class CCSDSAEMEulerAngle : public CCSDSEulerAngle
{

public:

    CCSDSAEMEulerAngle();
    CCSDSAEMEulerAngle(const CCSDSAEMEulerAngle &aemEA);
    const CCSDSAEMEulerAngle& CCSDSAEMEulerAngle::operator=(const CCSDSAEMEulerAngle &aemEA);
    virtual ~CCSDSAEMEulerAngle();

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAEMEulerAngle *myCCSDSAEMEulerAngle);

    friend class ProcessCCSDSAEMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAEMEulerAngleParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndCCSDSEulerAngleDataReps];

};

#endif	/* _CCSDSAEMEULERANGLE_HPP */