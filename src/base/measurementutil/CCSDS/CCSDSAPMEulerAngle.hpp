/*
 * File:   CCSDSAPMEULERANGLE.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

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