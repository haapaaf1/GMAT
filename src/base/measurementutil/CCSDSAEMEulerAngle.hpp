/*
 * File:   CCSDSAEMEULERANGLE.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSAEMEULERANGLE_HPP
#define	_CCSDSAEMEULERANGLE_HPP

#include "CCSDSEulerAngle.hpp"

class CCSDSAEMEulerAngle : public CCSDSEulerAngle
{

public:

    CCSDSAEMEulerAngle();
    CCSDSAEMEulerAngle(const CCSDSAEMEulerAngle &aemEA);
    const CCSDSAEMEulerAngle& CCSDSAEMEulerAngle::operator=(const CCSDSAEMEulerAngle &aemEA);
    ~CCSDSAEMEulerAngle();

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAEMEulerAngle *myCCSDSAEMEulerAngle);
};

#endif	/* _CCSDSAEMEULERANGLE_HPP */