/*
 * File:   CCSDSAPMSPINSTABILIZED.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSAPMSPINSTABILIZED_HPP
#define	_CCSDSAPMSPINSTABILIZED_HPP

#include "CCSDSSpinStabilized.hpp"

class CCSDSAPMSpinStabilized : public CCSDSSpinStabilized
{

public:

    CCSDSAPMSpinStabilized();
    CCSDSAPMSpinStabilized(const CCSDSAPMSpinStabilized &apmSS);
    const CCSDSAPMSpinStabilized& CCSDSAPMSpinStabilized::operator=(const CCSDSAPMSpinStabilized &apmSS);
    ~CCSDSAPMSpinStabilized();

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSAPMSpinStabilized *myCCSDSAPMSpinStabilized);
};

#endif	/* _CCSDSAPMSPINSTABILIZED_HPP */