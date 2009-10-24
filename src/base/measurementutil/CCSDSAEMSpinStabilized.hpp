/*
 * File:   CCSDSAEMSPINSTABILIZED.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSAEMSPINSTABILIZED_HPP
#define	_CCSDSAEMSPINSTABILIZED_HPP

class CCSDSAEMSpinStabilized : public CCSDSSpinStabilized
{

public:

    CCSDSAEMSpinStabilized();
    CCSDSAEMSpinStabilized(const CCSDSAEMSpinStabilized &aemSS);
    const CCSDSAEMSpinStabilized& CCSDSAEMSpinStabilized::operator=(const CCSDSAEMSpinStabilized &aemSS);
    ~CCSDSAEMSpinStabilized();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSAEMSpinStabilized *myCCSDSAEMSpinStabilized);
};

#endif	/* _CCSDSAEMSPINSTABILIZED_HPP */