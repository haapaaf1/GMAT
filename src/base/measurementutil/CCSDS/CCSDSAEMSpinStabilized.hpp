/*
 * File:   CCSDSAEMSPINSTABILIZED.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSAEMSPINSTABILIZED_HPP
#define	_CCSDSAEMSPINSTABILIZED_HPP

#include "CCSDSSpinStabilized.hpp"

class CCSDSAEMSpinStabilized : public CCSDSSpinStabilized
{

public:

    CCSDSAEMSpinStabilized();
    CCSDSAEMSpinStabilized(const CCSDSAEMSpinStabilized &aemSS);
    const CCSDSAEMSpinStabilized& CCSDSAEMSpinStabilized::operator=(const CCSDSAEMSpinStabilized &aemSS);
    virtual ~CCSDSAEMSpinStabilized();

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSAEMSpinStabilized *myCCSDSAEMSpinStabilized);

    friend class ProcessCCSDSAEMDataFile;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAEMSpinStabilizedParameters();
    bool Validate() const;

protected:

    static const bool CCSDS_IS_REQUIRED[EndCCSDSSpinStabilizedDataReps];
    
};

#endif	/* _CCSDSAEMSPINSTABILIZED_HPP */