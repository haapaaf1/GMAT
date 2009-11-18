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
    virtual ~CCSDSAPMSpinStabilized();

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSAPMSpinStabilized *myCCSDSAPMSpinStabilized);

    friend class ProcessCCSDSAPMDataFile;
    
    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMSpinStabilizedParameters();
    bool Validate() const;
    
protected:

    static const bool CCSDS_IS_REQUIRED[EndCCSDSSpinStabilizedDataReps];


};

#endif	/* _CCSDSAPMSPINSTABILIZED_HPP */