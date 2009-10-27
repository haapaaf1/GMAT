/*
 * File:   CCSDSAPMQUATERNION.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSAPMQUATERNION_HPP
#define	_CCSDSAPMQUATERNION_HPP

#include "CCSDSQuaternion.hpp"

class CCSDSAPMQuaternion : public CCSDSQuaternion
{

public:

    CCSDSAPMQuaternion();
    CCSDSAPMQuaternion(const CCSDSAPMQuaternion &apmQ);
    const CCSDSAPMQuaternion& CCSDSAPMQuaternion::operator=(const CCSDSAPMQuaternion &apmQ);
    ~CCSDSAPMQuaternion();
    
    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAPMQuaternion *myCCSDSAPMQuaternion);

};

#endif	/* _CCSDSAPMQUATERNION_HPP */