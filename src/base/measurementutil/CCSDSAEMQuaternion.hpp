/*
 * File:   CCSDSAEMQUATERNION.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSAEMQUATERNION_HPP
#define	_CCSDSAEMQUATERNION_HPP

class CCSDSAEMQuaternion : public CCSDSQuaternion
{

public:

    CCSDSAEMQuaternion();
    CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ);
    const CCSDSAEMQuaternion& CCSDSAEMQuaternion::operator=(const CCSDSAEMQuaternion &aemQ);
    ~CCSDSAEMQuaternion();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                const CCSDSAEMQuaternion *myCCSDSAEMQuaternion);

};

#endif	/* _CCSDSAEMQUATERNION_HPP */