/*
 * File:   CCSDSOPMStateVector.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSOPMStateVector_HPP
#define	_CCSDSOPMStateVector_HPP

#include "CCSDSStateVector.hpp"

class CCSDSOPMStateVector : public CCSDSStateVector
{

public:

    CCSDSOPMStateVector();
    CCSDSOPMStateVector(const CCSDSOPMStateVector &opmSV);
    const CCSDSOPMStateVector& CCSDSOPMStateVector::operator=(const CCSDSOPMStateVector &opmSV);
    ~CCSDSOPMStateVector();
    
    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSOPMStateVector *myCCSDSOPMStateVector);

};

#endif	/* _CCSDSOPMStateVector_HPP */

