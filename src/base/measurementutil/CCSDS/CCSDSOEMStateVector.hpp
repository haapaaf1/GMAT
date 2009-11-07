/*
 * File:   CCSDSOEMStateVector.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSOEMStateVector_HPP
#define	_CCSDSOEMStateVector_HPP

#include "CCSDSStateVector.hpp"

class CCSDSOEMStateVector : public CCSDSStateVector
{

public:

    CCSDSOEMStateVector();
    CCSDSOEMStateVector(const CCSDSOEMStateVector &oemSV);
    const CCSDSOEMStateVector& CCSDSOEMStateVector::operator=(const CCSDSOEMStateVector &oemSV);
    ~CCSDSOEMStateVector();

    friend class ProcessCCSDSOEMDataFile;
    
    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSOEMStateVector *myCCSDSOEMStateVector);
};

#endif	/* _CCSDSOEMStateVector_HPP */

