//$Header$
//------------------------------------------------------------------------------
//                             CCSDSOEMStateVector
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the CCSDS Orbit Ephemeris message format
 * implementation of the State Vector construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSOEMStateVector_HPP
#define	_CCSDSOEMStateVector_HPP

#include "CCSDSStateVector.hpp"

class CCSDSOEMStateVector : public CCSDSStateVector
{

public:

    CCSDSOEMStateVector();
    CCSDSOEMStateVector(const CCSDSOEMStateVector &oemSV);
    const CCSDSOEMStateVector& CCSDSOEMStateVector::operator=(const CCSDSOEMStateVector &oemSV);
    virtual ~CCSDSOEMStateVector();

    friend class ProcessCCSDSOEMDataFile;
    
    friend std::ostream& operator<< (std::ostream &output,
                              const CCSDSOEMStateVector *myCCSDSOEMStateVector);
};

#endif	/* _CCSDSOEMStateVector_HPP */

