//$Header$
//------------------------------------------------------------------------------
//                             CCSDSOPMStateVector
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
 * This class specifies the CCSDS Orbit Parameter message format
 * implementation of the State Vector construct.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSOPMStateVector_HPP
#define	_CCSDSOPMStateVector_HPP

#include "CCSDSStateVector.hpp"

class CCSDSOPMStateVector : public CCSDSStateVector
{

public:

    CCSDSOPMStateVector();
    CCSDSOPMStateVector(const CCSDSOPMStateVector &opmSV);
    const CCSDSOPMStateVector& CCSDSOPMStateVector::operator=(const CCSDSOPMStateVector &opmSV);
    virtual ~CCSDSOPMStateVector();

    friend class ProcessCCSDSOPMDataFile;

    friend std::ostream& operator<< (std::ostream &output,
                              const CCSDSOPMStateVector *myCCSDSOPMStateVector);

};

#endif	/* _CCSDSOPMStateVector_HPP */

