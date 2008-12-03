//$Header$
//------------------------------------------------------------------------------
//                             ProcessTLEData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/22
//
/**
 *
 * Implements DataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessTLEData_hpp
#define	ProcessTLEData_hpp

#include "gmatdefs.hpp"
#include <pcrecpp.h>
#include "RealUtilities.hpp"
#include "DataFile.hpp"

class ProcessTLEData : public DataFile
{

public:
    
    ProcessTLEData(const std::string &itsName);
    ~ProcessTLEData();

    // Initialization happens here
    bool Initialize();
   
    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    // Specific data type processing functions
    bool GetData(std::ifstream &theFile, tle_obtype &myTLEdata);
    bool GetTLEData(std::string &lff, std::string &lff2, 
			       tle_obtype &myTLEdata);

};

#endif	/* _ProcessTLEData_hpp */

