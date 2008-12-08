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

    bool GetData(tle_obtype* myTLE);

private:

    bool GetTLEData(std::string &lff, std::string &lff2, 
			       tle_obtype *myTLEdata);

    // Specific data type processing functions
    bool GetNextOb(std::ifstream &theFile, tle_obtype *myTLEdata);

    // Vector container of observations
    std::vector<tle_obtype> tleData;

    //Current iterator pointing at data
    std::vector<tle_obtype>::const_iterator *i;


};

#endif	/* _ProcessTLEData_hpp */

