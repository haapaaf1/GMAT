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
 * Implements ProcessDataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessDataFile_hpp
#define	ProcessDataFile_hpp

#include "gmatdefs.hpp"
#include <pcrecpp.h>
#include "RealUtilities.hpp"
#include "ProcessDataFile.hpp"

class ProcessTLEData :: ProcessDataFile
{

public:
    
    ProcessTLEData();
    ~ProcessTLEData();
    
    bool GetData(tle_obtype &myTLEdata);

private:
    
    // Specific data type processing functions
    bool GetTLEData(std::string &lff, std::string &lff2, 
			       tle_obtype &myTLEdata);

};

#endif	/* _ProcessTLEData_hpp */

