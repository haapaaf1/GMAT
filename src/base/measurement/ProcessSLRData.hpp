//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRData
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

#ifndef ProcessSLRData_hpp
#define	ProcessSLRData_hpp

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "ProcessDataFile.hpp"

class ProcessSLRData : ProcessDataFile
{

public:
    
    ProcessSLRData();
    ~ProcessSLRData();

    bool FindSLRHeaderLine( slr_header &mySLRheader );
    bool GetData(slr_header &mySLRheader, slr_obtype &mySLRdata);
   
private:    
    
    // Specific data type processing functions
    bool GetSLRHeader(std::string &lff, slr_header &mySLRheader);
    bool GetSLRData(std::string &lff, slr_header &mySLRheader, slr_obtype &mySLRdata);
  
};

#endif	/* _ProcessSLRData_hpp */

