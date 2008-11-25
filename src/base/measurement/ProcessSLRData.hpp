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
 * Implements DataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessSLRData_hpp
#define	ProcessSLRData_hpp

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "DataFile.hpp"

class ProcessSLRData : public DataFile
{

public:
    
    ProcessSLRData(const std::string &itsName);
    ~ProcessSLRData();

    // Initialization happens here
    void Initialize() const;

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    std::string Ilrs2Cospar(std::string ilrsSatnum);

    bool FindSLRHeaderLine( std::ifstream &theFile, slr_header &mySLRheader );
    bool GetData(std::ifstream &theFile, slr_header &mySLRheader, slr_obtype &mySLRdata);
   
private:    
    
    // Specific data type processing functions
    bool GetSLRHeader(std::string &lff, slr_header &mySLRheader);
    bool GetSLRData(std::string &lff, slr_header &mySLRheader, slr_obtype &mySLRdata);
  
};

#endif	/* _ProcessSLRData_hpp */

