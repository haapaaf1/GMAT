//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOEM
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/08/30
//
/**
 *
 * Implements DataFile base class to read files written in CCSDS orbit
 * ephemeris message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessCCSDSOEMDATAFILE_hpp
#define	ProcessCCSDSOEMDATAFILE_hpp

#include "ProcessCCSDSDataFile.hpp"
#include "Obtype.hpp"

class ProcessCCSDSOEMDataFile : public ProcessCCSDSDataFile
{

public:
    
    ProcessCCSDSOEMDataFile(const std::string &itsName);
    ~ProcessCCSDSOEMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool WriteData(CCSDSOEMObtype *myCCSDSOEM);
    
private:
    
    // Specific data type processing functions
    bool GetData();
    
    bool GetCCSDSMetadata(std::string &nextline);
    
    CCSDSOEMObtype::ccsds_oem_metadata *currentCCSDSMetadata;
    
};
#endif	/* _ProcessCCSDSOEMDATAFILE_hpp */

