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
#include "ObType.hpp"

class ProcessCCSDSOEMDataFile : public ProcessCCSDSDataFile
{

public:
    
    ProcessCCSDSOEMDataFile(const std::string &itsName);
    ProcessCCSDSOEMDataFile(const ProcessCCSDSOEMDataFile &CCSDSOEMdf);
    const ProcessCCSDSOEMDataFile& operator=(const ProcessCCSDSOEMDataFile &CCSDSOEMdf);
    ~ProcessCCSDSOEMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool WriteData(CCSDSOEMObType *myCCSDSOEM);
    
private:
    
    // Specific data type processing functions
    bool GetData(std::string line, CCSDSOEMObType *myOEM);
    bool GetCCSDSMetadata(std::string &nextline,
                          CCSDSOEMObType::ccsds_oem_metadata *myMetaData);
    
    CCSDSOEMObType::ccsds_oem_metadata *currentCCSDSMetadata;
    
};
#endif	/* _ProcessCCSDSOEMDATAFILE_hpp */

