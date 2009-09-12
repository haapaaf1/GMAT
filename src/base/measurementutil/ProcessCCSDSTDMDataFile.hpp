//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSTDMDataFile
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
 * Implements DataFile base class to read files written in CCSDS tracking
 * data message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessCCSDSTDMDataFile_hpp
#define	ProcessCCSDSTDMDataFile_hpp

#include "ProcessCCSDSDataFile.hpp"

class ProcessCCSDSTDMDataFile : public ProcessCCSDSDataFile
{
    
public:
    
    ProcessCCSDSTDMDataFile(const std::string &itsName);
    ~ProcessCCSDSTDMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;
    
private:

    // Specific data type processing functions
    bool GetData();
    
    bool GetCCSDSMetadata(std::string &nextline);

    CCSDSTDMObtype::ccsds_tdm_metadata *currentCCSDSMetadata;

};
#endif	/* _ProcessCCSDSTDMDataFileData_hpp */

