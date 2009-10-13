//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSAEMDataFile
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

#ifndef ProcessCCSDSAEMDataFile_hpp
#define	ProcessCCSDSAEMDataFile_hpp

#include "ProcessCCSDSDataFile.hpp"

class ProcessCCSDSAEMDataFile : public ProcessCCSDSDataFile
{

public:

    ProcessCCSDSAEMDataFile(const std::string &itsName);
    ProcessCCSDSAEMDataFile(const ProcessCCSDSAEMDataFile &CCSDSAEMdf);
    const ProcessCCSDSAEMDataFile& operator=(const ProcessCCSDSAEMDataFile &CCSDSAEMdf);
    ~ProcessCCSDSAEMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool WriteData(const ObType *myOb);
    bool GetData(ObType *myOb);

private:

    // Specific data type processing functions
    bool GetCCSDSMetaData(std::string &nextline,
                          CCSDSAEMMetaData *myMetaData);
    bool GetCCSDSAEMData(std::string &lff, CCSDSAEMObType *myOb);

    CCSDSAEMMetaData *currentCCSDSMetaData;
    CCSDSAEMMetaData *lastMetaDataWritten;
    bool isMetaDataWritten;

};
#endif	/* _ProcessCCSDSAEMDataFileData_hpp */

