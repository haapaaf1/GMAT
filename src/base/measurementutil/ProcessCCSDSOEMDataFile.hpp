//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOEMDataFile
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

#ifndef ProcessCCSDSOEMDataFile_hpp
#define	ProcessCCSDSOEMDataFile_hpp

#include "ProcessCCSDSDataFile.hpp"

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

    bool WriteData(ObType *myOb);
    bool GetData(ObType *myOb);

private:

    // Specific data type processing functions
    bool GetCCSDSMetaData(std::string &nextline,
                          CCSDSOEMMetaData *myMetaData);

    CCSDSOEMMetaData *currentCCSDSMetaData;
    CCSDSOEMMetaData *lastMetaDataWritten;
    bool isMetaDataWritten;

};
#endif	/* _ProcessCCSDSOEMDataFileData_hpp */

