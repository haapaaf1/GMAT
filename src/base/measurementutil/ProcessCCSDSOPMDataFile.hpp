//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOPMDataFile
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

#ifndef ProcessCCSDSOPMDataFile_hpp
#define	ProcessCCSDSOPMDataFile_hpp

#include "ProcessCCSDSDataFile.hpp"

class ProcessCCSDSOPMDataFile : public ProcessCCSDSDataFile
{

public:

    ProcessCCSDSOPMDataFile(const std::string &itsName);
    ProcessCCSDSOPMDataFile(const ProcessCCSDSOPMDataFile &CCSDSOPMdf);
    const ProcessCCSDSOPMDataFile& operator=(const ProcessCCSDSOPMDataFile &CCSDSOPMdf);
    ~ProcessCCSDSOPMDataFile();

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
                          CCSDSOPMMetaData *myMetaData);
    bool GetCCSDSOPMData(std::string &lff, CCSDSOPMObType *myOb);

    CCSDSOPMMetaData *currentCCSDSMetaData;
    CCSDSOPMMetaData *lastMetaDataWritten;
    bool isMetaDataWritten;

};
#endif	/* _ProcessCCSDSOPMDataFileData_hpp */

