//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRDataFile
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
 * Implements DataFile base class to read files written in the SLR format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessSLRDataFile_hpp
#define	ProcessSLRDataFile_hpp
 
#include "DataFile.hpp"
#include "ObType.hpp"

class ProcessSLRDataFile : public DataFile
{

public:
    
    ProcessSLRDataFile(const std::string &itsName);
    ProcessSLRDataFile(const ProcessSLRDataFile &SLRdf);
    const ProcessSLRDataFile& operator=(const ProcessSLRDataFile &SLRdf);
    ~ProcessSLRDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    // The GetData function is called during initialization
    bool GetData(ObType *mySLRdata);
    bool WriteData(const ObType *mySLRdata);

private:

    // Specific data type processing functions
    bool GetSLRHeader(std::string lff, SLRHeader *mySLRheader);
    bool GetSLRData(std::string lff, SLRObType *mySLRdata);

    // The WriteDataHeader function is private because
    // it is called by WriteData header to check if the header
    // line has already been written. You should not need to call
    // this function otherwise.
    bool WriteDataHeader(const ObType *mySLRdata);

    SLRHeader *currentHeader;
    SLRHeader *lastHeaderWritten;
    bool isHeaderWritten;

};

#endif	/* _ProcessSLRDataFile_hpp */