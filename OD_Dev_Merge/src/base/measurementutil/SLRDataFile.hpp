//$Header$
//------------------------------------------------------------------------------
//                             SLRDataFile
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

#ifndef SLRDataFile_hpp
#define	SLRDataFile_hpp
 
#include "DataFile.hpp"
#include "ObType.hpp"

class SLRDataFile : public DataFile
{

public:
    
    SLRDataFile(const std::string &itsName);
    SLRDataFile(const SLRDataFile &SLRdf);
    const SLRDataFile& operator=(const SLRDataFile &SLRdf);
    ~SLRDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    // The GetData function is called during initialization
    bool GetData(ObType *mySLRdata);
    bool WriteData(const ObType *mySLRdata);

private:

    // Specific data type ing functions
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

#endif	/* _SLRDataFile_hpp */
