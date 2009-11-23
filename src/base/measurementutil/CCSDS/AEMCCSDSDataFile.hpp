//$Header$
//------------------------------------------------------------------------------
//                             AEMCCSDSDataFile
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

#ifndef AEMCCSDSDataFile_hpp
#define	AEMCCSDSDataFile_hpp

#include "CCSDSDataFile.hpp"

class AEMCCSDSDataFile : public CCSDSDataFile
{

public:

    AEMCCSDSDataFile(const std::string &itsName);
    AEMCCSDSDataFile(const AEMCCSDSDataFile &CCSDSAEMdf);
    const AEMCCSDSDataFile& operator=(const AEMCCSDSDataFile &CCSDSAEMdf);
    virtual ~AEMCCSDSDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);

private:

    // Specific data type ing functions
    bool GetCCSDSMetaData(std::string &nextline, AEMCCSDSObType *myOb);
    bool GetCCSDSAEMData(std::string &lff, AEMCCSDSObType *myOb);
    
};
#endif	/* _AEMCCSDSDataFileData_hpp */

