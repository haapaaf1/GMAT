//$Header$
//------------------------------------------------------------------------------
//                             OEMCCSDSDataFile
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

#ifndef OEMCCSDSDataFile_hpp
#define	OEMCCSDSDataFile_hpp

#include "CCSDSDataFile.hpp"

class OEMCCSDSDataFile : public CCSDSDataFile
{

public:

    OEMCCSDSDataFile(const std::string &itsName);
    OEMCCSDSDataFile(const OEMCCSDSDataFile &CCSDSOEMdf);
    const OEMCCSDSDataFile& operator=(const OEMCCSDSDataFile &CCSDSOEMdf);
    virtual ~OEMCCSDSDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);

    friend class OEMCCSDSMetaData;
    friend class OEMCCSDSStateVector;

private:

    // Specific data type ing functions
    bool GetCCSDSMetaData(std::string &lff, OEMCCSDSObType *myOb);
    bool GetCCSDSOEMData(std::string &lff, OEMCCSDSObType *myOb);

};
#endif	/* _OEMCCSDSDataFileData_hpp */

