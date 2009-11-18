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

//using namespace CCSDSTDM;

class ProcessCCSDSTDMDataFile : public ProcessCCSDSDataFile
{
    
public:
    
    ProcessCCSDSTDMDataFile(const std::string &itsName);
    ProcessCCSDSTDMDataFile(const ProcessCCSDSTDMDataFile &CCSDSTDMdf);
    const ProcessCCSDSTDMDataFile& operator=(const ProcessCCSDSTDMDataFile &CCSDSTDMdf);
    virtual ~ProcessCCSDSTDMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);
    
private:

    // Specific data type processing functions
    bool GetCCSDSMetaData(std::string &nextline,
                          CCSDSTDMObType *myOb);
    bool GetCCSDSTDMData(std::string &lff, CCSDSTDMObType *myOb);



};
#endif	/* _ProcessCCSDSTDMDataFileData_hpp */

