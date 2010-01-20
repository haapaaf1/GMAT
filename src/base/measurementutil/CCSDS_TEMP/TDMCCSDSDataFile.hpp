//$Header$
//------------------------------------------------------------------------------
//                             TDMCCSDSDataFile
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

#ifndef TDMCCSDSDataFile_hpp
#define	TDMCCSDSDataFile_hpp

#include "CCSDSDataFile.hpp"

//using namespace CCSDSTDM;

class TDMCCSDSDataFile : public CCSDSDataFile
{
    
public:
    
    TDMCCSDSDataFile(const std::string &itsName);
    TDMCCSDSDataFile(const TDMCCSDSDataFile &CCSDSTDMdf);
    const TDMCCSDSDataFile& operator=(const TDMCCSDSDataFile &CCSDSTDMdf);
    virtual ~TDMCCSDSDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);
    
private:

    // Specific data type ing functions
    bool GetCCSDSMetaData(std::string &nextline,
                          TDMCCSDSObType *myOb);
    bool GetCCSDSTDMData(std::string &lff, TDMCCSDSObType *myOb);



};
#endif	/* _TDMCCSDSDataFileData_hpp */

