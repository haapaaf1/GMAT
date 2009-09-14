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
 * Implements DataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessSLRDataFile_hpp
#define	ProcessSLRDataFile_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessSLRDataFile : public DataFile
{

public:
    
    ProcessSLRDataFile(const std::string &itsName);
    ~ProcessSLRDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;
            
    Integer SLRCheckSum(const std::string &str);

    bool WriteDataHeader(SLRObtype::SLRHeader *mySLRheader);
    bool WriteData(SLRObtype *mySLRdata);
    bool WriteDataHeader(SLRObtype::SLRHeader *mySLRheader, fstream *myFile);
    bool WriteData(SLRObtype *mySLRdata, fstream *myFile);

private:
    
    bool GetData(std::string lff, SLRObtype *mySLRdata);

    // Specific data type processing functions
    bool GetSLRHeader(std::string lff, SLRObtype::SLRHeader *mySLRheader);
    bool GetSLRData(std::string lff, SLRObtype *mySLRdata);
    
    SLRObtype::SLRHeader *currentHeader;

};

#endif	/* _ProcessSLRDataFile_hpp */