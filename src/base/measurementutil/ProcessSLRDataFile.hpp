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

    bool WriteDataHeader(SLRHeader *mySLRheader);
    bool WriteData(SLRObtype *mySLRdata);
    bool WriteDataHeader(SLRHeader *mySLRheader, fstream *myFile);
    bool WriteData(SLRObtype *mySLRdata, fstream *myFile);

private:
    
    // Specific data type processing functions
    bool FindSLRHeaderLine(SLRHeader *mySLRheader, Integer &flag);
    bool GetData(SLRHeader *mySLRheader,SLRObtype *mySLRdata);


    bool GetSLRHeader(std::string &lff, SLRHeader *mySLRheader);
    bool GetSLRData(std::string &lff, SLRHeader *mySLRheader,
                    SLRObtype *mySLRdata);
    
    // Vector containers for the measurement data
    std::vector<SLRHeader*> slrHeader;
    std::vector<SLRObtype*> slrData;

    //Current iterator pointing at data
    std::vector<SLRObtype*>::iterator i;

    //Current iteratory pointing at header
    std::vector<SLRHeader*>::iterator i_h;
    
};

#endif	/* _ProcessSLRDataFile_hpp */