//$Header$
//------------------------------------------------------------------------------
//                             ProcessTLEDataFile
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
 * Implements DataFile base class to read files written in the TLE format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessTLEDataFile_hpp
#define	ProcessTLEDataFile_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessTLEDataFile : public DataFile
{

public:
    
    ProcessTLEDataFile(const std::string &itsName);
    ProcessTLEDataFile(const ProcessTLEDataFile &TLEdf);
    const ProcessTLEDataFile& operator=(const ProcessTLEDataFile &TLEdf);
    ~ProcessTLEDataFile();

    // Initialization happens here
    bool Initialize();
   
    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;
    
    Integer TLECheckSum(const std::string &str);

    // Function to write TLE to file
    bool WriteData(TLEObtype *myTLEdata);
    bool WriteData(TLEObtype *myTLEdata, fstream *myFile);
    
private:

    bool GetTLEData(std::string &lff, std::string &lff2,
			       TLEObtype *myTLEdata);

    // Specific data type processing functions
    bool GetData(TLEObtype *myTLEdata);

};

#endif	/* _ProcessTLEDataFile_hpp */

