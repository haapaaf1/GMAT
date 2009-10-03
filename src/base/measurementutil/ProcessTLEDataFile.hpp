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
#include "ObType.hpp"

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

    // Method to obtain a TLE data point
    bool GetData(ObType *myTLEdata);
    
    // Method to write TLE to file
    bool WriteData(ObType *myTLEdata);
    
private:

    bool GetTLEData(std::string &lff, std::string &lff2,
			       TLEObType *myTLEdata);



};

#endif	/* _ProcessTLEDataFile_hpp */

