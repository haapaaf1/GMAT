//$Header$
//------------------------------------------------------------------------------
//                             TLEDataFile
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

#ifndef TLEDataFile_hpp
#define	TLEDataFile_hpp

#include "DataFile.hpp"
#include "ObType.hpp"

class TLEDataFile : public DataFile
{

public:
    
    TLEDataFile(const std::string &itsName);
    TLEDataFile(const TLEDataFile &TLEdf);
    const TLEDataFile& operator=(const TLEDataFile &TLEdf);
    ~TLEDataFile();

    // Initialization happens here
    bool Initialize();
   
    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    // Method to obtain a TLE data point
    bool GetData(ObType *myTLEdata);
    
    // Method to write TLE to file
    bool WriteData(const ObType *myTLEdata);
    
private:

    bool GetTLEData(std::string &lff, std::string &lff2,
			       TLEObType *myTLEdata);



};

#endif	/* _TLEDataFile_hpp */

