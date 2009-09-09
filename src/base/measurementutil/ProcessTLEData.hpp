//$Header$
//------------------------------------------------------------------------------
//                             ProcessTLEData
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

#ifndef ProcessTLEData_hpp
#define	ProcessTLEData_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessTLEData : public DataFile
{

public:
    
    ProcessTLEData(const std::string &itsName);
    ~ProcessTLEData();

    // Initialization happens here
    bool Initialize();
   
    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();
    
    Integer TLECheckSum(const std::string &str);

    // Function to write TLE to file
    bool WriteData(TLEObtype *myTLEdata);
    
private:

    bool GetTLEData(std::string &lff, std::string &lff2,
			       TLEObtype *myTLEdata);

    // Specific data type processing functions
    bool GetData(TLEObtype *myTLEdata);

    // Vector container of observations
    std::vector<TLEObtype*> tleData;

    //Current iterator pointing at data
    std::vector<TLEObtype*>::iterator i;


};

#endif	/* _ProcessTLEData_hpp */

