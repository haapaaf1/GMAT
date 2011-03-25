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

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "DataFile.hpp"

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


    // Measurement Data Access functions
    bool AdvanceToNextOb();
    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real     GetRealDataParameter(const Integer id) const;
    Real     GetRealDataParameter(const std::string &label) const;
    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

private:


    bool GetTLEData(std::string &lff, std::string &lff2,
			       tle_obtype *myTLEdata);

    // Specific data type processing functions
    bool GetData(std::ifstream &theFile, tle_obtype *myTLEdata);

    // Vector container of observations
    std::vector<tle_obtype*> tleData;

    //Current iterator pointing at data
    std::vector<tle_obtype*>::iterator i;


};

#endif	/* _ProcessTLEData_hpp */

