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

    
    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
    
    // Measurement Data Access functions
    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();
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

    Integer TLECheckSum(const std::string &str);

    enum DATATYPE_REPS
    {
	BSTAR_ID = EndDataTypeReps,
	INCLINATION_ID,
	RAAN_ID,
	ECCENTRICITY_ID,
	ARGPERIGEE_ID,
	MEANANOMALY_ID,
	MEANMOTION_ID,
	EndTLETypeReps    
    };
    
    enum TIMESYSTEM_REPS
    {
	UT_ID = EndTimeReps,
	EndTLETimeReps
    };
    
    
private:

    static const std::string DATATYPE_DESCRIPTIONS[EndTLETypeReps];
    static const std::string TIMESYSTEM_DESCRIPTIONS[EndTLETimeReps];

    bool GetTLEData(std::string &lff, std::string &lff2,
			       TLEObtype *myTLEdata);

    // Specific data type processing functions
    bool GetData(TLEObtype *myTLEdata);

    // Function to write TLE to file
    bool WriteData(TLEObtype *myTLEdata);

    // Vector container of observations
    std::vector<TLEObtype*> tleData;

    //Current iterator pointing at data
    std::vector<TLEObtype*>::iterator i;


};

#endif	/* _ProcessTLEData_hpp */

