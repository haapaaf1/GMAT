//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRData
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

#ifndef ProcessSLRData_hpp
#define	ProcessSLRData_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessSLRData : public DataFile
{

public:
    
    ProcessSLRData(const std::string &itsName);
    ~ProcessSLRData();

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

    Integer SLRCheckSum(const std::string &str);
    
    enum DATATYPE_REPS
    {
	TWOWAYTIMEOFFLIGHT_ID = EndDataTypeReps,
	EndSLRTypeReps    
    };
    
    enum TIMESYSTEM_REPS
    {
	UTC_ID = EndTimeReps,
	GPS_ID,
	BIPM_ID,
	BIH_ID,
	EndSLRTimeReps
    };
    
private:
    
    static const std::string DATATYPE_DESCRIPTIONS[EndSLRTypeReps];
    static const std::string TIMESYSTEM_DESCRIPTIONS[EndSLRTimeReps];

    // Specific data type processing functions
    bool FindSLRHeaderLine(slr_header *mySLRheader, Integer &flag);
    bool GetData(slr_header *mySLRheader,SLRObtype *mySLRdata);


    bool GetSLRHeader(std::string &lff, slr_header *mySLRheader);
    bool GetSLRData(std::string &lff, slr_header *mySLRheader,
                    SLRObtype *mySLRdata);

    bool WriteDataHeader(slr_header *mySLRheader);
    bool WriteData(SLRObtype *mySLRdata);

    // Vector containers for the measurement data
    std::vector<slr_header*> slrHeader;
    std::vector<SLRObtype*> slrData;

    //Current iterator pointing at data
    std::vector<SLRObtype*>::iterator i;

    //Current iteratory pointing at header
    std::vector<slr_header*>::iterator i_h;
    
};

#endif	/* _ProcessSLRData_hpp */