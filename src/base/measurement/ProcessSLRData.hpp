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

    std::string Ilrs2Cospar(std::string ilrsSatnum);

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

private:

    // Specific data type processing functions
    bool FindSLRHeaderLine(slr_header *mySLRheader, Integer &flag);
    bool GetData(slr_header *mySLRheader,slr_obtype *mySLRdata);


    bool GetSLRHeader(std::string &lff, slr_header *mySLRheader);
    bool GetSLRData(std::string &lff, slr_header *mySLRheader,
                    slr_obtype *mySLRdata);

    bool WriteMeasurementHeader(slr_header *mySLRheader);
    bool WriteMeasurement(slr_obtype *mySLRdata);

    // Vector containers for the measurement data
    std::vector<slr_header*> slrHeader;
    std::vector<slr_obtype*> slrData;

    //Current iterator pointing at data
    std::vector<slr_obtype*>::iterator i;

    //Current iteratory pointing at header
    std::vector<slr_header*>::iterator i_h;
    
};

#endif	/* _ProcessSLRData_hpp */