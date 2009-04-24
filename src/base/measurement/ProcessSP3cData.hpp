//$Header$
//------------------------------------------------------------------------------
//                             ProcessSP3cData
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
 * Implements DataFile base class to read files written in the SP3c format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessSP3cData_hpp
#define	ProcessSP3cData_hpp

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <pcrecpp.h>
#include "RealUtilities.hpp"
#include "DataFile.hpp"

class ProcessSP3cData : public DataFile
{

public:
    
    ProcessSP3cData(const std::string &itsName);
    ~ProcessSP3cData();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;
    
    enum SP3c_TYPE_REPS
    {
	GPSONLY_ID,
        MIXED_ID,
        GLONASSONLY_ID,
	LEOONLY_ID,
	GALILEOONLY_ID,
	EndSP3cTypeReps
    };

    enum SP3c_TIME_REPS
    {
	GPSTIME_ID,
        GLONASSUTC_ID,
        GALILEOSYSTEMTIME_ID,
	TAI_ID,
	UTC_ID,
	EndSP3cTypeReps
    };


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

    bool GetData(std::ifstream &theFile, SP3c_obtype *mySP3cData);

    // Specific data type processing functions
    bool ExtractSP3cData(std::string &lff, SP3c_obtype *mySP3cData);
    
    static const std::string SP3c_TYPE_DESCRIPTIONS[EndSP3cTypeReps];
    static const std::string SP3c_TIME_DESCRIPTIONS[EndSP3cTimeReps];

    std::vector<SP3c_obtype*> SP3cData;

    //Current iterator pointing at data
    std::vector<SP3c_obtype*>::iterator i;

};

#endif	/* _ProcessSP3cData_hpp */

