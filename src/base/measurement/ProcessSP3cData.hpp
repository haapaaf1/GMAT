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

    // Measurement Data Access functions
    bool AdvanceToNextOb();
    std::string GetDataParameterText(const Integer id) const;
    Integer     GetDataParameterID(const std::string &str) const;
    Integer     GetFileTypeID(const std::string &str) const;
    Integer     GetTimeSystemID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    bool     GetBoolDataParameter(const Integer id) const;
    bool     GetBoolDataParameter(const std::string &label) const;
    Real     GetRealDataParameter(const Integer id) const;
    Real     GetRealDataParameter(const std::string &label) const;
    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
    IntegerArray     GetIntegerArrayDataParameter(const Integer id) const;
    IntegerArray     GetIntegerArrayDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

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
	EndSP3cTimeReps
    };

private:

    static const std::string SP3c_TYPE_DESCRIPTIONS[EndSP3cTypeReps];
    static const std::string SP3c_TIME_DESCRIPTIONS[EndSP3cTimeReps];

    // Specific data type processing functions
    bool GetData(std::ifstream &theFile);

    bool GetSP3cHeader(std::string firstline, std::ifstream &theFile);
    bool GetSP3cData(std::string &lff, std::ifstream &theFile);

    // Vector containers for the measurement data
    std::vector<sp3c_header*> SP3cHeader;
    std::vector<sp3c_obtype*> SP3cData;

    //Current iterator pointing at data
    std::vector<sp3c_obtype*>::iterator i;

    //Current iterator pointing at data
    std::vector<sp3c_position*>::iterator i_p;

    //Current iterator pointing at data
    std::vector<sp3c_velocity*>::iterator i_v;

    //Current iterator pointing at data
    std::vector<sp3c_posClockCorrelation*>::iterator i_ep;

    //Current iterator pointing at data
    std::vector<sp3c_velClockRateCorrelation*>::iterator i_ev;

    //Current iteratory pointing at header
    std::vector<sp3c_header*>::iterator i_h;

};
#endif	/* _ProcessSP3cData_hpp */

