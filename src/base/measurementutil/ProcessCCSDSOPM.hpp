//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOPM
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/08/30
//
/**
 *
 * Implements DataFile base class to read files written in CCSDS orbit
 * parameter message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessCCSDSOPM_hpp
#define	ProcessCCSDSOPM_hpp

#include "DataFile.hpp"

class ProcessCCSDSOPM : public DataFile
{

public:
    
    ProcessCCSDSOPM(const std::string &itsName);
    ~ProcessCCSDSOPM();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    // Measurement Data Access functions
    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();
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

    enum OPM_TYPE_REPS
    {
	GPSONLY_ID,
        MIXED_ID,
        GLONASSONLY_ID,
	LEOONLY_ID,
	GALILEOONLY_ID,
	EndOPMTypeReps
    };

    enum OPM_TIME_REPS
    {
	GPSTIME_ID,
        GLONASSUTC_ID,
        GALILEOSYSTEMTIME_ID,
	TAI_ID,
	UTC_ID,
	EndOPMTimeReps
    };

private:

    static const std::string OPM_TYPE_DESCRIPTIONS[EndOPMTypeReps];
    static const std::string OPM_TIME_DESCRIPTIONS[EndOPMTimeReps];

    // Specific data type processing functions
    bool GetData(fstream &theFile);

    bool GetSP3cHeader(std::string firstline, fstream &theFile);
    bool GetSP3cData(std::string &lff, fstream &theFile);

    // Vector containers for the measurement data
    std::vector<ccsds_header*> ccsdsOPMHeader;
    std::vector<ccsds_opm_obtype*> ccsdsOPMData;

    //Current iterator pointing at data
    std::vector<ccsds_opm_obtype*>::iterator i;

    //Current iterator pointing at data
    std::vector<sp3c_position*>::iterator i_p;

    //Current iterator pointing at data
    std::vector<sp3c_velocity*>::iterator i_v;

    //Current iterator pointing at data
    std::vector<sp3c_posClockCorrelation*>::iterator i_ep;

    //Current iterator pointing at data
    std::vector<sp3c_velClockRateCorrelation*>::iterator i_ev;

    //Current iteratory pointing at header
    std::vector<ccsds_header*>::iterator i_h;

};
#endif	/* _ProcessCCSDSOPMData_hpp */

