//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSTDM
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
 * Implements DataFile base class to read files written in CCSDS tracking
 * data message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessCCSDSTDM_hpp
#define	ProcessCCSDSTDM_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessCCSDSTDM : public DataFile
{

public:
    
    ProcessCCSDSTDM(const std::string &itsName);
    ~ProcessCCSDSTDM();

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

    enum DATATYPE_REPS
    {
	ANGLE1_ID = EndDataTypeReps,
	ANGLE2_ID,
	CARRIERPOWER_ID,
	CLOCKBIAS_ID,
	CLOCKDRIFT_ID,
	COMMENT_ID,
	DOPPLERINSTANTANEOUS_ID,
	DOPPLERINTEGRATED_ID,
	DOR_ID,
	PCN0_ID,
	PCN0_ID,
	PRESSURE_ID,
	RANGE_ID,
	RECEIVEFREQUENCY_ID,
	RECEIVEFREQUENCY1_ID,
	RECEIVEFREQUENCY2_ID,
	RECEIVEFREQUENCY3_ID,
	RECEIVEFREQUENCY4_ID,
	RECEIVEFREQUENCY5_ID,
	RHUMIDITY_ID,
	STEC_ID,
	TEMPERATURE_ID,
	TRANSMITFREQUENCY1_ID,
	TRANSMITFREQUENCY2_ID,
	TRANSMITFREQUENCY3_ID,
	TRANSMITFREQUENCY4_ID,
	TRANSMITFREQUENCY5_ID,
	TRANSMITFREQUENCYRate1_ID,
	TRANSMITFREQUENCYRate2_ID,
	TRANSMITFREQUENCYRate3_ID,
	TRANSMITFREQUENCYRate4_ID,
	TRANSMITFREQUENCYRate5_ID,
	TROPODRY_ID,
	TROPOWET_ID,
	VLBIDELAY_ID,
        EndTDMTypeReps
    };
    
    enum TIME_REPS
    {
	UTC_ID = EndTimeReps,
        TAI_ID,
        GPS_ID,
	SCLK_ID,
	EndTDMTimeReps
    };
    
    enum TDM_MODE_REPS
    {
	SEQUENTIAL_ID,
	SINGLE_DIFF_ID,
	EndTDMModeReps
    };
    
    enum TDM_TIMETAG_REPS
    {
	TRANSMIT_ID,
	RECEIVE_ID,
	EndTDMTimetagReps
    };
    
    enum TDM_INTEGRATION_REPS
    {
	START_ID,
	MIDDLE_ID,
	END_ID,
	EndIntegrationReps
    };
    
    enum TDM_RANGEMODE_REPS
    {
	COHERENT_ID,
	CONSTANT_ID,
	ONE_WAY_ID,
	EndTDMRangeModeReps
    };
    
    enum TDM_RANGEUNIT_REPS
    {
	KM_ID,
	S_ID,
	RU_ID,
	EndTDMRangeUnitReps
    };
    
    enum TDM_ANGLETYPE_REPS
    {
	AZEL_ID,
	RADEC_ID,
	XEYN_ID,
	XSYE_ID,
	EndTDMAngleTypeReps
    };
    
    enum TDM_DATAQUALITY_REPS
    {
	RAW_ID,
	VALIDATED_ID,
	DEGRADED_ID,
	EndTDMDataQualityReps
    };
    
private:

    static const std::string DATATYPE_DESCRIPTIONS[EndTDMTypeReps];
    static const std::string TIMESYSTEM_DESCRIPTIONS[EndTDMTimeReps];
    static const std::string MODE_DESCRIPTIONS[EndTDMModeReps]
    static const std::string TIMETAG_DESCRIPTIONS[EndTDMTimetagReps];
    static const std::string INTEGRATION_DESCRIPTIONS[EndTDMIntegrationReps];
    static const std::string INTEGRATION_DESCRIPTIONS[EndTDMIntegrationReps];
    static const std::string RANGEMODE_DESCRIPTIONS[EndTDMRangeModeReps];
    static const std::string RANGEUNIT_DESCRIPTIONS[EndTDMRangeUnitReps];
    static const std::string ANGLETYPE_DESCRIPTIONS[EndTDMAngleTypeReps];
    static const std::string DATAQUALITY_DESCRIPTIONS[EndTDMDataQualityReps];


    // Specific data type processing functions
    bool GetData(fstream &theFile);

    bool GetCCSDSHeader(std::string firstline, fstream &theFile);
    bool GetTDMData(std::string &lff, fstream &theFile);

    // Vector containers for the measurement data
    std::vector<ccsds_header*> ccsdsHeader;
    std::vector<ccsds_tdm_metadata*> ccsdsTDMMetadata;
    std::vector<CCSDSTDMObtype*> ccsdsTDMData;

    //Current iterators pointing at data
    std::vector<CCSDSTDMObtype*>::iterator i;

    //Current iterator pointing at header
    std::vector<ccsds_header*>::iterator i_h;

    //Current iterator pointing at metadata
    std::vector<ccsds_tdm_metadata*>::iterator i_m;
    
    std::vector<ccsds_tdm_data*>::iterator i_angle1;
    std::vector<ccsds_tdm_data*>::iterator i_angle2;
    std::vector<ccsds_tdm_data*>::iterator i_carrierPower;
    std::vector<ccsds_tdm_data*>::iterator i_clockBias;
    std::vector<ccsds_tdm_data*>::iterator i_clockDrift;
    std::vector<ccsds_tdm_data*>::iterator i_comment;
    std::vector<ccsds_tdm_data*>::iterator i_dopplerInstantaneous;
    std::vector<ccsds_tdm_data*>::iterator i_dopplerIntegrated;
    std::vector<ccsds_tdm_data*>::iterator i_dor;
    std::vector<ccsds_tdm_data*>::iterator i_pcn0;
    std::vector<ccsds_tdm_data*>::iterator i_prn0;
    std::vector<ccsds_tdm_data*>::iterator i_pressure;
    std::vector<ccsds_tdm_data*>::iterator i_range;
    std::vector<ccsds_tdm_data*>::iterator i_receiveFrequency;
    std::vector<ccsds_tdm_data*>::iterator i_receiveFrequency1;
    std::vector<ccsds_tdm_data*>::iterator i_receiveFrequency2;
    std::vector<ccsds_tdm_data*>::iterator i_receiveFrequency3;
    std::vector<ccsds_tdm_data*>::iterator i_receiveFrequency4;
    std::vector<ccsds_tdm_data*>::iterator i_receiveFrequency5;
    std::vector<ccsds_tdm_data*>::iterator i_rHumidity;
    std::vector<ccsds_tdm_data*>::iterator i_stec;
    std::vector<ccsds_tdm_data*>::iterator i_temperature;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequency1;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequency2;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequency3;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequency4;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequency5;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequencyRate1;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequencyRate2;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequencyRate3;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequencyRate4;
    std::vector<ccsds_tdm_data*>::iterator i_transmitFrequencyRate5;
    std::vector<ccsds_tdm_data*>::iterator i_tropoDry;
    std::vector<ccsds_tdm_data*>::iterator i_tropoWet;
    std::vector<ccsds_tdm_data*>::iterator i_vlbiDelay;

};
#endif	/* _ProcessCCSDSTDMData_hpp */

