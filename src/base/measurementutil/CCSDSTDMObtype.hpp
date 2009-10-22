/* 
 * File:   CCSDSFormatDescription.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 4:58 AM
 */

#ifndef _CCSDSTDMOBTYPE_HPP
#define	_CCSDSTDMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "CCSDSTDMMetaData.hpp"

class CCSDSTDMData : public CCSDSData
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSTDMData *myCCSDSTDMData);
};

class CCSDSTDMObType : public CCSDSObType
{   
    
public :
    
    CCSDSTDMObType();
    CCSDSTDMObType(const CCSDSTDMObType &tdm);
    const CCSDSTDMObType& CCSDSTDMObType::operator=(const CCSDSTDMObType &tdm);
    ~CCSDSTDMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSTDMObType *myTDM);
    	
    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    Integer	GetIntegerDataParameter(const Integer id) const;
    Integer	GetIntegerDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
        
    bool IsParameterRequired(const Integer id) const;

    enum CCSDS_DATA_REPS
    {
        CCSDS_TDM_KEYWORD_ID,
        CCSDS_TDM_TIMETAG_ID,
        CCSDS_TDM_MEASUREMENT_ID,
        CCSDS_TDM_COMMENTS_ID,
	EndCCSDSTDMDataReps
    };

    enum DATATYPE_REPS
    {
	ANGLE1_ID = EndCCSDSTypeReps,
	ANGLE2_ID,
	CARRIERPOWER_ID,
	CLOCKBIAS_ID,
	CLOCKDRIFT_ID,
	COMMENT_ID,
	DOPPLERINSTANTANEOUS_ID,
	DOPPLERINTEGRATED_ID,
	DOR_ID,
	PCN0_ID,
	PRN0_ID,
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
        EndCCSDSTDMTypeReps
    };
    
    enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        GPS_ID,
	SCLK_ID,
	EndCCSDSTDMTimeReps
    };
    
    enum TDM_MODE_REPS
    {
	SEQUENTIAL_ID,
	SINGLE_DIFF_ID,
	EndCCSDSTDMModeReps
    };
    
    enum TDM_TIMETAG_REPS
    {
	TRANSMIT_ID,
	RECEIVE_ID,
	EndCCSDSTDMTimetagReps
    };
    
    enum TDM_INTEGRATION_REPS
    {
	START_ID,
	MIDDLE_ID,
	END_ID,
	EndCCSDSTDMIntegrationReps
    };
    
    enum TDM_RANGEMODE_REPS
    {
	COHERENT_ID,
	CONSTANT_ID,
	ONE_WAY_ID,
	EndCCSDSTDMRangeModeReps
    };
    
    enum TDM_RANGEUNIT_REPS
    {
	KM_ID,
	S_ID,
	RU_ID,
	EndCCSDSTDMRangeUnitReps
    };
    
    enum TDM_ANGLETYPE_REPS
    {
	AZEL_ID,
	RADEC_ID,
	XEYN_ID,
	XSYE_ID,
	EndCCSDSTDMAngleTypeReps
    };
    
    enum TDM_DATAQUALITY_REPS
    {
	RAW_ID,
	VALIDATED_ID,
	DEGRADED_ID,
	EndCCSDSTDMDataQualityReps
    };
     	        
    friend class ProcessCCSDSTDMDataFile;
    
protected:

    static const std::string MODE_DESCRIPTIONS[EndCCSDSTDMModeReps];
    static const std::string TIMETAG_DESCRIPTIONS[EndCCSDSTDMTimetagReps];
    static const std::string INTEGRATION_DESCRIPTIONS[EndCCSDSTDMIntegrationReps];
    static const std::string RANGEMODE_DESCRIPTIONS[EndCCSDSTDMRangeModeReps];
    static const std::string RANGEUNIT_DESCRIPTIONS[EndCCSDSTDMRangeUnitReps];
    static const std::string ANGLETYPE_DESCRIPTIONS[EndCCSDSTDMAngleTypeReps];
    static const std::string DATAQUALITY_DESCRIPTIONS[EndCCSDSTDMDataQualityReps];
    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTDMTypeReps - EndCCSDSTypeReps];
    static const std::string CCSDS_TDM_KEYWORDS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps];    
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps - EndCCSDSTimeReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSTDMDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSTDMDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMDataReps];

    // Pointer to the metadata record associated with this data point
    CCSDSTDMMetaData *ccsdsTDMMetaData;

    // Pointers to the generic key epoch value data format
    CCSDSTDMData *ccsdsTDMData;
};
#endif	/* _CCSDSTDMOBTYPE_HPP */

