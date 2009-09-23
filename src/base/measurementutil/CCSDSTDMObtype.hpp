/* 
 * File:   CCSDSFormatDescription.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 4:58 AM
 */

#ifndef _CCSDSTDMOBTYPE_HPP
#define	_CCSDSTDMOBTYPE_HPP

#include "CCSDSObtype.hpp"

class CCSDSTDMObtype : public CCSDSObtype
{   
    
public :
    
    CCSDSTDMObtype();
    CCSDSTDMObtype(const CCSDSTDMObtype &tdm);
    const CCSDSTDMObtype& CCSDSTDMObtype::operator=(const CCSDSTDMObtype &tdm);
    ~CCSDSTDMObtype();

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSTDMObtype *myTDM);
    	
    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    Integer	GetIntegerDataParameter(const Integer id) const;
    Integer	GetIntegerDataParameter(const std::string &label) const;
    bool        GetBoolDataParameter(const Integer id) const;
    bool        GetBoolDataParameter(const std::string &label) const;    
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
        
    bool IsParameterRequired(const Integer id) const;
 
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
    
    enum TIME_REPS
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
     	
    enum CCSDS_DATA_REPS
    {
	CCSDS_TDM_METADATACOMMENTS_ID = EndCCSDSDataReps,
	CCSDS_TDM_TIMESYSTEM_ID,
	CCSDS_TDM_STARTTIME_ID,
	CCSDS_TDM_STOPTIME_ID,
	CCSDS_TDM_PARTICIPANT1_ID,
	CCSDS_TDM_PARTICIPANT2_ID,
	CCSDS_TDM_PARTICIPANT3_ID,
	CCSDS_TDM_PARTICIPANT4_ID,
	CCSDS_TDM_PARTICIPANT5_ID,
	CCSDS_TDM_MODE_ID,
	CCSDS_TDM_PATH_ID,
	CCSDS_TDM_PATH1_ID,
	CCSDS_TDM_PATH2_ID,
	CCSDS_TDM_TRANSMITBAND_ID,
	CCSDS_TDM_RECEIVEBAND_ID,
	CCSDS_TDM_TURNAROUNDNUMERATOR_ID,
	CCSDS_TDM_TURNAROUNDDENOMINATOR_ID,
	CCSDS_TDM_TIMETAGREF_ID,
	CCSDS_TDM_INTEGRATIONINTERVAL_ID,
	CCSDS_TDM_INTEGRATIONREF_ID,
	CCSDS_TDM_FREQUENCYOFFSET_ID,
	CCSDS_TDM_RANGEMODE_ID,
	CCSDS_TDM_RANGEMODULUS_ID,
	CCSDS_TDM_RANGEUNITS_ID,
	CCSDS_TDM_ANGLETYPE_ID,	
	CCSDS_TDM_REFERENCEFRAME_ID,
	CCSDS_TDM_TRANSMITDELAY1_ID,
	CCSDS_TDM_TRANSMITDELAY2_ID,
	CCSDS_TDM_TRANSMITDELAY3_ID,
	CCSDS_TDM_TRANSMITDELAY4_ID,
	CCSDS_TDM_TRANSMITDELAY5_ID,
	CCSDS_TDM_RECEIVEDELAY1_ID,
	CCSDS_TDM_RECEIVEDELAY2_ID,
	CCSDS_TDM_RECEIVEDELAY3_ID,
	CCSDS_TDM_RECEIVEDELAY4_ID,
	CCSDS_TDM_RECEIVEDELAY5_ID,
	CCSDS_TDM_DATAQUALITY_ID,
	CCSDS_TDM_CORRECTIONANGLE1_ID,
	CCSDS_TDM_CORRECTIONANGLE2_ID,
	CCSDS_TDM_CORRECTIONDOPPLER_ID,
	CCSDS_TDM_CORRECTIONRANGE_ID,
	CCSDS_TDM_CORRECTIONRECEIVE_ID,
	CCSDS_TDM_CORRECTIONTRANSMIT_ID,
	CCSDS_TDM_CORRECTIONAPPLIED_ID,	
	CCSDS_TDM_TIMETAG_ID,
	CCSDS_TDM_MEASUREMENT_ID,
	CCSDS_TDM_UNITS_ID,
	CCSDS_TDM_KEYWORD_ID,
	EndCCSDSTDMDataReps
    };
    
    struct ccsds_tdm_metadata
    {
	StringArray metadataComments;
	std::string timeSystem;
	std::string startTime;
	std::string stopTime;
	std::string participants[5];
	std::string mode;
	std::string path[3];
	std::string transmitBand;
	std::string receiveBand;
	Integer turnaroundNumerator;
	Integer turnaroundDenominator;
	std::string timeTagRef;
	Real integrationInterval;
	std::string integrationRef;
	Real frequencyOffset;
	std::string rangeMode;
	Real rangeModulus;
	std::string rangeUnits;
	std::string angleType;
	std::string referenceFrame;
	Real transmitDelay[5];
	Real receiveDelay[5];
	std::string dataQuality;
	Real correctionAngle1;
	Real correctionAngle2;
	Real correctionDoppler;
	Real correctionRange;
	Real correctionReceive;
	Real correctionTransmit;
	bool correctionsApplied;
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
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps-EndCCSDSTypeReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps - EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSTDMDataReps - EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSTDMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMDataReps - EndCCSDSDataReps];

    // Pointer to the metadata record associated with this data point
    ccsds_tdm_metadata *ccsdsTDMMetaData;
    
};

#endif	/* _CCSDSTDMOBTYPE_HPP */

