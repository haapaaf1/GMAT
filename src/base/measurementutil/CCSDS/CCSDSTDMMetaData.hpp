/* 
 * File:   CCSDSTDMMetaData.hpp
 * Author: mwilkins
 *
 * Created on October 19, 2009, 1:51 PM
 */

#ifndef _CCSDSTDMMETADATA_HPP
#define	_CCSDSTDMMETADATA_HPP

#include "CCSDSMetaData.hpp"

class CCSDSTDMMetaData : public CCSDSMetaData
{
    
public:

    CCSDSTDMMetaData();
    CCSDSTDMMetaData(const CCSDSTDMMetaData &tdmd);
    const CCSDSTDMMetaData& CCSDSTDMMetaData::operator=(const CCSDSTDMMetaData &tdmd);
    ~CCSDSTDMMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSTDMMetaData *myMetadata);

    friend class CCSDSObType;
    friend class CCSDSTDMObType;
    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;

    // Functions to verify data availability
    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    std::string GetModeText(const Integer id) const;
    Integer     GetModeID(const std::string &str) const;
    std::string GetTimeTagText(const Integer id) const;
    Integer     GetTimeTagID(const std::string &str) const;
    std::string GetIntegrationText(const Integer id) const;
    Integer     GetIntegrationID(const std::string &str) const;
    std::string GetRangeModeText(const Integer id) const;
    Integer     GetRangeModeID(const std::string &str) const;
    std::string GetRangeUnitText(const Integer id) const;
    Integer     GetRangeUnitID(const std::string &str) const;
    std::string GetAngleTypeText(const Integer id) const;
    Integer     GetAngleTypeID(const std::string &str) const;
    std::string GetDataQualityText(const Integer id) const;
    Integer     GetDataQualityID(const std::string &str) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    Integer	GetIntegerDataParameter(const Integer id) const;
    Integer	GetIntegerDataParameter(const std::string &label) const;
    bool        GetBoolDataParameter(const Integer id) const;
    bool        GetBoolDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;
    
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberTDMMetaDataParameters();
    bool Validate() const;
    
    enum CCSDS_METADATA_REPS
    {
	CCSDS_TDM_METADATACOMMENTS_ID,
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
	EndCCSDSTDMMetaDataReps
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

protected:

    static const std::string MODE_DESCRIPTIONS[EndCCSDSTDMModeReps];
    static const std::string TIMETAG_DESCRIPTIONS[EndCCSDSTDMTimetagReps];
    static const std::string INTEGRATION_DESCRIPTIONS[EndCCSDSTDMIntegrationReps];
    static const std::string RANGEMODE_DESCRIPTIONS[EndCCSDSTDMRangeModeReps];
    static const std::string RANGEUNIT_DESCRIPTIONS[EndCCSDSTDMRangeUnitReps];
    static const std::string ANGLETYPE_DESCRIPTIONS[EndCCSDSTDMAngleTypeReps];
    static const std::string DATAQUALITY_DESCRIPTIONS[EndCCSDSTDMDataQualityReps];
    static const std::string CCSDS_TDM_METADATA_KEYWORDS[EndCCSDSTDMMetaDataReps];
    static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMMetaDataReps];
    static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSTDMMetaDataReps];
    static const bool CCSDS_METADATA_IS_REQUIRED[EndCCSDSTDMMetaDataReps];
    static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSTDMMetaDataReps];

    StringArray comments;
    std::string timeSystem;
    std::string startTime;
    std::string stopTime;
    std::string participants[5];
    Integer mode;
    std::string path[3];
    std::string transmitBand;
    std::string receiveBand;
    Integer turnaroundNumerator;
    Integer turnaroundDenominator;
    Integer timeTagRef;
    Real integrationInterval;
    Integer integrationRef;
    Real frequencyOffset;
    Integer rangeMode;
    Real rangeModulus;
    Integer rangeUnits;
    Integer angleType;
    std::string referenceFrame;
    Real transmitDelay[5];
    Real receiveDelay[5];
    Integer dataQuality;
    Real correctionAngle1;
    Real correctionAngle2;
    Real correctionDoppler;
    Real correctionRange;
    Real correctionReceive;
    Real correctionTransmit;
    bool correctionsApplied;
    
};

#endif	/* _CCSDSTDMMETADATA_HPP */

