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
    bool CheckMetaDataAvailability(const std::string str) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

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
	CCSDS_TDM_TIMETAG_ID,
	CCSDS_TDM_MEASUREMENT_ID,
	CCSDS_TDM_UNITS_ID,
	CCSDS_TDM_KEYWORD_ID,
        CCSDS_GENERALDATA_KEYWORD_ID,
        CCSDS_GENERALDATA_TIMETAG_ID,
        CCSDS_GENERALDATA_MEASUREMENT_ID,
        CCSDS_GENERALDATA_COMMENTS_ID,
	EndCCSDSTDMMetaDataReps
    };

protected:

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

#endif	/* _CCSDSTDMMETADATA_HPP */

