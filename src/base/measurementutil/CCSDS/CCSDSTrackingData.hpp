/* 
 * File:   CCSDSTrackingData.hpp
 * Author: mwilkins
 *
 * Created on November 13, 2009, 9:59 AM
 */

#ifndef _CCSDSTRACKINGDATA_HPP
#define	_CCSDSTRACKINGDATA_HPP

#include "CCSDSData.hpp";

class CCSDSTrackingData : public CCSDSData
{

public:

    CCSDSTrackingData();
    CCSDSTrackingData(const CCSDSTrackingData &data);
    const CCSDSTrackingData& CCSDSTrackingData::operator=(const CCSDSTrackingData &data);
    virtual ~CCSDSTrackingData();

    friend std::ostream& operator<< (std::ostream &output,
                       const CCSDSTrackingData *myCCSDSTrackingData);

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CCSDSCountRequiredNumberDataParameters();
    bool Validate() const;

    enum DATATYPE_REPS
    {
	ANGLE1_ID,
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
        COMMENTS_ID,
        EndCCSDSTDMTypeReps
    };

    enum CCSDS_DATA_REPS
    {
        CCSDS_TRACKINGDATA_KEYWORD_ID,
        CCSDS_TRACKINGDATA_TIMETAG_ID,
        CCSDS_TRACKINGDATA_MEASUREMENT_ID,
        CCSDS_TRACKINGDATA_COMMENTS_ID,
        EndCCSDSTrackingDataReps
    };

    friend class ProcessCCSDSTDMDataFile;

protected:
    
    static const std::string CCSDS_TRACKINGDATA_KEYWORDS[EndCCSDSTDMTypeReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTDMTypeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSTDMTypeReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSTDMTypeReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTDMTypeReps];
    static const std::string CCSDS_TRACKINGDATA_DESCRIPTIONS[EndCCSDSTrackingDataReps];

    Integer keywordID;
    std::string keyword;
    std::string timeTag;
    Real measurement;
    StringArray comments;

};

#endif	/* _CCSDSTRACKINGDATA_HPP */

