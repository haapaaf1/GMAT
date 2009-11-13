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
    ~CCSDSTrackingData();

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

    static const std::string CCSDS_TRACKINGDATA_KEYWORDS[EndCCSDSTrackingDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSTrackingDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSTrackingDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSTrackingDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSTrackingDataReps];

    Integer keywordID;
    std::string keyword;
    std::string timeTag;
    Real measurement;
    StringArray comments;

};

#endif	/* _CCSDSTRACKINGDATA_HPP */

