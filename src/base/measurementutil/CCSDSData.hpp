/*
 * File:   CCSDSData.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 data
 */

#ifndef _CCSDSDATA_HPP
#define	_CCSDSDATA_HPP

class CCSDSData
{

public:

    CCSDSData();
    CCSDSData(const CCSDSData &data);
    const CCSDSData& CCSDSData::operator=(const CCSDSData &data);
    ~CCSDSData();

    GmatBase *Clone() const;

protected:

    static const std::string CCSDS_GENERICDATA_KEYWORDS[EndCCSDSGenericDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSGenericDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSGenericDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSGenericDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSGenericDataReps];


    Integer keywordID;
    std::string timeTag;
    Real measurement;
    StringArray comments;
};

#endif	/* _CCSDSDATA_HPP */

