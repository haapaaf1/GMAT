/*
 * File:   CCSDSData.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 data
 */

#ifndef _CCSDSDATA_HPP
#define	_CCSDSDATA_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "StringUtil.hpp"

class CCSDSData
{

public:

    CCSDSData();
    CCSDSData(const CCSDSData &data);
    const CCSDSData& CCSDSData::operator=(const CCSDSData &data);
    ~CCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                       const CCSDSData *myCCSDSData);

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Integer	GetIntegerDataParameter(const Integer id) const;
    Integer	GetIntegerDataParameter(const std::string &label) const;
    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    bool IsParameterRequired(const Integer id) const;
    friend Integer CCSDSCountRequiredNumberDataParameters();

    enum CCSDS_DATA_REPS
    {
        CCSDS_GENERICDATA_KEYWORDID_ID,
        CCSDS_GENERICDATA_KEYWORD_ID,
        CCSDS_GENERICDATA_TIMETAG_ID,
        CCSDS_GENERICDATA_MEASUREMENT_ID,
        CCSDS_GENERICDATA_COMMENTS_ID,
        EndCCSDSGenericDataReps
    };

    friend class ProcessCCSDSTDMDataFile;

protected:

    static const bool CCSDS_IS_REQUIRED[EndCCSDSGenericDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSGenericDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSGenericDataReps];

    Integer keywordID;
    std::string keyword;
    std::string timeTag;
    Real measurement;
    StringArray comments;

};

#endif	/* _CCSDSDATA_HPP */

