//$Header$
//------------------------------------------------------------------------------
//                             APMCCSDSMetaData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/20
//
/**
 *
 * This class specifies the CCSDS Attitude Parameter MetaData class.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAPMMETADATA_HPP
#define	_CCSDSAPMMETADATA_HPP

#include "CCSDSMetaData.hpp"

class APMCCSDSMetaData : public CCSDSMetaData
{

public:
    
    APMCCSDSMetaData();
    APMCCSDSMetaData(const APMCCSDSMetaData &apmMD);
    const APMCCSDSMetaData& APMCCSDSMetaData::operator=(const APMCCSDSMetaData &apmMD);
    ~APMCCSDSMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const APMCCSDSMetaData *myMetaData);

    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberAPMMetaDataParameters();
    bool Validate() const;

    enum CCSDS_METADATA_REPS
    {
        CCSDS_APM_OBJECTNAME_ID,
        CCSDS_APM_OBJECTID_ID,
        CCSDS_APM_CENTERNAME_ID,
	CCSDS_APM_TIMESYSTEM_ID,
        CCSDS_APM_METADATACOMMENTS_ID,
        EndAPMCCSDSMetaDataReps
    };

    friend class CCSDSDataFile;
    friend class APMCCSDSDataFile;

private:

    static const std::string CCSDS_APM_METADATA_KEYWORDS[EndAPMCCSDSMetaDataReps];
    static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndAPMCCSDSMetaDataReps];
    static const bool CCSDS_METADATA_IS_REQUIRED[EndAPMCCSDSMetaDataReps];
    static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndAPMCCSDSMetaDataReps];
    static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndAPMCCSDSMetaDataReps];

protected:

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string timeSystem;
    StringArray comments;
};

#endif	/* _CCSDSAPMMETADATA_HPP */

