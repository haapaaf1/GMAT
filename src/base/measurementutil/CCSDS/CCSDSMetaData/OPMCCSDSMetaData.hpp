//$Header$
//------------------------------------------------------------------------------
//                             OPMCCSDSMetaData
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
 * This class specifies the CCSDS Orbit Parameter MetaData class.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSOPMMETADATA_HPP
#define	_CCSDSOPMMETADATA_HPP

#include "CCSDSMetaData.hpp"

class OPMCCSDSMetaData : public CCSDSMetaData
{
    
public:

    OPMCCSDSMetaData();
    OPMCCSDSMetaData(const OPMCCSDSMetaData &opmMD);
    const OPMCCSDSMetaData& OPMCCSDSMetaData::operator=(const OPMCCSDSMetaData &opmMD);
    ~OPMCCSDSMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const OPMCCSDSMetaData *myMetadata);

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
    friend Integer CountRequiredNumberOPMMetaDataParameters();
    bool Validate() const;
    
    enum CCSDS_METADATA_REPS
    {
        CCSDS_OPM_OBJECTNAME_ID,
        CCSDS_OPM_OBJECTID_ID,
        CCSDS_OPM_CENTERNAME_ID,
        CCSDS_OPM_REFFRAME_ID,
	CCSDS_OPM_TIMESYSTEM_ID,
        CCSDS_OPM_METADATACOMMENTS_ID,
        EndOPMCCSDSMetaDataReps
    };

    friend class CCSDSDataFile;
    friend class OPMCCSDSDataFile;

private:

    static const std::string CCSDS_OPM_METADATA_KEYWORDS[EndOPMCCSDSMetaDataReps];
    static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndOPMCCSDSMetaDataReps];
    static const bool CCSDS_METADATA_IS_REQUIRED[EndOPMCCSDSMetaDataReps];
    static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndOPMCCSDSMetaDataReps];
    static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndOPMCCSDSMetaDataReps];

protected:

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string refFrame;
    std::string timeSystem;
    StringArray comments;
};

#endif	/* _CCSDSOPMMETADATA_HPP */

