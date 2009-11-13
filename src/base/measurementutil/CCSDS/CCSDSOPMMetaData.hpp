/* 
 * File:   CCSDSOPMMetaData.hpp
 * Author: mwilkins
 *
 * Created on October 20, 2009, 10:48 AM
 */

#ifndef _CCSDSOPMMETADATA_HPP
#define	_CCSDSOPMMETADATA_HPP

#include "CCSDSMetaData.hpp"

class CCSDSOPMMetaData : public CCSDSMetaData
{
    
public:

    CCSDSOPMMetaData();
    CCSDSOPMMetaData(const CCSDSOPMMetaData &opmMD);
    const CCSDSOPMMetaData& CCSDSOPMMetaData::operator=(const CCSDSOPMMetaData &opmMD);
    ~CCSDSOPMMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOPMMetaData *myMetadata);

    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

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
        EndCCSDSOPMMetaDataReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSOPMDataFile;

private:

    static const std::string CCSDS_OPM_METADATA_KEYWORDS[EndCCSDSOPMMetaDataReps];
    static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSOPMMetaDataReps];
    static const bool CCSDS_METADATA_IS_REQUIRED[EndCCSDSOPMMetaDataReps];
    static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSOPMMetaDataReps];
    static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSOPMMetaDataReps];

protected:

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string refFrame;
    std::string timeSystem;
    StringArray comments;
};

#endif	/* _CCSDSOPMMETADATA_HPP */

