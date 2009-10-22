/* 
 * File:   CCSDSOEMMetaData.hpp
 * Author: mwilkins
 *
 * Created on October 20, 2009, 11:27 AM
 */

#ifndef _CCSDSOEMMETADATA_HPP
#define	_CCSDSOEMMETADATA_HPP

#include "CCSDSObtype.hpp"

class CCSDSOEMMetaData : public CCSDSObType
{
    
public:

    CCSDSOEMMetaData();
    CCSDSOEMMetaData(const CCSDSOEMMetaData &oemMD);
    const CCSDSOEMMetaData& CCSDSOEMMetaData::operator=(const CCSDSOEMMetaData &oemMD);
    ~CCSDSOEMMetaData();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOEMMetaData *myMetaData);


    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
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
    friend Integer CCSDSOEMCountRequiredNumberMetaDataParameters();

    enum CCSDS_METADATA_REPS
    {
        CCSDS_OEM_OBJECTNAME_ID,
        CCSDS_OEM_OBJECTID_ID,
        CCSDS_OEM_CENTERNAME_ID,
        CCSDS_OEM_REFFRAME_ID,
	CCSDS_OEM_TIMESYSTEM_ID,
	CCSDS_OEM_STARTEPOCH_ID,
	CCSDS_OEM_USEABLE_STARTEPOCH_ID,
	CCSDS_OEM_USEABLE_STOPEPOCH_ID,
	CCSDS_OEM_STOPEPOCH_ID,
        CCSDS_OEM_INTERPOLATION_ID,
        CCSDS_OEM_INTERPOLATIONDEGREE_ID,
        CCSDS_OEM_METADATACOMMENTS_ID,
        EndCCSDSOEMMetaDataReps
    };


    friend class ProcessCCSDSOEMDataFile;

private:

    static const std::string CCSDS_OEM_METADATA_KEYWORDS[EndCCSDSOEMMetaDataReps];
    static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSOEMMetaDataReps];
    static const bool CCSDS_METADATA_IS_REQUIRED[EndCCSDSOEMMetaDataReps];
    static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSOEMMetaDataReps];
    static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSOEMMetaDataReps];

protected:

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string refFrame;
    std::string timeSystem;
    std::string startEpoch;
    std::string stopEpoch;
    std::string useableStartEpoch;
    std::string useableStopEpoch;
    std::string interpolationMethod;
    Integer interpolationDegree;
    StringArray comments;
};

#endif	/* _CCSDSOEMMETADATA_HPP */

