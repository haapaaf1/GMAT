//$Header$
//------------------------------------------------------------------------------
//                             AEMCCSDSMetaData
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
 * This class specifies the CCSDS Attitude Ephemeris MetaData class.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAEMMETADATA_HPP
#define	_CCSDSAEMMETADATA_HPP

#include "CCSDSMetaData.hpp"

class AEMCCSDSMetaData : public CCSDSMetaData
{

public:
    
    AEMCCSDSMetaData();
    AEMCCSDSMetaData(const AEMCCSDSMetaData &aemMD);
    const AEMCCSDSMetaData& AEMCCSDSMetaData::operator=(const AEMCCSDSMetaData &aemMD);
    ~AEMCCSDSMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const AEMCCSDSMetaData *myMetaData);

    Integer     GetIntegerDataParameter(const Integer id) const;
    Integer     GetIntegerDataParameter(const std::string &label) const;
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
    friend Integer CountRequiredNumberAEMMetaDataParameters();
    bool Validate() const;

    enum CCSDS_METADATA_REPS
    {
        CCSDS_AEM_OBJECTNAME_ID,
        CCSDS_AEM_OBJECTID_ID,
        CCSDS_AEM_CENTERNAME_ID,
        CCSDS_AEM_REFFRAMEA_ID,
        CCSDS_AEM_REFFRAMEB_ID,
        CCSDS_AEM_ATTITUDEDIR_ID,
        CCSDS_AEM_TIMESYSTEM_ID,
	CCSDS_AEM_STARTEPOCH_ID,
	CCSDS_AEM_USEABLE_STARTEPOCH_ID,
	CCSDS_AEM_USEABLE_STOPEPOCH_ID,
	CCSDS_AEM_STOPEPOCH_ID,
        CCSDS_AEM_ATTITUDETYPE_ID,
        CCSDS_AEM_QUATERNIONTYPE_ID,
        CCSDS_AEM_EULERROTSEQ_ID,
        CCSDS_AEM_RATEFRAME_ID,
        CCSDS_AEM_INTERPOLATION_ID,
        CCSDS_AEM_INTERPOLATIONDEGREE_ID,
        CCSDS_AEM_METADATACOMMENTS_ID,
        EndAEMCCSDSMetaDataReps
    };

    friend class CCSDSDataFile;
    friend class AEMCCSDSDataFile;

private:

    static const std::string CCSDS_AEM_METADATA_KEYWORDS[EndAEMCCSDSMetaDataReps];
    static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndAEMCCSDSMetaDataReps];
    static const bool CCSDS_METADATA_IS_REQUIRED[EndAEMCCSDSMetaDataReps];
    static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndAEMCCSDSMetaDataReps];
    static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndAEMCCSDSMetaDataReps];

protected:

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string frameA;
    std::string frameB;
    Integer direction;
    std::string timeSystem;
    std::string startEpoch;
    std::string stopEpoch;
    std::string useableStartEpoch;
    std::string useableStopEpoch;
    Integer attitudeType;
    Integer quaternionType;
    std::string eulerRotationSequence;
    Integer rateFrame;
    std::string interpolationMethod;
    Integer interpolationDegree;
    StringArray comments;
    
};
#endif	/* _CCSDSAEMMETADATA_HPP */

