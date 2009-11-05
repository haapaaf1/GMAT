/* 
 * File:   CCSDSMetaData.hpp
 * Author: mwilkins
 *
 * Created on November 4, 2009, 11:06 AM
 */

#ifndef _CCSDSMETADATA_HPP
#define	_CCSDSMETADATA_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "MessageInterface.hpp"

class CCSDSMetaData
{

public:

    CCSDSMetaData();
    CCSDSMetaData(const CCSDSMetaData &md);
    const CCSDSMetaData& CCSDSMetaData::operator=(const CCSDSMetaData &md);
    virtual ~CCSDSMetaData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSMetaData *myMetadata);

    virtual bool        GetBooleanDataParameter(const Integer id) const;
    virtual bool        GetBooleanDataParameter(const std::string &label) const;
    virtual Real        GetRealDataParameter(const Integer id) const;
    virtual Real        GetRealDataParameter(const std::string &label) const;
    virtual Integer     GetIntegerDataParameter(const Integer id) const;
    virtual Integer     GetIntegerDataParameter(const std::string &label) const;
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;
    virtual StringArray GetStringArrayDataParameter(const Integer id) const;
    virtual StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    //virtual bool CheckDataAvailability(const std::string str) const = 0;

    virtual const std::string* GetKeywords() const = 0;
    virtual const Integer GetKeywordID(const std::string str) const = 0;
    virtual std::string GetUnits(const Integer &id) const = 0;

    virtual Integer GetDataParameterID(const std::string &str) const = 0;
    virtual std::string GetDataParameterText(const Integer id) const = 0;
    virtual Gmat::ParameterType GetDataParameterType(const Integer id) const = 0;
    virtual std::string GetDataParameterTypeString(const Integer id) const = 0;

    virtual bool IsParameterRequired(const Integer id) const = 0;

    enum CCSDS_METADATA_REPS
    {
        EndCCSDSMetaDataReps = 0
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
    friend class ProcessCCSDSOPMDataFile;
    friend class ProcessCCSDSOEMDataFile;
    friend class ProcessCCSDSAPMDataFile;
    friend class ProcessCCSDSAEMDataFile;

private:

    //static const std::string CCSDS_METADATA_KEYWORDS[EndCCSDSMetaDataReps];
    //static const std::string CCSDS_METADATA_UNIT_DESCRIPTIONS[EndCCSDSMetaDataReps];
    //static const bool CCSDS_METADATA_IS_REQUIRED[EndCCSDSMetaDataReps];
    //static const Gmat::ParameterType CCSDS_METADATA_PARAMETER_TYPE[EndCCSDSMetaDataReps];
    //static const std::string CCSDS_METADATA_FILEFORMAT_DESCRIPTIONS[EndCCSDSMetaDataReps];

protected:

    std::string objectName;
    std::string internationalDesignator;
    std::string refFrameOrigin;
    std::string refFrame;
    std::string timeSystem;
    StringArray comments;
};

#endif	/* _CCSDSMETADATA_HPP */

