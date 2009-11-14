/* 
 * File:   CCSDSHeader.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:02 AM
 */

#ifndef _CCSDSHEADER_HPP
#define	_CCSDSHEADER_HPP


#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "StringUtil.hpp"
#include "MessageInterface.hpp"

class CCSDSHeader
{

public:

    CCSDSHeader();
    CCSDSHeader(const CCSDSHeader &header);
    const CCSDSHeader& CCSDSHeader::operator=(const CCSDSHeader &header);
    virtual ~CCSDSHeader();

    virtual Real        GetRealDataParameter(const Integer id) const;
    virtual Real        GetRealDataParameter(const std::string &label) const;
    virtual Integer     GetIntegerDataParameter(const Integer id) const;
    virtual Integer     GetIntegerDataParameter(const std::string &label) const;
    virtual std::string GetStringDataParameter(const Integer id) const;
    virtual std::string GetStringDataParameter(const std::string &label) const;
    virtual StringArray GetStringArrayDataParameter(const Integer id) const;
    virtual StringArray GetStringArrayDataParameter(const std::string &label) const;

    const std::string* GetDataTypes() const;
    std::string GetDataTypeText(const Integer &id) const;
    Integer GetDataTypeID(const std::string &label);

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;
    
    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    bool IsParameterRequired(const Integer id) const;
    virtual bool IsParameterDefined(std::string value) const;
    virtual bool IsParameterDefined(StringArray value) const;
    virtual bool IsParameterDefined(Real value) const;
    virtual bool IsParameterDefined(Integer value) const;
    virtual bool IsParameterDefined(bool value) const;
    virtual bool Validate() const;

    friend Integer CountRequiredNumberHeaderDataParameters();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSHeader *myCCSDSheader);

    enum CCSDS_HEADERDATA_REPS
    {
	CCSDS_VERSION_ID,
	CCSDS_CREATIONDATE_ID,
	CCSDS_ORIGINATOR_ID,
	CCSDS_HEADERCOMMENTS_ID,
	EndCCSDSHeaderDataReps
    };

    enum CCSDS_DATATYPE_REPS
    {
        QUATERNION_ID,
        EULERANGLE_ID,
        SPINSTABILIZED_ID,
        STATEVECTOR_ID,
        KEPLERIANELEMENTS_ID,
        SPACECRAFTPARAMETERS_ID,
        SPACECRAFTINERTIA_ID,
        MANEUVER_ID,
        ATTITUDEMANEUVER_ID,
        TRACKINGDATA_ID,
	EndCCSDSTypeReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSAPMDataFile;
    friend class ProcessCCSDSAEMDataFile;
    friend class ProcessCCSDSOPMDataFile;
    friend class ProcessCCSDSOEMDataFile;
    friend class ProcessCCSDSTDMDataFile;

protected:

    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSTypeReps];
    static const std::string CCSDS_HEADER_KEYWORDS[EndCCSDSHeaderDataReps];
    static const std::string CCSDS_HEADER_UNIT_DESCRIPTIONS[EndCCSDSHeaderDataReps];
    static const bool CCSDS_HEADER_IS_REQUIRED[EndCCSDSHeaderDataReps];
    static const Gmat::ParameterType CCSDS_HEADER_PARAMETER_TYPE[EndCCSDSHeaderDataReps];
    static const std::string CCSDS_HEADER_FILEFORMAT_DESCRIPTIONS[EndCCSDSHeaderDataReps];

    std::string fileType;
    Real ccsdsVersion;
    std::string creationDate;
    std::string originator;
    Integer dataType;
    StringArray comments;

};

#endif	/* _CCSDSHEADER_HPP */

