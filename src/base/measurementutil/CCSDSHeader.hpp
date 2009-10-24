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
#include "sstream"
#include <pcrecpp.h>
#include "StringUtil.hpp"

class CCSDSHeader
{

public:

    CCSDSHeader();
    CCSDSHeader(const CCSDSHeader &header);
    const CCSDSHeader& CCSDSHeader::operator=(const CCSDSHeader &header);
    virtual ~CCSDSHeader();

    virtual const std::string* GetDataTypes() const;
    virtual std::string GetDataTypeText(const Integer &id) const;
    virtual Integer GetDataTypeID(const std::string &label);

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;
    
    Integer GetDataParameterID(const std::string &str) const;
    std::string GetDataParameterText(const Integer id) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    virtual bool CheckDataAvailability(const std::string str) const;
    virtual bool IsParameterRequired(const Integer id) const;
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
        GENERICDATA_ID,
	EndCCSDSTypeReps
    };

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
    StringArray comments;
    Integer dataType;

};

#endif	/* _CCSDSHEADER_HPP */

