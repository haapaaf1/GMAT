/* 
 * File:   CCSDSHeader.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:02 AM
 */

#ifndef _CCSDSHEADER_HPP
#define	_CCSDSHEADER_HPP

#include "CCSDSObtype.hpp"

class CCSDSHeader : public CCSDSObType
{

public:

    CCSDSHeader();
    CCSDSHeader(const CCSDSHeader &header);
    const CCSDSHeader& CCSDSHeader::operator=(const CCSDSHeader &header);
    ~CCSDSHeader();

    GmatBase *Clone() const;

    virtual const std::string* GetDataTypes() const;
    virtual std::string GetDataTypeText(const Integer &id) const;
    virtual Integer GetDataTypeID(const std::string &label);


    friend std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myCCSDSheader);

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
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSHeaderDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSHeaderDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSHeaderDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSHeaderDataReps];


    std::string fileType;
    Real ccsdsVersion;
    std::string creationDate;
    std::string originator;
    StringArray comments;
    Integer dataType;

};

#endif	/* _CCSDSHEADER_HPP */

