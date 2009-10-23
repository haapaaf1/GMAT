/* 
 * File:   CCSDSHeader.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:02 AM
 */

#ifndef _CCSDSHEADER_HPP
#define	_CCSDSHEADER_HPP

class CCSDSHeader
{

public:

    CCSDSHeader();
    CCSDSHeader(const CCSDSHeader &header);
    const CCSDSHeader& CCSDSHeader::operator=(const CCSDSHeader &header);
    ~CCSDSHeader();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output, const CCSDSHeader *myCCSDSheader);

    enum CCSDS_HEADERDATA_REPS
    {
	CCSDS_VERSION_ID,
	CCSDS_CREATIONDATE_ID,
	CCSDS_ORIGINATOR_ID,
	CCSDS_HEADERCOMMENTS_ID,
	EndCCSDSHeaderDataReps
    };

protected:

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

