/* 
 * File:   ProcessCCSDSDataFile.hpp
 * Author: matthewwilkins
 *
 * Created on September 10, 2009, 3:19 PM
 */

#ifndef _ProcessCCSDSDATAFILE_HPP
#define	_ProcessCCSDSDATAFILE_HPP

#include "DataFile.hpp"

class ProcessCCSDSDataFile : public DataFile
{

public:
    
    ProcessCCSDSDataFile(const std::string &itsType, const std::string &itsName);
    ProcessCCSDSDataFile(const ProcessCCSDSDataFile &CCSDSdf);
    const ProcessCCSDSDataFile& operator=(const ProcessCCSDSDataFile &CCSDSdf);
    virtual ~ProcessCCSDSDataFile();
    
    CCSDSHeader* GetHeader();
    void SetHeader(CCSDSHeader *myHeader);

    // Initialization happens here
    virtual bool Initialize();
    
    // Utility functions
    bool CCSDSTimeTag2A1Date(std::string &timeTag, A1Date &myA1Date);
    //bool A1Date2CCSDSTimeTag(A1Date &myA1Date, std::string &timeTag,
    //                         Integer displayMode);

    // methods to get descriptions of allowable CCSDS time system
    // and reference frame values
    const std::string* GetCCSDSTimeSystemDescriptions() const;
    std::string GetCCSDSTimeSystemRep(const Integer &id) const;
    std::string GetCCSDSTimeSystemDescription(const Integer &id) const;
    Integer GetCCSDSTimeSystemID(const std::string &label);

    const std::string* GetCCSDSRefFrameDescriptions() const;
    std::string GetCCSDSRefFrameRep(const Integer &id) const;
    std::string GetCCSDSRefFrameDescription(const Integer &id) const;
    Integer GetCCSDSRefFrameID(const std::string &label);

    // This is coordinated with TimeSystemConverter
    enum TimeSystemValues
    {
        TAI_ID = 8,
        UTC_ID,
        UT1_ID,
        TDB_ID,
        TCB_ID,
        TT_ID,
        GMST_ID,
        GPS_ID,
        SCLK_ID,
        EndCCSDSTimeSystemReps
    };

    enum ReferenceFrameValues
    {
        EME2000_ID,
        ICRF_ID,
        ITRF2000_ID,
        ITRF93_ID,
        ITRF97_ID,
        TOD_ID,
        TRD_ID,
        GRC_ID,
        EndCCSDSRefFrameReps
    };

    bool WriteData(const ObType *myOb);
    bool WriteDataHeader(const ObType *myOb);
    bool WriteMetaData(const ObType *myOb);

    friend class CCSDSHeader;

protected:

    bool GetCCSDSHeader(std::string &lff, CCSDSObType *myOb);
    
    bool GetCCSDSValue(const std::string &lff, std::string &svalue);
    bool GetCCSDSValue(const std::string &lff, Real &value);
    bool GetCCSDSValue(const std::string &lff, Integer &value);

    bool GetCCSDSKeyword(const std::string &lff, std::string &keyword);
    bool GetCCSDSKeyEpochValueData(const std::string &lff, std::string &epoch,
                                   std::string &key, Real &value);
    bool GetCCSDSComments(std::string &lff, StringArray &comments);
    bool GetCCSDSComment(std::string &lff, std::string &comment);

    static const std::string REGEX_CCSDS_DATE;
    static const std::string REGEX_CCSDS_SAVETHEDATE1;
    static const std::string REGEX_CCSDS_SAVETHEDATE2;
    static const std::string REGEX_CCSDS_KEYWORD;
    static const std::string REGEX_CCSDS_SAVETHEKEYWORD;

    static const std::string CCSDS_TIME_SYSTEM_REPS[EndCCSDSTimeSystemReps-8];
    static const std::string CCSDS_TIME_SYSTEM_DESCRIPTIONS[EndCCSDSTimeSystemReps-8];
    static const std::string CCSDS_REF_FRAME_REPS[EndCCSDSRefFrameReps];
    static const std::string CCSDS_REF_FRAME_DESCRIPTIONS[EndCCSDSRefFrameReps];

    // Pointer to current header
    CCSDSHeader *currentCCSDSHeader;
    CCSDSHeader *lastHeaderWritten;
    bool isHeaderWritten;
    Integer requiredNumberHeaderParameters;
    CCSDSMetaData *currentCCSDSMetaData;
    CCSDSMetaData *lastMetaDataWritten;
    bool isMetaDataWritten;
    Integer requiredNumberMetaDataParameters;

    // This flag indicates if the data block is currently being written
    // and indicates whether comments are allowed to be written
    bool writingDataBlock;


};

#endif	/* _ProcessCCSDSDATAFILE_HPP */

