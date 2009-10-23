/* 
 * File:   CCSDSOEMObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSOEMOBTYPE_HPP
#define	_CCSDSOEMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "CCSDSOEMMetaData.hpp"
   
class CCSDSOEMObType : public CCSDSObType
{

public:

    CCSDSOEMObType();
    CCSDSOEMObType(const CCSDSOEMObType &oem);
    const CCSDSOEMObType& CCSDSOEMObType::operator=(const CCSDSOEMObType &oem);
    ~CCSDSOEMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOEMObType *myOEM);
    
    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;
    std::string GetDataUnits(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;
    
    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
        
    bool IsParameterRequired(const Integer id) const;

    enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        TT_ID,
        GPS_ID,
	TDB_ID,
        TCB_ID,
        GMST_ID,
        MET_ID,
        MRT_ID,
        SCLK_ID,
        UT1_ID,
	EndCCSDSOEMTimeReps
    };

    friend class ProcessCCSDSOEMDataFile;

protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps - EndCCSDSTimeReps];
   
    // Pointer to the data records
    CCSDSOEMMetaData* ccsdsOEMMetaData;
    CCSDSOEMStateVector *ccsdsOEMStateVector;

};   

#endif	/* _CCSDSOEMOBTYPE_HPP */

