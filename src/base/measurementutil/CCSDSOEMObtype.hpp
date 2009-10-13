/* 
 * File:   CCSDSOEMObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSOEMOBTYPE_HPP
#define	_CCSDSOEMOBTYPE_HPP

#include "CCSDSObType.hpp"

class CCSDSOEMStateVector : public CCSDSStateVector
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                    const CCSDSOEMStateVector *myCCSDSOEMStateVector);
};

class CCSDSOEMMetaData
{

public:

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOEMMetaData *myMetaData);

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

    enum CCSDS_DATA_REPS
    {
        CCSDS_OEM_OBJECTNAME_ID = EndCCSDSDataReps,
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
	CCSDS_OEM_STATEVECTOR_EPOCH_ID,
	CCSDS_OEM_STATEVECTOR_X_ID,
	CCSDS_OEM_STATEVECTOR_Y_ID,
	CCSDS_OEM_STATEVECTOR_Z_ID,
	CCSDS_OEM_STATEVECTOR_XDOT_ID,
        CCSDS_OEM_STATEVECTOR_YDOT_ID,
	CCSDS_OEM_STATEVECTOR_ZDOT_ID,
	CCSDS_OEM_STATEVECTOR_COMMENTS_ID,
        EndCCSDSOEMDataReps
    };

    friend class ProcessCCSDSOEMDataFile;

protected:

    static const std::string CCSDS_TIME_DESCRIPTIONS[EndCCSDSOEMTimeReps-EndCCSDSTimeReps];
    static const std::string CCSDS_DATATYPE_DESCRIPTIONS[EndCCSDSOEMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_OEM_KEYWORDS[EndCCSDSOEMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSOEMDataReps-EndCCSDSDataReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps - EndCCSDSTimeReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSOEMDataReps - EndCCSDSDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSOEMDataReps - EndCCSDSDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSOEMDataReps - EndCCSDSDataReps];
   
    // Pointer to the data records
    CCSDSOEMMetaData* ccsdsOEMMetaData;
    CCSDSOEMStateVector *ccsdsOEMStateVector;

};   

#endif	/* _CCSDSOEMOBTYPE_HPP */

