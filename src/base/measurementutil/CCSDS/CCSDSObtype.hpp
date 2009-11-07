/* 
 * File:   CCSDSObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:29 AM
 */

#ifndef _CCSDSOBTYPE_HPP
#define	_CCSDSOBTYPE_HPP

#include "ObType.hpp"
#include "CCSDSHeader.hpp"

class CCSDSObType : public ObType
{
    
public :
    
    CCSDSObType(const std::string &type, const std::string &name);
    CCSDSObType(const CCSDSObType &ob);
    const CCSDSObType& operator=(const CCSDSObType &ob);
    virtual ~CCSDSObType();

    GmatBase *Clone() const;

    // Validation methods
    std::string GetCCSDSObType();

    friend std::string GetAttitudeTypeText(const Integer id);
    friend Integer     GetAttitudeTypeID(const std::string &str);
    friend std::string GetAttitudeDirText(const Integer id);
    friend Integer     GetAttitudeDirID(const std::string &str);
    
    virtual const std::string* GetTimeSystems() const;
    virtual std::string GetTimeSystemText(const Integer &id) const;
    virtual Integer GetTimeSystemID(const std::string &label);    

    enum CCSDS_TIMESYSTEM_REPS
    {
	EndCCSDSTimeReps
    };
    
    enum CCSDS_ATTITUDE_TYPE
    {
        CCSDS_QUATERNION_ID = 0,
        CCSDS_QUATERNION_DERIVATIVE_ID,
        CCSDS_QUATERNION_RATE_ID,
        CCSDS_EULER_ANGLE_ID,
        CCSDS_EULER_ANGLE_RATE_ID,
        CCSDS_SPIN_ID,
        CCSDS_SPIN_NUTATION_ID,
        EndCCSDSAttitudeTypeReps
    };

    enum CCSDS_ATTITUDE_DIR
    {
        CCSDS_ATTITUDE_A2B_ID = 0,
        CCSDS_ATTITUDE_B2A_ID,
        EndCCSDSAttitudeDirReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
    friend class ProcessCCSDSOPMDataFile;
    friend class ProcessCCSDSOEMDataFile;
    friend class ProcessCCSDSAPMDataFile;
    friend class ProcessCCSDSAEMDataFile;
    
protected:

    static const std::string CCSDS_ATTITUDE_DIR[EndCCSDSAttitudeDirReps];
    static const std::string CCSDS_ATTITUDE_TYPE[EndCCSDSAttitudeTypeReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

    // Pointer to the header record associated with this data point
    CCSDSHeader *ccsdsHeader;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
