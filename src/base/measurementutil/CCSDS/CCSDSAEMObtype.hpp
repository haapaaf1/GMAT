/* 
 * File:   CCSDSAEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:23 AM
 */

#ifndef _CCSDSAEMOBTYPE_HPP
#define	_CCSDSAEMOBTYPE_HPP

#include "CCSDSObtype.hpp"
#include "CCSDSAEMMetaData.hpp"
#include "CCSDSAEMQuaternion.hpp"
#include "CCSDSAEMEulerAngle.hpp"
#include "CCSDSAEMSpinStabilized.hpp"

class CCSDSAEMObType : public CCSDSObType
{

public:

    CCSDSAEMObType();
    CCSDSAEMObType(const CCSDSAEMObType &AEM);
    const CCSDSAEMObType& CCSDSAEMObType::operator=(const CCSDSAEMObType &AEM);
    ~CCSDSAEMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAEMObType *myAEM);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        TT_ID,
        GPS_ID,
	TDB_ID,
        TCB_ID,
	EndCCSDSAEMTimeReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSAEMDataFile;

protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps - EndCCSDSTimeReps];

    // Pointer to the metadata record
    CCSDSAEMMetaData* ccsdsMetaData;
    CCSDSAEMQuaternion *ccsdsAEMQuaternion;
    CCSDSAEMEulerAngle *ccsdsAEMEulerAngle;
    CCSDSAEMSpinStabilized *ccsdsAEMSpinStabilized;

    bool commentsCurrentlyAllowed;
    
};

#endif	/* _CCSDSAEMOBTYPE_HPP */

