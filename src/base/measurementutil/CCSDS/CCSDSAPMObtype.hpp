/* 
 * File:   CCSDSAPMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSAPMOBTYPE_HPP
#define	_CCSDSAPMOBTYPE_HPP

#include "CCSDSObtype.hpp"
#include "CCSDSAPMMetaData.hpp"
#include "CCSDSAPMQuaternion.hpp"
#include "CCSDSAPMEulerAngle.hpp"
#include "CCSDSAPMSpinStabilized.hpp"
#include "CCSDSSpacecraftInertia.hpp"
#include "CCSDSAttitudeManeuver.hpp"

class CCSDSAPMObType : public CCSDSObType
{
    
public:

    CCSDSAPMObType();
    CCSDSAPMObType(const CCSDSAPMObType &APM);
    const CCSDSAPMObType& CCSDSAPMObType::operator=(const CCSDSAPMObType &APM);
    ~CCSDSAPMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSAPMObType *myAPM);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    //bool IsParameterRequired(const Integer id) const;

    enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        TT_ID,
        GPS_ID,
	TDB_ID,
        TCB_ID,
	EndCCSDSAPMTimeReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSAPMDataFile;

protected:

    static const std::string CCSDS_TIME_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps - EndCCSDSTimeReps];

    // Pointer to the data records
    CCSDSAPMMetaData *ccsdsMetaData;
    CCSDSAPMQuaternion *ccsdsAPMQuaternion;
    CCSDSAPMEulerAngle *ccsdsAPMEulerAngle;
    CCSDSAPMSpinStabilized *ccsdsAPMSpinStabilized;
    CCSDSSpacecraftInertia *ccsdsAPMSpacecraftInertia;
    std::vector<CCSDSAttitudeManeuver*> ccsdsAPMAttitudeManeuvers;
    std::vector<CCSDSAttitudeManeuver*>::const_iterator i_ccsdsAPMAttitudeManeuvers;




};

#endif	/* _CCSDSAPMOBTYPE_HPP */

