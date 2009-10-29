/* 
 * File:   CCSDSOPMObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:20 AM
 */

#ifndef _CCSDSOPMOBTYPE_HPP
#define	_CCSDSOPMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "CCSDSOPMMetaData.hpp"
#include "CCSDSOPMStateVector.hpp"
#include "CCSDSKeplerianElements.hpp"
#include "CCSDSSpacecraftParameters.hpp"
#include "CCSDSManeuver.hpp"

class CCSDSOPMObType : public CCSDSObType
{

public :

    CCSDSOPMObType();
    CCSDSOPMObType(const CCSDSOPMObType &opm);
    const CCSDSOPMObType& CCSDSOPMObType::operator=(const CCSDSOPMObType &opm);
    ~CCSDSOPMObType();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOPMObType *myOPM);

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
	EndCCSDSOPMTimeReps
    };

    friend class ProcessCCSDSOPMDataFile;

private:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps - EndCCSDSTimeReps];

    // Pointer to the data records
    CCSDSOPMMetaData *ccsdsOPMMetaData;
    CCSDSOPMStateVector *ccsdsOPMStateVector;
    CCSDSKeplerianElements *ccsdsOPMKeplerianElements;
    CCSDSSpacecraftParameters *ccsdsOPMSpacecraftParameters;
    std::vector<CCSDSManeuver*> ccsdsOPMManeuvers;
    std::vector<CCSDSManeuver*>::const_iterator i_ccsdsOPMManeuvers;
};

#endif	/* _CCSDSOPMOBTYPE_HPP */

