//$Header$
//------------------------------------------------------------------------------
//                             APMCCSDSObType
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/09/04
//
/**
 *
 * This class specifies the CCSDS Attitude Parameter observation data type.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAPMOBTYPE_HPP
#define	_CCSDSAPMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "APMCCSDSMetaData.hpp"
#include "APMQuaternionCCSDSData.hpp"
#include "APMEulerAngleCCSDSData.hpp"
#include "APMSpinStabilizedCCSDSData.hpp"
#include "SpacecraftInertiaCCSDSData.hpp"
#include "AttitudeManeuverCCSDSData.hpp"

class APMCCSDSObType : public CCSDSObType
{
    
public:

    APMCCSDSObType();
    APMCCSDSObType(const APMCCSDSObType &APM);
    const APMCCSDSObType& operator=(const APMCCSDSObType &APM);
    ~APMCCSDSObType();

    GmatBase *Clone() const;

    bool Validate() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const APMCCSDSObType *myAPM);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    void SetMetaData(APMCCSDSMetaData *myCCSDSMetaData);
    APMCCSDSMetaData* GetMetaData();

    void SetQuaternion(APMQuaternionCCSDSData *myQuaternion);
    APMQuaternionCCSDSData* GetQuaternion();

    void SetEulerAngle(APMEulerAngleCCSDSData *myEulerAngle);
    APMEulerAngleCCSDSData* GetEulerAngle();

    void SetSpinStabilized(APMSpinStabilizedCCSDSData *mySpinStabilized);
    APMSpinStabilizedCCSDSData* GetSpinStabilized();

    void SetSpacecraftInertia(SpacecraftInertiaCCSDSData *mySpacecraftInertia);
    SpacecraftInertiaCCSDSData* GetSpacecraftInertia();

    void AddAttitudeManeuver(AttitudeManeuverCCSDSData *myAttitudeManeuver);
    AttitudeManeuverCCSDSData* GetCurrentAttitudeManeuver();
    bool AdvanceToNextAttitudeManeuver();
    bool BackupToPreviousAttitudeManeuver();

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

    friend class CCSDSDataFile;
    friend class APMCCSDSDataFile;

protected:

    static const std::string CCSDS_TIME_DESCRIPTIONS[EndCCSDSAPMTimeReps-EndCCSDSTimeReps];
    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAPMTimeReps - EndCCSDSTimeReps];

    // Pointer to the data records
    APMCCSDSMetaData *ccsdsMetaData;
    APMQuaternionCCSDSData *ccsdsAPMQuaternion;
    APMEulerAngleCCSDSData *ccsdsAPMEulerAngle;
    APMSpinStabilizedCCSDSData *ccsdsAPMSpinStabilized;
    SpacecraftInertiaCCSDSData *ccsdsAPMSpacecraftInertia;
    std::vector<AttitudeManeuverCCSDSData*> ccsdsAPMAttitudeManeuvers;
    std::vector<AttitudeManeuverCCSDSData*>::const_iterator i_ccsdsAPMAttitudeManeuvers;




};

#endif	/* _CCSDSAPMOBTYPE_HPP */

