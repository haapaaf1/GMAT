//$Header$
//------------------------------------------------------------------------------
//                             AEMCCSDSObType
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
 * This class specifies the CCSDS Attitude Ephemeris observation data type.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSAEMOBTYPE_HPP
#define	_CCSDSAEMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "AEMCCSDSMetaData.hpp"
#include "AEMQuaternionCCSDSData.hpp"
#include "AEMEulerAngleCCSDSData.hpp"
#include "AEMSpinStabilizedCCSDSData.hpp"

class AEMCCSDSObType : public CCSDSObType
{

public:

    AEMCCSDSObType();
    AEMCCSDSObType(const AEMCCSDSObType &AEM);
    const AEMCCSDSObType& AEMCCSDSObType::operator=(const AEMCCSDSObType &AEM);
    ~AEMCCSDSObType();

    GmatBase *Clone() const;

    bool Validate() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const AEMCCSDSObType *myAEM);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    void SetMetaData(AEMCCSDSMetaData *myCCSDSMetaData);
    AEMCCSDSMetaData* GetMetaData();

    void SetQuaternion(AEMQuaternionCCSDSData *myQuaternion);
    AEMQuaternionCCSDSData* GetQuaternion();

    void SetEulerAngle(AEMEulerAngleCCSDSData *myEulerAngle);
    AEMEulerAngleCCSDSData* GetEulerAngle();

    void SetSpinStabilized(AEMSpinStabilizedCCSDSData *mySpinStabilized);
    AEMSpinStabilizedCCSDSData* GetSpinStabilized();

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

    friend class CCSDSDataFile;
    friend class AEMCCSDSDataFile;

protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSAEMTimeReps - EndCCSDSTimeReps];

    // Pointer to the metadata record
    AEMCCSDSMetaData* ccsdsMetaData;
    AEMQuaternionCCSDSData *ccsdsAEMQuaternion;
    AEMEulerAngleCCSDSData *ccsdsAEMEulerAngle;
    AEMSpinStabilizedCCSDSData *ccsdsAEMSpinStabilized;

private:
    
    bool commentsCurrentlyAllowed;
    
};

#endif	/* _CCSDSAEMOBTYPE_HPP */

