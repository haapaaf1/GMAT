//$Header$
//------------------------------------------------------------------------------
//                             OPMCCSDSObType
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
 * This class specifies the CCSDS Orbit Parameter observation data type.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSOPMOBTYPE_HPP
#define	_CCSDSOPMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "OPMCCSDSMetaData.hpp"
#include "OPMStateVectorCCSDSData.hpp"
#include "KeplerianElementsCCSDSData.hpp"
#include "SpacecraftParametersCCSDSData.hpp"
#include "ManeuverCCSDSData.hpp"

class OPMCCSDSObType : public CCSDSObType
{

public :

    OPMCCSDSObType();
    OPMCCSDSObType(const OPMCCSDSObType &opm);
    const OPMCCSDSObType& OPMCCSDSObType::operator=(const OPMCCSDSObType &opm);
    ~OPMCCSDSObType();

    GmatBase *Clone() const;

    bool Validate() const;

    friend std::ostream& operator<< (std::ostream &output,
                                     const OPMCCSDSObType *myOPM);

    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    void SetMetaData(OPMCCSDSMetaData *myCCSDSMetaData);
    OPMCCSDSMetaData* GetMetaData();

    void SetStateVector(OPMStateVectorCCSDSData *myOPMStateVector);
    OPMStateVectorCCSDSData* GetStateVector();

    void SetKeplerianElements(KeplerianElementsCCSDSData *myOPMKeplerianElements);
    KeplerianElementsCCSDSData* GetKeplerianElements();

    void SetSpacecraftParameters(SpacecraftParametersCCSDSData *mySpacecraftParameters);
    SpacecraftParametersCCSDSData* GetSpacecraftParameters();

    void AddManeuver(ManeuverCCSDSData *myManeuver);
    ManeuverCCSDSData* GetCurrentManeuver();
    bool AdvanceToNextManeuver();
    bool BackupToPreviousManeuver();

    //bool IsParameterRequired(const Integer id) const;

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

    friend class CCSDSDataFile;
    friend class OPMCCSDSDataFile;

private:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOPMTimeReps - EndCCSDSTimeReps];

    // Pointer to the data records
    OPMCCSDSMetaData *ccsdsMetaData;
    OPMStateVectorCCSDSData *ccsdsOPMStateVector;
    KeplerianElementsCCSDSData *ccsdsOPMKeplerianElements;
    SpacecraftParametersCCSDSData *ccsdsOPMSpacecraftParameters;
    std::vector<ManeuverCCSDSData*> ccsdsOPMManeuvers;
    std::vector<ManeuverCCSDSData*>::const_iterator i_ccsdsOPMManeuvers;
};

#endif	/* _CCSDSOPMOBTYPE_HPP */

