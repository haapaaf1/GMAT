//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAEMObType
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

    bool Validate() const;

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

