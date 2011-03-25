//$Header$
//------------------------------------------------------------------------------
//                             OEMCCSDSObType
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
 * This class specifies the CCSDS Orbit Ephemeris observation data type.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSOEMOBTYPE_HPP
#define	_CCSDSOEMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "OEMCCSDSMetaData.hpp"
#include "OEMStateVectorCCSDSData.hpp"
   
class OEMCCSDSObType : public CCSDSObType
{

public:

    OEMCCSDSObType();
    OEMCCSDSObType(const OEMCCSDSObType &oem);
    const OEMCCSDSObType& operator=(const OEMCCSDSObType &oem);
    ~OEMCCSDSObType();

    GmatBase *Clone() const;

    bool Validate() const;
    
    friend std::ostream& operator<< (std::ostream &output,
                                     const OEMCCSDSObType *myOEM);
    
    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    void SetMetaData(OEMCCSDSMetaData *myCCSDSMetaData);
    OEMCCSDSMetaData* GetMetaData();

    void SetStateVector(OEMStateVectorCCSDSData *myOEMStateVector);
    OEMStateVectorCCSDSData* GetStateVector();
        
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

    friend class CCSDSDataFile;
    friend class OEMCCSDSDataFile;

protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps - EndCCSDSTimeReps];
   
    // Pointer to the data records
    OEMCCSDSMetaData* ccsdsMetaData;
    OEMStateVectorCCSDSData *ccsdsOEMStateVector;

private:
    
    bool commentsCurrentlyAllowed;

};   

#endif	/* _CCSDSOEMOBTYPE_HPP */

