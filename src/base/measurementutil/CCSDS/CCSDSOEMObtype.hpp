//$Header$
//------------------------------------------------------------------------------
//                             CCSDSOEMObType
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
#include "CCSDSOEMMetaData.hpp"
#include "CCSDSOEMStateVector.hpp"
   
class CCSDSOEMObType : public CCSDSObType
{

public:

    CCSDSOEMObType();
    CCSDSOEMObType(const CCSDSOEMObType &oem);
    const CCSDSOEMObType& CCSDSOEMObType::operator=(const CCSDSOEMObType &oem);
    ~CCSDSOEMObType();

    GmatBase *Clone() const;

    bool Validate() const;
    
    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSOEMObType *myOEM);
    
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
        GMST_ID,
        MET_ID,
        MRT_ID,
        SCLK_ID,
        UT1_ID,
	EndCCSDSOEMTimeReps
    };

    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSOEMDataFile;

protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSOEMTimeReps - EndCCSDSTimeReps];
   
    // Pointer to the data records
    CCSDSOEMMetaData* ccsdsMetaData;
    CCSDSOEMStateVector *ccsdsOEMStateVector;

    bool commentsCurrentlyAllowed;

};   

#endif	/* _CCSDSOEMOBTYPE_HPP */

