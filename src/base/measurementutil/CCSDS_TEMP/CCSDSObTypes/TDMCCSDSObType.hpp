//$Header$
//------------------------------------------------------------------------------
//                             TDMCCSDSObType
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
 * This class specifies the CCSDS Tracking Data observation data type.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSTDMOBTYPE_HPP
#define	_CCSDSTDMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "TDMCCSDSMetaData.hpp"
#include "TrackingCCSDSData.hpp"

class TDMCCSDSObType : public CCSDSObType
{   
    
public :
    
    TDMCCSDSObType();
    TDMCCSDSObType(const TDMCCSDSObType &tdm);
    const TDMCCSDSObType& TDMCCSDSObType::operator=(const TDMCCSDSObType &tdm);
    ~TDMCCSDSObType();

    GmatBase *Clone() const;

    bool Validate() const;

    friend std::ostream& operator<< (std::ostream &output, 
                                     const TDMCCSDSObType *myTDM);
    	
    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);

    void SetMetaData(TDMCCSDSMetaData *myCCSDSMetaData);
    TDMCCSDSMetaData* GetMetaData();

    void SetTrackingData(TrackingCCSDSData *myTrackingCCSDSData);
    TrackingCCSDSData* GetTrackingData();

    //bool IsParameterRequired(const Integer id) const;

     enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        GPS_ID,
	SCLK_ID,
	EndCCSDSTDMTimeReps
    };
     	        
    friend class CCSDSDataFile;
    friend class TDMCCSDSDataFile;
    
protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps - EndCCSDSTimeReps];
    
    // Pointer to the metadata record associated with this data point
    TDMCCSDSMetaData *ccsdsMetaData;

    // Pointers to the generic key epoch value data format
    TrackingCCSDSData *ccsdsTrackingData;

private:
    
    bool commentsCurrentlyAllowed;

};
#endif	/* _CCSDSTDMOBTYPE_HPP */

