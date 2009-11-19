/* 
 * File:   CCSDSFormatDescription.hpp
 * Author: matthewwilkins
 *
 * Created on September 3, 2009, 4:58 AM
 */

#ifndef _CCSDSTDMOBTYPE_HPP
#define	_CCSDSTDMOBTYPE_HPP

#include "CCSDSObType.hpp"
#include "CCSDSTDMMetaData.hpp"
#include "CCSDSTrackingData.hpp"

class CCSDSTDMObType : public CCSDSObType
{   
    
public :
    
    CCSDSTDMObType();
    CCSDSTDMObType(const CCSDSTDMObType &tdm);
    const CCSDSTDMObType& CCSDSTDMObType::operator=(const CCSDSTDMObType &tdm);
    ~CCSDSTDMObType();

    GmatBase *Clone() const;

    bool Validate() const;

    friend std::ostream& operator<< (std::ostream &output, 
                                     const CCSDSTDMObType *myTDM);
    	
    const std::string* GetTimeSystems() const;
    std::string GetTimeSystemText(const Integer &id) const;
    Integer GetTimeSystemID(const std::string &label);
        
    //bool IsParameterRequired(const Integer id) const;

     enum CCSDS_TIMESYSTEM_REPS
    {
	UTC_ID = EndCCSDSTimeReps,
        TAI_ID,
        GPS_ID,
	SCLK_ID,
	EndCCSDSTDMTimeReps
    };
     	        
    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
    
protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTDMTimeReps - EndCCSDSTimeReps];
    
    // Pointer to the metadata record associated with this data point
    CCSDSTDMMetaData *ccsdsMetaData;

    // Pointers to the generic key epoch value data format
    CCSDSTrackingData *ccsdsTrackingData;

    bool commentsCurrentlyAllowed;

};
#endif	/* _CCSDSTDMOBTYPE_HPP */

