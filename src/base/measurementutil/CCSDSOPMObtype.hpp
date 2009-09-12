/* 
 * File:   CCSDSOPMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:20 AM
 */

#ifndef _CCSDSOPMOBTYPE_HPP
#define	_CCSDSOPMOBTYPE_HPP

#include "CCSDSObtype.hpp"
        
struct ccsds_opm_metadata
{
	std::string objectName;
	std::string objectID;
	std::string refFrameOrigin;
	std::string refFrame;
	std::string timeSystem;
	StringArray comments;
};
    
struct ccsds_opm_maneuverParameters
{
    std::string ignitionEpoch;
    Real duration;
    Real deltaMass;
    Real refFrame;
    Real deltaV1, deltaV2, deltaV3;
    StringArray comments;    
};
    
class CCSDSOPMObtype : public Obtype
{
    
    public :
	
	bool IsParameterRequired(const Integer id) const;

	    enum OPM_TYPE_REPS
    {
	GPSONLY_ID,
        MIXED_ID,
        GLONASSONLY_ID,
	LEOONLY_ID,
	GALILEOONLY_ID,
	EndOPMTypeReps
    };

    enum OPM_TIME_REPS
    {
	GPSTIME_ID,
        GLONASSUTC_ID,
        GALILEOSYSTEMTIME_ID,
	TAI_ID,
	UTC_ID,
	EndOPMTimeReps
    };

private:

    static const std::string OPM_TYPE_DESCRIPTIONS[EndOPMTypeReps];
    static const std::string OPM_TIME_DESCRIPTIONS[EndOPMTimeReps];

    // Iterator Pointer to the header record
    std::vector<ccsds_header*>::iterator headerVectorIndex;

    // Iterator Pointer to the metadata record
    std::vector<ccsds_opm_metadata*>::iterator metadataVectorIndex;
    std::vector<ccsds_stateVector*> stateVector;
    std::vector<ccsds_keplerianElements*> keplerianElements;
    std::vector<ccsds_spacecraftParameters*> spacecraftParameters;
    std::vector<ccsds_opm_maneuverParameters*> maneuverParameters;

    StringArray dataComments;
        
};

    


#endif	/* _CCSDSOPMOBTYPE_HPP */

