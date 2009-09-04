/* 
 * File:   CCSDSOPMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:20 AM
 */

#ifndef _CCSDSOPMOBTYPE_HPP
#define	_CCSDSOPMOBTYPE_HPP

#include "CCSDSObtype.hpp"

namespace DataFormats
{
        
    struct ccsds_opm_metadata
    {
	std::string objectName;
	std::string objectID;
	std::string refFrameOrigin;
	std::string refFrame;
	std::string timeSystem;
	StringArray comments;
    };
    
    struct ccsds_stateVector
    {
	std::string epoch;
	Real X, Y, Z;
	Real xDot, yDot, zDot;
    };
    
    struct ccsds_keplerianElements
    {
	Real semiMajorAxis;
	Real eccentricity;
	Real inclination;
	Real raan;
	Real argumentOfPericenter;
	Real trueAnomaly;
	Real meanAnomaly;
	Real gravitationalCoefficient;
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
    
    struct ccsds_spacecraftParameters
    {
	StringArray comments;
	Real mass;
	Real solarRadiationArea;
	Real solarRadiationCoefficient;
	Real dragArea;
	Real dragCoefficient;
	std::string intertiaRefFrame;
	Real i11, i22, i33, i12, i13, i23;
    };

    class CCSDSOPMObtype : public Obtype
    {
    
    public :
	
	bool IsParameterRequired(const Integer id) const;
	
	// Iterator Pointer to the header record
        std::vector<ccsds_header*>::iterator headerVectorIndex;
	// Iterator Pointer to the metadata record
        std::vector<ccsds_opm_metadata*>::iterator metadataVectorIndex;
	std::vector<ccsds_stateVector*> stateVector;
	std::vector<ccsds_keplerianElements*> keplerianElements;
	std::vector<ccsds_spacecraftParameters*> spacecraftParameters;
	std::vector<ccsds_opm_maneuverParameters*> maneuverParameters;
	StringArray comments;
    };

    
}


#endif	/* _CCSDSOPMOBTYPE_HPP */

