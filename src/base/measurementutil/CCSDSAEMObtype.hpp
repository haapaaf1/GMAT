/* 
 * File:   CCSDSAEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:23 AM
 */

#ifndef _CCSDSAEMOBTYPE_HPP
#define	_CCSDSAEMOBTYPE_HPP

#include "CCSDSObtype.hpp"

namespace DataFormats
{
    
    struct ccsds_aem_metadata
    {
	std::string objectName;
	std::string objectID;
	std::string refFrameOrigin;
	std::string frameA;
	std::string frameB;
	std::string direction;
	std::string timeSystem;
	std::string startTime;
	std::string stopTime;
	std::string useableStartTime;
	std::string useableStopTime;
	std::string attitudeType;
	std::string quaternionType;
	std::string eulerRotationSequence;
	std::string rateFrame;
	std::string interpolationMethod;
	Integer interpolationDegree;
	StringArray comments;
    };

    class CCSDSAEMObtype : public Obtype
    {
    
        public :
	
	bool IsParameterRequired(const Integer id) const;	
	
	// Iterator Pointer to the header record
        std::vector<ccsds_header*>::iterator headerVectorIndex;
	// Iterator Pointer to the metadata record
        std::vector<ccsds_aem_metadata*>::iterator metadataVectorIndex;
	std::vector<ccsds_quaternion*> quaternions;
	std::vector<ccsds_eulerAngle*> eulerAngles;
	std::vector<ccsds_spinStabilized*> spinStabilized;
    };
 
}

#endif	/* _CCSDSAEMOBTYPE_HPP */

