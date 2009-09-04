/* 
 * File:   CCSDSAPMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSAPMOBTYPE_HPP
#define	_CCSDSAPMOBTYPE_HPP

#include "CCSDSObtype.hpp"

namespace DataFormats
{
      
    struct ccsds_apm_metadata
    {
	std::string objectName;
	std::string objectID;
	std::string refFrameOrigin;
	std::string timeSystem;	
    };
    
    struct ccsds_apm_maneuverParameters
    {
	StringArray comments;
	std::string epochStart;
	Real duration;
	std::string refFrame;
	Real torqueVector1, torqueVector2, torqueVector3;
    };
    
    class CCSDSAPMObtype : public Obtype
    {
    
	public :
	
	bool IsParameterRequired(const Integer id) const;	
	
	// Iterator Pointer to the header record
        std::vector<ccsds_header*>::iterator headerVectorIndex;
	// Iterator Pointer to the metadata record
        std::vector<ccsds_apm_metadata*>::iterator metadataVectorIndex;
	std::vector<ccsds_quaternion*> quaternions;
	std::vector<ccsds_eulerAngle*> eulerAngles;
	std::vector<ccsds_spinStabilized*> spinStabilized;
	std::vector<ccsds_spacecraftParameters*> spacecraftParameters;
	std::vector<ccsds_adm_maneuverParameters*> maneuverParameters;
    };
    
}

#endif	/* _CCSDSAPMOBTYPE_HPP */

