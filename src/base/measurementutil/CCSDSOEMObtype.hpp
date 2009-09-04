/* 
 * File:   CCSDSOEMObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:22 AM
 */

#ifndef _CCSDSOEMOBTYPE_HPP
#define	_CCSDSOEMOBTYPE_HPP

#include "CCSDSObtype.hpp"

namespace DataFormats
{
    struct ccsds_oem_metadata
    {
	std::string objectName;
	std::string objectID;
	std::string refFrameOrigin;
	std::string refFrame;
	std::string timeSystem;
	std::string startTime;
	std::string stopTime;
	std::string useableStartTime;
	std::string useableStopTime;
	std::string interpolationMethod;
	Integer interpolationDegree;
	StringArray comments;
    };
    
    class CCSDSOEMObtype : public Obtype
    {
        public:

	bool IsParameterRequired(const Integer id) const;
		
	// Iterator Pointer to the header record
        std::vector<ccsds_header*>::iterator headerVectorIndex;
	// Iterator Pointer to the metadata record
        std::vector<ccsds_oem_metadata*>::iterator metadataVectorIndex;	
        std::vector<ccsds_stateVector*> stateVector;
    };   

}


#endif	/* _CCSDSOEMOBTYPE_HPP */

