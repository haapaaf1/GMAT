/* 
 * File:   CCSDSObType.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:29 AM
 */

#ifndef _CCSDSOBTYPE_HPP
#define	_CCSDSOBTYPE_HPP

#include "ObType.hpp"
#include "CCSDSHeader.hpp"

class CCSDSObType : public ObType
{
    
public :
    
    CCSDSObType(const std::string &type, const std::string &name);
    CCSDSObType(const CCSDSObType &ob);
    const CCSDSObType& operator=(const CCSDSObType &ob);
    virtual ~CCSDSObType();

    // Validation methods
    std::string GetCCSDSObType();
    
    virtual const std::string* GetTimeSystems() const;
    virtual std::string GetTimeSystemText(const Integer &id) const;
    virtual Integer GetTimeSystemID(const std::string &label);

    virtual bool Validate() const = 0;
    
    enum CCSDS_TIMESYSTEM_REPS
    {
	EndCCSDSTimeReps
    };
    
    friend class ProcessCCSDSDataFile;
    friend class ProcessCCSDSTDMDataFile;
    friend class ProcessCCSDSOPMDataFile;
    friend class ProcessCCSDSOEMDataFile;
    friend class ProcessCCSDSAPMDataFile;
    friend class ProcessCCSDSAEMDataFile;
    
protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

    // Pointer to the header record associated with this data point
    CCSDSHeader *ccsdsHeader;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
