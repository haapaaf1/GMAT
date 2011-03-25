//$Header$
//------------------------------------------------------------------------------
//                             CCSDSObType
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
 * This class specifies the CCSDS base observation data type from which the
 * various data format observation types flow.
 *
 */
//------------------------------------------------------------------------------

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

    void SetHeader(CCSDSHeader *myCCSDSHeader);
    CCSDSHeader* GetHeader();

    virtual bool Validate() const = 0;
    
    enum CCSDS_TIMESYSTEM_REPS
    {
	EndCCSDSTimeReps
    };
    
    friend class CCSDSDataFile;
    friend class TDMCCSDSDataFile;
    friend class OPMCCSDSDataFile;
    friend class OEMCCSDSDataFile;
    friend class APMCCSDSDataFile;
    friend class AEMCCSDSDataFile;
    
protected:

    static const std::string CCSDS_TIMESYSTEM_DESCRIPTIONS[EndCCSDSTimeReps];

    // Pointer to the header record associated with this data point
    CCSDSHeader *ccsdsHeader;
    
};

#endif	/* _CCSDSOBTYPE_HPP */
