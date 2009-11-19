//$Header$
//------------------------------------------------------------------------------
//                             CCSDSManeuver
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the maneuver data construct used in the
 * CCSDS Orbit Parameter message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSMANEUVER_HPP
#define	_CCSDSMANEUVER_HPP

#include "CCSDSData.hpp"

class CCSDSManeuver : public CCSDSData
{

public:

    CCSDSManeuver();
    CCSDSManeuver(const CCSDSManeuver &man);
    const CCSDSManeuver& CCSDSManeuver::operator=(const CCSDSManeuver &man);
    virtual ~CCSDSManeuver();

    friend std::ostream& operator<< (std::ostream &output,
                                     const CCSDSManeuver *myCCSDSManeuver);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberManeuverParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_DATA_REPS
    {
        CCSDS_MANUEVER_IGNITIONEPOCH_ID,
        CCSDS_MANUEVER_DURATION_ID,
        CCSDS_MANUEVER_DELTAMASS_ID,
        CCSDS_MANUEVER_REFFRAME_ID,
        CCSDS_MANUEVER_DELTAV1_ID,
        CCSDS_MANUEVER_DELTAV2_ID,
        CCSDS_MANUEVER_DELTAV3_ID,
        CCSDS_MANUEVER_COMMENTS_ID,
        EndCCSDSManeuverDataReps
    };

    friend class ProcessCCSDSOPMDataFile;
    
protected:

    static const std::string CCSDS_MANEUVER_KEYWORDS[EndCCSDSManeuverDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSManeuverDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSManeuverDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSManeuverDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSManeuverDataReps];

    std::string ignitionEpoch;
    Real duration;
    Real deltaMass;
    std::string refFrame;
    Real deltaV1, deltaV2, deltaV3;
    StringArray comments;
};


#endif	/* _CCSDSMANEUVER_HPP */

