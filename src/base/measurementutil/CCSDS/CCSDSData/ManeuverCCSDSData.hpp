//$Header$
//------------------------------------------------------------------------------
//                             ManeuverCCSDSData
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

class ManeuverCCSDSData : public CCSDSData
{

public:

    ManeuverCCSDSData();
    ManeuverCCSDSData(const ManeuverCCSDSData &man);
    const ManeuverCCSDSData& ManeuverCCSDSData::operator=(const ManeuverCCSDSData &man);
    virtual ~ManeuverCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                                     const ManeuverCCSDSData *myManeuverCCSDSData);

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

    bool SetDataParameter(const Integer id, const Real &value);
    bool SetDataParameter(const std::string &label, const Real &value);
    bool SetDataParameter(const Integer id, const std::string &value);
    bool SetDataParameter(const std::string &label, const std::string &value);
    bool SetDataParameter(const Integer id, const StringArray &value);
    bool SetDataParameter(const std::string &label, const StringArray &value);

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
        EndManeuverCCSDSDataDataReps
    };

    friend class OPMCCSDSDataFile;
    
protected:

    static const std::string CCSDS_MANEUVER_KEYWORDS[EndManeuverCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndManeuverCCSDSDataDataReps];
    static const bool CCSDS_IS_REQUIRED[EndManeuverCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndManeuverCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndManeuverCCSDSDataDataReps];

    std::string ignitionEpoch;
    Real duration;
    Real deltaMass;
    std::string refFrame;
    Real deltaV1, deltaV2, deltaV3;
    StringArray comments;
};


#endif	/* _CCSDSMANEUVER_HPP */

