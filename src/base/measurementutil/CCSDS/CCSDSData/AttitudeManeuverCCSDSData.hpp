//$Header$
//------------------------------------------------------------------------------
//                             AttitudeManeuverCCSDSData
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
 * This class specifies the attitude maneuver data construct used in the
 * CCSDS Attitude Parameter message format.
 *
 */
//------------------------------------------------------------------------------


#ifndef _CCSDSATTITUDEMANEUVER_HPP
#define	_CCSDSATTITUDEMANEUVER_HPP

#include "CCSDSData.hpp"

class AttitudeManeuverCCSDSData : public CCSDSData
{

public:

    AttitudeManeuverCCSDSData();
    AttitudeManeuverCCSDSData(const AttitudeManeuverCCSDSData &am);
    const AttitudeManeuverCCSDSData& AttitudeManeuverCCSDSData::operator=(const AttitudeManeuverCCSDSData &am);
    virtual ~AttitudeManeuverCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                       const AttitudeManeuverCCSDSData *myCCSDSAttitudemaneuver);

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
    friend Integer CountRequiredNumberAttitudeManeuverParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_DATA_REPS
    {
        CCSDS_ATTITUDEMANUEVER_EPOCHSTART_ID,
        CCSDS_ATTITUDEMANUEVER_DURATION_ID,
        CCSDS_ATTITUDEMANUEVER_REFFRAME_ID,
        CCSDS_ATTITUDEMANUEVER_TOR1_ID,
        CCSDS_ATTITUDEMANUEVER_TOR2_ID,
        CCSDS_ATTITUDEMANUEVER_TOR3_ID,
        CCSDS_ATTITUDEMANUEVER_COMMENTS_ID,
        EndAttitudeManeuverCCSDSDataDataReps
    };

    friend class APMCCSDSDataFile;

protected:

    static const std::string CCSDS_ATTITUDEMANEUVER_KEYWORDS[EndAttitudeManeuverCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndAttitudeManeuverCCSDSDataDataReps];
    static const bool CCSDS_IS_REQUIRED[EndAttitudeManeuverCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndAttitudeManeuverCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndAttitudeManeuverCCSDSDataDataReps];

    std::string epochStart;
    Real duration;
    std::string refFrame;
    Real tor1, tor2, tor3;
    StringArray comments;
    
};

#endif	/* _CCSDSATTITUDEMANEUVER_HPP */

