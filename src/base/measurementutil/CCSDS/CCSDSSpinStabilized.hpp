//$Header$
//------------------------------------------------------------------------------
//                             CCSDSSpinStabilized
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
 * This class specifies the base class for the Spin Stabilized data construct
 *  that is used by the CCSDS Attitude Parameter and Ephemeris Message formats.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSSPINSTABILIZED_HPP
#define	_CCSDSSPINSTABILIZED_HPP

#include "CCSDSData.hpp"

class CCSDSSpinStabilized : public CCSDSData
{

public:

    CCSDSSpinStabilized();
    CCSDSSpinStabilized(const CCSDSSpinStabilized &ss);
    const CCSDSSpinStabilized& CCSDSSpinStabilized::operator=(const CCSDSSpinStabilized &ss);
    virtual ~CCSDSSpinStabilized();

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Integer	GetIntegerDataParameter(const Integer id) const;
    Integer	GetIntegerDataParameter(const std::string &label) const;
    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    std::string GetStringDataParameter(const Integer id) const;
    std::string GetStringDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_DATA_REPS
    {
	CCSDS_SPINSTABILIZED_TIMETAG_ID,
	CCSDS_SPINSTABILIZED_FRAMEA_ID,
	CCSDS_SPINSTABILIZED_FRAMEB_ID,
	CCSDS_SPINSTABILIZED_DIRECTION_ID,
	CCSDS_SPINSTABILIZED_SPINALPHA_ID,
	CCSDS_SPINSTABILIZED_SPINDELTA_ID,
	CCSDS_SPINSTABILIZED_SPINANGLE_ID,
	CCSDS_SPINSTABILIZED_SPINANGLEVEOCITY_ID,
	CCSDS_SPINSTABILIZED_NUTATION_ID,
	CCSDS_SPINSTABILIZED_NUTATIONPERIOD_ID,
	CCSDS_SPINSTABILIZED_NUTATIONPHASE_ID,
	CCSDS_SPINSTABILIZED_COMMENTS_ID,
        EndCCSDSSpinStabilizedDataReps
    };
    
    friend class ProcessCCSDSAPMDataFile;

protected:

    static const std::string CCSDS_SPINSTABILIZED_KEYWORDS[EndCCSDSSpinStabilizedDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSSpinStabilizedDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSSpinStabilizedDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSSpinStabilizedDataReps];

    Integer attitudeType;
    std::string timeTag;
    std::string frameA;
    std::string frameB;
    Integer direction;
    Real spinAlpha;
    Real spinDelta;
    Real spinAngle;
    Real spinAngleVelocity;
    Real nutation;
    Real nutationPeriod;
    Real nutationPhase;
    StringArray comments;
};

#endif	/* _CCSDSSPINSTABILIZED_HPP */

