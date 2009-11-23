//$Header$
//------------------------------------------------------------------------------
//                             StateVectorCCSDSData
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
 * This class specifies the State Vector construct that is used by the
 * CCSDS Orbit Parameter and Ephemeris message formats.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSSTATEVECTOR_HPP
#define	_CCSDSSTATEVECTOR_HPP

#include "CCSDSData.hpp"

class StateVectorCCSDSData : public CCSDSData
{

public:

    StateVectorCCSDSData();
    StateVectorCCSDSData(const StateVectorCCSDSData &sv);
    const StateVectorCCSDSData& StateVectorCCSDSData::operator=(const StateVectorCCSDSData &sv);
    virtual ~StateVectorCCSDSData();

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
    friend Integer CountRequiredNumberStateVectorParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_DATA_REPS
    {
	CCSDS_STATEVECTOR_TIMETAG_ID,
	CCSDS_STATEVECTOR_X_ID,
	CCSDS_STATEVECTOR_Y_ID,
	CCSDS_STATEVECTOR_Z_ID,
	CCSDS_STATEVECTOR_XDOT_ID,
        CCSDS_STATEVECTOR_YDOT_ID,
	CCSDS_STATEVECTOR_ZDOT_ID,
	CCSDS_STATEVECTOR_COMMENTS_ID,
        EndStateVectorCCSDSDataDataReps
    };

    friend class CCSDSDataFile;
    friend class OPMCCSDSDataFile;
    friend class OEMCCSDSDataFile;

protected:

    static const std::string CCSDS_STATEVECTOR_KEYWORDS[EndStateVectorCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndStateVectorCCSDSDataDataReps];
    static const bool CCSDS_IS_REQUIRED[EndStateVectorCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndStateVectorCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndStateVectorCCSDSDataDataReps];

    std::string timeTag;
    Real x, y, z;
    Real xDot, yDot, zDot;
    StringArray comments;

};

#endif	/* _CCSDSSTATEVECTOR_HPP */

