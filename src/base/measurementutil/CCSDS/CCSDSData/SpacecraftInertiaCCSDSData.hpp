//$Header$
//------------------------------------------------------------------------------
//                             SpacecraftInertiaCCSDSData
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
 * This class specifies the Spacecraft Inertia data construct that is used
 * by the CCSDS Attitude Parameter Message format.
 *
 */
//------------------------------------------------------------------------------


#ifndef _CCSDSSPACECRAFTINERTIA_HPP
#define	_CCSDSSPACECRAFTINERTIA_HPP

#include "CCSDSData.hpp"

class SpacecraftInertiaCCSDSData : public CCSDSData
{

public:

    SpacecraftInertiaCCSDSData();
    SpacecraftInertiaCCSDSData(const SpacecraftInertiaCCSDSData &si);
    const SpacecraftInertiaCCSDSData& SpacecraftInertiaCCSDSData::operator=(const SpacecraftInertiaCCSDSData &si);
    virtual ~SpacecraftInertiaCCSDSData();

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
    friend Integer CountRequiredNumberSpacecraftInertiaParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    friend std::ostream& operator<< (std::ostream &output,
                     const SpacecraftInertiaCCSDSData *myCCSDSspacecraftInertia);

    enum CCSDS_DATA_REPS
    {
	CCSDS_SPACECRAFTINERTIA_INERTIAREFFRAME_ID,
	CCSDS_SPACECRAFTINERTIA_I11_ID,
	CCSDS_SPACECRAFTINERTIA_I22_ID,
	CCSDS_SPACECRAFTINERTIA_I33_ID,
	CCSDS_SPACECRAFTINERTIA_I12_ID,
	CCSDS_SPACECRAFTINERTIA_I13_ID,
	CCSDS_SPACECRAFTINERTIA_I23_ID,
	CCSDS_SPACECRAFTINERTIA_COMMENTS_ID,
        EndSpacecraftInertiaCCSDSDataDataReps
    };

    friend class APMCCSDSDataFile;

protected:

    static const std::string CCSDS_SPACECRAFTINERTIA_KEYWORDS[EndSpacecraftInertiaCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndSpacecraftInertiaCCSDSDataDataReps];
    static const bool CCSDS_IS_REQUIRED[EndSpacecraftInertiaCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndSpacecraftInertiaCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndSpacecraftInertiaCCSDSDataDataReps];

    std::string inertiaRefFrame;
    Real i11, i22, i33, i12, i13, i23;
    StringArray comments;
};

#endif	/* _CCSDSSPACECRAFTINERTIA_HPP */

