//$Header$
//------------------------------------------------------------------------------
//                             SpacecraftParametersCCSDSData
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
 * This class specifies the Spacecraft Parameter data construct that is used
 * by the CCSDS Orbit Parameter Message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSSPACECRAFTPARAMETERS_HPP
#define	_CCSDSSPACECRAFTPARAMETERS_HPP

#include "CCSDSData.hpp"

class SpacecraftParametersCCSDSData : public CCSDSData
{

public:

    SpacecraftParametersCCSDSData();
    SpacecraftParametersCCSDSData(const SpacecraftParametersCCSDSData &sp);
    const SpacecraftParametersCCSDSData& SpacecraftParametersCCSDSData::operator=
                                    (const SpacecraftParametersCCSDSData &sp);
    virtual ~SpacecraftParametersCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
               const SpacecraftParametersCCSDSData *myCCSDSspacecraftParameters);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    bool SetDataParameter(const Integer id, const Real &value);
    bool SetDataParameter(const std::string &label, const Real &value);
    bool SetDataParameter(const Integer id, const StringArray &value);
    bool SetDataParameter(const std::string &label, const StringArray &value);

    // Functions to verify data availability
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberSpacecraftParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;
    
    enum CCSDS_DATA_REPS
    {
	CCSDS_SPACECRAFTPARAMETERS_MASS_ID = 0,
	CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID,
	CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID,
	CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID,
	CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID,
	CCSDS_SPACECRAFTPARAMETERS_COMMENTS_ID,
        EndSpacecraftParametersCCSDSDataDataReps
    };

    friend class OPMCCSDSDataFile;

protected:

    static const std::string CCSDS_SPACECRAFTPARAMETERS_KEYWORDS[EndSpacecraftParametersCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndSpacecraftParametersCCSDSDataDataReps];
    static const bool CCSDS_IS_REQUIRED[EndSpacecraftParametersCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndSpacecraftParametersCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndSpacecraftParametersCCSDSDataDataReps];

    Real mass;
    Real solarRadiationArea;
    Real solarRadiationCoefficient;
    Real dragArea;
    Real dragCoefficient;
    StringArray comments;
};

#endif	/* _CCSDSSPACECRAFTPARAMETERS_HPP */