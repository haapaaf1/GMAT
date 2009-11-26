//$Header$
//------------------------------------------------------------------------------
//                             KeplerianElementsCCSDSData
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
 * This class specifies the Kepelerian Element data construct that is used
 * by the CCSDS Orbit Parameter Message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef _CCSDSKEPLERIANELEMENTS_HPP
#define	_CCSDSKEPLERIANELEMENTS_HPP

#include "CCSDSData.hpp"
#include "Anomaly.hpp"

class KeplerianElementsCCSDSData : public CCSDSData
{

public:

    KeplerianElementsCCSDSData();
    KeplerianElementsCCSDSData(const KeplerianElementsCCSDSData &ke);
    const KeplerianElementsCCSDSData& KeplerianElementsCCSDSData::operator=
                                       (const KeplerianElementsCCSDSData &ke);
    virtual ~KeplerianElementsCCSDSData();

    friend std::ostream& operator<< (std::ostream &output,
                        const KeplerianElementsCCSDSData *myKeplerianElementsCCSDSData);

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
    friend Integer CountRequiredNumberKeplerianElementsParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    enum CCSDS_DATA_REPS
    {
	CCSDS_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID = 0,
	CCSDS_KEPLERIANELEMENTS_ECCENTRICITY_ID,
	CCSDS_KEPLERIANELEMENTS_INCLINATION_ID,
	CCSDS_KEPLERIANELEMENTS_RAAN_ID,
	CCSDS_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID,
	CCSDS_KEPLERIANELEMENTS_TRUEANOMALY_ID,
	CCSDS_KEPLERIANELEMENTS_MEANANOMALY_ID,
	CCSDS_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID,
	CCSDS_KEPLERIANELEMENTS_COMMENTS_ID,
        EndKeplerianElementsCCSDSDataDataReps
    };

    friend class OPMCCSDSDataFile;

protected:

    static const std::string CCSDS_KEPLERIANELEMENTS_KEYWORDS[EndKeplerianElementsCCSDSDataDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndKeplerianElementsCCSDSDataDataReps];
    static const bool CCSDS_IS_REQUIRED[EndKeplerianElementsCCSDSDataDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndKeplerianElementsCCSDSDataDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndKeplerianElementsCCSDSDataDataReps];

    Real semiMajorAxis;
    Real eccentricity;
    Real inclination;
    Real raan;
    Real argumentOfPericenter;
    Anomaly theAnomaly;
    Real gravitationalCoefficient;
    StringArray comments;
};

#endif	/* _CCSDSKEPLERIANELEMENTS_HPP */

