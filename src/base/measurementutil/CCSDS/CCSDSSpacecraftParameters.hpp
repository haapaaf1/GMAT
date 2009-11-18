/*
 * File:   CCSDSSPACECRAFTPARAMETERS.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSSPACECRAFTPARAMETERS_HPP
#define	_CCSDSSPACECRAFTPARAMETERS_HPP

#include "CCSDSData.hpp"

class CCSDSSpacecraftParameters : public CCSDSData
{

public:

    CCSDSSpacecraftParameters();
    CCSDSSpacecraftParameters(const CCSDSSpacecraftParameters &sp);
    const CCSDSSpacecraftParameters& CCSDSSpacecraftParameters::operator=
                                    (const CCSDSSpacecraftParameters &sp);
    virtual ~CCSDSSpacecraftParameters();

    friend std::ostream& operator<< (std::ostream &output,
               const CCSDSSpacecraftParameters *myCCSDSspacecraftParameters);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

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
        EndCCSDSSpacecraftParametersDataReps
    };

    friend class ProcessCCSDSOPMDataFile;

protected:

    static const std::string CCSDS_SPACECRAFTPARAMETERS_KEYWORDS[EndCCSDSSpacecraftParametersDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSSpacecraftParametersDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSSpacecraftParametersDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSSpacecraftParametersDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSSpacecraftParametersDataReps];

    Real mass;
    Real solarRadiationArea;
    Real solarRadiationCoefficient;
    Real dragArea;
    Real dragCoefficient;
    StringArray comments;
};

#endif	/* _CCSDSSPACECRAFTPARAMETERS_HPP */