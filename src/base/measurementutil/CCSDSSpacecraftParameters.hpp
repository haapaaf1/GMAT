/*
 * File:   CCSDSSPACECRAFTPARAMETERS.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSSPACECRAFTPARAMETERS_HPP
#define	_CCSDSSPACECRAFTPARAMETERS_HPP

class CCSDSOPMSpacecraftParameters
{

public:

    CCSDSOPMSpacecraftParameters();
    CCSDSOPMSpacecraftParameters(const CCSDSOPMSpacecraftParameters &opmSP);
    const CCSDSOPMSpacecraftParameters& CCSDSOPMSpacecraftParameters::operator=
                                    (const CCSDSOPMSpacecraftParameters &opmSP);
    ~CCSDSOPMSpacecraftParameters();

    friend std::ostream& operator<< (std::ostream &output,
               const CCSDSOPMSpacecraftParameters *myCCSDSspacecraftParameters);

    enum CCSDS_DATA_REPS
    {
	CCSDS_SPACECRAFTPARAMETERS_MASS_ID,
	CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONAREA_ID,
	CCSDS_SPACECRAFTPARAMETERS_SOLARRADIATIONCOEFFICIENT_ID,
	CCSDS_SPACECRAFTPARAMETERS_DRAGAREA_ID,
	CCSDS_SPACECRAFTPARAMETERS_DRAGCOEFFICIENT_ID,
	CCSDS_SPACECRAFTPARAMETERS_COMMENTS_ID,
        EndCCSDSSpacecraftParametersDataReps
    };

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