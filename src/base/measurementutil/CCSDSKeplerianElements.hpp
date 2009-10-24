/*
 * File:   CCSDSKEPLERIANELEMENTS.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSKEPLERIANELEMENTS_HPP
#define	_CCSDSKEPLERIANELEMENTS_HPP

#include "Anomaly.hpp"

class CCSDSKeplerianElements : public CCSDSObType
{

public:

    CCSDSKeplerianElements();
    CCSDSKeplerianElements(const CCSDSKeplerianElements &ke);
    const CCSDSKeplerianElements& CCSDSKeplerianElements::operator=
                                       (const CCSDSKeplerianElements &ke);
    ~CCSDSKeplerianElements();

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSKeplerianElements *myCCSDSKeplerianElements);
    enum CCSDS_DATA_REPS
    {
	CCSDS_OPM_KEPLERIANELEMENTS_EPOCH_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_SEMIMAJORAXIS_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_ECCENTRICITY_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_INCLINATION_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_RAAN_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_ARGUMENTOFPERICENTER_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_TRUEANOMALY_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_MEANANOMALY_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_GRAVITATIONALCOEFFICIENT_ID,
	CCSDS_OPM_KEPLERIANELEMENTS_COMMENTS_ID,
        EndCCSDSKeplerianElementsDataReps
    };

protected:

    static const std::string CCSDS_KEPLERIANELEMENTS_KEYWORDS[EndCCSDSKeplerianElementsDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSKeplerianElementsDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSKeplerianElementsDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSKeplerianElementsDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSKeplerianElementsDataReps];

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

