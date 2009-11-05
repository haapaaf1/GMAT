/*
 * File:   CCSDSKEPLERIANELEMENTS.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSKEPLERIANELEMENTS_HPP
#define	_CCSDSKEPLERIANELEMENTS_HPP

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <pcrecpp.h>
#include "Anomaly.hpp"

class CCSDSKeplerianElements
{

public:

    CCSDSKeplerianElements();
    CCSDSKeplerianElements(const CCSDSKeplerianElements &ke);
    const CCSDSKeplerianElements& CCSDSKeplerianElements::operator=
                                       (const CCSDSKeplerianElements &ke);
    ~CCSDSKeplerianElements();

    friend std::ostream& operator<< (std::ostream &output,
                        const CCSDSKeplerianElements *myCCSDSKeplerianElements);

    std::string GetDataParameterText(const Integer id) const;
    Integer    GetDataParameterID(const std::string &str) const;
    Gmat::ParameterType GetDataParameterType(const Integer id) const;
    std::string GetDataParameterTypeString(const Integer id) const;

    Real	GetRealDataParameter(const Integer id) const;
    Real	GetRealDataParameter(const std::string &label) const;
    StringArray GetStringArrayDataParameter(const Integer id) const;
    StringArray GetStringArrayDataParameter(const std::string &label) const;

    // Functions to verify data availability
    bool CheckDataAvailability(const std::string str) const;
    bool IsParameterRequired(const Integer id) const;
    friend Integer CountRequiredNumberKeplerianElementsParameters();

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
        EndCCSDSKeplerianElementsDataReps
    };

    friend class ProcessCCSDSOPMDataFile;

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

