/*
 * File:   CCSDSSpacecraftInertia.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSSPACECRAFTINERTIA_HPP
#define	_CCSDSSPACECRAFTINERTIA_HPP

#include "CCSDSData.hpp"

class CCSDSSpacecraftInertia : public CCSDSData
{

public:

    CCSDSSpacecraftInertia();
    CCSDSSpacecraftInertia(const CCSDSSpacecraftInertia &si);
    const CCSDSSpacecraftInertia& CCSDSSpacecraftInertia::operator=(const CCSDSSpacecraftInertia &si);
    virtual ~CCSDSSpacecraftInertia();

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
    friend Integer CountRequiredNumberSpacecraftInertiaParameters();
    bool Validate() const;

    const std::string* GetKeywords() const;
    const Integer GetKeywordID(const std::string str) const;
    std::string GetUnits(const Integer &id) const;

    friend std::ostream& operator<< (std::ostream &output,
                     const CCSDSSpacecraftInertia *myCCSDSspacecraftInertia);

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
        EndCCSDSSpacecraftInertiaDataReps
    };

    friend class ProcessCCSDSAPMDataFile;

protected:

    static const std::string CCSDS_SPACECRAFTINERTIA_KEYWORDS[EndCCSDSSpacecraftInertiaDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSSpacecraftInertiaDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSSpacecraftInertiaDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSSpacecraftInertiaDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSSpacecraftInertiaDataReps];

    std::string inertiaRefFrame;
    Real i11, i22, i33, i12, i13, i23;
    StringArray comments;
};

#endif	/* _CCSDSSPACECRAFTINERTIA_HPP */

