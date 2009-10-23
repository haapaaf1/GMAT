/*
 * File:   CCSDSSpacecraftInertia.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSSPACECRAFTINERTIA_HPP
#define	_CCSDSSPACECRAFTINERTIA_HPP

class CCSDSSpacecraftInertia
{

public:

    CCSDSSpacecraftInertia();
    CCSDSSpacecraftInertia(const CCSDSSpacecraftInertia &si);
    const CCSDSSpacecraftInertia& CCSDSSpacecraftInertia::operator=(const CCSDSSpacecraftInertia &si);
    ~CCSDSSpacecraftInertia();

    GmatBase *Clone() const;

    friend std::ostream& operator<< (std::ostream &output,
                     const CCSDSSpacecraftInertia *myCCSDSspacecraftInertia);

    enum CCSDS_DATA_REPS
    {

	CCSDS_APM_SPACECRAFTINERTIA_INERTIAREFFRAME_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I11_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I22_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I33_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I12_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I13_ID,
	CCSDS_APM_SPACECRAFTINERTIA_I23_ID,
	CCSDS_APM_SPACECRAFTINERTIA_COMMENTS_ID,
        EndCCSDSSpacecraftInertiaDataReps
    };

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

