/* 
 * File:   CCSDSStateVector.hpp
 * Author: mwilkins
 *
 * Created on October 22, 2009, 10:01 AM
 */

#ifndef _CCSDSSTATEVECTOR_HPP
#define	_CCSDSSTATEVECTOR_HPP

class CCSDSStateVector
{

public:

    CCSDSStateVector();
    CCSDSStateVector(const CCSDSStateVector &sv);
    const CCSDSStateVector& CCSDSStateVector::operator=(const CCSDSStateVector &sv);
    ~CCSDSStateVector();

    enum CCSDS_DATA_REPS
    {
	CCSDS_OPM_STATEVECTOR_EPOCH_ID,
	CCSDS_OPM_STATEVECTOR_X_ID,
	CCSDS_OPM_STATEVECTOR_Y_ID,
	CCSDS_OPM_STATEVECTOR_Z_ID,
	CCSDS_OPM_STATEVECTOR_XDOT_ID,
        CCSDS_OPM_STATEVECTOR_YDOT_ID,
	CCSDS_OPM_STATEVECTOR_ZDOT_ID,
	CCSDS_OPM_STATEVECTOR_COMMENTS_ID,
        EndCCSDSStateVectorDataReps
    }

protected:

    static const std::string CCSDS_STATEVECTOR_KEYWORDS[EndCCSDSStateVectorDataReps];
    static const std::string CCSDS_UNIT_DESCRIPTIONS[EndCCSDSStateVectorDataReps];
    static const bool CCSDS_IS_REQUIRED[EndCCSDSStateVectorDataReps];
    static const Gmat::ParameterType CCSDS_PARAMETER_TYPE[EndCCSDSStateVectorDataReps];
    static const std::string CCSDS_FILEFORMAT_DESCRIPTIONS[EndCCSDSStateVectorDataReps];

    std::string epoch;
    Real x, y, z;
    Real xDot, yDot, zDot;
    StringArray comments;

};

#endif	/* _CCSDSSTATEVECTOR_HPP */

