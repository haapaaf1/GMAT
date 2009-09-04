/* 
 * File:   CCSDSObtype.hpp
 * Author: matthewwilkins
 *
 * Created on September 4, 2009, 5:29 AM
 */

#ifndef _CCSDSOBTYPE_HPP
#define	_CCSDSOBTYPE_HPP

namespace DataFormats
{

    // The CCSDS header specification that is common to all CCSDS formats
    struct ccsds_header
    {
	Real ccsdsVersion;
	std::string creationDate;
	std::string originator;
	std::vector<std::string*> headerComments;
    };
    
    // The CCSDS quaternion specification. Some formats do not use
    // all the parameters such as the rates.
    struct ccsds_quaternion
    {
	std::string quarternionType;
	std::string frameA;
	std::string frameB;
	std::string direction;
	Real q1, q2, q3, qC;
	Real q1Dot, q2Dot, q3Dot, qcDot;
	Real xRate, yRate, zRate;
    };
    
    // The CCSDS Euler angle specification. Some formats do not use
    // all the parameters such as the rates.
    struct ccsds_eulerAngle
    {
	std::string eulerAngleType;
	StringArray comments;
	std::string frameA;
	std::string frameB;
	std::string direction;
	std::string rotationSequence;
	std::string rateFrame;
	Real xAngle, yAngle, zAngle;
	Real xRate, yRate, zRate;
    };
    
    // The CCSDS spin stabilized attitude specification.    
    struct ccsds_spinStabilized
    {
	std::string attitudeType;
	StringArray comments;
	std::string frameA;
	std::string frameB;
	std::string direction;
	Real spinAlpha;
	Real spinDelta;
	Real spinAngle;
	Real spinAngleVelocity;
	Real nutation;
	Real nutationPeriod;
	Real nutationPhase;
    };
    
}


#endif	/* _CCSDSOBTYPE_HPP */

