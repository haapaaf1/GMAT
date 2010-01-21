//$Header$
//------------------------------------------------------------------------------
//                             APMCCSDSDataFile
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/08/30
//
/**
 *
 * Implements DataFile base class to read files written in CCSDS tracking
 * data message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef APMCCSDSDataFile_hpp
#define	APMCCSDSDataFile_hpp

#include "CCSDSDataFile.hpp"

class APMCCSDSDataFile : public CCSDSDataFile
{

public:

    APMCCSDSDataFile(const std::string &itsName);
    APMCCSDSDataFile(const APMCCSDSDataFile &CCSDSAPMdf);
    const APMCCSDSDataFile& operator=(const APMCCSDSDataFile &CCSDSAPMdf);
    virtual ~APMCCSDSDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);

private:

    // Specific data type ing functions
    bool GetCCSDSMetaData(std::string &nextline, APMCCSDSObType *myOb);
    bool GetCCSDSAPMData(std::string &lff, APMCCSDSObType *myOb);
    bool GetAPMQuaternionCCSDSData(std::string &lff, APMCCSDSObType *myOb);
    bool GetAPMEulerAngleCCSDSData(std::string &lff, APMCCSDSObType *myOb);
    bool GetAPMSpinStabilizedCCSDSData(std::string &lff, APMCCSDSObType *myOb);
    bool GetSpacecraftInertiaCCSDSData(std::string &lff, APMCCSDSObType *myOb);
    bool GetAttitudeManeuverCCSDSData(std::string &lff, APMCCSDSObType *myOb);

    Integer requiredNumberQuaternionParameters;
    Integer requiredNumberEulerAngleParameters;
    Integer requiredNumberSpinStabilizedParameters;
    Integer requiredNumberSpacecraftInertiaParameters;
    Integer requiredNumberAttitudeManeuverParameters;


};
#endif	/* _APMCCSDSDataFileData_hpp */

