//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSAPMDataFile
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

#ifndef ProcessCCSDSAPMDataFile_hpp
#define	ProcessCCSDSAPMDataFile_hpp

#include "ProcessCCSDSDataFile.hpp"

class ProcessCCSDSAPMDataFile : public ProcessCCSDSDataFile
{

public:

    ProcessCCSDSAPMDataFile(const std::string &itsName);
    ProcessCCSDSAPMDataFile(const ProcessCCSDSAPMDataFile &CCSDSAPMdf);
    const ProcessCCSDSAPMDataFile& operator=(const ProcessCCSDSAPMDataFile &CCSDSAPMdf);
    ~ProcessCCSDSAPMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);

private:

    // Specific data type processing functions
    bool GetCCSDSMetaData(std::string &nextline, CCSDSAPMObType *myOb);
    bool GetCCSDSAPMData(std::string &lff, CCSDSAPMObType *myOb);
    bool GetCCSDSAPMQuaternion(std::string &lff, CCSDSAPMObType *myOb);
    bool GetCCSDSAPMEulerAngle(std::string &lff, CCSDSAPMObType *myOb);
    bool GetCCSDSAPMSpinStabilized(std::string &lff, CCSDSAPMObType *myOb);
    bool GetCCSDSSpacecraftInertia(std::string &lff, CCSDSAPMObType *myOb);
    bool GetCCSDSAttitudeManeuver(std::string &lff, CCSDSAPMObType *myOb);

    Integer requiredNumberQuaternionParameters;
    Integer requiredNumberEulerAngleParameters;
    Integer requiredNumberSpinStabilizedParameters;
    Integer requiredNumberSpacecraftInertiaParameters;
    Integer requiredNumberAttitudeManeuverParameters;


};
#endif	/* _ProcessCCSDSAPMDataFileData_hpp */

