//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOPMDataFile
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

#ifndef ProcessCCSDSOPMDataFile_hpp
#define	ProcessCCSDSOPMDataFile_hpp

#include "ProcessCCSDSDataFile.hpp"

class ProcessCCSDSOPMDataFile : public ProcessCCSDSDataFile
{

public:

    ProcessCCSDSOPMDataFile(const std::string &itsName);
    ProcessCCSDSOPMDataFile(const ProcessCCSDSOPMDataFile &CCSDSOPMdf);
    const ProcessCCSDSOPMDataFile& operator=(const ProcessCCSDSOPMDataFile &CCSDSOPMdf);
    ~ProcessCCSDSOPMDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);

    friend class CCSDSOPMMetaData;
    friend class CCSDSOPMStateVector;
    friend class CCSDSKeplerianElements;
    friend class CCSDSSpacecraftParameters;
    friend class CCSDSManeuvers;

private:

    // Specific data type processing functions
    bool GetCCSDSMetaData(std::string &lff, CCSDSOPMObType *myOb);
    bool GetCCSDSOPMData(std::string &lff, CCSDSOPMObType *myOb);

    bool GetCCSDSOPMStateVector(std::string &lff, CCSDSOPMObType *myOb);
    bool GetCCSDSKeplerianElements(std::string &lff, CCSDSOPMObType *myOb);
    bool GetCCSDSSpacecraftParameters(std::string &lff, CCSDSOPMObType *myOb);
    bool GetCCSDSManeuver(std::string &lff, CCSDSOPMObType *myOb);

    Integer requiredNumberStateVectorParameters;
    Integer requiredNumberKeplerianElementsParameters;
    Integer requiredNumberSpacecraftParameters;
    Integer requiredNumberManeuverParameters;

};
#endif	/* _ProcessCCSDSOPMDataFileData_hpp */

