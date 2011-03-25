//$Header$
//------------------------------------------------------------------------------
//                             OPMCCSDSDataFile
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

#ifndef OPMCCSDSDataFile_hpp
#define	OPMCCSDSDataFile_hpp

#include "CCSDSDataFile.hpp"

class OPMCCSDSDataFile : public CCSDSDataFile
{

public:

    OPMCCSDSDataFile(const std::string &itsName);
    OPMCCSDSDataFile(const OPMCCSDSDataFile &CCSDSOPMdf);
    const OPMCCSDSDataFile& operator=(const OPMCCSDSDataFile &CCSDSOPMdf);
    virtual ~OPMCCSDSDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool GetData(ObType *myOb);

    friend class OPMCCSDSMetaData;
    friend class OPMStateVectorCCSDSData;
    friend class KeplerianElementsCCSDSData;
    friend class SpacecraftParametersCCSDSData;
    friend class ManeuverCCSDSData;

private:

    // Specific data type ing functions
    bool GetCCSDSMetaData(std::string &lff, OPMCCSDSObType *myOb);
    bool GetCCSDSOPMData(std::string &lff, OPMCCSDSObType *myOb);

    bool GetOPMStateVectorCCSDSData(std::string &lff, OPMCCSDSObType *myOb);
    bool GetKeplerianElementsCCSDSData(std::string &lff, OPMCCSDSObType *myOb);
    bool GetSpacecraftParametersCCSDSData(std::string &lff, OPMCCSDSObType *myOb);
    bool GetManeuverCCSDSData(std::string &lff, OPMCCSDSObType *myOb);

    Integer requiredNumberStateVectorParameters;
    Integer requiredNumberKeplerianElementsParameters;
    Integer requiredNumberSpacecraftParameters;
    Integer requiredNumberManeuverParameters;

};
#endif	/* _OPMCCSDSDataFileData_hpp */

