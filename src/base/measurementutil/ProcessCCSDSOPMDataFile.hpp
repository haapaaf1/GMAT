//$Header$
//------------------------------------------------------------------------------
//                             ProcessCCSDSOPM
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
 * Implements DataFile base class to read files written in CCSDS orbit
 * parameter message format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessCCSDSOPM_hpp
#define	ProcessCCSDSOPM_hpp

#include "DataFile.hpp"

class ProcessCCSDSOPM : public DataFile
{

public:
    
    ProcessCCSDSOPM(const std::string &itsName);
    ~ProcessCCSDSOPM();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    // Measurement Data Access functions
    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();

protected:

    // Specific data type processing functions
    bool GetData(fstream &theFile);

    bool GetSP3cHeader(std::string firstline, fstream &theFile);
    bool GetSP3cData(std::string &lff, fstream &theFile);

    // Vector containers for the measurement data
    std::vector<ccsds_header*> ccsdsOPMHeader;
    std::vector<ccsds_opm_obtype*> ccsdsOPMData;

    //Current iterator pointing at data
    std::vector<ccsds_opm_obtype*>::iterator i;

    //Current iteratory pointing at header
    std::vector<ccsds_header*>::iterator i_h;

};
#endif	/* _ProcessCCSDSOPMData_hpp */

