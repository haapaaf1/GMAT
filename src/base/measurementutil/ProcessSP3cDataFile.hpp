//$Header$
//------------------------------------------------------------------------------
//                             ProcessSP3cDataFile
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/10/22
//
/**
 *
 * Implements DataFile base class to read files written in the SP3c format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessSP3cDataFile_hpp
#define	ProcessSP3cDataFile_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessSP3cDataFile : public DataFile
{

public:
    
    ProcessSP3cDataFile(const std::string &itsName);
    ~ProcessSP3cDataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();

private:
    
    // Specific data type processing functions
    bool GetData(fstream &theFile);

    bool GetSP3cHeader(std::string firstline, fstream &theFile);
    bool GetSP3cData(std::string &lff, fstream &theFile);

    // Vector containers for the measurement data
    std::vector<sp3c_header*> SP3cHeader;
    std::vector<SP3cObtype*> SP3cData;

    //Current iterator pointing at data
    std::vector<SP3cObtype*>::iterator i;

    //Current iterator pointing at data
    std::vector<sp3c_position*>::iterator i_p;

    //Current iterator pointing at data
    std::vector<sp3c_velocity*>::iterator i_v;

    //Current iterator pointing at data
    std::vector<sp3c_posClockCorrelation*>::iterator i_ep;

    //Current iterator pointing at data
    std::vector<sp3c_velClockRateCorrelation*>::iterator i_ev;

    //Current iteratory pointing at header
    std::vector<sp3c_header*>::iterator i_h;

};
#endif	/* _ProcessSP3cDataFile_hpp */

