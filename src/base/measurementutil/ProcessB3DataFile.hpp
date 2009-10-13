//$Header$
//------------------------------------------------------------------------------
//                             ProcessB3DataFile
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
 * Implements DataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessB3DataFile_hpp
#define	ProcessB3DataFile_hpp

#include "DataFile.hpp"

class ProcessB3DataFile : public DataFile
{

public:
    
    ProcessB3DataFile(const std::string &itsName);
    ProcessB3DataFile(const ProcessB3DataFile &B3df);
    const ProcessB3DataFile& operator=(const ProcessB3DataFile &B3df);
    ~ProcessB3DataFile();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;
    bool        IsParameterRequired(const Integer id) const;
    bool        IsParameterRequired(const std::string &label) const;
    
    bool GetData(ObType *myB3data);
    bool WriteData(const ObType *myB3data);

private:

    //bool GetData(std::string lff, B3ObType *myB3data);
    
};

#endif	/* _ProcessB3DataFile_hpp */

