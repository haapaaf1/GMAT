//$Header$
//------------------------------------------------------------------------------
//                             B3DataFile
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

#ifndef B3DataFile_hpp
#define	B3DataFile_hpp

#include "DataFile.hpp"

class B3DataFile : public DataFile
{

public:
    
    B3DataFile(const std::string &itsName);
    B3DataFile(const B3DataFile &B3df);
    const B3DataFile& operator=(const B3DataFile &B3df);
    ~B3DataFile();

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

#endif	/* _B3DataFile_hpp */

