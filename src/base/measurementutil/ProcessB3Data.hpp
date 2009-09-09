//$Header$
//------------------------------------------------------------------------------
//                             ProcessB3Data
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

#ifndef ProcessB3Data_hpp
#define	ProcessB3Data_hpp

#include "DataFile.hpp"
#include "Obtype.hpp"

class ProcessB3Data : public DataFile
{

public:
    
    ProcessB3Data(const std::string &itsName);
    ~ProcessB3Data();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;
    bool        IsParameterRequired(const Integer id) const;
    bool        IsParameterRequired(const std::string &label) const;
    
    enum DATATYPE_REPS
    {
	RANGERATEONLY_ID = EndDataTypeReps,
        AZEL_ID,
	RAZEL_ID,
	RAZELRR_ID,
	RAZELRR2_ID,
	RADEC_ID,
	RANGEONLY_ID,
	AZELSENSORPOS_ID = 8,
	RADECSENSORPOS_ID,
	EndB3TypeReps    
    };
    
    bool WriteData(B3Obtype *myB3data);
        
    // Measurement Data Access functions
    bool AdvanceToNextOb();
    bool BackUpToPreviousOb();

private:
    
    static const std::string B3_DATATYPE_DESCRIPTIONS[EndB3TypeReps];

    bool GetData(B3Obtype *myB3data);
    
    std::vector<B3Obtype*> b3Data;

    //Current iterator pointing at data
    std::vector<B3Obtype*>::iterator i;

};

#endif	/* _ProcessB3Data_hpp */

