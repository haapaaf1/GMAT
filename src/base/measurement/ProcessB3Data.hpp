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

#include "GmatBase.hpp"
#include "gmatdefs.hpp"
#include <pcrecpp.h>
#include "RealUtilities.hpp"
#include "DataFile.hpp"

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

    const std::string* GetB3TypeDescriptions() const;
    std::string GetB3TypeNameText(const Integer &id) const;

    bool GetNextOb(b3_obtype *myB3);
    
    enum B3_TYPE_REPS {
	RANGERATEONLY_ID,
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
  
private:

    bool GetData(std::ifstream &theFile, b3_obtype *myB3data);

    // Specific data type processing functions
    bool ExtractB3Data(std::string &lff, b3_obtype *myB3data);
    
    static const std::string B3_TYPE_DESCRIPTIONS[EndB3TypeReps];
    std::vector<b3_obtype*> b3Data;

    //Current iterator pointing at data
    std::vector<b3_obtype*>::iterator i;

};

#endif	/* _ProcessB3Data_hpp */

