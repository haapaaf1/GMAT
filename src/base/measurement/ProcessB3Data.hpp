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
 * Implements ProcessDataFile base class to read files written in the B3 format.
 *
 */
//------------------------------------------------------------------------------

#ifndef ProcessB3Data_hpp
#define	ProcessB3Data_hpp

#include "gmatdefs.hpp"
#include <pcrecpp.h>
#include "RealUtilities.hpp"
#include "ProcessDataFile.hpp"

class ProcessB3Data : ProcessDataFile
{

public:
    
    ProcessB3Data();
    ~ProcessB3Data();

    // Override generic data call from base class
    bool GetData(b3_obtype &myB3data);
    // Specific data type processing functions
    bool GetB3Data(std::string &lff, b3_obtype &myB3data);
    

    enum B3_TYPE_REPS {
	RANGERATEONLY_ID = 1,
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

   
    static const Integer NUM_B3_TYPES = 9;
    static const std::string B3_TYPE_DESCRIPTIONS[NUM_B3_TYPES];    

};

#endif	/* _ProcessB3Data_hpp */

