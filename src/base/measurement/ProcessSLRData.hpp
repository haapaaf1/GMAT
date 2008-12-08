//$Header$
//------------------------------------------------------------------------------
//                             ProcessSLRData
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

#ifndef ProcessSLRData_hpp
#define	ProcessSLRData_hpp

#include "gmatdefs.hpp"
#include "RealUtilities.hpp"
#include "DataFile.hpp"

class ProcessSLRData : public DataFile
{

public:
    
    ProcessSLRData(const std::string &itsName);
    ~ProcessSLRData();

    // Initialization happens here
    bool Initialize();

    GmatBase *Clone() const;
    bool        IsParameterReadOnly(const Integer id) const;
    bool        IsParameterReadOnly(const std::string &label) const;

    std::string Ilrs2Cospar(std::string ilrsSatnum);

    bool GetNextOb(slr_obtype *mySLR);
   
private:    

    // Specific data type processing functions
    bool FindSLRHeaderLine( std::ifstream &theFile,
                            slr_header *mySLRheader, Integer &flag );
    bool GetData(std::ifstream &theFile, slr_header *mySLRheader,
                 slr_obtype *mySLRdata);


    bool GetSLRHeader(std::string &lff, slr_header *mySLRheader);
    bool GetSLRData(std::string &lff, slr_header *mySLRheader,
                    slr_obtype *mySLRdata);

    std::vector<slr_header> slrHeader;
    std::vector<slr_obtype> slrData;

    //Current iterator pointing at data
    std::vector<slr_obtype>::const_iterator *i;

    //Current iteratory pointing at header
    std::vector<slr_header>::const_iterator *i_h;


};

#endif	/* _ProcessSLRData_hpp */

